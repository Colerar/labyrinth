#include "labyrinth/GlobalsEncryption.h"
#include "effolkronium/random.hpp"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/BLAKE3.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"

#include <ranges>

using Random = effolkronium::random_thread_local;

using llvm::BLAKE3;
using llvm::Constant, llvm::ConstantInt, llvm::ConstantArray,
    llvm::ConstantDataArray, llvm::Type, llvm::ArrayType, llvm::StructType,
    llvm::IntegerType, llvm::Function, llvm::FunctionType, llvm::BasicBlock,
    llvm::IRBuilder, llvm::GlobalVariable;
using llvm::errs, llvm::outs, llvm::toHex;
using llvm::Module, llvm::ModuleAnalysisManager, llvm::PreservedAnalyses;

namespace labyrinth {

GlobalsEncryptionPass::GlobalsEncryptionPass(bool only_str, uint8_t obf_time)
    : only_str(only_str), obf_time(obf_time) {}

auto GlobalsEncryptionPass::run(Module &M, ModuleAnalysisManager &AM)
    -> PreservedAnalyses {
  for (uint8_t priority : std::views::iota(0U, obf_time)) {
    for (auto &gv : M.getGlobalList()) {
      bool is_int_ty = gv.getValueType()->isIntegerTy();
      bool is_arr_ty = gv.getValueType()->isArrayTy();
      bool is_struct_ty = gv.getValueType()->isStructTy();
      if (!gv.hasInitializer())
        continue;
      if (gv.getSection() == "llvm.metadata")
        continue;

#if LLVM_VERSION_MAJOR <= 15
      if (only_str && !gv.getName().startswith(".str"))
        continue;
#elif LLVM_VERSION_MAJOR == 16
      if (only_str && !gv.getName().starts_with(".str"))
        continue;
#elif
#error "Unsupported LLVM Version"
#endif

      auto *init = gv.getInitializer();
      // zero initializer
      if (llvm::isa<llvm::ConstantAggregateZero>(init)) {
        continue;
      }

      if (is_int_ty) {
        auto *data = cast<ConstantInt>(init);
        uint64_t mask = data->getType()->getBitMask();
        uint64_t key = Random::get<uint64_t>() & mask;
        auto *encrypted =
            ConstantInt::get(gv.getValueType(), data->getZExtValue() ^ key);
        gv.setInitializer(encrypted);
        gv.setConstant(false);
        insertIntDecryption(M, &gv, key, priority);
      } else if (is_arr_ty) {
        if (!isa<ConstantDataArray>(init)) {
          continue;
        }
        auto *data = cast<ConstantDataArray>(init);
        if (!isa<ArrayType>(gv.getValueType())) {
          continue;
        }
        auto *arr_ty = cast<ArrayType>(gv.getValueType());
        if (!isa<IntegerType>(arr_ty->getElementType())) {
          continue;
        }
        auto *ele_ty = cast<IntegerType>(arr_ty->getArrayElementType());
        auto keys = rand_with_ty<8>(ele_ty);
        if (!xor_bit_width_with_keys(ele_ty->getBitWidth(),
                                     data->getRawDataValues(), keys)) {
          errs() << "GlobalEncryption unsupported integer width: "
                 << ele_ty->getBitWidth() << "\n";
          continue;
        }
        gv.setConstant(false);
        insertArrayDecryption(M, &gv, arr_ty, ele_ty, keys, priority);
      } else if (is_struct_ty) {
        // Rust str and arr
        auto *data = cast<llvm::ConstantStruct>(init);
        auto struct_ty = data->getType();
        if (struct_ty->getNumElements() != 1) {
          continue;
        }
        auto first = data->getAggregateElement(0U);
        if (!isa<ConstantDataArray>(first)) {
          continue;
        }
        auto *inner_arr = cast<ConstantDataArray>(first);
        if (!isa<IntegerType>(inner_arr->getElementType())) {
          continue;
        }
        auto *inner_ele_ty = cast<IntegerType>(inner_arr->getElementType());

        auto keys = rand_with_ty<8>(inner_ele_ty);
        if (!xor_bit_width_with_keys(inner_ele_ty->getBitWidth(),
                                     inner_arr->getRawDataValues(), keys)) {
          errs() << "GlobalEncryption unsupported integer width: "
                 << inner_ele_ty->getBitWidth() << "\n";
          continue;
        }

        gv.setConstant(false);
        insertArrayDecryption(M, &gv, inner_arr->getType(), inner_ele_ty, keys,
                              priority);
      }
    }
  }
  return PreservedAnalyses::all();
}

std::string genHashedName(GlobalVariable *gv, int priority) {
  auto m = gv->getParent();
  BLAKE3 blake_3;
  blake_3.init();
  blake_3.update(m->getName());
  blake_3.update(gv->getName());
  blake_3.update(m->getMDKindID(gv->getName()));
  blake_3.update(priority);
  auto digest = blake_3.final();
  return toHex(digest);
}

void insertIntDecryption(Module &M, GlobalVariable *gv, uint64_t key,
                         int priority) {
  auto &cx = M.getContext();
  auto *fn_ty = FunctionType::get(Type::getVoidTy(M.getContext()), false);
  auto fn_name = genHashedName(gv, priority);
  auto callee = M.getOrInsertFunction(fn_name, fn_ty);
  auto *fn = cast<Function>(callee.getCallee());

  auto ir = IRBuilder(cx);

  auto *body = BasicBlock::Create(cx, "body", fn);
  ir.SetInsertPoint(body);
  auto *encrypted = ir.CreateLoad(gv->getValueType(), gv);
  auto *decrypted =
      ir.CreateXor(encrypted, ConstantInt::get(gv->getValueType(), key));
  ir.CreateStore(decrypted, gv);
  ir.CreateRetVoid();

  appendToGlobalCtors(M, fn, priority);
}

void insertArrayDecryption(llvm::Module &M, llvm::GlobalVariable *gv,
                           llvm::ArrayType *arr_ty, llvm::IntegerType *ele_ty,
                           std::array<uint64_t, 8> &keys, int priority) {
  auto &cx = M.getContext();
  auto ir = IRBuilder(cx);

  auto i32_ty = IntegerType::getInt32Ty(cx);

  auto callee =
      M.getOrInsertFunction(labyrinth::genHashedName(gv, priority),
                            FunctionType::get(Type::getVoidTy(cx), false));
  auto *fn = cast<Function>(callee.getCallee());

  auto entry = BasicBlock::Create(cx, "entry", fn);
  auto for_cond = BasicBlock::Create(cx, "for.cond", fn);
  auto for_body = BasicBlock::Create(cx, "for.body", fn);
  auto ret = BasicBlock::Create(cx, "ret", fn);

  auto const_i32_0 = ConstantInt::get(i32_ty, 0);
  auto const_i32_1 = ConstantInt::get(i32_ty, 1);
  auto const_i32_8 = ConstantInt::get(i32_ty, 8);

  // start:
  ir.SetInsertPoint(entry);
  auto get_key = [&](std::size_t idx) {
    return ConstantInt::get(ele_ty, keys[idx]);
  };
  std::array<Constant *, 8> const_keys{
      get_key(0), get_key(1), get_key(2), get_key(3),
      get_key(4), get_key(5), get_key(6), get_key(7),
  };
  auto const_key_arr =
      ConstantArray::get(ArrayType::get(ele_ty, 8), const_keys);

  auto keys_arr_ptr = ir.CreateAlloca(ele_ty, const_i32_8, "keys_arr_ptr");
  ir.CreateStore(const_key_arr, keys_arr_ptr, "keys_arr_ptr");
  auto idx_ptr = ir.CreateAlloca(i32_ty, const_i32_1, "idx_ptr");
  ir.CreateStore(const_i32_0, idx_ptr);

  ir.CreateBr(for_cond);

  // for.cond:
  ir.SetInsertPoint(for_cond);
  auto idx_load = ir.CreateLoad(i32_ty, idx_ptr);
  auto cmp_inst = ir.CreateICmpULT(
      idx_load, ConstantInt::get(i32_ty, arr_ty->getNumElements()));
  ir.CreateCondBr(cmp_inst, for_body, ret);

  // for.body:
  ir.SetInsertPoint(for_body);
  auto idx = ir.CreateLoad(i32_ty, idx_ptr, "idx");

  llvm::Value *arr_ele_ptr;
  if (gv->getValueType()->isStructTy()) {
    auto struct_ptr = ir.CreateGEP(gv->getValueType(), gv,
                                   {const_i32_0, const_i32_0}, "struct_ptr");
    arr_ele_ptr =
        ir.CreateGEP(arr_ty, struct_ptr, {const_i32_0, idx}, "arr_ele_ptr");
  } else {
    arr_ele_ptr = ir.CreateGEP(ele_ty, gv, {idx}, "arr_ele_ptr");
  }
  auto gv_ele = ir.CreateLoad(ele_ty, arr_ele_ptr, "gv_ele");

  auto key_idx = ir.CreateSRem(idx, const_i32_8, "key_idx");
  auto key_ptr = ir.CreateGEP(ele_ty, keys_arr_ptr, {key_idx}, "key_ptr");
  auto key = ir.CreateLoad(ele_ty, key_ptr);

  auto decrypted = ir.CreateXor(key, gv_ele, "decrypted");
  ir.CreateStore(decrypted, arr_ele_ptr);

  auto sum = ir.CreateAdd(idx, const_i32_1);
  ir.CreateStore(sum, idx_ptr);
  ir.CreateBr(for_cond);

  // ret
  ir.SetInsertPoint(ret);
  ir.CreateRetVoid();

  appendToGlobalCtors(M, fn, priority);
}

} // namespace labyrinth
