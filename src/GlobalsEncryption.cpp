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
    llvm::ConstantDataArray, llvm::Type, llvm::ArrayType, llvm::IntegerType,
    llvm::Function, llvm::FunctionType, llvm::BasicBlock, llvm::IRBuilder,
    llvm::GlobalVariable;
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
      if (!is_arr_ty && !is_int_ty)
        continue;
      if (!gv.hasInitializer())
        continue;
      if (gv.getSection() == "llvm.metadata")
        continue;
      if (only_str && !gv.getName().starts_with(".str"))
        continue;
      auto *init = gv.getInitializer();

      if (is_int_ty) {
        auto *data = cast<ConstantInt>(init);
        uint64_t mask = data->getType()->getBitMask();
        uint64_t key = Random::get<uint64_t>() & mask;
        auto *encrypted =
            ConstantInt::get(gv.getValueType(), data->getZExtValue() ^ key);
        gv.setInitializer(encrypted);
        gv.setConstant(false);
        insertIntDecryption(M, &gv, key, priority);
      } else if (isa<ConstantDataArray>(init) &&
                 isa<ArrayType>(gv.getValueType()) &&
                 isa<IntegerType>(gv.getValueType()->getArrayElementType())) {
        auto *data = cast<ConstantDataArray>(init);
        auto *gv_ele_ty =
            cast<IntegerType>(gv.getValueType()->getArrayElementType());
        uint64_t mask = gv_ele_ty->getBitMask();
        auto rand = [&]() { return Random::get<uint64_t>() & mask; };
        std::array<uint64_t, 8> keys = {
            rand(), rand(), rand(), rand(), rand(), rand(), rand(), rand(),
        };
        auto raw_data = data->getRawDataValues();

        switch (gv_ele_ty->getBitWidth()) {
        case 8:
          labyrinth::xor_with_keys<uint8_t>(raw_data, keys);
          break;
        case 16:
          labyrinth::xor_with_keys<uint16_t>(raw_data, keys);
          break;
        case 32:
          labyrinth::xor_with_keys<uint32_t>(raw_data, keys);
          break;
        case 64:
          labyrinth::xor_with_keys<uint64_t>(raw_data, keys);
          break;
        default:
          errs() << "GlobalEncryption unsupported integer width: "
                 << gv_ele_ty->getBitWidth() << "\n";
          return PreservedAnalyses::all();
        }

        gv.setConstant(false);
        insertArrayDecryption(M, &gv, gv_ele_ty, keys, priority);
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
                           llvm::IntegerType *gv_ele_ty,
                           std::array<uint64_t, 8> &keys, int priority) {
  auto &cx = M.getContext();
  auto ir = IRBuilder(cx);

  auto *gv_ty = cast<ArrayType>(gv->getValueType());
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
    return ConstantInt::get(gv_ele_ty, keys[idx]);
  };
  std::array<Constant *, 8> const_keys{
      get_key(0), get_key(1), get_key(2), get_key(3),
      get_key(4), get_key(5), get_key(6), get_key(7),
  };
  auto const_key_arr =
      ConstantArray::get(ArrayType::get(gv_ele_ty, 8), const_keys);

  auto keys_arr_ptr = ir.CreateAlloca(gv_ele_ty, const_i32_8, "keys_arr_ptr");
  ir.CreateStore(const_key_arr, keys_arr_ptr, "keys_arr_ptr");
  auto idx_ptr = ir.CreateAlloca(i32_ty, const_i32_1, "idx_ptr");
  ir.CreateStore(const_i32_0, idx_ptr);

  ir.CreateBr(for_cond);

  // for.cond:
  ir.SetInsertPoint(for_cond);
  auto idx_load = ir.CreateLoad(i32_ty, idx_ptr);
  auto cmp_inst =
      ir.CreateICmpULT(idx_load, ConstantInt::get(IntegerType::getInt32Ty(cx),
                                                  gv_ty->getNumElements()));
  ir.CreateCondBr(cmp_inst, for_body, ret);

  // for.body:
  ir.SetInsertPoint(for_body);
  auto idx = ir.CreateLoad(i32_ty, idx_ptr, "idx");

  auto gv_ele_ptr = ir.CreateGEP(gv_ele_ty, gv, {idx}, "gv_ele_ptr");
  auto gv_ele = ir.CreateLoad(gv_ele_ty, gv_ele_ptr, "gv_ele");

  auto key_idx = ir.CreateSRem(idx, const_i32_8, "key_idx");
  auto key_ptr = ir.CreateGEP(gv_ele_ty, keys_arr_ptr, {key_idx}, "key_ptr");
  auto key = ir.CreateLoad(gv_ele_ty, key_ptr);

  auto decrypted = ir.CreateXor(key, gv_ele, "decrypted");
  ir.CreateStore(decrypted, gv_ele_ptr);

  auto sum = ir.CreateAdd(idx, const_i32_1);
  ir.CreateStore(sum, idx_ptr);
  ir.CreateBr(for_cond);

  // ret
  ir.SetInsertPoint(ret);
  ir.CreateRetVoid();

  appendToGlobalCtors(M, fn, priority);
}

} // namespace labyrinth
