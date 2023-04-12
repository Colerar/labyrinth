#include "labyrinth/Flattening.h"

#include "effolkronium/random.hpp"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Transforms/Utils/LowerSwitch.h"

using Random = effolkronium::random_thread_local;

using llvm::AllocaInst, llvm::SwitchInst, llvm::LoadInst, llvm::BranchInst,
    llvm::InvokeInst, llvm::SelectInst, llvm::ReturnInst, llvm::Instruction,
    llvm::StoreInst;
using llvm::Argument, llvm::GlobalValue, llvm::Value, llvm::Constant,
    llvm::ConstantInt, llvm::ConstantArray, llvm::ConstantDataArray, llvm::Type,
    llvm::ArrayType, llvm::StructType, llvm::IntegerType, llvm::Function,
    llvm::FunctionType, llvm::BasicBlock, llvm::IRBuilder, llvm::GlobalVariable;
using llvm::DataLayout;
using llvm::errs, llvm::outs, llvm::toHex;
using llvm::isa, llvm::cast, llvm::dyn_cast;
using llvm::Module, llvm::ModuleAnalysisManager, llvm::PreservedAnalyses;
using llvm::SmallVector;

namespace labyrinth {
auto FlatteningPass::run(Module &M, ModuleAnalysisManager &AM) const
    -> PreservedAnalyses {
  SmallVector<Function *, 16> fns;
  for (auto &fn : M.functions()) {
    fns.emplace_back(&fn);
  }
  if (fns.empty()) {
    return PreservedAnalyses::all();
  }
  for (auto fn : fns) {
    flatten(&M, fn);
  }

  return PreservedAnalyses::none();
}

void fix_stack(Function *F) {
  SmallVector<llvm::PHINode *> origPHI;
  SmallVector<llvm::Instruction *> origReg;
  auto &entryBB = F->getEntryBlock();
  for (auto &BB : *F) {
    for (auto &I : BB) {
      if (auto *PN = dyn_cast<llvm::PHINode>(&I)) {
        origPHI.emplace_back(PN);
      } else if (!(isa<AllocaInst>(&I) && I.getParent() == &entryBB) &&
                 I.isUsedOutsideOfBlock(&BB)) {
        origReg.emplace_back(&I);
      }
    }
  }
  for (llvm::PHINode *PN : origPHI) {
    DemotePHIToStack(PN, entryBB.getTerminator());
  }
  for (llvm::Instruction *I : origReg) {
    DemoteRegToStack(*I, entryBB.getTerminator());
  }
}

Function *insert_update_fn(Module *M, BasicBlock *i,
                           std::unordered_map<BasicBlock *, uint32_t> &idx_map,
                           const SmallVector<BasicBlock *, 8> &doms,
                           IntegerType *switch_ty) {
  auto &cx = M->getContext();
  auto DL = M->getDataLayout();
  auto bool_ty = Type::getIntNTy(cx, 1);
  auto opaque_ptr_ty = llvm::PointerType::get(cx, DL.getAllocaAddrSpace());
  auto i32_ty = IntegerType::getInt32Ty(cx);

  FunctionType *fn_ty = FunctionType::get(Type::getVoidTy(cx),
                                          {
                                              opaque_ptr_ty,
                                              opaque_ptr_ty,
                                              opaque_ptr_ty,
                                          },
                                          false);
  Function *fn = Function::Create(fn_ty, GlobalValue::PrivateLinkage,
                                  "labyrinth_fla_update_key", M);
  auto iter = fn->arg_begin();
  Value *flag_arr = iter, *case_arr = ++iter, *key_arr = ++iter;

  auto *entry = BasicBlock::Create(cx, "entry", fn);
  auto *body = BasicBlock::Create(cx, "body", fn);
  auto *ret = BasicBlock::Create(cx, "ret", fn);

  // entry
  IRBuilder<> ir(entry);
  auto flag_ptr = ir.CreateGEP(
      bool_ty, flag_arr, {ConstantInt::get(i32_ty, idx_map[i])}, "flag_ptr");
  auto flag = ir.CreateLoad(bool_ty, flag_ptr, "flag");
  auto visited = ir.CreateICmpEQ(flag, ConstantInt::get(bool_ty, 1), "visited");
  ir.CreateCondBr(visited, ret, body);

  // body
  ir.SetInsertPoint(body);
  ir.CreateStore(ConstantInt::get(bool_ty, 1), flag_ptr);
  for (auto dom : doms) {
    if (dom == i)
      continue;
    auto dom_idx = ConstantInt::get(i32_ty, idx_map[dom]);
    auto i_idx = ConstantInt::get(i32_ty, idx_map[i]);

    auto case_ptr = ir.CreateGEP(switch_ty, case_arr, {dom_idx}, "case_ptr");
    auto key_ptr = ir.CreateGEP(switch_ty, key_arr, {i_idx}, "key_ptr");

    auto enc_case = ir.CreateLoad(switch_ty, case_ptr, "enc_case");
    auto key = ir.CreateLoad(switch_ty, key_ptr, "key");

    auto dec_case = ir.CreateXor(enc_case, key, "dec_case");
    ir.CreateStore(dec_case, case_ptr);
  }
  ir.CreateBr(ret);

  // ret
  ir.SetInsertPoint(ret);
  ir.CreateRetVoid();
  return fn;
}

void FlatteningPass::flatten(Module *M, Function *f) const {
  SmallVector<BasicBlock *> orig_bb;

  // skip exception block
  for (BasicBlock &b : *f) {
    if (b.isEHPad() || b.isLandingPad()) {
      return;
    }
    if (!isa<BranchInst>(b.getTerminator()) &&
        !isa<ReturnInst>(b.getTerminator())) {
      return;
    }
    orig_bb.emplace_back(&b);
  }

  const DataLayout &DL = f->getParent()->getDataLayout();
  auto &cx = f->getContext();
  auto switch_ty = IntegerType::getIntNTy(cx, bit_width);
  auto bool_ty = IntegerType::getIntNTy(cx, 1);

  llvm::PassBuilder pb;
  llvm::FunctionAnalysisManager fam;
  llvm::FunctionPassManager fpm;
  pb.registerFunctionAnalyses(fam);
  fpm.addPass(llvm::LowerSwitchPass());
  fpm.run(*f, fam);

  // Nothing to flatten
  if (orig_bb.size() <= 1)
    return;

  // Remove first BB
  orig_bb.erase(orig_bb.begin());

  // Get a pointer on the first BB
  Function::iterator tmp = f->begin();
  BasicBlock *insert = &*tmp;

  {
    // If main begin with an if
    auto *br = dyn_cast<BranchInst>(insert->getTerminator());

    if ((br && br->isConditional()) ||
        insert->getTerminator()->getNumSuccessors() > 1) {
      BasicBlock::iterator i = insert->end();
      --i;

      if (insert->size() > 1) {
        --i;
      }

      auto *tmp_bb = insert->splitBasicBlock(i, "first");
      orig_bb.insert(orig_bb.begin(), tmp_bb);
    }
  }

  std::unordered_map<BasicBlock *, uint32_t> idx_map(orig_bb.size());
  SmallVector<Constant *, 16> cases(orig_bb.size());
  SmallVector<Constant *, 16> keys(orig_bb.size());
  {
    SmallVector<uint64_t, 16> rands(orig_bb.size());
    for (std::size_t i = 0; i < orig_bb.size(); i++) {
      auto b = orig_bb[i];
      idx_map[b] = i;
      uniq_int_rands<16, 16>(bit_width, rands, cases, switch_ty, i);
      uniq_int_rands<16, 16>(bit_width, rands, keys, switch_ty, i);
    }
  }
  SmallVector<Constant *, 16> enc_cases(cases);
  llvm::DominatorTree tree(*f);
  for (auto i : orig_bb) {
    for (auto j : orig_bb) {
      if (i == j || !tree.dominates(i, j))
        continue;
      enc_cases[idx_map[j]] = ConstantInt::get(
          switch_ty, cast<ConstantInt>(enc_cases[idx_map[j]])->getValue() ^
                         cast<ConstantInt>(keys[idx_map[i]])->getValue());
    }
  }
  auto get_case_const = [&](BasicBlock *b) -> Constant * {
    return cases[idx_map[b]];
  };

  // Remove jump
  Instruction *old_term = insert->getTerminator();

  // Create switch variable and set as it
  auto switch_var = new AllocaInst(switch_ty, DL.getAllocaAddrSpace(),
                                   "switch_var", old_term);

  auto switch_arr_ty = ArrayType::get(switch_ty, keys.size());
  auto key_arr = new AllocaInst(switch_arr_ty, DL.getAllocaAddrSpace(),
                                "key_arr", old_term);
  auto enc_case_arr = new AllocaInst(switch_arr_ty, DL.getAllocaAddrSpace(),
                                     "enc_case_arr", old_term);
  auto visited_arr =
      new AllocaInst(ArrayType::get(bool_ty, keys.size()),
                     DL.getAllocaAddrSpace(), "visited_arr", old_term);
  auto keys_const =
      ConstantArray::get(ArrayType::get(switch_ty, keys.size()), keys);
  new StoreInst(keys_const, key_arr, old_term);

  auto enc_cases_arr_const = ConstantArray::get(
      ArrayType::get(switch_ty, enc_cases.size()), enc_cases);
  new StoreInst(enc_cases_arr_const, enc_case_arr, old_term);

  SmallVector<Constant *, 16> zeros(keys.size());
  std::fill_n(zeros.begin(), keys.size(), ConstantInt::get(bool_ty, 0));
  new StoreInst(ConstantArray::get(ArrayType::get(bool_ty, keys.size()), zeros),
                visited_arr, old_term);

  // Remove jump
  old_term->eraseFromParent();

  new StoreInst(cases[0], switch_var, insert);

  // Create main loop
  auto loop_entry = BasicBlock::Create(cx, "loop_entry", f, insert);
  auto loop_end = BasicBlock::Create(cx, "loop_end", f, insert);

  auto *load = new LoadInst(switch_ty, switch_var, "switch", loop_entry);

  // Move first BB on top
  insert->moveBefore(loop_entry);
  BranchInst::Create(loop_entry, insert);

  // loop_end jump to loop_entry
  BranchInst::Create(loop_entry, loop_end);

  BasicBlock *swDefault = BasicBlock::Create(cx, "default", f, loop_end);
  BranchInst::Create(loop_end, swDefault);

  // Create switch instruction itself and set condition
  auto *switchI = SwitchInst::Create(&*f->begin(), swDefault, 0, loop_entry);
  switchI->setCondition(load);

  // Remove branch jump from 1st BB and make a jump to the while
  f->begin()->getTerminator()->eraseFromParent();

  BranchInst::Create(loop_entry, &*f->begin());

  // Put BB in the switch
  for (BasicBlock *i : orig_bb) {
    // Move the BB inside the switch (only visual, no code logic)
    i->moveBefore(loop_end);

    // Add case to switch
    auto *numCase = get_case_const(i);
    switchI->addCase(cast<ConstantInt>(numCase), i);
  }

  // Recalculate switch_var
  for (BasicBlock *i : orig_bb) {
    SmallVector<BasicBlock *, 8> doms;
    for (BasicBlock *j : orig_bb) {
      if (i == j || !tree.dominates(i, j))
        continue;
      doms.emplace_back(j);
    }

    // If it's a non-conditional jump
    if (i->getTerminator()->getNumSuccessors() == 1) {
      // Get successor and delete terminator
      BasicBlock *succ = i->getTerminator()->getSuccessor(0);
      i->getTerminator()->eraseFromParent();

      if (!doms.empty()) {
        auto fn = insert_update_fn(M, i, idx_map, doms, switch_ty);
        llvm::CallInst::Create(fn,
                               {
                                   visited_arr,
                                   enc_case_arr,
                                   key_arr,
                               },
                               "", i);
      }

      auto idx_const =
          ConstantInt::get(IntegerType::getInt32Ty(cx), idx_map[succ]);
      auto case_ptr = llvm::GetElementPtrInst::Create(
          switch_ty, enc_case_arr, {idx_const}, "case_ptr", i);
      auto dec_case = new llvm::LoadInst(switch_ty, case_ptr, "dec_case", i);

      // Update switch_var and jump to the end of loop
      new StoreInst(dec_case, switch_var, i);

      BranchInst::Create(loop_end, i);
      continue;
    }

    // If it's a conditional jump
    if (i->getTerminator()->getNumSuccessors() == 2) {
      // Get next cases
      auto succ0 = i->getTerminator()->getSuccessor(0);
      auto succ1 = i->getTerminator()->getSuccessor(1);

      auto orig_term = i->getTerminator();
      auto *cond = cast<BranchInst>(orig_term)->getCondition();
      orig_term->eraseFromParent();

      if (!doms.empty()) {
        auto fn = insert_update_fn(M, i, idx_map, doms, switch_ty);
        llvm::CallInst::Create(fn,
                               {
                                   visited_arr,
                                   enc_case_arr,
                                   key_arr,
                               },
                               "", i);
      }

      auto idx0_const =
          ConstantInt::get(IntegerType::getInt32Ty(cx), idx_map[succ0]);
      auto case0_ptr = llvm::GetElementPtrInst::Create(
          switch_ty, enc_case_arr, {idx0_const}, "case0_ptr", i);
      auto dec_case0 = new llvm::LoadInst(switch_ty, case0_ptr, "dec_case0", i);

      auto idx1_const =
          ConstantInt::get(IntegerType::getInt32Ty(cx), idx_map[succ1]);
      auto case1_ptr = llvm::GetElementPtrInst::Create(
          switch_ty, enc_case_arr, {idx1_const}, "case1_ptr", i);
      auto dec_case1 = new llvm::LoadInst(switch_ty, case1_ptr, "dec_case1", i);

      SelectInst *sel = SelectInst::Create(cond, dec_case0, dec_case1, "", i);

      // Update switch_var and jump to the end of loop
      new StoreInst(sel, switch_var, i);
      BranchInst::Create(loop_end, i);
      continue;
    }
  }

  fix_stack(f);
}

FlatteningPass::FlatteningPass(uint32_t switch_bit_width)
    : bit_width(switch_bit_width) {}

} // namespace labyrinth
