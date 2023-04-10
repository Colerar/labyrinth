#include "labyrinth/Flattening.h"

#include "effolkronium/random.hpp"
#include "labyrinth/Utils.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"

#include <array>
#include <cassert>

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
auto FlatteningPass::run(Module &M, ModuleAnalysisManager &AM)
    -> PreservedAnalyses {
  auto update_fn = insert_update_key_fn(M);
  for (auto &fn : M.functions()) {
    if (std::addressof(fn) == update_fn)
      continue;
    flatten(&fn);
  }

  return PreservedAnalyses::all();
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

void FlatteningPass::flatten(Function *f) const {
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
  auto switch_ty = IntegerType::getIntNTy(cx, switch_bit_width);

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
  SmallVector<ConstantInt *, 16> cases(orig_bb.size());
  {
    SmallVector<uint64_t, 16> rands(orig_bb.size());
    for (std::size_t i = 0; i < orig_bb.size(); i++) {
      auto b = orig_bb[i];
      idx_map[b] = i;
      if (switch_bit_width <= 64) {
        uint64_t rand = rand_unique<uint64_t>(rands);
        rands.emplace_back(rand);
        cases[i] = ConstantInt::get(switch_ty, rand & switch_ty->getBitMask());
      } else {
        uint32_t size = ceil(static_cast<double>(switch_bit_width) / 64);
        std::vector<uint64_t> vec(size);
        for (uint64_t &item : vec) {
          uint64_t rand = rand_unique<uint64_t>(rands);
          rands.emplace_back(rand);
          item = rand;
        }
        uint32_t rem = switch_bit_width % 64;
        if (rem != 0) {
          vec.back() = vec.back() & IntegerType::get(cx, rem)->getBitMask();
        }
        cases[i] = cast<ConstantInt>(
            ConstantInt::get(switch_ty, llvm::APInt(switch_bit_width, vec)));
      }
    }
  }
  auto get_case_const = [&](BasicBlock *b) -> ConstantInt * {
    return cases[idx_map[b]];
  };

  // Remove jump
  Instruction *old_term = insert->getTerminator();

  // Create switch variable and set as it
  auto switch_var = new AllocaInst(switch_ty, DL.getAllocaAddrSpace(),
                                   "switch_var", old_term);

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
    switchI->addCase(numCase, i);
  }

  // Recalculate switch_var
  for (BasicBlock *i : orig_bb) {
    // If it's a non-conditional jump
    if (i->getTerminator()->getNumSuccessors() == 1) {
      // Get successor and delete terminator
      BasicBlock *succ = i->getTerminator()->getSuccessor(0);
      i->getTerminator()->eraseFromParent();

      // Get next case
      ConstantInt *case0 = switchI->findCaseDest(succ);

      if (!case0) {
        errs() << "Successor 0 of the terminator is nullptr.";
        std::abort();
      }

      // Update switch_var and jump to the end of loop
      new StoreInst(case0, switch_var, i);
      BranchInst::Create(loop_end, i);
      continue;
    }

    // If it's a conditional jump
    if (i->getTerminator()->getNumSuccessors() == 2) {
      // Get next cases
      auto *case0 = switchI->findCaseDest(i->getTerminator()->getSuccessor(0));
      auto *case1 = switchI->findCaseDest(i->getTerminator()->getSuccessor(1));

      if (!case0) {
        errs() << "Successor 0 of the terminator is nullptr.";
        std::abort();
      }

      // Create a SelectInst
      auto *br = cast<BranchInst>(i->getTerminator());
      SelectInst *sel = SelectInst::Create(br->getCondition(), case0, case1, "",
                                           i->getTerminator());

      // Erase terminator
      i->getTerminator()->eraseFromParent();
      // Update switch_var and jump to the end of loop
      new StoreInst(sel, switch_var, i);
      BranchInst::Create(loop_end, i);
      continue;
    }
  }

  fix_stack(f);
}

FlatteningPass::FlatteningPass(uint32_t switch_bit_width)
    : switch_bit_width(switch_bit_width) {}

} // namespace labyrinth
