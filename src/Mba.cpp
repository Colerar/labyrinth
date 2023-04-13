#include "labyrinth/Mba.h"
#include "effolkronium/random.hpp"
#include "labyrinth/MbaUtils.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instructions.h"

using llvm::AllocaInst, llvm::SwitchInst, llvm::LoadInst, llvm::BranchInst,
    llvm::InvokeInst, llvm::SelectInst, llvm::ReturnInst, llvm::Instruction,
    llvm::StoreInst, llvm::CmpInst;
using llvm::BinaryOperator;
using llvm::Constant, llvm::ConstantInt, llvm::ConstantArray,
    llvm::ConstantDataArray, llvm::Type, llvm::ArrayType, llvm::StructType,
    llvm::IntegerType, llvm::Function, llvm::FunctionType, llvm::BasicBlock,
    llvm::GlobalVariable, llvm::Value;
using llvm::Instruction;
using llvm::SmallVector;

using Random = effolkronium::random_thread_local;

namespace labyrinth {

MbaPass::MbaPass(uint8_t times, uint8_t prob, uint8_t terms_num)
    : times(times), prob(prob), terms_num(terms_num) {}

llvm::PreservedAnalyses MbaPass::run(llvm::Module &M,
                                     llvm::ModuleAnalysisManager &AM) const {
  if (M.getFunctionList().empty()) {
    return llvm::PreservedAnalyses::all();
  }
  for (int i = 0; i < times; i++) {
    for (auto &fn : M.getFunctionList()) {
      obf_fn(fn);
    }
  }

  return llvm::PreservedAnalyses::none();
}

void MbaPass::obf_fn(llvm::Function &fn) const {
  for (auto &block : fn) {
    if (block.empty())
      continue;
    SmallVector<Instruction *, 16> origInst;
    for (Instruction &I : block) {
      origInst.push_back(&I);
    }
    for (Instruction *I : origInst) {
      if (isa<BinaryOperator>(I)) {
        auto *BI = cast<BinaryOperator>(I);

        if (BI->getOperand(0)->getType()->isIntegerTy() &&
            Random::get<bool>(prob / 100.0)) {
          // Do not support 128-bit integers now
          if (BI->getOperand(0)->getType()->getIntegerBitWidth() > 64) {
            continue;
          }
          substitute(BI);
        }
      } else {
        for (int j = 0; j < I->getNumOperands(); j++) {
          if (I->getOperand(0)->getType()->isIntegerTy() &&
              Random::get<bool>(prob / 100.0)) {
            if (isa<StoreInst>(I) || isa<CmpInst>(I)) {
              substituteConstant(I, j);
            }
          }
        }
      }
    }
  }
}

void MbaPass::substituteConstant(Instruction *I, int i) const {
  auto *val = dyn_cast<ConstantInt>(I->getOperand(i));
  if (val && val->getBitWidth() <= 64) {
    auto terms = generateLinearMBA(terms_num);
    // NOLINTNEXTLINE, we've checked bit width before
    terms[14] -= val->getValue().getZExtValue();
    Value *mbaExpr = insertLinearMBA(terms, I);
    if (val->getBitWidth() <= 32) {
      mbaExpr = insertPolynomialMBA(mbaExpr, I);
    }
    I->setOperand(i, mbaExpr);
  }
}

void MbaPass::substitute(BinaryOperator *BI) const {
  Value *mbaExpr = nullptr;
  switch (BI->getOpcode()) {
  case BinaryOperator::Add:
    mbaExpr = substituteAdd(BI);
    break;
  case BinaryOperator::Sub:
    mbaExpr = substituteSub(BI);
    break;
  case BinaryOperator::And:
    mbaExpr = substituteAnd(BI);
    break;
  case BinaryOperator::Or:
    mbaExpr = substituteOr(BI);
    break;
  case BinaryOperator::Xor:
    mbaExpr = substituteXor(BI);
    break;
  default:
    break;
  }
  if (mbaExpr) {
    if (BI->getOperand(0)->getType()->getIntegerBitWidth() <= 32) {
      mbaExpr = insertPolynomialMBA(mbaExpr, BI);
    }
    BI->replaceAllUsesWith(mbaExpr);
  }
}

Value *MbaPass::substituteAdd(BinaryOperator *BI) const {
  auto terms = generateLinearMBA(terms_num);
  terms[2] += 1;
  terms[4] += 1;
  return insertLinearMBA(terms, BI);
}

Value *MbaPass::substituteSub(BinaryOperator *BI) const {
  auto terms = generateLinearMBA(terms_num);
  terms[2] += 1;
  terms[4] -= 1;
  return insertLinearMBA(terms, BI);
}

Value *MbaPass::substituteXor(BinaryOperator *BI) const {
  auto terms = generateLinearMBA(terms_num);
  terms[5] += 1;
  return insertLinearMBA(terms, BI);
}

Value *MbaPass::substituteAnd(BinaryOperator *BI) const {
  auto terms = generateLinearMBA(terms_num);
  terms[0] += 1;
  return insertLinearMBA(terms, BI);
}

Value *MbaPass::substituteOr(BinaryOperator *BI) const {
  auto terms = generateLinearMBA(terms_num);
  terms[6] += 1;
  return insertLinearMBA(terms, BI);
}

} // namespace labyrinth
