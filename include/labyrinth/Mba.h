#ifndef LABYRINTH_MBA_H
#define LABYRINTH_MBA_H

#include "llvm/IR/Constants.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Transforms/Utils/Local.h"

namespace labyrinth {

class MbaPass : public llvm::PassInfoMixin<MbaPass> {
public:
  uint8_t times = 1;
  uint8_t prob = 40;
  uint8_t terms_num = 10;
  MbaPass(uint8_t times, uint8_t prob, uint8_t terms_num);
  llvm::PreservedAnalyses run(llvm::Module &M,
                              llvm::ModuleAnalysisManager &AM) const;

  void obf_fn(llvm::Function &fn) const;
  void substituteConstant(llvm::Instruction *I, int i) const;

  void substitute(llvm::BinaryOperator *BI) const;
  llvm::Value *substituteAdd(llvm::BinaryOperator *BI) const;
  llvm::Value *substituteSub(llvm::BinaryOperator *BI) const;
  llvm::Value *substituteAnd(llvm::BinaryOperator *BI) const;
  llvm::Value *substituteOr(llvm::BinaryOperator *BI) const;
  llvm::Value *substituteXor(llvm::BinaryOperator *BI) const;
};

} // namespace labyrinth

#endif // LABYRINTH_MBA_H
