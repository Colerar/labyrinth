#ifndef LABYRINTH_FLATTENING_H
#define LABYRINTH_FLATTENING_H

#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Transforms/Utils/Local.h"

namespace labyrinth {
class FlatteningPass : public llvm::PassInfoMixin<FlatteningPass> {
public:
  uint32_t switch_bit_width;
  llvm::PreservedAnalyses run(llvm::Module &M,
                                     llvm::ModuleAnalysisManager &AM) const;
  explicit FlatteningPass(uint32_t switch_bit_width);
  void flatten(llvm::Function *fn) const;
};

void fix_stack(llvm::Function *F);

} // namespace labyrinth

#endif
