#ifndef LABYRINTH_FLATTENING_H
#define LABYRINTH_FLATTENING_H

#include "Utils.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Transforms/Utils/Local.h"

namespace labyrinth {
class FlatteningPass : public llvm::PassInfoMixin<FlatteningPass> {
public:
  uint32_t bit_width;
  llvm::PreservedAnalyses run(llvm::Module &M,
                              llvm::ModuleAnalysisManager &AM) const;
  explicit FlatteningPass(uint32_t switch_bit_width);
  void flatten(llvm::Module *M, llvm::Function *fn) const;
};

template <std::size_t ISize, std::size_t OSize>
void uniq_int_rands(uint32_t bit_width,
                    llvm::SmallVector<uint64_t, ISize> &rands,
                    llvm::SmallVector<llvm::Constant *, OSize> &output,
                    llvm::IntegerType *int_ty, std::size_t insert_pos) {
  if (bit_width <= 64) {
    uint64_t rand = rand_unique<uint64_t>(rands);
    rands.emplace_back(rand);
    output[insert_pos] =
        llvm::ConstantInt::get(int_ty, rand & int_ty->getBitMask());
  } else {
    uint32_t size = ceil(static_cast<double>(bit_width) / 64);
    // 64 * 8 = 512, bit_width is in [32, 512]
    llvm::SmallVector<uint64_t, 8> vec(size);
    for (uint64_t &item : vec) {
      uint64_t rand = rand_unique<uint64_t>(rands);
      rands.emplace_back(rand);
      item = rand;
    }
    uint32_t rem = bit_width % 64;
    if (rem != 0) {
      vec.back() = vec.back() & bit_mask_by_width(rem);
    }
    output[insert_pos] = cast<llvm::ConstantInt>(
        llvm::ConstantInt::get(int_ty, llvm::APInt(bit_width, vec)));
  }
}

void fix_stack(llvm::Function *F);

} // namespace labyrinth

#endif
