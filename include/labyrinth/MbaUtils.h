#ifndef LABYRINTH_MBA_UTILS_H
#define LABYRINTH_MBA_UTILS_H

#include "llvm/IR/IRBuilder.h"

#include <cstdint>
#include <vector>

namespace labyrinth {

std::vector<int64_t> generateLinearMBA(int exprNumber);
llvm::Value *insertLinearMBA(std::vector<int64_t> &params,
                             llvm::Instruction *insertBefore);
llvm::Value *insertPolynomialMBA(llvm::Value *linear_mba_expr,
                                 llvm::Instruction *insertBefore);

} // namespace labyrinth

#endif // LABYRINTH_MBA_UTILS_H
