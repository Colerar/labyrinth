#ifndef LABYRINTH_REGISTRY_H
#define LABYRINTH_REGISTRY_H

#include "labyrinth_export.h"

#include <span>

namespace labyrinth {
void passBuilderCallback(llvm::PassBuilder &builder);
} // namespace labyrinth

#endif // LABYRINTH_REGISTRY_H
