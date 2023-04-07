#ifndef LABYRINTH_GLOBALS_ENCRYPTION_H
#define LABYRINTH_GLOBALS_ENCRYPTION_H

#include "llvm/ADT/iterator_range.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Transforms/Utils/Local.h"

#include <array>

namespace labyrinth {

class GlobalsEncryptionPass
    : public llvm::PassInfoMixin<GlobalsEncryptionPass> {
public:
  bool only_str = false;
  uint8_t obf_time = 1;
  GlobalsEncryptionPass(bool only_str, uint8_t obf_time);
  llvm::PreservedAnalyses run(llvm::Module &M, llvm::ModuleAnalysisManager &AM);
};

std::string genHashedName(llvm::GlobalVariable *gv, int priority);
void insertIntDecryption(llvm::Module &M, llvm::GlobalVariable *gv,
                         uint64_t key, int priority);
void insertArrayDecryption(llvm::Module &M, llvm::GlobalVariable *gv,
                           llvm::IntegerType *gv_ele_ty,
                           std::array<uint64_t, 8> &keys, int priority);

template <class Ty>
void xor_with_keys(llvm::StringRef raw_data, std::array<uint64_t, 8> &keys) {
  std::size_t j = 0;
  for (auto &item : llvm::make_range(
           const_cast<Ty *>(reinterpret_cast<const Ty *>(raw_data.begin())),
           const_cast<Ty *>(reinterpret_cast<const Ty *>(raw_data.end())))) {
    item ^= keys[j % 8];
    j++;
  }
}

} // namespace labyrinth

#endif // LABYRINTH_GLOBALS_ENCRYPTION_H
