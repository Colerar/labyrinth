#ifndef LABYRINTH_GLOBALS_ENCRYPTION_H
#define LABYRINTH_GLOBALS_ENCRYPTION_H

#include "effolkronium/random.hpp"
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
                           llvm::ArrayType *arr_ty, llvm::IntegerType *ele_ty,
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

template <size_t Size>
[[nodiscard("handle error when return false")]] bool
xor_bit_width_with_keys(unsigned bit_width, llvm::StringRef raw_data,
                        std::array<uint64_t, Size> &keys) {
  switch (bit_width) {
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
    return false;
  }
  return true;
}

template <size_t Size>
auto rand_with_ty(llvm::IntegerType *ty) -> std::array<uint64_t, Size> {
  using Random = effolkronium::random_thread_local;
  uint64_t mask = ty->getBitMask();
  auto rand = [&]() { return Random::get<uint64_t>() & mask; };
  std::array<uint64_t, Size> keys{0};
  for (auto &item : keys) {
    item = rand();
  }
  return keys;
}

} // namespace labyrinth

#endif // LABYRINTH_GLOBALS_ENCRYPTION_H
