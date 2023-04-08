#ifndef LABYRINTH_REGISTRY_H
#define LABYRINTH_REGISTRY_H

#include "labyrinth_export.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Passes/PassBuilder.h"

#include <functional>

namespace labyrinth {
void passBuilderCallback(llvm::PassBuilder &args);

bool parseArgs(
    llvm::StringRef args, llvm::StringRef name,
    const std::function<bool(llvm::StringRef k, llvm::StringRef v)> &parser =
        [](auto, auto) { return false; });
} // namespace labyrinth

enum class OnEmpty {
  True,
  False,
  Fail,
};

/**
 * @param val which reference the result will be assigned to
 * @return Returns true if parsing is successful, false otherwise
 */
template <OnEmpty on_empty = OnEmpty::Fail>
bool parseBoolValue(llvm::StringRef key, llvm::StringRef value, bool &val) {
  if (value.empty()) {
    if constexpr (on_empty == OnEmpty::True) {
      val = true;
      return true;
    } else if constexpr (on_empty == OnEmpty::False) {
      val = false;
      return true;
    } else {
      llvm::errs() << "Invalid empty value for key `" << key << "`\n";
      return false;
    }
  }

  if (value.equals_insensitive("on") || value.equals_insensitive("true") ||
      value.equals_insensitive("1")) {
    val = true;
  } else if (value.equals_insensitive("off") ||
             value.equals_insensitive("false") ||
             value.equals_insensitive("0")) {
    val = false;
  } else {
    llvm::errs() << "Invalid value for `" << key << "`: " << value << '\n';
    return false;
  }

  return true;
}

#endif // LABYRINTH_REGISTRY_H
