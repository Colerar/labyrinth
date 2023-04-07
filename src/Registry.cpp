#include <llvm/ADT/StringRef.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/PassPlugin.h>

#include "labyrinth/GlobalsEncryption.h"
#include "labyrinth/Registry.h"

using labyrinth::GlobalsEncryptionPass;
using llvm::ModulePassManager, llvm::OptimizationLevel;
using llvm::outs, llvm::errs;
using llvm::StringRef;

void labyrinth::passBuilderCallback(llvm::PassBuilder &builder) {
  builder.registerPipelineParsingCallback(
      [](StringRef name, ModulePassManager &manager, auto opt_level) {
        llvm::SmallVector<StringRef, 8> split;
        name.split(split, ":", -1, false);
        if (split.empty()) {
          errs() << "Pass name is empty\n";
          return false;
        }
        if (split.front() != "gle") {
          return false;
        }
        bool only_str = false;
        uint32_t obf_time = 1;
        for (auto entry : llvm::make_range(split.begin() + 1, split.end())) {
          auto [k, v] = entry.split("=");
          if (k.equals_insensitive("only-str") ||
              k.equals_insensitive("onlystr")) {
            if (v.empty() || v.equals_insensitive("on") ||
                v.equals_insensitive("true")) {
              only_str = true;
            } else if (v.equals_insensitive("off") ||
                       v.equals_insensitive("false")) {
              only_str = false;
            } else {
              errs() << "Invalid value for `" << k << "`: " << v << '\n';
              return false;
            }
          } else if (k.equals_insensitive("times") ||
                     k.equals_insensitive("obftime") ||
                     k.equals_insensitive("obf-time")) {
            if (!to_integer(v, obf_time, 10)) {
              errs() << "Invalid value for `" << k << "`: " << v << '\n';
              return false;
            }
          }
        }
        manager.addPass(GlobalsEncryptionPass(only_str, obf_time));
        return true;
      });
}

extern "C" llvm::PassPluginLibraryInfo LABYRINTH_EXPORT
llvmGetPassPluginInfo() {
  return {LLVM_PLUGIN_API_VERSION, "Labyrinth", LLVM_VERSION_STRING,
          labyrinth::passBuilderCallback};
}
