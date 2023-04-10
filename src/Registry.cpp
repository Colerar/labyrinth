#include "labyrinth/Registry.h"
#include "labyrinth/Flattening.h"
#include "labyrinth/GlobalsEncryption.h"
#include "llvm/Passes/PassPlugin.h"

using labyrinth::GlobalsEncryptionPass, labyrinth::FlatteningPass;
using llvm::ModulePassManager, llvm::OptimizationLevel;
using llvm::outs, llvm::errs;
using llvm::StringRef;

namespace labyrinth {

void passBuilderCallback(llvm::PassBuilder &builder) {
  builder.registerPipelineParsingCallback([](StringRef args,
                                             ModulePassManager &manager, auto) {
    bool only_str = false;
    uint8_t obf_time = 1;
    bool parse_status = parseArgs(args, "gle", [&](StringRef k, StringRef v) {
      if (k.equals_insensitive("onlystr")) {
        if (!parseBoolValue<OnEmpty::True>(k, v, only_str)) {
          return false;
        }
      } else if (k.equals_insensitive("times")) {
        if (!to_integer(v, obf_time, 10)) {
          errs() << "Invalid value for `" << k << "`: " << v << '\n';
          return false;
        }
      } else {
        errs() << "Unknown configuration key `" << k << "`\n";
        return false;
      }
      return true;
    });
    if (!parse_status) {
      return false;
    }

    manager.addPass(GlobalsEncryptionPass(only_str, obf_time));
    return true;
  });
  builder.registerPipelineParsingCallback(
      [](StringRef args, ModulePassManager &manager, auto) {
        uint32_t width = 128;
        bool status = parseArgs(args, "fla", [&](StringRef k, StringRef v) {
          if (k.equals_insensitive("width")) {
            if (!to_integer(v, width, 10) || width < 32 || width > 512) {
              errs() << "Invalid value for `" << k << "`: " << v << '\n';
              return false;
            }
          }
          return true;
        });
        if (!status) {
          return false;
        }

        manager.addPass(FlatteningPass(width));
        return true;
      });
}

bool parseArgs(
    StringRef args, StringRef name,
    const std::function<bool(llvm::StringRef k, llvm::StringRef v)> &parser) {
  llvm::SmallVector<StringRef, 8> split;
  args.split(split, ":", -1, false);
  if (split.empty()) {
    errs() << "Pass name is empty\n";
    return false;
  }
  if (split.front() != name) {
    return false;
  }
  return std::ranges::all_of(split.begin() + 1, split.end(), [&](auto entry) {
    auto [k, v] = entry.split("=");
    if (k.empty()) {
      errs() << "A parameter for pass `" << name << "` is empty\n";
      return false;
    }
    if (!parser(k, v)) {
      errs() << "Invalid configuration entry: `" << entry << "`\n";
      return false;
    }
    return true;
  });
}

} // namespace labyrinth

extern "C" llvm::PassPluginLibraryInfo LABYRINTH_EXPORT
llvmGetPassPluginInfo() {
  return {LLVM_PLUGIN_API_VERSION, "Labyrinth", LLVM_VERSION_STRING,
          labyrinth::passBuilderCallback};
}
