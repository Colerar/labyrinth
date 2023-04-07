# Labyrinth

> 🚧️ WIP

Labyrinth, a LLVM obfuscation plugin
for [the New Pass Manager](https://blog.llvm.org/posts/2021-03-26-the-new-pass-manager/).

## Build

Requirements:

- CMake v3.21 or later
- LLVM v15 or later
- **Ninja (recommended)** or Make
- A compiler that supports C++ 20
    - **Clang v16 (recommended)**
    - GCC v12
    - MSVC 2022

Build receipts:

```bash
export LLVM_HOME=/path/to/llvm/home
cmake -B build -G Ninja # or 'Unix Makefiles'
cd build
ninja
# if everything works well, liblabyrinth.{so/dylib/dll} will be generate
```

## Usage

> 🚧 TODO
