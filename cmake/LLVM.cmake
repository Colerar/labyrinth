if (NOT DEFINED ENV{LLVM_HOME})
    message(FATAL_ERROR "$LLVM_HOME is not defined")
endif ()
set(ENV{LLVM_DIR} $ENV{LLVM_HOME}/lib/cmake/llvm)

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

llvm_map_components_to_libnames(llvm_libs core passes irreader support)

if (${LLVM_VERSION_MAJOR} VERSION_LESS 14)
    message(FATAL_ERROR "LLVM version is too small, expected: 14 or greater, actual: ${LLVM_PACKAGE_VERSION}")
endif ()
