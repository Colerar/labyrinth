cmake_minimum_required(VERSION 3.21 FATAL_ERROR)
include(GenerateExportHeader)

project(labyrinth LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

if (EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/cmake/custom.cmake")
    message(STATUS "Loading Custom CMake Script ./cmake/custom.cmake")
    include(cmake/custom.cmake)
endif ()

include(cmake/get-cpm.cmake)
include(cmake/LLVM.cmake)
include(cmake/Eigen.cmake)

set(Random_BuildTests OFF)
CPMAddPackage(
        NAME effolkronium-random
        VERSION 1.4.1
        URL https://github.com/effolkronium/random/archive/refs/tags/v1.4.1.tar.gz
        URL_HASH SHA256=ec6beb67496ad2ce722d311d3fa5efb7e847dac5fd1c16b8920b51562fe20f53
)

include_directories("./include")
include_directories(${CMAKE_CURRENT_BINARY_DIR}/exports)

file(GLOB srcs "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
add_library(labyrinth SHARED ${srcs})
generate_export_header(
        labyrinth
        EXPORT_FILE_NAME ${CMAKE_BINARY_DIR}/exports/labyrinth_export.h
)

add_compile_options(-fno-rtti)
add_link_options(-fvisibility=hidden)
if (APPLE)
    set_target_properties(labyrinth PROPERTIES LINK_FLAGS "-undefined dynamic_lookup")
endif (APPLE)

target_link_libraries(labyrinth PRIVATE effolkronium_random Eigen ${llvm_libs})
