#ifndef LABYRINTH_UTILS_H
#define LABYRINTH_UTILS_H

#include "effolkronium/random.hpp"
#include "llvm/ADT/iterator_range.h"

#include <cstdint>

template <typename T>
std::enable_if<effolkronium::details::is_uniform_int<T>::value, T>::type
rand_unique(llvm::iterator_range<T *> rands) {
  using Random = effolkronium::random_thread_local;
  while (true) {
    auto rand = Random::get<T>();
    if (std::none_of(rands.begin(), rands.end(),
                     [&](auto a) { return a == rand; })) {
      return rand;
    }
  }
}

#endif
