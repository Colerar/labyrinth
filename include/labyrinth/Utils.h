#ifndef LABYRINTH_UTILS_H
#define LABYRINTH_UTILS_H

#include "effolkronium/random.hpp"
#include "llvm/ADT/iterator_range.h"

#include <cstdint>

template <typename T>
typename std::enable_if<effolkronium::details::is_uniform_int<T>::value,
                        T>::type
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

constexpr uint64_t bit_mask_by_width(uint32_t n) {
  return ~uint64_t(0UL) >> (64 - n);
}

#endif
