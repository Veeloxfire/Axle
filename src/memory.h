#pragma once
#include "safe_lib.h"

//Anything allocated via this memory will not be destroyed

struct MemoryPool {
  u8* mem;
  usize total;
  usize top;

  u8* push_alloc_bytes(usize size, usize align);

  template<typename T>
  T* push() {
    T* ast = (T*)push_alloc_bytes(sizeof(T), alignof(T));
    new (ast) T();
    return ast;
  }

  template<typename T>
  T* push_n(usize n) {
    T* ast = (T*)push_alloc_bytes(sizeof(T) * n, alignof(T));

    for (usize i = 0; i < n; i++) {
      new (ast + i) T();
    }

    return ast;
  }
};
