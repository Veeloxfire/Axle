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

template<typename T>
struct MemoryStack {
  T* base;
  usize total;
  usize top;

  void push(const T& t) {
    ASSERT(top < total);
    base[top++] = t;
  }

  T pop() {
    ASSERT(top > 0);
    return base[top--];
  }
};

MemoryPool new_sub_pool(MemoryPool* pool, usize size, usize align);

template<typename T>
MemoryStack<T> new_stack(MemoryPool* pool, usize count) {
  MemoryStack<T> s ={};
  s.base = pool->push_n<T>(count);
  s.total = count;
  s.top = 0;

  return s;
}


