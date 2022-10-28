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

template<usize BLOCK_SIZE> 
struct GrowingMemoryPool {
  struct Block {
    Block* prev = nullptr;
    usize top = 0;
    u8 mem[BLOCK_SIZE];
  };

  Block* curr = nullptr;

  u8* push_unaligned_bytes(usize size) {
    ASSERT(size <= BLOCK_SIZE);

    if (curr == nullptr || (curr->top + size > BLOCK_SIZE)) {
      Block* old = curr;
      curr = new Block();
      curr->prev = old;
    }

    u8* ptr = curr->mem + curr->top;
    curr->top += size;

    return ptr;
  }

  GrowingMemoryPool() = default;
  GrowingMemoryPool(const GrowingMemoryPool&) = delete;
  GrowingMemoryPool(GrowingMemoryPool&&) = delete;
  ~GrowingMemoryPool() {
    while (curr != nullptr) {
      Block* save = curr->prev;

      delete curr;

      curr = save;
    }
  }
};