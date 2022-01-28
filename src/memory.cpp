#include "memory.h"

u8* MemoryPool::push_alloc_bytes(usize size, usize align) {
  if (top % align > 0) {
    top += align - (top % align);
  }

  ASSERT(total > size && total - size > top);//temp - cannot alloc past the end of the block of memory

  u8* m = mem + top;
  top += size;
  return m;
}

MemoryPool new_sub_pool(MemoryPool* pool, usize size, usize align) {
  MemoryPool new_pool ={};

  pool->mem = pool->push_alloc_bytes(size, align);
  pool->top = 0;
  pool->total = size;

  return new_pool;
}