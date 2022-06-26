#include "memory.h"
#include "utility.h"
u8* MemoryPool::push_alloc_bytes(usize size, usize align) {
  usize new_top = top;
  if(top % align != 0) {
    new_top += align - (top % align);
  }

  u8* new_ptr = mem + new_top;
  new_top += size;

  ASSERT(new_top < total);
  top = new_top;

  return new_ptr;
}