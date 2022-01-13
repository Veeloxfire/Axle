#include "ast.h"

u8* AstStorage::push_alloc_bytes(usize size, usize align) {
  if (top % align > 0) {
    top += align - (top % align);
  }

  ASSERT(BLOCK_SIZE - size > top);//temp - cannot alloc past the end of the block of memory

  u8* m = ast_mem + top;
  top += size;
  return m;
}