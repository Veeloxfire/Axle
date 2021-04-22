#include "utility.h"

void load_to_bytes(Array<uint8_t>& bytes,
                   const size_t offset,
                   const uint8_t* in_bytes,
                   const size_t len) {
  bytes.reserve_total(offset + len);
  memcpy_ts(bytes.data + offset, bytes.capacity - offset, in_bytes, len);
}

void ArenaAllocator::add_to_free_list(ArenaAllocator::FreeList* new_fl) {
  //Should otherwise all be combined
  //Just need to find if this is after a free space or before a free space


  FreeList* prev = nullptr;
  FreeList* fl = (FreeList*)free_list;

  if (fl == nullptr) {
    free_list = (void*)new_fl;
    return;
  }

  bool found_before = false;
  bool found_after = false;

  while (fl != nullptr) {
    if (!found_after && fl == (new_fl + new_fl->qwords_available + 1)) {
      //Remove from list
      if (prev != nullptr) {
        prev->next = fl->next;
      }

      new_fl->qwords_available += fl->qwords_available + 1;

      fl = fl->next;

      //Can we exit?
      if (found_before) {
        return;
      }
      found_after = true;
    }
    else if (!found_before && new_fl == (fl + fl->qwords_available + 1)) {
      //Remove from list
      if (prev != nullptr) {
        prev->next = fl->next;
      }

      fl->qwords_available += new_fl->qwords_available + 1;
      
      new_fl = fl;
      fl = fl->next;

      //Can we exit
      if (found_after) {
        return;
      }
      found_before = true;
    }
    else {
      prev = fl;
      fl = fl->next;
    }
  }

  new_fl->next = (FreeList*)free_list;
  free_list = (void*)new_fl;
}

void* ArenaAllocator::alloc_no_construct(size_t bytes) {
  size_t req_size = ceil_div(bytes, 8);

  FreeList* prev = nullptr;
  FreeList* fl = (FreeList*)free_list;

  if (bytes > Block::BLOCK_SIZE - 1) {
    throw std::exception("Arena allocator does not have enough space");
  }

  //Find free space
  while (fl != nullptr && fl->qwords_available < req_size) {
    prev = fl;
    fl = fl->next;
  }

  if (fl == nullptr) {
    //Allocate more data
    new_block();
  }


  const uint64_t available_space = fl->qwords_available;
  uint64_t used_space = req_size;

  uint64_t* current_alloc = (uint64_t*)fl;

  //Fix the free list - do we need a new node?
  if (available_space - req_size >= 2) {
    //Yay there is more space

    FreeList* new_fl = (FreeList*)(current_alloc + used_space);

    if (prev != nullptr) {
      prev->next = new_fl;
    }

    //-1 for the bytes available
    //not -2 because the free list ptr is part of allocated space
    new_fl->qwords_available = available_space - req_size - 1;
    new_fl->next = fl->next;
  }
  else {
    //Not enough space for another value
    req_size = available_space;

    if (prev != nullptr) {
      prev->next = fl->next;
    }
  }

  //How much data to free
  *current_alloc = used_space;
  current_alloc += 1;

  return current_alloc;
}

void ArenaAllocator::free_no_destruct(void* val) {

  uint64_t* ptr = (uint64_t*)val;

  const uint64_t free_size = ptr[-1];

  FreeList* new_fl = (FreeList*)ptr;
  new_fl->qwords_available = free_size;
  new_fl->next = nullptr;

  add_to_free_list(new_fl);
}

void ArenaAllocator::new_block() {
  Block* block = allocate_default<Block>(1);

  FreeList* fl = (FreeList*)base->data;

  fl->qwords_available = Block::BLOCK_SIZE - 1;

  fl->next = (FreeList*)free_list;
  free_list = (void*)fl;

  block->next = base;
  base = block;
}

ArenaAllocator::~ArenaAllocator() {
  ::free(base);
}

ArenaAllocator::Block::~Block() { ::free(next); }