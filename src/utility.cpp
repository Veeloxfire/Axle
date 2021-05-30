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

uint8_t* ArenaAllocator::alloc_no_construct(size_t bytes) {
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
    fl = (FreeList*)free_list;
  }

  const uint64_t available_space = fl->qwords_available;
 
  uint64_t* const used_space = (uint64_t*)fl;
  uint64_t* current_alloc = (uint64_t*)used_space + 1;


  //Fix the free list - do we need a new node?
  if (available_space - req_size >= 2) {
    //Yay there is more space

    FreeList* new_fl = (FreeList*)(current_alloc + req_size);

    if (prev != nullptr) {
      prev->next = new_fl;
    }
    else {
      free_list = new_fl;
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
    else {
      free_list = nullptr;
    }
  }

  //How much data to free
  *used_space = req_size;

  return (uint8_t*)current_alloc;
}

void ArenaAllocator::free_no_destruct(void* val) {

  uint64_t* ptr = (uint64_t*)val;

  const uint64_t free_size = ptr[-1];

  FreeList* new_fl = (FreeList*)(ptr - 1);
  new_fl->qwords_available = free_size;
  new_fl->next = nullptr;

  add_to_free_list(new_fl);
}

void ArenaAllocator::new_block() {
  Block* block = allocate_default<Block>(1);

  FreeList* fl = (FreeList*)block->data;

  fl->qwords_available = Block::BLOCK_SIZE - 1;

  fl->next = (FreeList*)free_list;
  free_list = (void*)fl;

  block->next = base;
  base = block;
}

ArenaAllocator::~ArenaAllocator() {
  ::free_destruct_single(base);
}

ArenaAllocator::Block::~Block() { ::free_destruct_single(next); }

SquareBitMatrix::~SquareBitMatrix() {
  free();
}

void SquareBitMatrix::free() {
  ::free_no_destruct<uint8_t>(data);

  data = nullptr;
  side_length = 0;
  capacity = 0;
}

bool SquareBitMatrix::test_a_intersects_b(size_t a, size_t b) const {
  assert(a < side_length);
  assert(b < side_length);

  const size_t bytes_per_val = bytes_per_val_per_side(side_length);
  const uint8_t* a_data = data + bytes_per_val * a;

  const size_t b_mod8 = b % 8;
  const size_t b_div8 = b / 8;

  assert(a_data + b_div8 < data + capacity);

  return (a_data[b_div8] & (1 << b_mod8)) > 0;
}

void SquareBitMatrix::set_a_intersects_b(size_t a, size_t b) {
  assert(a < side_length);
  assert(b < side_length);

  const size_t bytes_per_val = bytes_per_val_per_side(side_length);
  uint8_t* a_data = data + bytes_per_val * a;

  const size_t b_mod8 = b % 8;
  const size_t b_div8 = b / 8;

  assert(a_data + b_div8 < data + capacity);

  a_data[b_div8] |= (uint8_t)(1 << b_mod8);
}

void SquareBitMatrix::remove_a_intersects_b(size_t a, size_t b) {
  assert(a < side_length);
  assert(b < side_length);

  const size_t bytes_per_val = bytes_per_val_per_side(side_length);
  uint8_t* a_data = data + bytes_per_val * a;

  const size_t b_mod8 = b % 8;
  const size_t b_div8 = b / 8;

  assert(a_data + b_div8 < data + capacity);

  a_data[b_div8] &= ~(uint8_t)(1 << b_mod8);
}

size_t SquareBitMatrix::new_value() {
  const size_t bytes_per_val_now = bytes_per_val_per_side(side_length);
  const size_t bytes_per_val_next = bytes_per_val_per_side(side_length + 1);
  const size_t required_capacity = bytes_per_val_next * (side_length + 1);

  //check if we have enough space
  if (required_capacity > capacity) {
    const size_t old_capacity = capacity;

    if (capacity == 0) {
      capacity = 8;
    }
    else {
      capacity = (size_t)1 << small_log_2_ceil(required_capacity);
    }
    data = reallocate_default(data, old_capacity, capacity);
  }

  //Check if we need to fix data shape
  if (bytes_per_val_now < bytes_per_val_next) {
    auto block_i = side_length == 0 ? data
      : data + bytes_per_val_now * (side_length - 1);
    auto new_i = data + bytes_per_val_next * side_length;

    //Dont need to fix the first block as it will stay the same
    const auto block_end = data + bytes_per_val_now;

    const size_t diff = bytes_per_val_next - bytes_per_val_now;

    while (block_i > block_end) {
      size_t i = 0;
      for (; i < diff; i++) {
        new_i[bytes_per_val_next - (i + 1)] = 0;
      }

      i = 0;
      for (size_t i = 0; i < bytes_per_val_now; i++) {
        new_i[bytes_per_val_now - (i + 1)] = block_i[bytes_per_val_now - (i + 1)];
      }

      block_i -= bytes_per_val_now;
      new_i -= bytes_per_val_now;
    }
  }

  //Finally return new value
  return side_length++;
}

void print_as_bytes(const uint8_t* bytes, size_t length) {
  for (size_t i = 0; i < length; i++) {
    printf("0x%hhx ", bytes[i]);
  }
}

BumpAllocator::BumpAllocator()
  : top(::allocate_default<BLOCK>()) {
}

BumpAllocator::~BumpAllocator() {
  while (top != nullptr) {
    BLOCK* const next = top->prev;
    free_no_destruct<BLOCK>(top);
    top = next;
  }
}

void BumpAllocator::new_block() {
  BLOCK* const new_b = ::allocate_default<BLOCK>();
  new_b->prev = top;
  top = new_b;
}

uint8_t* BumpAllocator::allocate_no_construct(size_t bytes) {
  assert(bytes < BLOCK::BLOCK_SIZE);

  if (top->filled + bytes > BLOCK::BLOCK_SIZE) {
    new_block();
  }

  uint8_t* ptr = top->data + top->filled;
  top->filled += bytes;

  return ptr;
}