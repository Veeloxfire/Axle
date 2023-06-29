#include "utility.h"
#include "strings.h"
#include "trace.h"

void throw_testing_assertion(const char* message) {
  if (std::uncaught_exceptions() == 0) {
    throw std::exception(message);
  }
}

void load_to_bytes(Array<uint8_t>& bytes,
                   const size_t offset,
                   const uint8_t* in_bytes,
                   const size_t len) {
  bytes.reserve_total(offset + len);
  memcpy_ts(bytes.data + offset, bytes.capacity - offset, in_bytes, len);
}

#ifdef ARENA_ALLOCATOR_DEBUG

uint8_t* ArenaAllocator::alloc_no_construct(size_t bytes) {
  void* d = malloc(bytes);
  allocated.insert(d);

  return (uint8_t*)d;
}

void ArenaAllocator::free_no_destruct(void* val) {
  usize check = allocated.size;
  ASSERT(check != 0);

  allocated.remove_if([val](void* data) { return val == data; });

  ASSERT(check == allocated.size + 1);
  
  free(val);
}

ArenaAllocator::~ArenaAllocator() {
  ASSERT(allocated.size == 0);
}

#else

void ArenaAllocator::add_to_free_list(ArenaAllocator::FreeList* new_fl) {
  //Should otherwise all be combined
  //Just need to find if this is after a free space or before a free space

  ASSERT(!_debug_freelist_loops());

  FreeList* prev = nullptr;
  FreeList* fl = free_list;

  if (fl == nullptr) {
    free_list = new_fl;
    return;
  }

  ASSERT(fl->next != fl);

  bool found_before = false;
  bool found_after = false;

  while (fl != nullptr) {
    if (!found_after && (uint64_t*)fl == ((uint64_t*)new_fl + new_fl->qwords_available + 1)) {
      //fl comes directly after new_fl

      //Remove from list
      if (prev != nullptr) {
        prev->next = fl->next;
      }

      new_fl->qwords_available += fl->qwords_available + 1;

      fl = fl->next;

      //Can we exit?
      if (found_before) {
        break;
      }
      found_after = true;
    }
    else if (!found_before && (uint64_t*)new_fl == ((uint64_t*)fl + fl->qwords_available + 1)) {
      //new_fl comes directly after fl

      //Remove from list
      if (prev != nullptr) {
        prev->next = fl->next;
      }

      auto* save_next = fl->next;

      fl->qwords_available += new_fl->qwords_available + 1;
      fl->next = nullptr;

      new_fl = fl;
      
      fl = save_next;

      //Can we exit
      if (found_after) {
        break;
      }
      found_before = true;
    }
    else {
      prev = fl;
      fl = fl->next;
    }
  }

  if (prev != nullptr) {
    //free_list is not new_fl
    new_fl->next = (FreeList*)free_list;
  }
  free_list = new_fl;

  ASSERT(free_list->next != free_list);
}

bool ArenaAllocator::_debug_freelist_loops() const {
  Array<FreeList*> list_elements ={};

  FreeList* list = free_list;

  while (list != nullptr) {
    if(list_elements.contains(list)) return true;

    list_elements.insert(list);
    list = list->next;
  }

  return false;
}

bool ArenaAllocator::_debug_valid_pointer(void* ptr) const {
  Block* block = base;

  //Checking the pointer is actually from one of these blocks

  while (block != nullptr) {
    if(ptr >= (block->data + 1)/*+ 1 for the saved free size*/
       && ptr < (block->data + Block::BLOCK_SIZE)) return true;

    block = block->next;
  }

  return false;
}

bool ArenaAllocator::_debug_is_allocated_data(uint64_t* ptr, usize len) const {
  auto list = free_list;

  auto ptr_end = ptr + len;
  //Try to find if its in the freelist

  while (list != nullptr) {
    u64* start = (u64*)list;
    u64* end = start + list->qwords_available + 1;

    //Check if the ranges overlap - error if they do
    if((ptr <= start && start < ptr_end)
       || (ptr < end && end <= ptr_end)
       || (ptr <= start && end <= ptr_end))
      return false;

    list = list->next;
  }

  return true;
}

uint8_t* ArenaAllocator::alloc_no_construct(size_t bytes) {
  ASSERT(bytes != 0);
  ASSERT(!_debug_freelist_loops());

  size_t req_size = ceil_div(bytes, 8);
  ASSERT(req_size != 0);

  FreeList* prev = nullptr;
  FreeList* fl = (FreeList*)free_list;

  if (bytes > Block::BLOCK_SIZE - 1) {
    INVALID_CODE_PATH("Arena allocator block does not have enough space");
  }

  //Find free space
  while (fl != nullptr && fl->qwords_available < req_size) {
    prev = fl;
    fl = fl->next;
  }

  if (fl == nullptr) {
    //Allocate more data
    new_block();
    fl = free_list;
  }

  const uint64_t available_space = fl->qwords_available;
 
  uint64_t* const used_space = (uint64_t*)fl;
  uint64_t* current_alloc = used_space + 1;


  //can we fit a new node?
  if (available_space - req_size >= 2) {
    //Yay there is more space

    FreeList* new_fl = (FreeList*)(current_alloc + req_size);

    if (prev != nullptr) {
      prev->next = new_fl;
    }
    else {
      free_list = new_fl;
    }

    new_fl->qwords_available = available_space - (req_size + 1);
    new_fl->next = fl->next;
  }
  else {
    //Not enough space for another value
    req_size = available_space;

    if (prev != nullptr) {
      //not the top of the free list
      prev->next = fl->next;
    }
    else {
      //is top of free list
      free_list = fl->next;
    }
  }

  //How much data to free
  *used_space = req_size;

  ASSERT(!_debug_freelist_loops());
  return (uint8_t*)current_alloc;
}


void ArenaAllocator::free_no_destruct(void* val) {
  ASSERT(val != nullptr);
  ASSERT(!_debug_freelist_loops());
  ASSERT(_debug_valid_pointer(val));

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

  fl->next = free_list;
  free_list = fl;

  block->next = base;
  base = block;
}

ArenaAllocator::~ArenaAllocator() {
  ::free_destruct_single(base);
}

ArenaAllocator::Block::~Block() { ::free_destruct_single(next); }
#endif

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
  ASSERT(a < side_length);
  ASSERT(b < side_length);

  const size_t bytes_per_val = bytes_per_val_per_side(side_length);
  const uint8_t* a_data = data + bytes_per_val * a;

  const size_t b_mod8 = b % 8;
  const size_t b_div8 = b / 8;

  ASSERT(a_data + b_div8 < data + capacity);

  return (a_data[b_div8] & (1 << b_mod8)) > 0;
}

void SquareBitMatrix::set_a_intersects_b(size_t a, size_t b) {
  ASSERT(a < side_length);
  ASSERT(b < side_length);

  const size_t bytes_per_val = bytes_per_val_per_side(side_length);
  uint8_t* a_data = data + bytes_per_val * a;

  const size_t b_mod8 = b % 8;
  const size_t b_div8 = b / 8;

  ASSERT(a_data + b_div8 < data + capacity);

  a_data[b_div8] |= (uint8_t)(1 << b_mod8);
}

void SquareBitMatrix::remove_a_intersects_b(size_t a, size_t b) {
  ASSERT(a < side_length);
  ASSERT(b < side_length);

  const size_t bytes_per_val = bytes_per_val_per_side(side_length);
  uint8_t* a_data = data + bytes_per_val * a;

  const size_t b_mod8 = b % 8;
  const size_t b_div8 = b / 8;

  ASSERT(a_data + b_div8 < data + capacity);

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

BitArray::BitArray(size_t length_) : data(new u8[length_]), length(length_) {}
BitArray::~BitArray() { delete[] data; }

void BitArray::set(size_t a) {
  ASSERT(a < length);

  size_t index = a / 8;
  size_t offset = a % 8;

  data[index] |= 1 << offset;
}

bool BitArray::test(size_t a) const {
  ASSERT(a < length);

  size_t index = a / 8;
  size_t offset = a % 8;

  return (data[index] & (1 << offset)) > 0;
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
  ASSERT(bytes < BLOCK::BLOCK_SIZE);

  if (top->filled + bytes > BLOCK::BLOCK_SIZE) {
    new_block();
  }

  uint8_t* ptr = top->data + top->filled;
  top->filled += bytes;

  return ptr;
}

static SpinLockMutex io_mutex = {};

void IO_Single::lock() {
  io_mutex.acquire();
}

void IO_Single::unlock() {
  io_mutex.release();
}

void IO_Single::print_impl(const char* string) {
  TRACING_FUNCTION();
  fputs(string, stdout);
}

void IO_Single::print_impl(const OwnedPtr<char>& string) {
  TRACING_FUNCTION();
  fputs(string.ptr, stdout);
}

void IO_Single::print_impl(const char c) {
  TRACING_FUNCTION();
  putc(c, stdout);
}

void IO_Single::err_print_impl(const char* string) {
  TRACING_FUNCTION();
  fputs(string, stderr);
}

void IO_Single::err_print_impl(const OwnedPtr<char>& string) {
  TRACING_FUNCTION();
  fputs(string.ptr, stderr);
}

void IO_Single::err_print_impl(const char c)  {
  TRACING_FUNCTION();
  putc(c, stderr);
}

void serialize_zeros(Array<u8>& bytes, usize size, usize alignment) {
   //Align
  const auto align_to = ceil_to_n(bytes.size, alignment);

  usize bytes_to_write = size + (align_to - bytes.size);
  usize old_top = bytes.size;

  bytes.insert_uninit(bytes_to_write);

  u8* d = (bytes.data + old_top);

  for (usize i = 0; i < bytes_to_write; i++) {
    d[i] = '\0';
  }
}

void serialize_bytes(Array<u8>& bytes, const u8* data, usize size, usize alignment) {
  //Align
  const auto align_to = ceil_to_n(bytes.size, alignment);

  usize align_bytes = (align_to - bytes.size);
  usize old_top = bytes.size;

  bytes.insert_uninit(align_bytes + size);

  u8* d = (bytes.data + old_top);

  for (usize i = 0; i < align_bytes; i++) {
    d[i] = '\0';
  }

  for (usize i = 0; i < size; i++) {
    d[i + align_bytes] = data[i];
  }
}