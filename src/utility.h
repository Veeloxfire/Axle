#pragma once

#include <stdint.h>
#include <string.h>
#include <utility>
#include <new>
#include <assert.h>

#include "safe_lib.h"

#define BYTE(a) (static_cast<uint8_t>(a))
#define JOIN2(a, b) a ## b
#define JOIN(a, b) JOIN2(a, b)

template<typename T>
void reset_type(T* t) noexcept {
  t->~T();
  new(t) T();
}

template<typename T, size_t i>
constexpr size_t array_size(T(&)[i]) {
  return i;
}

constexpr size_t ceil_div(size_t x, size_t y) noexcept {
  return x / y + (x % y != 0);
}

//Log 2 for uniform random 64 bit number
constexpr inline uint64_t log_2(uint64_t v) {
  if (v == 0) {
    throw std::exception("MATH ERROR! Cannot log of 0");
    return 0;
  }

  uint64_t max = 63;
  uint64_t min = 0;

  while (max >= min + 1) {
    uint64_t mid = (max + min) / 2;
    if (v > ((uint64_t)1 << mid)) {
      min = mid;
    }
    else {
      max = mid;
    }
  }

  return max;
}

constexpr inline uint64_t pow_10(uint64_t v) {
  if (v > 19) {
    throw std::exception("Power too high!");
    return 0;
  }

  constexpr uint64_t pow10[] ={
    1ull,
    10ull,
    100ull,
    1000ull,
    10000ull,
    100000ull,
    1000000ull,
    10000000ull,
    100000000ull,
    1000000000ull,
    10000000000ull,
    100000000000ull,
    1000000000000ull,
    10000000000000ull,
    100000000000000ull,
    1000000000000000ull,
    10000000000000000ull,
    100000000000000000ull,
    1000000000000000000ull,
    10000000000000000000ull
  };

  return pow10[v];
}

constexpr inline uint64_t log_10_floor(uint64_t v) {
  if (v == 0) {
    throw std::exception("MATH ERROR! Cannot log of 0");
    return 0;
  }

  //Max uint64_t = 18446744073709551615
  //Max pow 10   = 10000000000000000000ull

  auto max = 19;
  auto min = 0;

  if (v >= pow_10(max)) { return max; }

  while (max >= min + 1) {
    auto mid = (max + min) / 2;
    if (v > pow_10(mid)) {
      min = mid;
    }
    else {
      max = mid;
    }
  }

  return max;
}

//Log 2 optimised for small numbers
//Floors the output
constexpr inline uint64_t small_log_2_floor(uint64_t v) {
  if (v == 0) {
    throw std::exception("MATH ERROR! Cannot log of 0");
    return 0;
  }

  uint64_t min = 0;

  while (v > (uint64_t)1 << min) {
    min++;
  }

  return min;
}

constexpr inline uint64_t small_log_2_ceil(uint64_t v) {
  if (v == 0) {
    throw std::exception("MATH ERROR! Cannot log of 0");
    return 0;
  }

  uint8_t found1 = 0;//max value of 1
  uint64_t min = 0;

  while (v > (uint64_t)1 << min) {
    found1 |= (v >> min) & 0b1;//tests if it is a 1 
    min++;
  }

  return min + found1;
}

constexpr inline uint8_t absolute(int8_t i) {
  if (i == INT32_MIN) {
    return static_cast<uint16_t>(INT8_MAX) + 1u;
  }
  else if (i < 0) {
    return static_cast<uint8_t>(-i);
  }
  else {
    return static_cast<uint8_t>(i);
  }
}

constexpr inline uint16_t absolute(int16_t i) {
  if (i == INT32_MIN) {
    return static_cast<uint16_t>(INT16_MAX) + 1u;
  }
  else if (i < 0) {
    return static_cast<uint16_t>(-i);
  }
  else {
    return static_cast<uint16_t>(i);
  }
}

constexpr inline uint32_t absolute(int32_t i) {
  if (i == INT32_MIN) {
    return static_cast<uint32_t>(INT32_MAX) + 1u;
  }
  else if (i < 0) {
    return static_cast<uint32_t>(-i);
  }
  else {
    return static_cast<uint32_t>(i);
  }
}

constexpr inline uint64_t absolute(int64_t i) {
  if (i == INT64_MIN) {
    return 0x8000000000000000ull;
  }
  else if (i < 0) {
    return static_cast<uint64_t>(-i);
  }
  else {
    return static_cast<uint64_t>(i);
  }
}

template<typename T>
struct Array {
  T* data     = nullptr;// ptr to data in the array
  size_t size     = 0;// used size
  size_t capacity = 0;

  //No copy!
  Array(const Array&) = delete;

  Array(Array&& arr) noexcept : data(arr.data), size(arr.size), capacity(arr.capacity)
  {
    arr.data = nullptr;
    arr.size = 0;
    arr.capacity = 0;
  }

  Array() noexcept = default;
  Array(size_t s) noexcept : data(allocate_default<T>(s)), size(s), capacity(s) {}

  Array& operator=(Array&& arr) noexcept {
    free();

    data = std::exchange(arr.data, nullptr);
    size = std::exchange(arr.size, 0);
    capacity = std::exchange(arr.capacity, 0);

    return *this;
  }

  void free() {
    ::free<T>(data);
    data = nullptr;
    size = 0;
    capacity = 0;
  }

  ~Array() noexcept {
    free();
  }

  const T* begin() const { return data; }
  const T* end() const { return data + size; }

  T* back() { return data + size - 1; }

  T* mut_begin() { return data; }
  T* mut_end() { return data + size; }

  T remove_at(const size_t index) {
    T t = std::move(data[index]);

    for (size_t i = index; i < size - 1; i++) {
      data[index] = std::move(data[index + 1]);
    }
    size--;
    return t;
  }

  template<typename L>
  void remove_if(L&& lambda) {
    size_t num_removed = 0;

    for (size_t i = 0; i < size; i++) {
      if (lambda(data[i])) {
        num_removed++;
      }
      else {
        if (num_removed > 0) {
          data[i - num_removed] = std::move(data[i]);
        }
      }
    }
    size -= num_removed;
  }

  template<typename L>
  bool any_of(L&& lambda) const {
    auto i = begin();
    const auto i_end = end();

    for (; i < i_end; i++) {
      if (lambda(i)) {
        return true;
      }
    }

    return false;
  }

  void replace_a_with_b(const T& a, const T& b) {
    for (size_t i = 0; i < size; i++) {
      if (data[i] == a) {
        data[i] = b;
      }
    }
  }

  void insert(T t) noexcept {
    try_reserve_next(size + 1);

    new(data + size) T(std::move(t));
    size++;
  }

  void insert_uninit(const size_t num) noexcept {
    reserve_extra(num);
    size += num;
  }

  //Test for extra space after the size (not capacity)
  void reserve_extra(const size_t extra) noexcept {
    try_reserve_next(size + extra);
  }

  //Test for a total needed space
  void reserve_total(const size_t total) noexcept {
    try_reserve_next(total);
  }

  void try_reserve_next(const size_t total_required) noexcept {
    const size_t prev = capacity;

    //Min capacity should be 8
    if (capacity < 8) {
      capacity = 8;

      //8 might still be less that total_required
      while (total_required > capacity) {
        capacity <<= 1;
      }
    }
    else if (total_required > capacity) {
      do {
        capacity <<= 1;
      } while (total_required > capacity);
    }
    else {
      return;
    }

    data = reallocate_default<T>(data, prev, capacity);
  }

  void shrink() noexcept {
    if (size == 0) {
      free();
    }
    else {
      capacity = size;
      data = reallocate_uninit<T>(data, capacity);
    }
  }

  void clear() noexcept {
    auto i = mut_begin();
    auto end = mut_end();

    for (; i < end; i++) {
      i->~T();
    }

    size = 0;
  }

  void pop() noexcept {
    size--;
    (data + size)->~T();
  }

  void pop(size_t num) noexcept {
    const auto* old_end = data + size;

    if (num > size) {
      size = 0;
    }
    else {
      size -= num;
    }

    auto* i = data + size;

    for (; i < old_end; i++) {
      i->~T();
    }
  }

  bool contains(const T& t) const noexcept {
    auto i = begin();
    const auto end_i = end();

    for (; i < end_i; i++) {
      if (*i == t) {
        return true;
      }
    }

    return false;
  }

  void concat(Array<T>&& arr) noexcept {
    reserve_extra(arr.size);
    memcpy_ts(data + size, (capacity - size), arr.data, arr.size);

    size += arr.size;

    arr.free();
  }
};

template<typename T>
struct LinkedList {
  struct BLOCK {
    constexpr static size_t BLOCK_SIZE = 32;

    size_t filled = 0;
    BLOCK* next = nullptr;

    T data[BLOCK_SIZE];

    ~BLOCK() {
      free<BLOCK>(next);
    }
  };

  struct Iter {
    size_t index = 0;
    BLOCK* block = nullptr;

    Iter() = default;

    void next() {
      index++;
      if (index == BLOCK::BLOCK_SIZE) {
        index = 0;
        block = block->next;
      }
      else if (index == block->filled) {
        index = 0;
        block = nullptr;
      }
    }

    T* get() {
      return block->data + index;
    }

    bool operator!=(const Iter& i) const {
      return (index != i.index) || (block != i.block);
    }
    bool operator==(const Iter& i) const {
      return (index == i.index) && (block == i.block);
    }
  };

  struct ConstIter {
    size_t index = 0;
    const BLOCK* block = nullptr;

    ConstIter() = default;

    void next() {
      index++;
      if (index == BLOCK::BLOCK_SIZE) {
        index = 0;
        block = block->next;
      }
      else if (index == block->filled) {
        index = 0;
        block = nullptr;
      }
    }

    const T* get() const {
      return block->data + index;
    }

    bool operator!=(const ConstIter& i) const {
      return (index != i.index) || (block != i.block);
    }
    bool operator==(const ConstIter& i) const {
      return (index == i.index) && (block == i.block);
    }
  };

  Iter begin_iter() {
    return Iter{ 0, first };
  }

  Iter end_iter() {
    return Iter();
  }

  ConstIter begin_const_iter() const {
    return ConstIter{ 0, first };
  }

  ConstIter end_const_iter() const {
    return ConstIter();
  }

  BLOCK* first = nullptr;
  BLOCK* last = nullptr;

  LinkedList() : first(allocate_default<BLOCK>()), last(first) {}
  ~LinkedList() {
    free<BLOCK>(first);
  }

  template<typename ... U>
  T* insert(U&& ... u) {
    if (last->filled == BLOCK::BLOCK_SIZE) {
      last->next = allocate_default<BLOCK>();
      last = last->next;
    }

    new (last->data + last->filled) T(std::forward<U>(u)...);
    last->filled++;

    return last->data + last->filled - 1;
  }
};

struct ArenaAllocator {
  static_assert(sizeof(void*) == sizeof(uint64_t), "Must be 8 bytes");

  struct Block {
    constexpr static size_t BLOCK_SIZE = 128;
    uint64_t data[BLOCK_SIZE] ={};

    Block* next = nullptr;

    Block() = default;
    ~Block();
  };

  struct FreeList {
    uint64_t qwords_available = 0;
    FreeList* next = nullptr;
  };

  Block* base = nullptr;
  void* free_list = nullptr;

  ArenaAllocator() = default;
  ~ArenaAllocator();


  void new_block();
  void add_to_free_list(FreeList* fl);

  void* alloc_no_construct(size_t bytes);
  void free_no_destruct(void* val);
};

template<typename T>
struct FreelistBlockAllocator {
  struct Element;

  struct Header {
    Element* next = nullptr;
  };

  struct Element {
    union {
      Header header ={};
      T el;
    };

    Element() : header() {}
    ~Element() {}
  };

  struct BLOCK {
    constexpr static size_t BLOCK_SIZE = 32;

    size_t filled = 0;
    BLOCK* prev = nullptr;

    Element data[BLOCK_SIZE] ={};
  };

  BLOCK* top;
  Element* alloc_list;

  FreelistBlockAllocator()
    : top(::allocate_default<BLOCK>()) {
    alloc_list = top->data;

    for (auto i = 0; i < BLOCK::BLOCK_SIZE - 1; i++) {
      top->data[i].header.next = top->data + i + 1;
    }

    top->data[BLOCK::BLOCK_SIZE - 1].header.next = nullptr;
  }

  ~FreelistBlockAllocator() {
    while (top != nullptr) {
      BLOCK* next = top->prev;
      ::free<BLOCK>(top);
      top = next;
    }

    top = nullptr;
    alloc_list = nullptr;
  }

  void new_block() {
    BLOCK* const new_b = ::allocate_default<BLOCK>();

    new_b->prev = top;
    top = new_b;


    for (auto i = 0; i < BLOCK::BLOCK_SIZE - 1; i++) {
      new_b->data[i].header.next = new_b->data + i + 1;
    }

    new_b->data[BLOCK::BLOCK_SIZE - 1].header.next = alloc_list;
    alloc_list = new_b->data;
  }

  T* allocate() {
    if (alloc_list == nullptr) {
      new_block();
    }

    T* const new_t = (T*)alloc_list;
    alloc_list = alloc_list->header.next;

    new(new_t) T();

    return new_t;
  }

  void free(const T* t) {
    Element* new_e = (Element*)t;
    new_e->el.~T();
    new_e->header.next = alloc_list;

    alloc_list = new_e;
  }
};

struct SquareBitMatrix {
  //Per block: ceil(side_length / 8) bytes = (side_length / 8) + 1

  //Block length = (side_length / 8) + 1
  //Block pointer = data + (Block length * val)
  //each bit corresponds to its equivalent value at that bit index

  uint8_t* data = nullptr;
  size_t side_length = 0;
  size_t capacity = 0;

  void free() {
    ::free<uint8_t>(data);

    data = nullptr;
    side_length = 0;
    capacity = 0;
  }

  ~SquareBitMatrix() {
    free();
  }

  bool test_a_intersects_b(size_t a, size_t b) const {
    const size_t bytes_per_val = (side_length / 8) + 1;
    const uint8_t* a_data = data + bytes_per_val * a;

    const size_t b_mod8 = b % 8;
    const size_t b_div8 = b / 8;

    return (a_data[b_div8] & (1 << b_mod8)) > 0;
  }

  void set_a_intersects_b(size_t a, size_t b) {
    const size_t bytes_per_val = (side_length / 8) + 1;
    uint8_t* a_data = data + bytes_per_val * a;

    const size_t b_mod8 = b % 8;
    const size_t b_div8 = b / 8;

    a_data[b_div8] |= (uint8_t)(1 << b_mod8);
  }

  void remove_a_intersects_b(size_t a, size_t b) {
    const size_t bytes_per_val = (side_length / 8) + 1;
    uint8_t* a_data = data + bytes_per_val * a;

    const size_t b_mod8 = b % 8;
    const size_t b_div8 = b / 8;

    a_data[b_div8] &= ~(uint8_t)(1 << b_mod8);
  }

  size_t new_value() {
    const size_t bytes_per_val_now = side_length == 0 ? 0
      : ((side_length - 1) / 8) + 1;
    const size_t bytes_per_val_next = ((side_length + 1 - 1) / 8) + 1;
    const size_t required_capacity = bytes_per_val_next * (side_length + 1);

    //check if we have enough space
    if (required_capacity > capacity) {
      const size_t old_capacity = capacity;

      capacity = (size_t)1 << small_log_2_ceil(required_capacity);
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
};

template<typename T>
struct OwnedPtr {
  T* ptr;

  OwnedPtr() = delete;
  OwnedPtr(const OwnedPtr&) = delete;

  OwnedPtr(OwnedPtr&& ptr_in) : ptr(ptr_in.ptr) {
    ptr_in.ptr = nullptr;
  }

  OwnedPtr(Array<T>&& arr) : ptr(arr.data) {
    arr.data     = nullptr;
    arr.size     = 0;
    arr.capacity = 0;
  }

  ~OwnedPtr() {
    free<T>(ptr);
  }
};

template<typename T>
void copy_array(const Array<T>& from, Array<T>& to) noexcept {
  to.clear();
  to.reserve_total(from.size);

  for (size_t i = 0; i < from.size; i++) {
    to.data[i] = from.data[i];
  }

  to.size += from.size;
}

template<typename T>
void combine_unique(const Array<T>& from, Array<T>& to) noexcept {
  auto i = from.begin();
  const auto end = from.end();

  const size_t initial_size = to.size;
  for (; i < end; i++) {
    for (size_t i_to = 0; i_to < initial_size; i_to++) {
      if (*i == to.data[i_to]) {
        goto NEXT_COMBINE;
      }
    }

    to.insert(*i);

  NEXT_COMBINE:
    continue;
  }
}

template<typename T>
void reverse_array(Array<T>& arr) noexcept {
  if (arr.size == 0) { return; }

  auto* i_beg  = arr.data;
  auto* i_back = arr.data + arr.size - 1;

  //Swap first with back - then step inwards
  while (i_beg < i_back) {
    T temp  = std::move(*i_beg);
    *i_beg  = std::move(*i_back);
    *i_back = std::move(temp);

    i_beg++;
    i_back--;
  }
}

#define ERROR_CODES_X \
modify(OK)\
modify(COULD_NOT_CREATE_FILE)\
modify(COULD_NOT_CLOSE_FILE)\
modify(UNDEFINED_INSTRUCTION)\
modify(STACK_OVERFLOW)

enum class ErrorCode {
#define modify(NAME) NAME,
  ERROR_CODES_X
#undef modify
};

namespace ErrorCodeString {
#define modify(NAME) inline constexpr char NAME[] = #NAME;
  ERROR_CODES_X
  #undef modify
}

constexpr const char* error_code_string(const ErrorCode code) {
  switch (code) {
  #define modify(NAME) case ErrorCode:: ## NAME : return ErrorCodeString:: ## NAME ;
    ERROR_CODES_X
    #undef modify
  }

  return nullptr;
}


template<typename T>
void load_to_bytes(Array<uint8_t>& bytes, size_t offset, const T& t) {
  bytes.reserve_total(offset + sizeof(T));
  memcpy_ts((T*)bytes.data + offset, bytes.size - offset, &t, 1);
}

template<typename T>
void load_to_bytes(uint8_t* bytes, size_t bytes_size, size_t offset, const T& t) {
  memcpy_ts((T*)bytes + offset, bytes_size - offset, &t, 1);
}

void load_to_bytes(Array<uint8_t>& bytes,
                   const size_t offset,
                   const uint8_t* in_bytes,
                   const size_t len);

constexpr inline void load_to_bytes(uint8_t* bytes, size_t bytes_size,
                                    const size_t offset,
                                    const uint8_t* in_bytes,
                                    const size_t len) {
  memcpy_ts(bytes + offset, bytes_size - offset, in_bytes, len);
}

namespace BIT_MASKS {
  enum BIT_MASKS : uint8_t {
    B0 = 0b0000'0001,
    B1 = 0b0000'0010,
    B2 = 0b0000'0100,
    B3 = 0b0000'1000,
    B4 = 0b0001'0000,
    B5 = 0b0010'0000,
    B6 = 0b0100'0000,
    B7 = 0b1000'0000,
  };
}

#define SET_MASK(val, mask) (val |= mask)
#define RESET_MASK(val, mask) (val &= ~mask)
#define TEST_MASK(val, mask) ((val & mask) != 0)

template<typename T>
constexpr T combine_flag(const T full, const T mask, const bool set) {
  return (full & ~mask) | (mask * set);
}

#define COMBINE_FLAG(full, mask, set) (combine_flag(full, mask, set))
#define SET_FLAG(full, mask, set) (full = COMBINE_FLAG(full, mask, set))



union X64_UNION {
  uint64_t val = 0;
  int64_t sig_val;
  double flt;
  void* vptr;

  constexpr X64_UNION() = default;
  constexpr X64_UNION(X64_UNION&& v) = default;
  constexpr X64_UNION(const X64_UNION& v) = default;

  constexpr X64_UNION& operator=(X64_UNION&& v) = default;
  constexpr X64_UNION& operator=(const X64_UNION& v) = default;

  constexpr X64_UNION(uint64_t v) : val(v) {}
  constexpr X64_UNION(int64_t v) : sig_val(v) {}
  constexpr X64_UNION(double v) : flt(v) {}

  template<typename T>
  constexpr X64_UNION(T* v) : vptr((void*)v) {}

  inline constexpr operator uint64_t() const {
    return val;
  }

  inline constexpr operator int64_t() const {
    return sig_val;
  }

  inline constexpr operator double() const {
    return flt;
  }

  template<typename T>
  inline constexpr operator T* () const {
    return (T*)vptr;
  }
};

//TODO: Big endian??
inline constexpr X64_UNION x64_from_bytes(const uint8_t* const bytes) noexcept {
  X64_UNION val;
  val.val = (static_cast<uint64_t>(bytes[7]) << 56)
    | (static_cast<uint64_t>(bytes[6]) << 48)
    | (static_cast<uint64_t>(bytes[5]) << 40)
    | (static_cast<uint64_t>(bytes[4]) << 32)
    | (static_cast<uint64_t>(bytes[3]) << 24)
    | (static_cast<uint64_t>(bytes[2]) << 16)
    | (static_cast<uint64_t>(bytes[1]) << 8)
    |  static_cast<uint64_t>(bytes[0]);

  return val;
}

inline constexpr ErrorCode x64_from_bytes_s(const uint8_t* buffer,
                                            size_t buffer_length,
                                            X64_UNION& uint,
                                            uint8_t* const bytes) noexcept {
  if (bytes >= buffer && buffer + buffer_length - 8 <= bytes) {
    //safe
    uint = x64_from_bytes(bytes);
    return ErrorCode::OK;
  }
  else {
    return ErrorCode::STACK_OVERFLOW;
  }
}

inline constexpr uint32_t x32_from_bytes(const uint8_t* const bytes) noexcept {
  return (static_cast<uint32_t>(bytes[3]) << 24)
    | (static_cast<uint32_t>(bytes[2]) << 16)
    | (static_cast<uint32_t>(bytes[1]) << 8)
    |  static_cast<uint32_t>(bytes[0]);
}

//TODO: Big endian??
inline constexpr void x64_to_bytes(const X64_UNION uint, uint8_t* const bytes) noexcept {
  bytes[7] = static_cast<uint8_t>(uint.val >> 56);
  bytes[6] = static_cast<uint8_t>(uint.val >> 48);
  bytes[5] = static_cast<uint8_t>(uint.val >> 40);
  bytes[4] = static_cast<uint8_t>(uint.val >> 32);
  bytes[3] = static_cast<uint8_t>(uint.val >> 24);
  bytes[2] = static_cast<uint8_t>(uint.val >> 16);
  bytes[1] = static_cast<uint8_t>(uint.val >> 8);
  bytes[0] = static_cast<uint8_t>(uint.val);
}

inline constexpr void x32_to_bytes(const uint32_t uint, uint8_t* const bytes) noexcept {
  bytes[3] = static_cast<uint8_t>(uint >> 24);
  bytes[2] = static_cast<uint8_t>(uint >> 16);
  bytes[1] = static_cast<uint8_t>(uint >> 8);
  bytes[0] = static_cast<uint8_t>(uint);
}

inline constexpr ErrorCode x64_to_bytes_s(const uint8_t* buffer,
                                          size_t buffer_length,
                                          const X64_UNION uint,
                                          uint8_t* const bytes) noexcept {
  if (bytes >= buffer && buffer + buffer_length - 8 <= bytes) {
    //safe
    x64_to_bytes(uint, bytes);
    return ErrorCode::OK;
  }
  else {
    return ErrorCode::STACK_OVERFLOW;
  }
}

template<typename RET, typename ... PARAMS>
struct FUNCTION_PTR_IMPL {
  FUNCTION_PTR_IMPL() = delete;

  using TYPE = RET(*)(PARAMS...);
};

template<typename RET, typename ... PARAMS>
using FUNCTION_PTR = typename FUNCTION_PTR_IMPL<RET, PARAMS...>::TYPE;

inline constexpr size_t NUM_VM_REGS = 16;

template<typename T>
constexpr inline T square(T t) { return t * t; }

struct ValueIndex {
  uint64_t val;

  constexpr bool operator==(const ValueIndex v) const {
    return v.val == val;
  }
};

template<typename T>
constexpr inline T larger(T t1, T t2) noexcept {
  return t1 > t2 ? t1 : t2;
}

template<typename T>
constexpr inline T smaller(T t1, T t2) noexcept {
  return t1 < t2 ? t1 : t2;
}

template<typename T>
struct EXECUTE_AT_END {
  T t;

  constexpr EXECUTE_AT_END(T&& t_) : t(std::move(t_)) {}

  ~EXECUTE_AT_END() {
    t();
  }
};

template<typename T>
EXECUTE_AT_END(T&& t) -> EXECUTE_AT_END<T>;

#define DEFER(...) EXECUTE_AT_END JOIN(defer, __LINE__) = [__VA_ARGS__]() mutable ->void 