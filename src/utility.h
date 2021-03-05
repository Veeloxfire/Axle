#pragma once

#include <stdint.h>
#include <string.h>
#include <utility>
#include <new>

#include "allocators.h"

#define BYTE(a) (static_cast<uint8_t>(a))

struct DefaultAllocator {
  DefaultAllocator() noexcept = default;

  template<typename T>
  T* reallocate(T* data, size_t capacity) {
    return ::reallocate<T>(data, capacity);
  }

  template<typename T>
  void free(T* data) {
    ::free<T>(data);
  }
};

template<typename T>
struct Array {
  T*     data     = nullptr;// ptr to data in the array
  size_t size     = 0;// used size
  size_t capacity = 0;

  //No copy yet
  Array(const Array&) = delete;
  Array(Array&&) noexcept = default;

  Array() noexcept = default;
  Array(size_t s) noexcept : data(allocate_zerod<T>(s)) {}

  Array& operator=(Array&& arr) noexcept {
    this->~Array();

    data = std::exchange(arr.data, nullptr);
    size = std::exchange(arr.size, 0);
    capacity = std::exchange(arr.capacity, 0);

    return *this;
  }

  ~Array() noexcept {
    free<T>(data);
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


  template<typename ... U>
  void insert(U&& ... u) noexcept {
    try_reserve_next(size + 1);

    new(data + size) T(std::forward<U>(u)...);
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

    data = reallocate<T>(data, capacity);

    //Zero new memory
    memset(data + prev, 0, (capacity - prev) * sizeof(T));
  }
};

template<typename T>
struct LinkedList {
  struct BLOCK {
    constexpr static size_t BLOCK_SIZE = 32;

    size_t filled;
    BLOCK* next;

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

  Iter begin_iter() {
    return Iter{ 0, first };
  }

  Iter end_iter() {
    return Iter();
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

template<typename T>
struct Array_View {
  const T* data = nullptr;
  size_t size = 0;

  Array_View() noexcept = default;

  template<size_t I>
  Array_View(const T (&arr)[I]) noexcept : data(arr), size(I) {}

  Array_View(const Array<T>& arr)
    : data(arr.data), size(arr.size)
  {}

  bool has_value() const { return data != NULL && size > 0; }
};

#define ERROR_CODES_X \
modify(NO_ERROR)\
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
}


template<typename T>
inline void load_to_bytes(Array<uint8_t>& bytes, size_t offset, const T& t) {
  bytes.reserve_total(offset + sizeof(T));
  memcpy(bytes.data + offset, &t, sizeof(T));
}

template<typename T>
inline void load_to_bytes(uint8_t* bytes, size_t offset, const T& t) {
  memcpy(bytes + offset, &t, sizeof(T));
}

inline void load_to_bytes(Array<uint8_t>& bytes,
                          const size_t offset,
                          const uint8_t* in_bytes,
                          const size_t len) {
  bytes.reserve_total(offset + len);
  memcpy(bytes.data + offset, in_bytes, len);
}

inline void load_to_bytes(uint8_t* bytes,
                          const size_t offset,
                          const uint8_t* in_bytes,
                          const size_t len) {
  memcpy(bytes + offset, in_bytes, len);
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
  inline constexpr operator T*() const {
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
    return ErrorCode::NO_ERROR;
  }
  else {
    return ErrorCode::STACK_OVERFLOW;
  }
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
    return ErrorCode::NO_ERROR;
  }
  else {
    return ErrorCode::STACK_OVERFLOW;
  }
}

template<typename RET, typename ... PARAMS>
struct FUNCTION_PTR_IMPL {
  FUNCTION_PTR_IMPL() = delete;

  using TYPE = RET (*)(PARAMS...);
};

template<typename RET, typename ... PARAMS>
using FUNCTION_PTR = typename FUNCTION_PTR_IMPL<RET, PARAMS...>::TYPE;