#pragma once
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <new>

#include <memory>

using u8 = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;
using i8 = int8_t;
using i16 = int16_t;
using i32 = int32_t;
using i64 = int64_t;

//#define COUNT_ALLOC

template<typename T>
constexpr inline void memcpy_ts(T* dest, size_t dest_size, const T* source, size_t src_size) {
  const errno_t res = memcpy_s((void*)dest, dest_size * sizeof(T), (const void*)source, src_size * sizeof(T));
  assert(res == 0);
}

template<typename T>
constexpr inline int memcmp_ts(const T* buff1, const T* buff2, size_t length) {
  return memcmp((const void*)buff1, (const void*)buff2, length * sizeof(T));
}

template<typename T>
constexpr inline void zero_init(T* const dest, const size_t dest_size) {
  memset((void*)dest, 0, dest_size * sizeof(T));
}

template<typename T>
constexpr inline void default_init(T* const dest, const size_t dest_size) {
  zero_init(dest, dest_size);

  for (auto i = 0; i < dest_size; i++) {
    new(dest + i) T();
  }
}

template<typename T>
constexpr inline void destruct_arr(T* const ptr, const size_t num) {
  for (size_t i = 0; i < num; i++) {
    ptr[i].~T();
  }
}

template<typename T>
constexpr inline void destruct_single(T* const ptr) {
  ptr->~T();
}

template<typename T>
constexpr inline void default_init(T* const dest) {
  new(dest) T();
}

#ifdef COUNT_ALLOC
template<typename T>
void free_heap_check(T* ptr, size_t num_bytes) {
  const uint8_t* ptr_end = (const uint8_t*)ptr + num_bytes;

  for (size_t i = 0; i < 4; i++) {
    assert(ptr_end[i] == 0xFD);
  }
}

struct ALLOC_COUNTER {
  struct Allocation {
    const char* type_name;
    const void* mem;
    size_t size;
  };

  Allocation* allocs = nullptr;
  size_t num_allocs = 0;
  size_t capacity = 0;

  size_t current_allocated_size = 0;

  size_t max_allocated_blocks = 0;
  size_t max_allocated_size   = 0;

  size_t update_calls = 0;
  size_t null_remove_calls = 0;
  size_t valid_remove_calls = 0;
  size_t insert_calls = 0;

  template<typename T>
  void insert(T* t, size_t num) {
    insert_calls++;

    if (capacity == num_allocs) {
      if (capacity == 0) {
        capacity = 8;
      }
      else {
        capacity <<= 1;
      }


      auto* new_allocs = (Allocation*)std::realloc(allocs, capacity * sizeof(Allocation));
      assert(new_allocs != nullptr);

      allocs = new_allocs;
    }

    allocs[num_allocs].type_name = typeid(T*).name();
    allocs[num_allocs].mem  = (const void*)t;
    allocs[num_allocs].size = num * sizeof(T);

    num_allocs++;
    if (num_allocs > max_allocated_blocks) {
      max_allocated_blocks = num_allocs;
    }

    current_allocated_size += num * sizeof(T);
    if (current_allocated_size > max_allocated_size) {
      max_allocated_size = current_allocated_size;
    }
  }

  template<typename T>
  void update(T* from, T* to, size_t num) {
    if (from == nullptr) {
      insert<T>(to, num);
      return;
    }

    update_calls++;

    const void* f_v = (const void*)from;

    auto i = allocs;
    const auto end = allocs + num_allocs;

    for (; i < end; i++) {
      if (i->mem == f_v) {
        i->mem = (const void*)to;

        current_allocated_size -= i->size;
        i->size = num * sizeof(T);
        current_allocated_size += i->size;

        if (current_allocated_size > max_allocated_size) {
          max_allocated_size = current_allocated_size;
        }

        return;
      }
    }
  }

  template<typename T>
  void remove(T* t) {
    if (t == nullptr) {
      null_remove_calls++;
      return;
    }

    valid_remove_calls++;

    const void* t_v = (void*)t;

    auto i = allocs;
    const auto end = allocs + num_allocs;

    for (; i < end; i++) {
      if (i->mem == t_v) {
        free_heap_check(t, i->size);

        current_allocated_size -= i->size;
        num_allocs--;
        goto REMOVE;
      }
    }

    return;

  REMOVE:
    auto mov_i = i++;

    for (; i < end; (i++, mov_i++)) {
      *mov_i = std::move(*i);
    }
  }


  static ALLOC_COUNTER& allocated() {
    static ALLOC_COUNTER allocated_s ={};

    return allocated_s;
  }
};
#endif

template<typename T>
T* allocate_default(const size_t num) {
  T* t = (T*)std::malloc(sizeof(T) * num);

  assert(t != nullptr);

#ifdef COUNT_ALLOC
  ALLOC_COUNTER::allocated().insert(t, num);
#endif

  default_init(t, num);
  return t;
}

template<typename T>
inline T* allocate_default() {
  return allocate_default<T>(1);
}

template<typename T, typename ... U>
T* allocate_single_constructed(U&& ... u) {
  T* t = (T*)std::malloc(sizeof(T));

  assert(t != nullptr);

#ifdef COUNT_ALLOC
  ALLOC_COUNTER::allocated().insert(t, 1);
#endif

  new(t) T(std::forward<U>(u...));
  return t;
}

template<typename T>
T* reallocate_default(T* ptr, const size_t old_size, const size_t new_size) {
  T* val = (T*)std::realloc((void*)ptr, sizeof(T) * new_size);
  assert(val != nullptr);

  if (old_size < new_size) {
    default_init(val + old_size, new_size - old_size);
  }

#ifdef COUNT_ALLOC
  ALLOC_COUNTER::allocated().update(ptr, val, new_size);
#endif

  return val;
}

template<typename T>
void free_destruct_single(T* ptr) {
#ifdef COUNT_ALLOC
  ALLOC_COUNTER::allocated().remove(ptr);
#endif

  if (ptr == nullptr) return;

  ptr->~T();
  std::free((void*)ptr);
}

template<typename T>
void free_destruct_n(T* ptr, size_t num) {
#ifdef COUNT_ALLOC
  ALLOC_COUNTER::allocated().remove(ptr);
#endif

  if (ptr == nullptr) return;

  for (size_t i = 0; i < num; i++) {
    ptr[i].~T();
  }

  std::free((void*)ptr);
}

template<typename T>
void free_no_destruct(T* ptr) {
#ifdef COUNT_ALLOC
  ALLOC_COUNTER::allocated().remove(ptr);
#endif

  if (ptr == nullptr) return;

  std::free((void*)ptr);
}

constexpr size_t strlen_ts(const char* c) {
  const char* const base = c;
  while (*c != '\0') { c++; }

  return c - base;
}