#pragma once
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <new>

#include <memory>

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
T* allocate_default(const size_t num = 1) {
  T* t = (T*)std::malloc(sizeof(T) * num);

  assert(t != nullptr);

  default_init(t, num);
  return t;
}

template<typename T, typename ... U>
T* allocate_single_constructed(U&& ... u) {
  T* t = (T*)std::malloc(sizeof(T));

  assert(t != nullptr);

  new(t) T(std::forward<U>(u...));
  return t;
}

template<typename T>
T* allocate_uninit(size_t num = 1) {
  T* val = (T*)std::malloc(sizeof(T) * num);
  assert(val != nullptr);
  return val;
}

template<typename T>
T* reallocate_uninit(T* ptr, const size_t num = 1) {
  T* val = (T*)std::realloc((void*)ptr, sizeof(T) * num);
  assert(val != nullptr);
  return val;
}

template<typename T>
T* reallocate_default(T* ptr, const size_t old_size, const size_t new_size) {
  T* val = (T*)std::realloc((void*)ptr, sizeof(T) * new_size);
  assert(val != nullptr);

  if (old_size < new_size) {
    default_init(val + old_size, new_size - old_size);
  }

  return val;
}

template<typename T>
void free(T* ptr) {
  std::free((void*)ptr);
}

constexpr size_t strlen_ts(const char* c) {
  const char* const base = c;
  while(*c != '\0') {c++;}

  return c - base;
}