#pragma once
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <new>

#include <algorithm>

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
T* allocate() {
  return (T*)malloc(sizeof(T));
}

template<typename T>
T* allocate_zerod(const size_t num) {
  T* t = (T*)std::malloc(sizeof(T) * num);

  assert(t != nullptr);

  zero_init(t, num);
  return t;
}

template<typename T>
inline T* allocate_zerod() {
  return allocate_zerod<T>(1);
}

template<typename T>
T* allocate(size_t num) {
  T* val = (T*)std::malloc(sizeof(T) * num);
  assert(val != nullptr);
  return val;
}

template<typename T>
T* reallocate(T* ptr, size_t num) {
  T* val = (T*)std::realloc((void*)ptr, sizeof(T) * num);
  assert(val != nullptr);
  return val;
}

template<typename T>
void free(T* ptr) {
  std::free((void*)ptr);
}