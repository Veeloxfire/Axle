#pragma once
#include <stdlib.h>
#include <new>

template<typename T>
T* allocate() {
  return (T*) malloc(sizeof(T));
}

template<typename T>
T* allocate_zerod() {
  T* t = (T*)malloc(sizeof(T));
  memset((void*)t, 0, sizeof(T));
  return t;
}

template<typename T>
T* allocate_zerod(const size_t num) {
  T* t = (T*)malloc(sizeof(T) * num);
  memset((void*)t, 0, sizeof(T) * num);
  return t;
}

template<typename T>
T* allocate_default() {
  T* t = (T*)malloc(sizeof(T));
  new(t) T();
  return t;
}

template<typename T>
T* allocate(size_t num) {
  return (T*) malloc(sizeof(T) * num);
}

template<typename T>
T* reallocate(T* ptr, size_t num) {
  return (T*) realloc((void*)ptr, sizeof(T) * num);
}

template<typename T>
void free(T* ptr) {
  free((void*)ptr);
}