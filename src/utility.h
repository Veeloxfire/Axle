#pragma once

#include <stdint.h>
#include <string.h>
#include <utility>
#include <new>

template<typename T>
struct Array {
  T*     data     = nullptr;// ptr to data in the array
  size_t size     = 0;
  size_t capacity = 0;

  Array(size_t s) : data(new T[s]), size(s) {}
  ~Array() {
    if(data != nullptr) delete[] data;
  }
  void insert(T&& t) {
    if(size == capacity)
      reserve(capacity);

    new(data + size) T(std::move(t));
    size++;
  }

  void reserve(size_t extra) {
    capacity += extra;
    T* new_data = new T[capacity];

    for(unsigned int i = 0; i < size; i++)
      new_data[i] = std::move(data[i]);

    delete[] data;
    data = new_data;
  }
};

enum class ErrorCode {
  NO_ERROR = 0,
  COULD_NOT_CREATE_FILE,
  COULD_NOT_CLOSE_FILE,
}

template<typename T>
inline void load_to_bytes(uint8_t* bytes, size_t offset, const T& t) {
  memcpy(bytes + offset, &t, sizeof(T));
}
