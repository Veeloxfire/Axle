#pragma once

#include <stdint.h>
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
