#pragma once
#include "utility.h"

#include <windows.h>
#include <memoryapi.h>


namespace Windows {
  template<typename T> 
  struct VirtualPtr {
    T* ptr;
    size_t size;

    template<typename U, typename ... J>
    U call(J&& ... t) {
      return ((FUNCTION_PTR<U, J...>)ptr)(std::forward<J>(t)...);
    }

    template<typename U>
    U call() {
      return ((FUNCTION_PTR<U>)ptr)();
    }
  };

  template<typename T>
  VirtualPtr<T> get_exectuable_memory(size_t num) {
    VirtualPtr<T> ptr;
    ptr.ptr = (T*) VirtualAlloc(nullptr, sizeof(T) * num, MEM_COMMIT | MEM_RESERVE, PAGE_EXECUTE_READWRITE);
    ptr.size = sizeof(T) * num;

    return ptr;
  }

  template<typename T>
  void free_executable_memory(VirtualPtr<T>& ptr) {
    VirtualFree(ptr.ptr, ptr.size, MEM_RELEASE);
  }
}

