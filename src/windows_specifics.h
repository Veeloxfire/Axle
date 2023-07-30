#pragma once
#include <stdint.h>
#include <windows.h>
#include <memoryapi.h>

#include "utility.h"

namespace Windows {
  template<typename T> 
  struct VirtualPtr {
    T* ptr = nullptr;
    size_t size = 0;
    size_t entry = 0;

    template<typename U, typename ... J>
    U call(J&& ... t) {
      return ((FUNCTION_PTR<U, J...>)ptr)(std::forward<J>(t)...);
    }

    template<typename U>
    U call() {
      return ((FUNCTION_PTR<U>)(ptr + entry))();
    }
  };

  template<typename T>
  VirtualPtr<T> get_exectuable_memory(size_t num) {
    VirtualPtr<T> ptr ={};
    ptr.ptr = (T*) VirtualAlloc(nullptr, sizeof(T) * num, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    ptr.size = sizeof(T) * num;

    return ptr;
  }

  template<typename T>
  void free_executable_memory(VirtualPtr<T>& ptr) {
    VirtualFree(ptr.ptr, 0, MEM_RELEASE);
  }

  struct ActiveDll {
    HMODULE lib = 0;

    ~ActiveDll() {
      if (lib != 0) {
        FreeLibrary(lib);
        lib = 0;
      }
    }
  };

  struct MAX_PATH_STR {
    char str[MAX_PATH + 1];
  };

  MAX_PATH_STR get_current_directory();
}
