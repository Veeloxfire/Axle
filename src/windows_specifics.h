#pragma once
#include <windows.h>
#include <memoryapi.h>

#include "safe_lib.h"

namespace Windows {
  template<typename T> 
  struct VirtualPtr {
    T* ptr = nullptr;
    usize size = 0;
    usize entry = 0;

    template<typename U, typename ... J>
    U call(J&& ... t) {
      using PTR = U(*)(J...);
      return ((PTR)ptr)(std::forward<J>(t)...);
    }

    template<typename U>
    U call() {
      using PTR = U(*)();
      return ((U)(ptr + entry))();
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

  struct NativePath {
    char path[MAX_PATH + 1] = {};

    constexpr NativePath() = default;
    constexpr NativePath(const ViewArr<const char>& vr) {
      usize len = vr.size > MAX_PATH ? MAX_PATH : vr.size;
      for (usize i = 0; i < len; ++i) {
        path[i] = vr[i];
      }
    }

    const char* c_str() const {
      return path;
    }

    ViewArr<const char> view() const {
      return { path, strlen_ts(path) };
    }
  };


  NativePath get_current_directory();
  void set_current_directory(const ViewArr<const char>& str);
}
