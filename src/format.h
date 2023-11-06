#pragma once
#include "formattable.h"
#include "utility.h"

namespace Format {
  struct ArrayFormatter {
    struct HeapArr {
      bool heap;
      Array<char> arr;
    };

    struct LocalArr {
      bool heap = false;
      u8 size = 0;
      char arr[sizeof(HeapArr) - 2] = {};

      constexpr ~LocalArr() = default;
    };

    constexpr static usize LOCAL_ARR_SIZE = ArraySize<decltype(LocalArr::arr)>::VAL;

    static_assert(sizeof(HeapArr) == sizeof(LocalArr));
    static_assert(sizeof(LocalArr) < 256);

    union {
      LocalArr local_arr = {};
      HeapArr heap_arr;
    };

    ArrayFormatter() : local_arr() {};
    ~ArrayFormatter() {
      if (local_arr.heap) {
        heap_arr.~HeapArr();
      }
      else {
        local_arr.~LocalArr();
      }
    }

    OwnedArr<char> take() {
      if (local_arr.heap) {
        return bake_arr(std::move(heap_arr.arr));
      }
      else {
        return copy_arr(local_arr.arr, static_cast<usize>(local_arr.size));
      }
    }

    ViewArr<char> view() {
      if (local_arr.heap) {
        return view_arr(heap_arr.arr);
      }
      else {
        return { local_arr.arr, static_cast<usize>(local_arr.size) };
      }
    }

    inline void load_string(const char* str, usize N) {
      ASSERT(N > 0);

      if (local_arr.heap) {
        heap_arr.arr.concat(str, N);
      }
      else {
        if (LOCAL_ARR_SIZE - static_cast<usize>(local_arr.size) < N) {
          Array<char> arr = {};
          arr.reserve_total(local_arr.size + N);

          arr.concat(local_arr.arr, local_arr.size);
          arr.concat(str, N);

          local_arr.~LocalArr();

          new (&heap_arr) HeapArr{
            true,
            std::move(arr),
          };
        }
        else {
          memcpy_ts(local_arr.arr + local_arr.size, LOCAL_ARR_SIZE - local_arr.size,
                    str, N);

          local_arr.size += static_cast<u8>(N);
        }
      }
    }


    template<usize N>
    void load_string_lit(const char(&str)[N]) {
      ASSERT(str[N - 1] == '\0');
      load_string(str, N - 1);
    }

    template<usize N>
    void load_string_exact(const char(&str)[N]) {
      load_string(str, N);
    }


    inline void load_char(char c) {
      ASSERT(c != '\0');
      if (local_arr.heap) {
        heap_arr.arr.insert(c);
      }
      else {
        if (LOCAL_ARR_SIZE - static_cast<usize>(local_arr.size) == 0) {
          Array<char> arr = {};
          arr.reserve_total(local_arr.size + 1);

          arr.concat(local_arr.arr, local_arr.size);
          arr.insert(c);

          heap_arr = {
            true,
            std::move(arr),
          };
        }
        else {
          local_arr.arr[local_arr.size] = c;
          local_arr.size += 1;
        }
      }
    }
  };
}

//Does null terminate
template<typename ... T>
OwnedArr<char> format(const Format::FormatString& format, const T& ... ts) {
  Format::ArrayFormatter result = {};
  //result.arr.reserve_total(format.len);

  Format::format_to_formatter(result, format, ts...);

  return result.take();
}

OwnedArr<char> format_type_set(const ViewArr<const char>& format, size_t prepend_spaces, size_t max_width);