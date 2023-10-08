#pragma once
#include "utility.h"
#include "trace.h"

//For printing character as it appears in code
struct DisplayChar {
  char c;
};

struct PrintHexByte {
  u8 c;
};

struct PrintPtr {
  const void* ptr;
};

struct MagicNumber {
  uint16_t num;
};

struct ByteArray {
  const u8* ptr;
  usize size;
};

template<typename T>
struct PrintList {
  const T* arr;
  usize size;
};

namespace Format {
  template<typename F>
  concept Formatter = requires(F & f, char c, const char* ptr, usize n,
                               const char(&arr1)[1], const char(&arr2)[10], const char(&arr3)[100]) {
                                 {f.load_char(c)};
                                 {f.load_string(ptr, n)};
                                 {f.load_string_lit(arr1)};
                                 {f.load_string_exact(arr1)};
                                 {f.load_string_lit(arr2)};
                                 {f.load_string_exact(arr2)};
                                 {f.load_string_lit(arr3)};
                                 {f.load_string_exact(arr3)};
  };

  template<typename T>
  struct FormatArg {
    static_assert(TemplateFalse<T>::VAL, "Attempted to use unspecialized format arg");
  };

  template<>
  struct FormatArg<bool> {
    template<Formatter F>
    constexpr static void load_string(F& res, bool b) {
      if (b) {
        res.load_string_lit("true");
      }
      else {
        res.load_string_lit("false");
      }
    }
  };

  template<>
  struct FormatArg<char> {
    template<Formatter F>
    constexpr static void load_string(F& res, char c) {
      res.load_char(c);
    }
  };

  template<>
  struct FormatArg<DisplayChar> {
    template<Formatter F>
    constexpr static void load_string(F& res, DisplayChar c) {
      switch (c.c) {
        case '\n': {
            res.load_string_lit("\\n");
            break;
          }
        case '\r': {
            res.load_string_lit("\\r");
            break;
          }
        case '\f': {
            res.load_string_lit("\\f");
            break;
          }
        case '\t': {
            res.load_string_lit("\\t");
            break;
          }
        case '\0': {
            res.load_string_lit("\\0");
            break;
          }
        default:
          res.load_char(c.c);
          break;
      }
    }
  };

  constexpr char hex_char(int c) {
    if (0 <= c && c <= 9) {
      return static_cast<char>(c + '0');
    }
    else if (0xa <= c && c <= 0xf) {
      return static_cast<char>((c - 0xa) + 'a');
    }
    return '\0';
  };

  template<>
  struct FormatArg<MagicNumber> {
    template<Formatter F>
    constexpr static void load_string(F& res, MagicNumber c) {
      char chars[4] = {};

      chars[0] = hex_char(((int)c.num & 0x00f0) >> 4);
      chars[1] = hex_char(((int)c.num & 0x000f));
      chars[2] = hex_char(((int)c.num & 0xf000) >> 12);
      chars[3] = hex_char(((int)c.num & 0x0f00) >> 8);

      res.load_string_exact(chars);
    }
  };

  template<>
  struct FormatArg<const char*> {
    template<Formatter F>
    constexpr static void load_string(F& res, const char* str) {
      res.load_string(str, strlen_ts(str));
    }
  };

  template<>
  struct FormatArg<OwnedArr<const char>> {
    template<Formatter F>
    constexpr static void load_string(F& res, const OwnedArr<const char>& str) {
      res.load_string(str.data, str.size);
    }
  };

  template<>
  struct FormatArg<OwnedArr<char>> {
    template<Formatter F>
    constexpr static void load_string(F& res, const OwnedArr<char>& str) {
      res.load_string(str.data, str.size);
    }
  };

  template<>
  struct FormatArg<ViewArr<const char>> {
    template<Formatter F>
    constexpr static void load_string(F& res, const ViewArr<const char>& str) {
      res.load_string(str.data, str.size);
    }
  };

  template<>
  struct FormatArg<ViewArr<char>> {
    template<Formatter F>
    constexpr static void load_string(F& res, const ViewArr<char>& str) {
      res.load_string(str.data, str.size);
    }
  };

  template<>
  struct FormatArg<const Array<char>> {
    template<Formatter F>
    constexpr static void load_string(F& res, const Array<char>& str) {
      res.load_string(str.data, str.size);
    }
  };

  template<>
  struct FormatArg<const Array<const char>> {
    template<Formatter F>
    constexpr static void load_string(F& res, const Array<const char>& str) {
      res.load_string(str.data, str.size);
    }
  };

  template<>
  struct FormatArg<ERROR_CODE> {
    template<Formatter F>
    constexpr static void load_string(F& res, ERROR_CODE er) {
      ViewArr<const char> err_str = error_code_string(er);
      res.load_string(err_str.data, err_str.size);
    }
  };

  template<Formatter F>
  constexpr static void load_unsigned(F& res, uint64_t u) {
    if (u == 0) {
      return res.load_char('0');
    }

    constexpr usize MAX = 20;
    usize i = MAX;
    char arr[MAX + 1] = {};

    while (u > 0) {
      ASSERT(i > 0);
      auto d = u % 10;
      arr[i - 1] = static_cast<char>('0' + d);
      i -= 1;
      u /= 10;
    }

    return res.load_string(arr + i, MAX - i);
  }

  template<Formatter F>
  constexpr static void load_unsigned_hex(F& res, uint64_t u) {
    constexpr size_t LEN = 16;

    char string_res[2 + LEN] = {
      '0', 'x',
      '0', '0', '0', '0', '0', '0', '0', '0',
      '0', '0', '0', '0', '0', '0', '0', '0'
    };

    for (u32 i = 0; i < LEN; i++) {
      u8 digit = u & 0xF;
      u >>= 4;

      if (digit >= 10) {
        ASSERT(digit < 16);
        string_res[((LEN - 1) - i) + 2] = ('A' + (digit - 10));
      }
      else {
        string_res[((LEN - 1) - i) + 2] = ('0' + digit);
      }
    }

    res.load_string_exact(string_res);
  }

  template<>
  struct FormatArg<PrintPtr> {
    template<Formatter F>
    constexpr static void load_string(F& res, PrintPtr ptr) {
      if (ptr.ptr == nullptr) {
        return res.load_string_lit("nullptr");
      }
      else {
        return load_unsigned_hex(res, (uintptr_t)ptr.ptr);
      }
    }
  };

  template<>
  struct FormatArg<unsigned char> {
    template<Formatter F>
    constexpr static void load_string(F& res, unsigned char u) {
      return load_unsigned(res, u);
    }
  };

  template<>
  struct FormatArg<unsigned short> {
    template<Formatter F>
    constexpr static void load_string(F& res, unsigned short u) {
      return load_unsigned(res, u);
    }
  };

  template<>
  struct FormatArg<unsigned int> {
    template<Formatter F>
    constexpr static void load_string(F& res, unsigned int u) {
      return load_unsigned(res, u);
    }
  };

  template<>
  struct FormatArg<unsigned long> {
    template<Formatter F>
    constexpr static void load_string(F& res, unsigned long u) {
      return load_unsigned(res, u);
    }
  };

  template<>
  struct FormatArg<unsigned long long> {
    template<Formatter F>
    constexpr static void load_string(F& res, unsigned long long u) {
      return load_unsigned(res, u);
    }
  };

  template<>
  struct FormatArg<signed char> {
    template<Formatter F>
    constexpr static void load_string(F& res, signed char i) {
      if (i < 0) {
        res.load_char('-');
      }

      return load_unsigned(res, absolute(i));
    }
  };

  template<>
  struct FormatArg<signed short> {
    template<Formatter F>
    constexpr static void load_string(F& res, signed short i) {
      if (i < 0) {
        res.load_char('-');
      }

      return load_unsigned(res, absolute(i));
    }
  };


  template<>
  struct FormatArg<signed int> {
    template<Formatter F>
    constexpr static void load_string(F& res, signed int i) {
      if (i < 0) {
        res.load_char('-');
      }

      return load_unsigned(res, absolute(i));
    }
  };

  template<>
  struct FormatArg<signed long> {
    template<Formatter F>
    constexpr static void load_string(F& res, signed long i) {
      if (i < 0) {
        res.load_char('-');
      }

      return load_unsigned(res, absolute((int64_t)i));
    }
  };

  template<>
  struct FormatArg<signed long long> {
    template<Formatter F>
    constexpr static void load_string(F& res, signed long long i) {
      if (i < 0) {
        res.load_char('-');
      }

      return load_unsigned(res, absolute(i));
    }
  };

  template<>
  struct FormatArg<PrintHexByte> {
    template<Formatter F>
    constexpr static void load_string(F& res, PrintHexByte hb) {
      char str[2] = { '0', '0' };
      {
        u8 digit = hb.c & 0xF;
        hb.c >>= 4;

        if (digit >= 10) {
          ASSERT(digit < 16);
          str[1] = ('A' + (digit - 10));
        }
        else {
          str[1] = ('0' + digit);
        }
      }

      {
        u8 digit = hb.c & 0xF;
        hb.c >>= 4;

        if (digit >= 10) {
          ASSERT(digit < 16);
          str[0] = ('A' + (digit - 10));
        }
        else {
          str[0] = ('0' + digit);
        }
      }

      return res.load_string_exact(str);
    }
  };


  template<>
  struct FormatArg<ByteArray> {
    template<Formatter F>
    constexpr static void load_string(F& res, const ByteArray& arr) {
      char as_string[] = ", 0x00";

      const u8* i = arr.ptr;
      const u8* end = i + arr.size;

      if (i < end) {
        as_string[4] = hex_char(((*i) >> 4) & 0xf);
        as_string[5] = hex_char((*i) & 0x0f);

        res.load_string(as_string + 2, 4);
        i += 1;

        while (i < end) {
          as_string[4] = hex_char(((*i) >> 4) & 0xf);
          as_string[5] = hex_char((*i) & 0x0f);
          res.load_string(as_string, 6);
          i += 1;
        }
      }
    }
  };

  template<typename T>
  struct FormatArg<PrintList<T>> {
    template<Formatter F>
    constexpr static void load_string(F& res, const PrintList<T>& arr) {
      usize i = 0;
      if (i < arr.size) {
        FormatArg<T>::load_string(res, arr.arr[i]);
        ++i;
        for (; i < arr.size; ++i) {
          res.load_string_lit(", ");
          FormatArg<T>::load_string(res, arr.arr[i]);
        }
      }
    }
  };

  template<usize N>
  struct FormatArg<const char[N]> {
    template<Formatter F>
    constexpr static void load_string(F& res, const char(&arr)[N]) {
      res.load_string_lit(arr);
    }
  };

  template<Formatter F>
  struct FormatDispatch {
    const char* format_string;

    F& result;
  };

  template<Formatter F, typename T>
  constexpr FormatDispatch<F>& operator<<(FormatDispatch<F>& f, const T& t) {
    const char* const string = f.format_string;

    while (true) {
      if (f.format_string[0] == '\0') {
        INVALID_CODE_PATH("Invalid format");
      }
      else if (f.format_string[0] == '{' && f.format_string[1] == '}') {
        const size_t num_chars = f.format_string - string;
        if (num_chars > 0) {
          f.result.load_string(string, num_chars);
        }

        FormatArg<T>::load_string(f.result, t);

        f.format_string += 2;
        return f;
      }

      f.format_string++;
    }
  }

  //Doesnt null terminate!
  template<Formatter F, typename ... T>
  constexpr void format_to_formatter(F& result, const char* format, const T& ... ts) {
    TRACING_FUNCTION();

    if constexpr (sizeof...(T) > 0) {
      FormatDispatch<F> f = { format, result };

      (f << ... << ts);
      format = f.format_string;
    }

    const char* const string = format;

    while (true) {
      if (format[0] == '{' && format[1] == '}') {
        INVALID_CODE_PATH("Invalid format");
        break;
      }
      else if (format[0] == '\0') {
        const size_t num_chars = format - string;
        if (num_chars > 0) {
          result.load_string(string, num_chars);
        }
        return;
      }

      format++;
    }
  }

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
      ASSERT(str[N - 1] != '\0');

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

  struct STPrintFormatter {
    template<usize N>
    void load_string_lit(const char(&str)[N]) {
      IO_Single::print_impl(str);
    }

    template<usize N>
    void load_string_exact(const char(&str)[N]) {
      IO_Single::print_impl(str);
    }

    inline void load_string(const char* str, usize N) {
      ASSERT(N > 0);
      IO_Single::print_impl(str, N);
    }

    inline void load_char(char c) {
      IO_Single::print_impl(c);
    }
  };

  struct STErrPrintFormatter {
    template<usize N>
    void load_string_lit(const char(&str)[N]) {
      IO_Single::err_print_impl(str);
    }

    template<usize N>
    void load_string_exact(const char(&str)[N]) {
      IO_Single::err_print_impl(str);
    }

    inline void load_string(const char* str, usize N) {
      ASSERT(N > 0);
      IO_Single::err_print_impl(str, N);
    }

    inline void load_char(char c) {
      IO_Single::err_print_impl(c);
    }
  };
}

struct FormatString {
  const char* arr;
  usize len;

  template<usize N>
  constexpr FormatString(const char(&_arr)[N]) : arr(_arr), len(N) {}

  constexpr FormatString(const char* _arr, usize _len) : arr(_arr), len(_len) {}
};

//Does null terminate
template<typename ... T>
OwnedArr<char> format(const FormatString& format, const T& ... ts) {
  Format::ArrayFormatter result = {};
  //result.arr.reserve_total(format.len);

  Format::format_to_formatter(result, format.arr, ts...);

  return result.take();
}

template<typename ... T>
void format_print(const FormatString& format, const T& ... ts) {
  IO_Single::lock();
  DEFER() { IO_Single::unlock(); };
  Format::STPrintFormatter result = {};

  Format::format_to_formatter(result, format.arr, ts...);
}

template<typename ... T>
void format_err_print(const FormatString& format, const T& ... ts) {
  IO_Single::lock();
  DEFER() { IO_Single::unlock(); };
  Format::STErrPrintFormatter result = {};

  Format::format_to_formatter(result, format.arr, ts...);
}

template<typename ... T>
void format_print_ST(const FormatString& format, const T& ... ts) {
  Format::STPrintFormatter result = {};

  Format::format_to_formatter(result, format.arr, ts...);
}

OwnedArr<char> format_type_set(const ViewArr<const char>& format, size_t prepend_spaces, size_t max_width);