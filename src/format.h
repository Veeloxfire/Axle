#pragma once
#include "utility.h"
#include "trace.h"

struct InternString;
struct TokenTypeString;
enum struct AxleTokenType : uint8_t;
enum struct ErrorCode : uint8_t;
enum struct VALUE_CATEGORY : uint8_t;

namespace IR { struct Function; }
struct CallSignature;
struct FileLocation;
struct SignatureStructure;
struct ASTFunctionCallExpr;

//For printing character as it appears in code
struct DisplayChar {
  char c;
};

struct PrintHexByte {
  u8 c;
};

struct PrintFuncSignature {
  const IR::Function* func;
};

struct PrintSignatureType {
  const SignatureStructure* sig;
};

struct PrintCallSignature {
  const ASTFunctionCallExpr* call;
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

void load_string(Array<char>& res, char c);
void load_string(Array<char>& res, int8_t i8);
void load_string(Array<char>& res, uint8_t u8);
void load_string(Array<char>& res, int16_t i16);
void load_string(Array<char>& res, uint16_t u16);
void load_string(Array<char>& res, int32_t i32);
void load_string(Array<char>& res, uint32_t u32);
void load_string(Array<char>& res, int64_t i64);
void load_string(Array<char>& res, uint64_t u64);
void load_string(Array<char>& res, long l);
void load_string(Array<char>& res, PrintPtr ptr);
void load_string(Array<char>& res, const ByteArray& byte_array);

void load_string(Array<char>& res, DisplayChar c);
void load_string(Array<char>& res, PrintHexByte c);
void load_string(Array<char>& res, MagicNumber mn);
void load_string(Array<char>& res, const char* str);
void load_string(Array<char>& res, const OwnedArr<char>& str);
void load_string(Array<char>& res, const OwnedArr<const char>& str);
void load_string(Array<char>& res, const ViewArr<char>& str);
void load_string(Array<char>& res, const ViewArr<const char>& str);
void load_string(Array<char>& res, const Array<char>& str);
void load_string(Array<char>& res, const Array<char>& str);
void load_string(Array<char>& res, const InternString* str);
void load_string(Array<char>& res, const TokenTypeString& str);
void load_string(Array<char>& res, AxleTokenType tt);
void load_string(Array<char>& res, ErrorCode ec);
void load_string(Array<char>& res, VALUE_CATEGORY vc);

void load_string(Array<char>& res, PrintFuncSignature func);
void load_string(Array<char>& res, PrintSignatureType sig);
void load_string(Array<char>& res, PrintCallSignature call);
void load_string(Array<char>& res, const CallSignature& call_sig);

template<usize N>
void load_string(Array<char>& res, const char(&str)[N]) {
  res.concat(str, N);
}

template<typename T>
void load_string(Array<char>& res, const PrintList<T>& arr) {
  usize i = 0;
  if (i < arr.size) {
    load_string(res, arr.arr[i]);
    ++i;
    for (; i < arr.size; ++i) {
      load_string(res, ", ");
      load_string(res, arr.arr[i]);
    }
  }
}

struct Formatter {
  const char* format_string;

  Array<char>& result;
};

template<typename T>
Formatter& operator<<(Formatter& f, const T& t) {
  const char* const string = f.format_string;

  while (true) {
    if (f.format_string[0] == '\0') {
      INVALID_CODE_PATH("Invalid format");
    }
    else if (f.format_string[0] == '{' && f.format_string[1] == '}') {
      const size_t num_chars = f.format_string - string;
      f.result.reserve_extra(num_chars);

      memcpy_ts(f.result.data + f.result.size,
                f.result.capacity - f.result.size,
                string, num_chars);

      f.result.size += num_chars;

      load_string(f.result, t);

      f.format_string += 2;
      return f;
    }

    f.format_string++;
  }
}

//Doesnt null terminate!
template<typename ... T>
void format_to_array(Array<char>& result, const char* format, const T& ... ts) {
  TRACING_FUNCTION();

  //Account for no formatting
  format = ([](Array<char>& result, const char* format, const auto& ... ts) {
    if constexpr (sizeof...(T) == 0) {
      return format;
    }
    else {
      Formatter f ={ format, result };

      (f << ... << ts);
      return f.format_string;
    }
  })(result, format, ts...);

  const char* const string = format;

  while (true) {
    if (format[0] == '{' && format[1] == '}') {
      INVALID_CODE_PATH("Invalid format");
      break;
    }
    else if (format[0] == '\0') {
      const size_t num_chars = format - string;
      result.reserve_extra(num_chars);

      memcpy_ts(result.data + result.size,
                result.capacity - result.size,
                string, num_chars);

      result.size += num_chars;
      return;
    }

    format++;
  }
}

struct FormatString {
  const char* arr;
  usize len;

  template<usize N>
  constexpr FormatString(const char(&_arr)[N]) : arr(_arr), len(N) {}

  constexpr FormatString(const char*_arr, usize _len) : arr(_arr), len(_len) {}
};

//Does null terminate
template<typename ... T>
OwnedArr<char> format(const FormatString& format, const T& ... ts) {
  Array<char> result ={};
  result.reserve_total(format.len);

  format_to_array(result, format.arr, ts...);
  result.insert('\0');

  return bake_arr(std::move(result));
}

template<typename ... T>
void format_print(const FormatString& format, const T& ... ts) {
  Array<char> result ={};
  result.reserve_total(format.len);

  format_to_array(result, format.arr, ts...);
  result.insert('\0');

  IO::print(result.data);
}

template<typename ... T>
void format_print_ST(const FormatString& format, const T& ... ts) {
  Array<char> result = {};
  result.reserve_total(format.len);

  format_to_array(result, format.arr, ts...);
  result.insert('\0');

  IO_Single::print(result.data);
}

OwnedArr<char> format_type_set(const char* format, size_t prepend_spaces, size_t max_width);