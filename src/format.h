#pragma once
#include "utility.h"
struct InternString;
struct TokenTypeString;
enum struct AxleTokenType : uint8_t;
enum struct ErrorCode : uint8_t;

namespace IR { struct Function; }
struct CallSignature;
struct FileLocation;
struct SignatureStructure;
struct ASTFunctionCallExpr;

//For printing character as it appears in code
struct DisplayChar {
  char c;
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
void load_string(Array<char>& res, MagicNumber mn);
void load_string(Array<char>& res, const char* str);
void load_string(Array<char>& res, const Array<char>& str);
void load_string(Array<char>& res, const InternString* str);
void load_string(Array<char>& res, const TokenTypeString& str);
void load_string(Array<char>& res, AxleTokenType tt);
void load_string(Array<char>& res, ErrorCode ec);

void load_string(Array<char>& res, PrintFuncSignature func);
void load_string(Array<char>& res, PrintSignatureType sig);
void load_string(Array<char>& res, PrintCallSignature call);
void load_string(Array<char>& res, const CallSignature& call_sig);


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

//Does null terminate
template<typename ... T>
OwnedPtr<char> format(const char* format, const T& ... ts) {
  Array<char> result ={};

  format_to_array(result, format, ts...);
  result.insert('\0');

  result.shrink();
  return result;
}

template<typename ... T>
void format_print(const char* format, const T& ... ts) {
  Array<char> result ={};

  format_to_array(result, format, ts...);
  result.insert('\0');

  IO::print(result.data);
}

template<typename ... T>
void format_print_ST(const char* format, const T& ... ts) {
  Array<char> result = {};

  format_to_array(result, format, ts...);
  result.insert('\0');

  IO_Single::print(result.data);
}

OwnedPtr<char> format_type_set(const char* format, size_t prepend_spaces, size_t max_width);