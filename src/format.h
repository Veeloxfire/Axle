#pragma once
#include "utility.h"
struct InternString;
struct TokenTypeString;
enum class TokenType : uint8_t;

//For printing character as it appears in code
struct DisplayChar {
  char c;
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

void load_string(Array<char>& res, DisplayChar c);
void load_string(Array<char>& res, const char* str);
void load_string(Array<char>& res, const InternString& str);
void load_string(Array<char>& res, const TokenTypeString& str);
void load_string(Array<char>& res, TokenType tt);


struct Formatter {
  const char* format_string;

  Array<char>& result;
};

template<typename T>
Formatter& operator<<(Formatter& f, const T& t) {
  const char* const string = f.format_string;

  while (true) {
    if (f.format_string[0] == '\0') {
      throw std::exception("Invalid format");
      break;
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

template<typename ... T>
OwnedPtr<char> format(const char* format, const T& ... ts) {
  Array<char> result = {};
  Formatter f = { format, result };

  (f << ... << ts);

  const char* const string = f.format_string;

  while (true) {
    if (f.format_string[0] == '{' && f.format_string[1] == '}') {
      throw std::exception("Invalid format");
      break;
    }
    else if (f.format_string[0] == '\0') {
      const size_t num_chars = (f.format_string + 1) - string;
      result.reserve_extra(num_chars);

      memcpy_ts(result.data + result.size,
                result.capacity - result.size,
                string, num_chars);

      result.size += num_chars;

      result.shrink();
      return result;
    }

    f.format_string++;
  }
}