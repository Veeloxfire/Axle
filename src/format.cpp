#include "format.h"
#include "parser.h"

void load_string(Array<char>& res, char c) {
  res.insert(c);
}

void load_string(Array<char>& res, DisplayChar c) {
  switch (c.c) {
  case '\n': {
    res.insert('\\');
    res.insert('n');
    break;
  }
  case '\r': {
    res.insert('\\');
    res.insert('r');
    break;
  }
  case '\f': {
    res.insert('\\');
    res.insert('f');
    break;
  }
  case '\t': {
    res.insert('\\');
    res.insert('t');
    break;
  }
  case '\0': {
    res.insert('\\');
    res.insert('0');
    break;
  }
  default:
    res.insert(c.c);
    break;
  }

}

void load_string(Array<char>& res, const char* str) {
  const size_t size = strlen_ts(str);

  res.reserve_extra(size);

  memcpy_ts(res.data + res.size,
            res.capacity - res.size,
            str, size);

  res.size += size;
}

void load_string(Array<char>& res, const InternString* str) {
  load_string(res, str->string);
}


void load_string(Array<char>& res, const TokenTypeString& str) {
  res.reserve_extra(str.length - 1);

  memcpy_ts(res.data + res.size,
            res.capacity - res.size,
            str.string, str.length - 1);

  res.size += str.length - 1;
}

void load_string(Array<char>& res, const TokenType tt) {
  const TokenTypeString str = token_type_string(tt);
  load_string(res, str);
}

static void load_unsigned(Array<char>& res, uint64_t u64) {
  bool is_0 = true;

  for (auto i = 20; i > 0; i--) {
    char res_digit = '0';

    const auto digit = pow_10((uint64_t)i - 1ull);
    if (u64 < digit) {
      if (!is_0) {
        res.insert('0');
      }

      continue;
    }

    while (u64 >= digit) {
      u64 -= digit;
      res_digit += 1;
    }

    is_0 = false;

    res.insert(res_digit);
  }

  if (is_0) {
    res.insert('0');
  }
}

void load_string(Array<char>& res, uint64_t u64) {
  load_unsigned(res, u64);
}

void load_string(Array<char>& res, int64_t i64) {
  if (i64 < 0) {
    res.insert('-');
  }

  load_unsigned(res, absolute(i64));
}

void load_string(Array<char>& res, int8_t i8) {
  if (i8 < 0) {
    res.insert('-');
  }

  load_unsigned(res, (uint64_t)absolute(i8));
}

void load_string(Array<char>& res, uint8_t u8) {
  load_unsigned(res, (uint64_t) u8);
}

void load_string(Array<char>& res, int16_t i16) {
  if (i16 < 0) {
    res.insert('-');
  }

  load_unsigned(res, (uint64_t)absolute(i16));
}

void load_string(Array<char>& res, uint16_t u16) {
  load_unsigned(res, (uint64_t) u16);
}

void load_string(Array<char>& res, int32_t i32) {
  if (i32 < 0) {
    res.insert('-');
  }

  load_unsigned(res, (uint64_t)absolute(i32));
}

void load_string(Array<char>& res, uint32_t u32) {
  load_unsigned(res, (uint64_t) u32);
}