#include "format.h"
#include "parser.h"
#include "type.h"
#include "compiler.h"
#include "ast.h"

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

constexpr char hex_char(int c) {
  if (0 <= c && c <= 9) {
    return c + '0';
  }
  else if (0xa <= c && c <= 0xf) {
    return (c - 0xa) + 'a';
  }
  return '\0';
};

void load_string(Array<char>& res, MagicNumber c) {
  char chars[5] = {};

  chars[0] = hex_char(((int)c.num & 0x00f0) >> 4);
  chars[1] = hex_char(((int)c.num & 0x000f));
  chars[2] = hex_char(((int)c.num & 0xf000) >> 12);
  chars[3] = hex_char(((int)c.num & 0x0f00) >> 8);

  load_string(res, chars);
}

static void load_string_raw(Array<char>& res, const char* str, usize size) {
  res.concat(str, size);
}

void load_string(Array<char>& res, const char* str) {
  load_string_raw(res, str, strlen_ts(str));
}

void load_string(Array<char>& res, const OwnedArr<const char>& str) {
  load_string_raw(res, str.data, str.size);
}

void load_string(Array<char>& res, const OwnedArr<char>& str) {
  load_string_raw(res, str.data, str.size);
}

void load_string(Array<char>& res, const ViewArr<const char>& str) {
  load_string_raw(res, str.data, str.size);
}

void load_string(Array<char>& res, const ViewArr<char>& str) {
  load_string_raw(res, str.data, str.size);
}


void load_string(Array<char>& res, const Array<char>& str) {
  load_string_raw(res, str.data, str.size);
}

void load_string(Array<char>& res, const Array<const char>& str) {
  load_string_raw(res, str.data, str.size);
}

void load_string(Array<char>& res, const InternString* str) {
  const size_t size = str->len;

  res.reserve_extra(size);

  memcpy_ts(res.data + res.size,
            res.capacity - res.size,
            str->string, size);

  res.size += size;
}


void load_string(Array<char>& res, const TokenTypeString& str) {
  res.reserve_extra(str.length - 1);

  memcpy_ts(res.data + res.size,
            res.capacity - res.size,
            str.string, str.length - 1);

  res.size += str.length - 1;
}

void load_string(Array<char>& res, const AxleTokenType tt) {
  const TokenTypeString str = token_type_string(tt);
  load_string(res, str);
}

void load_string(Array<char>& res, ErrorCode er) {
  ViewArr<const char> err_str = error_code_string(er);
  load_string(res, err_str);
}

void load_string(Array<char>& res, VALUE_CATEGORY vc) {
  ViewArr<const char> str = VC::category_name(vc);
  load_string(res, str);
}

static void load_unsigned(Array<char>& res, uint64_t u) {
  if (u == 0) {
    return load_string(res, '0');
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

  return load_string_raw(res, arr + i, MAX - i);
}

static void load_unsigned_hex(Array<char>& res, uint64_t u) {
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

  res.concat(string_res, LEN + 2);
}

void load_string(Array<char>& res, PrintPtr ptr) {
  if (ptr.ptr == nullptr) {
    load_string(res, "nullptr");
  }
  else {
    load_unsigned_hex(res, (uintptr_t)ptr.ptr);
  }
}

void load_string(Array<char>& res, uint64_t u64) {
  return load_unsigned(res, u64);
}

void load_string(Array<char>& res, long l) {
  return load_unsigned(res, l);
}

void load_string(Array<char>& res, int64_t i64) {
  if (i64 < 0) {
    res.insert('-');
  }

  return load_unsigned(res, absolute(i64));
}

void load_string(Array<char>& res, int8_t i8) {
  if (i8 < 0) {
    res.insert('-');
  }

  return load_unsigned(res, (uint64_t)absolute(i8));
}

void load_string(Array<char>& res, PrintHexByte hb) {
  char str[3] = "00";
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

  return load_string(res, str);
}

void load_string(Array<char>& res, uint8_t u8) {
  return load_unsigned(res, (uint64_t)u8);
}

void load_string(Array<char>& res, int16_t i16) {
  if (i16 < 0) {
    res.insert('-');
  }

  return load_unsigned(res, (uint64_t)absolute(i16));
}

void load_string(Array<char>& res, uint16_t u16) {
  return load_unsigned(res, (uint64_t)u16);
}

void load_string(Array<char>& res, int32_t i32) {
  if (i32 < 0) {
    res.insert('-');
  }

  return load_unsigned(res, (uint64_t)absolute(i32));
}

void load_string(Array<char>& res, uint32_t u32) {
  return load_unsigned(res, (uint64_t)u32);
}

void load_string(Array<char>& res, const ByteArray& arr) {
  char as_string[] = ", 0x00";

  const u8* i = arr.ptr;
  const u8* end = i + arr.size;

  if (i < end) {
    as_string[4] = hex_char(((*i) >> 4) & 0xf);
    as_string[5] = hex_char((*i) & 0x0f);

    load_string(res, as_string + 2);
    i += 1;

    while (i < end) {
      as_string[4] = hex_char(((*i) >> 4) & 0xf);
      as_string[5] = hex_char((*i) & 0x0f);
      load_string(res, as_string);
      i += 1;
    }
  }
}

void load_string(Array<char>& res, PrintCallSignature p_call) {
  const ASTFunctionCallExpr* call = p_call.call;

  res.insert('(');

  auto l = call->arguments.start;

  if (l) {
    load_string(res, l->curr->node_type.name);
    l = l->next;

    while (l) {
      load_string(res, ", ");
      load_string(res, l->curr->node_type.name);
      l = l->next;
    }
  }

  load_string(res, ')');
}

void load_string(Array<char>& res, PrintFuncSignature p_func) {
  return load_string(res, PrintSignatureType{ p_func.func->signature.sig_struct });
}

void load_string(Array<char>& res, PrintSignatureType p_sig) {
  const SignatureStructure* sig = p_sig.sig;

  res.insert('(');

  auto i = sig->parameter_types.begin();
  const auto end = sig->parameter_types.end();

  if (i < end) {
    for (; i < (end - 1); i++) {
      load_string(res, i->name);
      load_string(res, ", ");
    }

    load_string(res, i->name);
  }

  load_string(res, ") -> ");
  load_string(res, sig->return_type.name);
}

void load_string(Array<char>& res, const CallSignature& call_sig) {
  auto i = call_sig.arguments.begin();
  const auto end = call_sig.arguments.end();

  res.insert('(');

  if (i < end) {
    for (; i < (end - 1); i++) {
      load_string(res, i->name->string);
      load_string(res, ", ");
    }

    load_string(res, i->name->string);
  }

  res.insert(')');
}

OwnedArr<char> format_type_set(const char* format, const size_t prepend_spaces, const size_t max_width) {
  Array<char> result = {};

  const char* string = format;
  const char* last_space = format;
  size_t curr_length = 0;

  const auto load_to_string = [&](const char* start, const char* end) {
    const size_t num_chars = end - start;
    result.reserve_extra(num_chars);

    memcpy_ts(result.data + result.size,
              result.capacity - result.size,
              start, num_chars);

    result.size += num_chars;
  };

  const auto prepend = [&] {
    result.reserve_extra(prepend_spaces);
    for (size_t i = 0; i < prepend_spaces; i++) {
      result.insert(' ');
    }
  };

  prepend();

  while (true) {
    if (curr_length == max_width && format[0] != '\n') {
      //Need to insert a new line
      load_to_string(string, last_space);
      result.insert('\n');
      prepend();
      string = last_space + 1;
    }
    else if (format[0] == '\n') {
      load_to_string(string, format + 1);
      prepend();
      string = format + 1;
      last_space = string;

      format++;
      curr_length = prepend_spaces;
      continue;
    }
    else if (format[0] == ' ') {
      last_space = format;
    }
    else if (format[0] == '\0') {
      load_to_string(string, format + 1);

      result.shrink();
      return bake_arr(std::move(result));
    }

    format++;
    curr_length++;
  }
}