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

void load_string(Array<char>& res, MagicNumber c) {
  char chars[5] ={};

  constexpr auto display_char = [](int c) -> char {
    if (0 <= c && c <= 9) {
      return c + '0';
    }
    else if (0xa <= c && c <= 0xf) {
      return (c - 0xa) + 'a';
    }
    return '\0';
  };

  chars[0] = display_char(((int)c.num & 0x00f0) >> 4);
  chars[1] = display_char(((int)c.num & 0x000f));
  chars[2] = display_char(((int)c.num & 0xf000) >> 12);
  chars[3] = display_char(((int)c.num & 0x0f00) >> 8);

  load_string(res, chars);
}

void load_string(Array<char>& res, const char* str) {
  const size_t size = strlen_ts(str);

  res.reserve_extra(size);

  memcpy_ts(res.data + res.size,
            res.capacity - res.size,
            str, size);

  res.size += size;
}

void load_string(Array<char>& res, const Array<char>& str) {
  res.reserve_extra(str.size);

  memcpy_ts(res.data + res.size,
            res.capacity - res.size,
            str.data, str.size);

  res.size += str.size;
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
  const char* err_str = error_code_string(er);
  load_string(res, err_str);
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

static void load_unsigned_hex(Array<char>& res, uint64_t u) {
  constexpr size_t LEN = 16;
  
  char string_res[2 + LEN] ={
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

void load_string(Array<char>& res, PrintCallSignature p_call) {
  const ASTFunctionCallExpr* call = p_call.call;

  res.insert('(');

  auto l = call->arguments.start;

  if (l) {
    load_string(res, l->curr->node_type.name);
    l = l->next;

    while(l) {
      load_string(res, ", ");
      load_string(res, l->curr->node_type.name);
      l = l->next;
    }
  }

  load_string(res, ')');
}

void load_string(Array<char>& res, PrintFuncSignature p_func) {
  load_string(res, PrintSignatureType{p_func.func->signature.sig_struct});
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
  load_string(res, call_sig.name);

  auto i = call_sig.arguments.begin();
  const auto end = call_sig.arguments.end();

  res.insert('(');

  if (i < end) {
    for (; i < (end - 1); i++) {
      load_string(res, i->type.name->string);
      load_string(res, ", ");
    }

    load_string(res, i->type.name->string);
  }

  res.insert(')');
}

OwnedPtr<char> format_type_set(const char* format, const size_t prepend_spaces, const size_t max_width) {
  Array<char> result ={};

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
      return result;
    }

    format++;
    curr_length++;
  }
}