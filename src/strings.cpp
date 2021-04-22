#include "strings.h"
#include "safe_lib.h"

InternString StringInterner::intern(const char* string) {
  assert(string != nullptr);

  //Compute outside loop for obvious reasons
  const size_t str_len_new = strlen_ts(string);

  for (const char* str : strings) {
    const size_t str_len_in = strlen_ts(str);

    if (str_len_in == str_len_new
        && memcmp_ts(string, str, str_len_new) == 0) {
      return { str };
    }
  }

  //Copy string
  char* c = allocate_default<char>(str_len_new + 1);
  memcpy_ts(c, str_len_new + 1, string, str_len_new);

  strings.insert(c);
  return { c };
}

InternString StringInterner::intern(const char* string, const size_t str_len_new) {
  assert(string != nullptr);
  
  for (const char* str : strings) {
    const size_t str_len_in = strlen_ts(str);

    if (str_len_in == str_len_new
        && memcmp_ts(string, str, str_len_new) == 0) {
      return { str };
    }
  }

  //Copy string
  char* c = allocate_default<char>(str_len_new + 1);
  memcpy_ts(c, str_len_new + 1, string, str_len_new);

  strings.insert(c);
  return { c };
}

TempUTF8String ascii_to_utf8(const char* string) {
  return {};
}