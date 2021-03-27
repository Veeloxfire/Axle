#include "strings.h"
#include "safe_lib.h"

#include "bytecode_incs.h"

InternString StringInterner::intern(const char* string) {
  //Compute outside loop for obvious reasons
  const size_t str_len_new = strlen(string);

  for (const char* str : strings) {
    const size_t str_len_in = strlen(str);

    if (str_len_in == str_len_new
        && memcmp(string, str, str_len_new) == 0) {
      return { str };
    }
  }

  //Copy string
  char* c = allocate_zerod<char>(str_len_new + 1);
  memcpy_ts(c, str_len_new + 1, string, str_len_new);

  strings.insert(c);
  return { c };
}

InternString StringInterner::intern(const char* string, const size_t str_len_new) {
  for (const char* str : strings) {
    const size_t str_len_in = strlen(str);

    if (str_len_in == str_len_new
        && memcmp_ts(string, str, str_len_new) == 0) {
      return { str };
    }
  }

  //Copy string
  char* c = allocate_zerod<char>(str_len_new + 1);
  memcpy_ts(c, str_len_new + 1, string, str_len_new);

  strings.insert(c);
  return { c };
}