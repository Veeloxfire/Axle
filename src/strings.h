#pragma once
#include "utility.h"


struct InternString {
  const char* string;

  constexpr bool operator==(const InternString& i)const {
    return i.string == string;
  }

  constexpr bool operator!=(const InternString& i)const {
    return i.string != string;
  }
};

struct StringInterner {
  //CONSIDER: Hash table
  Array<const char*> strings;

  InternString intern(const char* string);
  InternString intern(const char* string, size_t len);
};