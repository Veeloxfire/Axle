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

struct TempUTF8String {
  uint8_t* bytes = nullptr;
  size_t size = 0;

  TempUTF8String() = default;

  TempUTF8String(TempUTF8String&& t_utf8) noexcept 
    : bytes(std::exchange(t_utf8.bytes, nullptr)) , size(std::exchange(t_utf8.size, 0)) {

  }

  ~TempUTF8String() {
    free(bytes);
  }
};

//NOT IMPLEMENTED YET - DO NOT USE
TempUTF8String ascii_to_utf8(const char* string);

