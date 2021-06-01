#pragma once
#include "utility.h"

struct InternString {
  uint64_t hash;
  size_t len;
  char string[1];//placeholder for any length array

  constexpr bool operator==(const InternString& i)const {
    return i.string == string;
  }

  constexpr bool operator!=(const InternString& i)const {
    return i.string != string;
  }

  constexpr static size_t alloc_size(size_t string_len) {
    return sizeof(InternString) + string_len + 1 /* for null byte */;
  }
};

extern const InternString* TOMBSTONE;

struct Table {
  constexpr static float LOAD_FACTOR = 0.75;
  
  const InternString** data = nullptr;
  size_t num_full = 0;
  size_t size = 0;

  Table();

  void try_resize();
  const InternString** find(const char* str, size_t len, uint64_t hash) const;
  const InternString** find_empty(uint64_t hash) const;
};

struct StringInterner {
  BumpAllocator allocs ={};
  Table table ={};

  const InternString* intern(const char* string);
  const InternString* intern(const char* string, size_t len);
};

struct TempUTF8String {
  uint8_t* bytes = nullptr;
  size_t size = 0;

  TempUTF8String() = default;

  TempUTF8String(TempUTF8String&& t_utf8) noexcept 
    : bytes(std::exchange(t_utf8.bytes, nullptr)) , size(std::exchange(t_utf8.size, 0)) {

  }

  ~TempUTF8String() {
    free_no_destruct(bytes);
  }
};

