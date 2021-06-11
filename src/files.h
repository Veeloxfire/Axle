#pragma once
#include <stdio.h>
#include "utility.h"

namespace FILES {
  enum struct OPEN_MODE : uint8_t {
    READ = 'r', WRITE = 'w', APPEND = 'a'
  };

  enum struct DATA_MODE : uint8_t {
    STANDARD = '\0', BINARY = 'b'
  };

  enum struct SEEK_MODE : int {
    BEGIN = SEEK_SET,
    END = SEEK_END,
    CURRENT = SEEK_CUR,
  };

  struct OpenedFile {
    FILE* file;
    errno_t error_code;
  };

  OpenedFile open(const char* name,
                  OPEN_MODE open_mode,
                  DATA_MODE data_mode);
  ErrorCode close(FILE* file);

  ErrorCode read_as_string(FILE* file, size_t num_bytes, char** out_string);
  ErrorCode read_to_bytes(FILE* file, uint8_t* bytes, size_t num_bytes);
  uint8_t read_byte(FILE* file);

  ErrorCode read_to_structure(FILE* file,
                              uint8_t* ptr_to_s,
                              size_t size_of_s, size_t num_of_s);

  template<typename T>
  ErrorCode read(FILE* file, T* ptr, size_t num) {
    return read_to_structure(file, (uint8_t*)ptr, sizeof(T), num);
  }

  size_t size_of_file(FILE* file);
  ErrorCode seek(FILE* file, SEEK_MODE mode, int32_t location);
  long current_pos(FILE* file);

  OwnedPtr<const char> load_file_to_string(const char* file_name);

  ErrorCode write(FILE* file, const uint8_t* arr, size_t length);
  ErrorCode write_padding_bytes(FILE* file, uint8_t byte, size_t num);

  ErrorCode write_aligned_array(FILE* file, const uint8_t* arr, const size_t size, const size_t align);
  ErrorCode write_aligned_array(FILE* file, const Array<uint8_t>& arr, const size_t align);

  template<typename T>
  ErrorCode write(FILE* file, const T& t) {
    return write(file, (uint8_t*)&t, sizeof(T));
  }
}

struct InternString;
struct StringInterner;

struct FileLocation {
  const InternString* directory;
  const InternString* extension;
  const InternString* full_name;

  constexpr bool operator== (const FileLocation& a) const {
    return full_name == a.full_name;
  }
};

FileLocation parse_file_location(const char* path,
                                 const char* file,
                                 StringInterner* strings);