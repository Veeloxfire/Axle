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

  struct OpenedFile {
    FILE* file;
    errno_t error_code;
  };

  OpenedFile open(const char* name,
                  OPEN_MODE open_mode,
                  DATA_MODE data_mode);
  ErrorCode close(FILE* file);

  ErrorCode read_as_string(FILE* file, size_t num_bytes, char** out_string);

  size_t size_of_file(FILE* file);

  const char* load_file_to_string(const char* file_name);

  ErrorCode write(FILE* file, const uint8_t* arr, size_t length);
  ErrorCode write_padding_bytes(FILE* file, uint8_t byte, size_t num);

  ErrorCode write_aligned_array(FILE* file, const uint8_t* arr, const size_t size, const size_t align);
  ErrorCode write_aligned_array(FILE* file, const Array<uint8_t>& arr, const size_t align);

  template<typename T>
  ErrorCode write(FILE* file, const T& t) {
    return write(file, (uint8_t*)&t, sizeof(T));
  }
}
