#pragma once
#include "utility.h"

struct FileData;

namespace FILES {
  enum struct OPEN_MODE : uint8_t {
    READ = 'r', WRITE = 'w'
  };



  struct OpenedFile {
    FileData* file;
    ErrorCode error_code;
  };

  OpenedFile open(const char* name,
                  OPEN_MODE open_mode);
  OpenedFile create(const char* name,
                  OPEN_MODE open_mode);
  OpenedFile replace(const char* name,
                    OPEN_MODE open_mode);

  ErrorCode close(FileData* file);

  ErrorCode read_as_string(FileData* file, size_t num_bytes, char** out_string);
  ErrorCode read_to_bytes(FileData* file, uint8_t* bytes, size_t num_bytes);
  uint8_t read_byte(FileData* file);

  ErrorCode read_to_structures(FileData* file,
                              uint8_t* ptr_to_s,
                              size_t size_of_s, size_t num_of_s);

  template<typename T>
  ErrorCode read(FileData* file, T* ptr, size_t num) {
    return read_to_structures(file, (uint8_t*)ptr, sizeof(T), num);
  }

  size_t size_of_file(FileData* file);
  void seek_from_start(FileData* file, size_t offset);
  size_t get_current_pos(FileData* file);

  OwnedPtr<const char> load_file_to_string(const char* file_name);

  ErrorCode write(FileData* file, const uint8_t* arr, size_t length);
  ErrorCode write_padding_bytes(FileData* file, uint8_t byte, size_t num);

  ErrorCode write_aligned_array(FileData* file, const uint8_t* arr, const size_t size, const size_t align);
  ErrorCode write_aligned_array(FileData* file, const Array<uint8_t>& arr, const size_t align);

  template<typename T>
  ErrorCode write_obj(FileData* file, const T& t) {
    return write(file, (const uint8_t*)&t, sizeof(T));
  }

  template<usize N>
  ErrorCode write_str(FileData* file, const char(&str)[N]) {
    return write(file, (const uint8_t*)str, N - 1);
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

const InternString* get_extension(const char* path,
                                  StringInterner* strings);