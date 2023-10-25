#pragma once
#include "utility.h"
#include "format.h"

namespace FILES {
#define FILE_ERROR_CODES_X \
modify(OK)\
modify(COULD_NOT_CREATE_FILE)\
modify(COULD_NOT_OPEN_FILE)\
modify(COULD_NOT_CLOSE_FILE)\

  enum struct ErrorCode : uint8_t {
  #define modify(NAME) NAME,
    FILE_ERROR_CODES_X
  #undef modify
  };

  namespace ErrorCodeString {
  #define modify(NAME) inline constexpr char NAME[] = #NAME;
    FILE_ERROR_CODES_X
  #undef modify
  }

  constexpr ViewArr<const char> error_code_string(const ErrorCode code) {
    switch (code) {
  #define modify(NAME) case ErrorCode :: NAME :\
return lit_view_arr(ErrorCodeString :: NAME);
      FILE_ERROR_CODES_X
  #undef modify
    }

    return {};
  }

#undef FILE_ERROR_CODES_X

  enum struct OPEN_MODE : uint8_t {
    READ = 'r', WRITE = 'w'
  };

  struct FileData;

  struct OpenedFile {
    FileData* file;
    ErrorCode error_code;
  };

  OpenedFile open(const ViewArr<const char>& name,
                  OPEN_MODE open_mode);
  OpenedFile create(const ViewArr<const char>& name,
                    OPEN_MODE open_mode);
  OpenedFile replace(const ViewArr<const char>& name,
                     OPEN_MODE open_mode);

  bool exist(const ViewArr<const char>& name);

  void close(FileData* file);

  ErrorCode read_to_bytes(FileData* file, uint8_t* bytes, size_t num_bytes);
  uint8_t read_byte(FileData* file);

  template<typename T>
  ErrorCode read(FileData* file, T* ptr, size_t num) {
    return read_to_bytes(file, (uint8_t*)ptr, sizeof(T) * num);
  }

  size_t size_of_file(FileData* file);
  void seek_from_start(FileData* file, size_t offset);
  size_t get_current_pos(FileData* file);

  OwnedArr<const char> load_file_to_string(const ViewArr<const char>& file_name);

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

namespace Format {
  template<>
  struct FormatArg<FILES::ErrorCode> {
    template<Formatter F>
    constexpr static void load_string(F& res, FILES::ErrorCode er) {
      ViewArr<const char> err_str = FILES::error_code_string(er);
      res.load_string_raw(err_str.data, err_str.size);
    }
  };
}

struct InternString;
struct StringInterner;

struct Directory {
  const InternString* directory;
};

struct FileLocation {
  const InternString* directory;
  const InternString* extension;
  const InternString* full_name;

  constexpr bool operator== (const FileLocation& a) const {
    return full_name == a.full_name;
  }
};

struct AllocFilePath {
  OwnedArr<const char> raw;
  usize directory_size;
  usize file_name_start;
  usize file_name_size;
  usize extension_start;
  usize extension_size;
};

AllocFilePath format_file_path(const ViewArr<const char>& path_str,
                               const ViewArr<const char>& file_str,
                               const ViewArr<const char>& extension);

AllocFilePath format_file_path(const ViewArr<const char>& path_str,
                               const ViewArr<const char>& file_str);

OwnedArr<const char> normalize_path(const ViewArr<const char>& base_directory);
OwnedArr<const char> normalize_path(const ViewArr<const char>& current,
                                    const ViewArr<const char>& relative);

FileLocation parse_file_location(const ViewArr<const char>& path,
                                 const ViewArr<const char>& file,
                                 StringInterner* strings);

FileLocation parse_file_location(const AllocFilePath& path,
                                 StringInterner* strings);