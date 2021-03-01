#include <stdio.h>
#include "utility.h"

namespace FILES {
  enum struct OPEN_MODE : uint8_t {
    READ = 'r', WRITE = 'w', APPEND = 'a'
  };

  enum struct DATA_MODE : uint8_t {
    STANDARD = '\0', BINARY = 'b'
  };

  inline FILE* open(const char* name,
                    OPEN_MODE open_mode,
                    DATA_MODE data_mode) {
    char mode[3] ={ (char)open_mode, (char)data_mode, '\0' };
    FILE* out_file;
    fopen_s(&out_file, name, mode);
    return out_file;
  }

  inline ErrorCode close(FILE* file) {
    return fclose(file) == -1 ? ErrorCode::COULD_NOT_CLOSE_FILE
      : ErrorCode::NO_ERROR;
  }

  inline ErrorCode write(FILE* file, const uint8_t* arr, size_t length) {
    fwrite(arr, 1, length, file);
    //TODO: Error Codes from fwrite
    return ErrorCode::NO_ERROR;
  }

  template<typename T>
  inline ErrorCode write(FILE* file, const T& t) {
    return write(file, (uint8_t*)&t, sizeof(T));
  }
}
