#include <stdio.h>
#include "utility.h"

namespace FILES {
  enum struct OPEN_MODE : uint8_t {
    READ = 'r', WRITE = 'w', APPEND = 'a'
  };

  enum struct DATA_MODE : uint8_t {
    STANDARD = '\0', BINARY = 'b'
  };

  inline FILE* open(const FILE_NAME& name,
                    OPEN_MODE open_mode,
                    DATA_MODE data_mode = DATA_MODE::STANDARD);

  inline ErrorCode close(FILE* file);

  inline ErrorCode write(FILE* file, const uint8_t* arr, size_t length);

  template<typename T>
  inline ErrorCode write(FILE* file, const T& t) {
    return write(file, (uint8_t*)&t, sizeof(T));
  }
}
