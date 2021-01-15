#include "file.h"
namespace FILES {
  inline FILE* open(const char* name,
                    OPEN_MODE open_mode,
                    DATA_MODE data_mode) {
    char mode[3] = {(char)open_mode, (char)data_mode, '\0'};
    return fopen(name, mode);
  }

  inline ErrorCode close(FILE* file) {
    return fclose(file) == -1 ? ErrorCode::COULD_NOT_CLOSE_FILE
                              : ErrorCode::NO_ERROR;
  }

  inline ErrorCode write(FILE* file, const uint8_t* arr, size_t length) {
    fwrite(arr, 1, length, file);
    //TODO: Error Codes from fwrite
    return ErroCode::NO_ERROR
  }
}
