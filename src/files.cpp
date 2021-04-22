#include "files.h"
namespace FILES {

  OpenedFile open(const char* name,
                  OPEN_MODE open_mode,
                  DATA_MODE data_mode) {
    OpenedFile opened_file;
    char mode[3] ={ (char)open_mode, (char)data_mode, '\0' };
    opened_file.error_code = fopen_s(&opened_file.file, name, mode);
    return opened_file;
  }

  ErrorCode close(FILE* file) {
    return fclose(file) == -1 ? ErrorCode::COULD_NOT_CLOSE_FILE
      : ErrorCode::OK;
  }

  ErrorCode read_as_string(FILE* file, size_t num_bytes, char** out_string) {
    *out_string = allocate_default<char>(num_bytes + 1);

    fread((void*)*out_string, sizeof(char), num_bytes, file);

    (*out_string)[num_bytes] = '\0';

    //TODO: Error Codes
    return ErrorCode::OK;
  }

  size_t size_of_file(FILE* file) {
    const long cur = ftell(file);

    fseek(file, 0, SEEK_END);
    const size_t size = ftell(file);

    fseek(file, cur, SEEK_SET);
    return size;
  }

  const char* load_file_to_string(const char* file_name) {
    const OpenedFile file = open(file_name, OPEN_MODE::READ, DATA_MODE::BINARY);

    if (file.error_code != 0) {
      return nullptr;
    }

    const size_t file_size = size_of_file(file.file);

    char* string;
    read_as_string(file.file, file_size, &string);

    fclose(file.file);

    return string;
  }

  ErrorCode write(FILE* file, const uint8_t* arr, size_t length) {
    fwrite(arr, 1, length, file);
    //TODO: Error Codes from fwrite
    return ErrorCode::OK;
  }

  ErrorCode write_padding_bytes(FILE* file, uint8_t byte, size_t num) {
    for (size_t i = 0; i < num; i++) {
      fputc(byte, file);
    }
    return ErrorCode::OK;
  }

  ErrorCode write_aligned_array(FILE* file, const uint8_t* arr, const size_t size, const size_t align) {
    //Write the data
    fwrite(arr, 1, size, file);

    //Padding
    const size_t size_mod_align = size % align;

    if (size_mod_align != 0) {
      return write_padding_bytes(file, '\0', align - size_mod_align);
    }
    else {
      //TODO: Error Codes from fwrite
      return ErrorCode::OK;
    }
  }

  ErrorCode write_aligned_array(FILE* file, const Array<uint8_t>& arr, const size_t align) {
    return write_aligned_array(file, arr.data, arr.size, align);
  }
}