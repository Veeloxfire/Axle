#include "files.h"
#include "strings.h"
#include "trace.h"

namespace FILES {

  OpenedFile open(const char* name,
                  OPEN_MODE open_mode,
                  DATA_MODE data_mode) {
    TRACING_FUNCTION();

    OpenedFile opened_file ={};
    char mode[3] ={ (char)open_mode, (char)data_mode, '\0' };
    opened_file.error_code = fopen_s(&opened_file.file, name, mode);
    return opened_file;
  }

  ErrorCode close(FILE* file) {
    TRACING_FUNCTION();
    return fclose(file) == -1 ? ErrorCode::COULD_NOT_CLOSE_FILE
      : ErrorCode::OK;
  }

  ErrorCode read_as_string(FILE* file, size_t num_bytes, char** out_string) {
    TRACING_FUNCTION();


    *out_string = allocate_default<char>(num_bytes + 1);


    fread((void*)*out_string, sizeof(char), num_bytes, file);
    (*out_string)[num_bytes] = '\0';



    //TODO: Error Codes
    return ErrorCode::OK;
  }

  ErrorCode read_to_bytes(FILE* file, uint8_t* bytes, size_t num_bytes) {
    size_t i = fread(bytes, sizeof(uint8_t), num_bytes, file);

    return ErrorCode::OK;
  }

  ErrorCode read_to_structure(FILE* file,
                              uint8_t* ptr_to_s,
                              size_t size_of_s, size_t num_of_s) {
    size_t i = fread(ptr_to_s, size_of_s, num_of_s, file);

    return ErrorCode::OK;
  }

  uint8_t read_byte(FILE* file) {
    return (uint8_t)fgetc(file);
  }

  ErrorCode seek(FILE* file, SEEK_MODE mode, int32_t location) {
    static_assert(sizeof(int32_t) == sizeof(long), "Must be same");
    int err = fseek(file, static_cast<long>(location), static_cast<int>(mode));

    return ErrorCode::OK;
  }

  long current_pos(FILE* file) {
    return ftell(file);
  }

  size_t size_of_file(FILE* file) {
    const long cur = ftell(file);

    fseek(file, 0, SEEK_END);
    const size_t size = ftell(file);

    fseek(file, cur, SEEK_SET);
    return size;
  }

  OwnedPtr<const char> load_file_to_string(const char* file_name) {
    TRACING_FUNCTION();

    const OpenedFile file = open(file_name, OPEN_MODE::READ, DATA_MODE::BINARY);

    if (file.error_code != 0) {
      return nullptr;
    }

    const size_t file_size = size_of_file(file.file);

    char* string = nullptr;
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

FileLocation parse_file_location(const char* path_str,
                                 const char* file_str,
                                 StringInterner* const strings) {

  //check if file is absolute path
  if (file_str != nullptr) {
    const char* str_ptr = file_str;
    while (true) {
      const char c = *str_ptr;
      if (c == '\0') {
        //Relative path
        break;
      }
      else if (c == ':') {
        //Absolute path
        path_str = file_str;
        file_str = nullptr;
        break;
      }
      else if (c == '\\' || c == '/') {
        //Also Relative path
        break;
      }

      str_ptr++;
    }
  }


  struct Range {
    const char* start;
    const char* end;
  };

  Array<Range> path ={};

  const char* holder = path_str;

  const auto add_dir = [&](const char* start, const char* end) {
    if (start == end) {
      return;
    }

    Range dir ={};
    dir.start = start;
    dir.end = end;


    if (dir.end - dir.start == 2 && memcmp(dir.start, "..", 2) == 0) {
      //Is "go up a directory"
      //Pop last dir

      Range* back_r = path.back();
      if (back_r->end - back_r->start == 2 && memcmp(back_r->start, "..", 2) == 0) {
        //Dont pop
        path.insert(dir);
      }
      else if (back_r->end - back_r->start == 1 && back_r->start[0] == '.') {
        path.pop();
        path.insert(dir);
      }
      else {
        path.pop();
      }
    }
    else if (dir.end - dir.start == 1 && dir.start[0] == '.') {
      //Is "stay in same directory"
      //do nothing
    }
    else {
      //Directory name
      path.insert(dir);
    }
  };

  while (true) {
    const char c = *path_str;
    if (c == '/' || c == '\\') {
      add_dir(holder, path_str);

      path_str++;
      holder = path_str;
    }
    else if (c == '\0') {
      if (file_str != nullptr && file_str[0] != '\0') {
        //Do the file path
        add_dir(holder, path_str);

        holder = file_str;
        path_str = file_str;
        file_str = nullptr;
      }
      else {
        //end of path
        path_str++;
        break;
      }
    }
    else {
      path_str++;
    }
  }

  Array<char> str ={};

  FileLocation loc ={};
  //Directory
  {
    auto i = path.begin();
    const auto end = path.end();

    for (; i < end; i++) {
      const size_t len = i->end - i->start;
      str.insert_uninit(len);

      memcpy_ts(str.data + str.size - len, len, i->start, len);
      str.insert('\\');
    }

    loc.directory = strings->intern(str.data, str.size);
  }

  //Add the file
  {
    //Load the extension
    loc.extension = get_extension(holder, strings);

    //Load the file
    const size_t len = path_str - holder;
    str.insert_uninit(len);

    memcpy_ts(str.data + str.size - len, len, holder, len);

    loc.full_name = strings->intern(str.data);
  }

  return loc;
}

const InternString* get_extension(const char* path, StringInterner* strings) {
  //Try find the extension
  const char* extension = nullptr;
  while (true) {
    char c = *path;
    if (c == '\0') {
      break;
    }
    else if (c == '.') {
      extension = path;
    }
    else if (c == '\\' || c == '/') {
      extension = nullptr;
    }

    path++;
  }

  if (extension == nullptr) {
    return nullptr;
  }
  else {
    return strings->intern(extension + 1);
  }
}