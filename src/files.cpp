#include "files.h"
#include "strings.h"
#include "trace.h"
#include <Windows.h>

FILES::OpenedFile FILES::open(const char* name,
                OPEN_MODE open_mode) {
  TRACING_FUNCTION();

  OpenedFile opened_file ={};

  DWORD access;
  DWORD share;

  switch (open_mode) {
    case OPEN_MODE::READ: {
        access = GENERIC_READ;
        share = FILE_SHARE_READ;
        break;
      }
    case OPEN_MODE::WRITE: {
        access = GENERIC_WRITE;
        share = 0;
        break;
      }
  }


  opened_file.file = (FileData*)CreateFileA(name, access, share, 0, OPEN_EXISTING, 0, 0);
  if (opened_file.file == INVALID_HANDLE_VALUE) {
    opened_file.error_code = ErrorCode::COULD_NOT_OPEN_FILE;
  }
  else {
    opened_file.error_code = ErrorCode::OK;
  }

  return opened_file;
}

FILES::OpenedFile FILES::create(const char* name,
                              OPEN_MODE open_mode) {
  TRACING_FUNCTION();

  OpenedFile opened_file ={};

  DWORD access;
  DWORD share;

  switch (open_mode) {
    case OPEN_MODE::READ: {
        access = GENERIC_READ;
        share = FILE_SHARE_READ;
        break;
      }
    case OPEN_MODE::WRITE: {
        access = GENERIC_WRITE;
        share = 0;
        break;
      }
  }


  opened_file.file = (FileData*)CreateFileA(name, access, share, 0, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
  if (opened_file.file == INVALID_HANDLE_VALUE) {
    opened_file.error_code = ErrorCode::COULD_NOT_CREATE_FILE;
  }
  else {
    opened_file.error_code = ErrorCode::OK;
  }

  return opened_file;
}

FILES::OpenedFile FILES::replace(const char* name,
                                OPEN_MODE open_mode) {
  TRACING_FUNCTION();

  OpenedFile opened_file ={};

  DWORD access;
  DWORD share;

  switch (open_mode) {
    case OPEN_MODE::READ: {
        access = GENERIC_READ;
        share = FILE_SHARE_READ;
        break;
      }
    case OPEN_MODE::WRITE: {
        access = GENERIC_WRITE;
        share = 0;
        break;
      }
  }

  opened_file.file = (FileData*)CreateFileA(name, access, share, 0, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  if (opened_file.file == INVALID_HANDLE_VALUE) {
    opened_file.error_code = ErrorCode::COULD_NOT_CREATE_FILE;
  }
  else {
    opened_file.error_code = ErrorCode::OK;
  }

  return opened_file;
}

ErrorCode FILES::close(FileData* file) {
  TRACING_FUNCTION();
  CloseHandle(file);

  return ErrorCode::OK;
}

ErrorCode FILES::read_as_string(FileData* file, size_t num_bytes, char** out_string) {
  TRACING_FUNCTION();


  *out_string = allocate_default<char>(num_bytes + 1);

  ReadFile(file, *out_string, (DWORD)num_bytes, 0, 0);

  (*out_string)[num_bytes] = '\0';


  //TODO: Error Codes
  return ErrorCode::OK;
}

ErrorCode FILES::read_to_bytes(FileData* file, uint8_t* bytes, size_t num_bytes) {
  ReadFile(file, bytes, (DWORD)num_bytes, 0, 0);

  return ErrorCode::OK;
}

ErrorCode FILES::read_to_structures(FileData* file,
                             uint8_t* ptr_to_s,
                             size_t size_of_s, size_t num_of_s) {
  ReadFile(file, ptr_to_s, (DWORD)(size_of_s * num_of_s), 0, 0);
  return ErrorCode::OK;
}

uint8_t FILES::read_byte(FileData* file) {
  uint8_t v = 0;
  ReadFile(file, &v, 1, 0, 0);

  return v;
}

size_t FILES::size_of_file(FileData* file) {
  LARGE_INTEGER li ={};

  GetFileSizeEx(file, &li);

  return (size_t)li.QuadPart;
}

OwnedPtr<const char> FILES::load_file_to_string(const char* file_name) {
  TRACING_FUNCTION();

  const OpenedFile file = open(file_name, OPEN_MODE::READ);

  if (file.error_code != ErrorCode::OK) {
    return nullptr;
  }

  const size_t file_size = size_of_file(file.file);

  char* string = nullptr;
  read_as_string(file.file, file_size, &string);

  close(file.file);

  return string;
}

ErrorCode FILES::write(FileData* file, const uint8_t* arr, size_t length) {
  WriteFile(file, arr, (DWORD)length, 0, 0);

  return ErrorCode::OK;
}

ErrorCode FILES::write_padding_bytes(FileData* file, uint8_t byte, size_t num) {
  for (size_t i = 0; i < num; i++) {
    WriteFile(file, &byte, 1, 0, 0);
  }


  return ErrorCode::OK;
}

ErrorCode FILES::write_aligned_array(FileData* file, const uint8_t* arr, const size_t size, const size_t align) {
  //Write the data
  write(file, arr, size);

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

ErrorCode FILES::write_aligned_array(FileData* file, const Array<uint8_t>& arr, const size_t align) {
  return write_aligned_array(file, arr.data, arr.size, align);
}

void FILES::seek_from_start(FileData* file, size_t offset) {
  LARGE_INTEGER li ={};
  li.QuadPart = (LONGLONG)offset;
  SetFilePointerEx(file, li, NULL, FILE_BEGIN);
}

size_t FILES::get_current_pos(FileData* file) {
  LARGE_INTEGER li ={};
  SetFilePointerEx(file, {}, &li, FILE_CURRENT);

  return (size_t)li.QuadPart;
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