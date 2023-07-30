#include "files.h"
#include "strings.h"
#include "trace.h"
#include <Windows.h>

static constexpr usize BUFFER_SIZE = 1024;

namespace FILES {
  struct FileData {
    HANDLE handle;
    usize real_file_ptr;
    usize real_file_size;

    usize abstract_file_ptr;
    usize abstract_file_size;

    usize real_buffer_ptr;

    bool in_sync;
    u32 buffer_size;
    u8 buffer[BUFFER_SIZE];

    FileData(HANDLE h);
    ~FileData() noexcept(false);
  };
}

FILES::FileData::FileData(HANDLE h) : handle(h) {
  LARGE_INTEGER li = { 0 };
  GetFileSizeEx(h, &li);

  real_file_size = (usize)li.QuadPart;

  li.QuadPart = 0;
  SetFilePointerEx(h, li, &li, FILE_CURRENT);

  real_file_ptr = (usize)li.QuadPart;

  abstract_file_ptr = real_file_ptr;
  abstract_file_size = real_file_size;

  real_buffer_ptr = 0;
  buffer_size = 0;
  in_sync = true;
}

FILES::OpenedFile FILES::open(const char* name,
                              OPEN_MODE open_mode) {
  TRACING_FUNCTION();

  OpenedFile opened_file = {};

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


  HANDLE h = CreateFileA(name, access, share, 0, OPEN_EXISTING, 0, 0);
  if (h == INVALID_HANDLE_VALUE) {
    opened_file.error_code = ErrorCode::COULD_NOT_OPEN_FILE;
    opened_file.file = nullptr;
    return opened_file;
  }
  else {
    FileData* file = allocate_single_constructed<FileData>(h);

    opened_file.error_code = ErrorCode::OK;
    opened_file.file = file;
    return opened_file;
  }
}

FILES::OpenedFile FILES::create(const char* name,
                                OPEN_MODE open_mode) {
  TRACING_FUNCTION();

  OpenedFile opened_file = {};

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


  HANDLE h = CreateFileA(name, access, share, 0, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
  if (h == INVALID_HANDLE_VALUE) {
    opened_file.error_code = ErrorCode::COULD_NOT_OPEN_FILE;
    opened_file.file = nullptr;
    return opened_file;
  }
  else {
    FileData* file = allocate_single_constructed<FileData>(h);

    opened_file.error_code = ErrorCode::OK;
    opened_file.file = file;
    return opened_file;
  }
}

FILES::OpenedFile FILES::replace(const char* name,
                                 OPEN_MODE open_mode) {
  TRACING_FUNCTION();

  OpenedFile opened_file = {};

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

  HANDLE h = CreateFileA(name, access, share, 0, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  if (h == INVALID_HANDLE_VALUE) {
    opened_file.error_code = ErrorCode::COULD_NOT_OPEN_FILE;
    opened_file.file = nullptr;
    return opened_file;
  }
  else {
    FileData* file = allocate_single_constructed<FileData>(h);

    opened_file.error_code = ErrorCode::OK;
    opened_file.file = file;
    return opened_file;
  }
}

bool FILES::exist(const char* name) {
  return GetFileAttributesA(name) != INVALID_FILE_ATTRIBUTES;
}

void FILES::close(FileData* file) {
  TRACING_FUNCTION();

  free_destruct_single(file);
}

static void real_seek(FILES::FileData* file, usize ptr) {
  if (file->real_file_ptr != ptr) {
    LARGE_INTEGER li = {};
    li.QuadPart = (LONGLONG)ptr - (LONGLONG)file->real_file_ptr;

    SetFilePointerEx(file->handle, li, &li, FILE_CURRENT);

    ASSERT(li.QuadPart == ptr);
    file->real_file_ptr = ptr;
  }
}

static void force_sync_buffer(FILES::FileData* file) {
  real_seek(file, file->real_buffer_ptr);

  DWORD bytes_written = 0;
  BOOL wrote = WriteFile(file->handle, file->buffer, file->buffer_size, &bytes_written, NULL);

  ASSERT(wrote);
  ASSERT(bytes_written == file->buffer_size);
  file->in_sync = true;
  file->real_file_ptr = file->real_buffer_ptr + file->buffer_size;
  file->real_file_size = file->abstract_file_size;
}

static void sync_buffer(FILES::FileData* file) {
  if (!file->in_sync) {
    force_sync_buffer(file);
  }
}

FILES::FileData::~FileData() noexcept(false) {
  sync_buffer(this);
  CloseHandle(handle);
}

struct BufferRange {
  usize ptr_start;
  u32 buffer_start;
  u32 size;
};

static BufferRange get_loaded_range(FILES::FileData* file, usize ptr, usize num_bytes) {
  BufferRange range = {};

  if (ptr < file->real_buffer_ptr) {
    if (ptr + num_bytes <= file->real_buffer_ptr) {
      //No overlap
      return {};
    }

    range.ptr_start = file->real_buffer_ptr - ptr;
    range.buffer_start = 0;

    usize total_after = file->real_buffer_ptr - (ptr + num_bytes);
    if (total_after < (usize)file->buffer_size) {
      range.size = (u32)total_after;
    }
    else {
      range.size = file->buffer_size;
    }
  }
  else {
    usize buffer_end_ptr = file->real_buffer_ptr + (usize)file->buffer_size;
    if (buffer_end_ptr <= ptr) {
      //No overlap
      return {};
    }

    range.ptr_start = 0;
    range.buffer_start = (u32)(ptr - file->real_buffer_ptr);

    if (buffer_end_ptr - ptr < num_bytes) {
      range.size = (u32)(buffer_end_ptr - ptr);
    }
    else {
      range.size = (u32)num_bytes;
    }
  }

  return range;
}


static void small_buffer_read(FILES::FileData* file, usize abstract_ptr, uint8_t* bytes, size_t num_bytes) {
  usize space_in_file = file->real_file_size - file->real_buffer_ptr;
  usize can_read_size = BUFFER_SIZE > space_in_file ? space_in_file : BUFFER_SIZE;

  ASSERT(num_bytes <= can_read_size);

  BOOL read = ReadFile(file->handle, file->buffer, (DWORD)can_read_size, NULL, NULL);
  ASSERT(read);
  file->real_file_ptr += can_read_size;

  file->real_buffer_ptr = abstract_ptr;
  file->buffer_size = (u32)can_read_size;
  memcpy_s(bytes, num_bytes, file->buffer, num_bytes);
}

static void big_buffer_read(FILES::FileData* file, usize abstract_ptr, uint8_t* bytes, size_t num_bytes) {
  ASSERT(num_bytes > BUFFER_SIZE);
  ASSERT(num_bytes < file->real_file_size - file->real_buffer_ptr);

  BOOL read = ReadFile(file->handle, bytes, (DWORD)num_bytes, NULL, NULL);
  ASSERT(read);
  file->real_file_ptr += num_bytes;

  //Take the back bits
  file->real_buffer_ptr = abstract_ptr + (num_bytes - BUFFER_SIZE);
  memcpy_s(file->buffer, BUFFER_SIZE, bytes + (num_bytes - BUFFER_SIZE), BUFFER_SIZE);
  file->buffer_size = BUFFER_SIZE;
}

static void generic_buffer_read(FILES::FileData* file, usize abstract_ptr, uint8_t* bytes, size_t num_bytes) {
  if (num_bytes > BUFFER_SIZE) {
    big_buffer_read(file, abstract_ptr, bytes, num_bytes);
  }
  else {
    big_buffer_read(file, abstract_ptr, bytes, num_bytes);
  }
}

static void small_write_after_buffer(FILES::FileData* file, usize abstract_ptr, const uint8_t* bytes, size_t num_bytes) {
  ASSERT(file->in_sync);
  ASSERT(num_bytes < BUFFER_SIZE);
  ASSERT(file->real_buffer_ptr + file->buffer_size == abstract_ptr);
  ASSERT(file->real_file_ptr == file->real_buffer_ptr);

  WriteFile(file->handle, bytes, (DWORD)num_bytes, NULL, NULL);
  file->real_file_ptr += num_bytes;
  if (file->real_file_ptr > file->real_file_size) {
    file->real_file_size = file->real_file_ptr;
  }

  usize space_in_file = file->real_file_size - file->real_buffer_ptr;
  usize can_read_size = BUFFER_SIZE > space_in_file ? space_in_file : BUFFER_SIZE;

  ASSERT(num_bytes <= can_read_size);

  usize can_reuse = can_read_size > file->buffer_size;
  usize from_previous = (can_read_size - num_bytes);

  memmove_s(file->buffer, from_previous, file->buffer + (num_bytes), from_previous);
  memcpy_s(file->buffer + from_previous, num_bytes, bytes, num_bytes);


  file->real_buffer_ptr = abstract_ptr - from_previous;
  file->buffer_size = (u32)can_read_size;
}

static void write_new_buffer(FILES::FileData* file, usize abstract_ptr, const uint8_t* bytes, size_t num_bytes) {
  BOOL wrote = WriteFile(file->handle, bytes, (DWORD)num_bytes, NULL, NULL);
  ASSERT(wrote);

  file->real_file_ptr += num_bytes;
  if (file->real_file_ptr > file->real_file_size) {
    file->real_file_size = file->real_file_ptr;
  }

  usize size = num_bytes > BUFFER_SIZE ? BUFFER_SIZE : num_bytes;

  file->real_buffer_ptr = file->abstract_file_ptr + (num_bytes - size);
  file->in_sync = true;
  file->buffer_size = (u32)size;
  memcpy_s(file->buffer, size, bytes + (num_bytes - size), size);
}

ErrorCode FILES::read_to_bytes(FileData* file, uint8_t* bytes, size_t num_bytes) {
  ASSERT(num_bytes <= ULONG_MAX);

  BufferRange range = get_loaded_range(file, file->abstract_file_ptr, num_bytes);

  if (num_bytes > range.size) {
    sync_buffer(file); //always need to sync here

    bool at_buffer_start = range.ptr_start == 0;
    bool at_buffer_end = range.ptr_start + range.size == num_bytes;

    bool straddles = range.size == BUFFER_SIZE && (!at_buffer_start || !at_buffer_end);

    if (straddles) {
      //straddles the buffer
      //lets just do-over
      big_buffer_read(file, file->abstract_file_ptr, bytes, num_bytes);
    }
    else if (range.ptr_start == 0) {
      memcpy_s(bytes, range.size, file->buffer + range.buffer_start, range.size);
      usize remaining = num_bytes - (usize)range.size;
      generic_buffer_read(file, file->abstract_file_ptr + range.size,
                          bytes + range.size, remaining);
    }
    else {
      ASSERT(range.buffer_start == 0);
      memcpy_s(bytes + range.ptr_start, range.size, file->buffer + range.buffer_start, range.size);
      usize remaining = range.ptr_start;
      generic_buffer_read(file, file->abstract_file_ptr, bytes, remaining);
    }
  }
  else {
    ASSERT(range.size == num_bytes);
    ASSERT(range.ptr_start == 0);
    //all is loaded
    memcpy_s(bytes, num_bytes, file->buffer + range.buffer_start, num_bytes);
  }

  file->abstract_file_ptr += num_bytes;

  return ErrorCode::OK;
}

uint8_t FILES::read_byte(FileData* file) {
  u8 byte = 0;
  read_to_bytes(file, &byte, 1);
  return byte;
}

size_t FILES::size_of_file(FileData* file) {
  return file->abstract_file_size;
}

OwnedArr<const char> FILES::load_file_to_string(const char* file_name) {
  TRACING_FUNCTION();

  HANDLE h = CreateFileA(file_name, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, 0, 0);
  if (h == INVALID_HANDLE_VALUE) return {};
  LARGE_INTEGER li = {};
  GetFileSizeEx(h, &li);

  char* string = allocate_default<char>(li.QuadPart + 1);
  BOOL read = ReadFile(h, string, (DWORD)li.QuadPart, NULL, NULL);
  ASSERT(read);

  string[li.QuadPart] = '\0';

  CloseHandle(h);

  return { string, static_cast<usize>(li.QuadPart) };
}

ErrorCode FILES::write(FileData* file, const uint8_t* bytes, size_t num_bytes) {
  usize end = file->abstract_file_ptr + num_bytes;
  usize max_buffer_end = file->real_buffer_ptr + BUFFER_SIZE;

  if (file->real_buffer_ptr <= file->abstract_file_ptr && end <= max_buffer_end) {
    usize start = (file->abstract_file_ptr - file->real_buffer_ptr);

    //write over part of the buffer
    memcpy_s(file->buffer + start, num_bytes, bytes, num_bytes);

    if (file->buffer_size < start + num_bytes) file->buffer_size = (u32)(start + num_bytes);

    file->in_sync = false;
  }
  else {
    //lazy
    sync_buffer(file);
    seek_from_start(file, file->abstract_file_ptr);
    write_new_buffer(file, file->abstract_file_ptr, bytes, num_bytes);
  }

  file->abstract_file_ptr += num_bytes;
  if (file->abstract_file_size < file->abstract_file_ptr) {
    file->abstract_file_size = file->abstract_file_ptr;
  }

  return ErrorCode::OK;
}

ErrorCode FILES::write_padding_bytes(FileData* file, uint8_t byte, size_t num) {
  //TODO: actual buffered io

  sync_buffer(file);
  seek_from_start(file, file->abstract_file_ptr);

  usize remaining = num;

  while (remaining > BUFFER_SIZE) {
    memset(file->buffer, byte, BUFFER_SIZE);
    file->buffer_size = BUFFER_SIZE;
    file->real_buffer_ptr = file->abstract_file_ptr;
    force_sync_buffer(file);

    remaining -= BUFFER_SIZE;
  }

  if (num > 0) {
    memset(file->buffer, byte, remaining);
    file->buffer_size = (u32)remaining;
    file->real_buffer_ptr = file->abstract_file_ptr;
    force_sync_buffer(file);
  }

  file->abstract_file_ptr += num;

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

void FILES::seek_from_start(FileData* file, size_t location) {
  ASSERT(location <= file->abstract_file_size);
  file->abstract_file_ptr = location;
}

size_t FILES::get_current_pos(FileData* file) {
  LARGE_INTEGER li = {};
  SetFilePointerEx(file, {}, &li, FILE_CURRENT);

  return (size_t)li.QuadPart;
}

struct Range {
  const char* start;
  const char* end;
};

constexpr static bool is_character(char c) {
  return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'z');
}

constexpr static bool is_absolute_path(const Range& r) {
  return (r.end - r.start >= 3)
    && is_character(r.start[0])
    && r.start[1] == ':'
    && (r.start[2] == '\\' || r.start[2] == '/');
}

constexpr static bool logical_xor(bool a, bool b) {
  return (a || b) && !(a && b);
}

static void append_single_to_path(Array<Range>& path, const char* start, const char* end) {
  if (start == end) {
    return;
  }

  Range dir = {};
  dir.start = start;
  dir.end = end;


  if (dir.end - dir.start == 2 && memcmp(dir.start, "..", 2) == 0 && path.size > 0) {
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
}

struct ParseInfo {
  const char* final_start;
  const char* final_end;
};

static ParseInfo append_path_to_path(Array<Range>& path, const char* start, const char* end) {
  const char* save = start;


  while (true) {
    if (start == end) {
      break;
    }

    const char c = *start;
    ASSERT(c != '\0');

    if (c == '/' || c == '\\') {
      append_single_to_path(path, save, start);

      start++;
      save = start;
    }
    else {
      start++;
    }
  }

  return {
    save, end,
  };
}

static const char* find_dot_in_file_name(const char* start, const char* end) {
  while (start < end) {
    if (*start == '.') return start;

    start += 1;
  }

  return nullptr;
}

static OwnedArr<const char> normalize_path(Range base_directory) {
  Range& path_str = base_directory;

  ASSERT(path_str.start != nullptr);
  ASSERT(path_str.start < path_str.end);

  const bool absolute_path = is_absolute_path(path_str);

  Array<Range> path = {};

  {
    ParseInfo path_p_info = append_path_to_path(path, path_str.start, path_str.end);

    if (path_p_info.final_start < path_p_info.final_end) {
      append_single_to_path(path, path_p_info.final_start, path_p_info.final_end);
    }
  }

  Array<char> str = {};

  if (!absolute_path) {
    str.insert('.');
    str.insert('\\');
  }

  {
    auto i = path.begin();
    const auto end = path.end();

    if (i < end) {
      const size_t len = i->end - i->start;
      str.insert_uninit(len);

      memcpy_ts(str.data + str.size - len, len, i->start, len);
      i += 1;
      while (i < end) {
        str.insert('\\');
        const size_t len = i->end - i->start;
        str.insert_uninit(len);

        memcpy_ts(str.data + str.size - len, len, i->start, len);
        i += 1;
      }
    }
  }

  str.insert('\0');

  auto arr = bake_const_arr(std::move(str));
  arr.size -= 1;
  return arr;
}

OwnedArr<const char> normalize_path(const char* current) {
  ASSERT(current != nullptr);

  const usize curr_size = strlen_ts(current);

  return normalize_path({ current,  current + curr_size });
}

static OwnedArr<const char> normalize_path(Range base_directory,
                                           Range relative) {

  Range& path_str = base_directory;
  Range& file_str = relative;

  ASSERT(path_str.start != nullptr);
  ASSERT(path_str.start < path_str.end);

  ASSERT(file_str.start != nullptr);
  ASSERT(file_str.start < file_str.end);

  const bool absolute_path = is_absolute_path(path_str);
  ASSERT(!is_absolute_path(file_str));

  Array<Range> path = {};

  {
    ParseInfo path_p_info = append_path_to_path(path, path_str.start, path_str.end);

    if (path_p_info.final_start < path_p_info.final_end) {
      append_single_to_path(path, path_p_info.final_start, path_p_info.final_end);
    }
  }

  {
    ParseInfo file_p_info = append_path_to_path(path, file_str.start, file_str.end);
    if (file_p_info.final_start < file_p_info.final_end) {
      append_single_to_path(path, file_p_info.final_start, file_p_info.final_end);
    }
  }

  Array<char> str = {};

  if (!absolute_path) {
    str.insert('.');
    str.insert('\\');
  }

  {
    auto i = path.begin();
    const auto end = path.end();

    if (i < end) {
      const size_t len = i->end - i->start;
      str.insert_uninit(len);

      memcpy_ts(str.data + str.size - len, len, i->start, len);
      i += 1;
      while (i < end) {
        str.insert('\\');
        const size_t len = i->end - i->start;
        str.insert_uninit(len);

        memcpy_ts(str.data + str.size - len, len, i->start, len);
        i += 1;
      }
    }
  }

  str.insert('\0');

  auto arr = bake_const_arr(std::move(str));
  arr.size -= 1;
  return arr;
}

OwnedArr<const char> normalize_path(const char* current, const char* relative) {
  ASSERT(current != nullptr);
  ASSERT(relative != nullptr);

  const usize curr_size = strlen_ts(current);
  const usize rel_size = strlen_ts(relative);

  return normalize_path({ current,  current + curr_size },
                        { relative, relative + rel_size });
}

static AllocFilePath format_file_path(Range base_directory,
                                      Range relative_file,
                                      Range extension) {
  Range& path_str = base_directory;
  Range& file_str = relative_file;

  ASSERT(path_str.start != nullptr);
  ASSERT(path_str.start < path_str.end);

  ASSERT(file_str.start != nullptr);
  ASSERT(file_str.start < file_str.end);

  const bool absolute_path = is_absolute_path(path_str);
  ASSERT(!is_absolute_path(file_str));

  Array<Range> path = {};

  {
    ParseInfo path_p_info = append_path_to_path(path, path_str.start, path_str.end);
    if (path_p_info.final_start < path_p_info.final_end) {
      append_single_to_path(path, path_p_info.final_start, path_p_info.final_end);
    }
  }


  ParseInfo file_p_info = append_path_to_path(path, file_str.start, file_str.end);

  const char* dot = find_dot_in_file_name(file_p_info.final_start, file_p_info.final_end);

  const bool inline_extension = dot != nullptr && (dot + 1) < file_p_info.final_end;
  const bool has_extension = inline_extension || extension.start < extension.end;

  //save is the start of the file

  Array<char> str = {};

  if (!absolute_path) {
    str.insert('.');
    str.insert('\\');
  }

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
  }

  const usize directory_size = str.size;
  const usize file_name_index = str.size;

  const char* name_end;
  if (inline_extension) {
    name_end = dot;
  }
  else {
    name_end = file_p_info.final_end;
  }

  //Load the file name
  {
    const size_t len = (name_end - file_p_info.final_start);
    str.insert_uninit(len);

    memcpy_ts(str.data + str.size - len, len, file_p_info.final_start, len);
  }

  const usize file_name_size = str.size - file_name_index;
  usize extension_index = 0;
  usize extension_size = 0;

  if (has_extension) {
    ASSERT(!(inline_extension && extension.start < extension.end));
    str.insert('.');
    extension_index = str.size;

      //Load the extension if there isn't one already
    {
      const char* e_start;
      const char* e_end;
      if (inline_extension) {
        ASSERT(extension.start == extension.end);
        e_start = dot + 1;
        e_end = file_p_info.final_end;
      }
      else {
        e_start = extension.start;
        e_end = extension.end;
      }

      ASSERT(e_end > e_start);
      const size_t len = (e_end - e_start);
      str.insert_uninit(len);

      memcpy_ts(str.data + str.size - len, len, e_start, len);
    }

    extension_size = str.size - extension_index;
  }
  
  str.insert('\0');

  auto arr = bake_const_arr(std::move(str));
  arr.size -= 1;

  return {
    std::move(arr),
    directory_size,
    file_name_index,
    file_name_size,
    extension_index,
    extension_size,
  };
}

AllocFilePath format_file_path(const char* path_str,
                               const char* file_str,
                               const char* extension) {
  ASSERT(path_str != nullptr);
  ASSERT(file_str != nullptr);
  ASSERT(extension != nullptr);

  const usize path_size = strlen_ts(path_str);
  const usize file_size = strlen_ts(file_str);
  const usize extension_size = strlen_ts(extension);

  return format_file_path({ path_str, path_str + path_size },
                          { file_str, file_str + file_size },
                          { extension, extension + extension_size });
}

AllocFilePath format_file_path(const char* path_str,
                               const char* file_str) {
  ASSERT(path_str != nullptr);
  ASSERT(file_str != nullptr);

  const usize path_size = strlen_ts(path_str);
  const usize file_size = strlen_ts(file_str);

  return format_file_path({ path_str, path_str + path_size },
                          { file_str, file_str + file_size },
                          { nullptr, nullptr });
}

FileLocation parse_file_location(const AllocFilePath& path,
                                 StringInterner* const strings) {
  const char* raw = path.raw.data;

  FileLocation loc = {};

  loc.full_name = strings->intern(raw, path.raw.size);
  loc.directory = strings->intern(raw, path.directory_size);
  if (path.extension_size > 0) {
    loc.extension = strings->intern(raw + path.extension_start, path.extension_size);
  }
  else {
    loc.extension = nullptr;
  }

  return loc;
}

FileLocation parse_file_location(const char* const path_str_in,
                                 const char* const file_str_in,
                                 StringInterner* const strings) {
  const usize path_size = strlen_ts(path_str_in);
  const usize file_size = strlen_ts(file_str_in);

  const auto p = format_file_path({ path_str_in, path_str_in + path_size },
                                  { file_str_in, file_str_in + file_size },
                                  { nullptr, nullptr });

  return parse_file_location(p, strings);
}