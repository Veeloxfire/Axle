#include "windows_specifics.h"
#include "os.h"
#include <processthreadsapi.h>

u8* virtual_alloc(usize bytes) {
  return (u8*)VirtualAlloc(0, bytes, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
}
void virtual_free(u8* bytes) {
  VirtualFree(bytes, 0, MEM_RELEASE);
}

usize get_page_size() {
  SYSTEM_INFO info ={};
  GetSystemInfo(&info);

  return (usize)info.dwPageSize;
}

Windows::NativePath Windows::get_current_directory() {
  NativePath str = {};
  memset(str.path, 0, MAX_PATH + 1);

  GetCurrentDirectoryA(MAX_PATH + 1, str.path);

  return str;
}

void Windows::set_current_directory(const ViewArr<const char>& path) {
  NativePath str = path;

  SetCurrentDirectoryA(str.c_str());
}