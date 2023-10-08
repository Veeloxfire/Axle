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

Windows::MAX_PATH_STR Windows::get_current_directory() {
  MAX_PATH_STR str = {};
  memset(str.str, 0, MAX_PATH + 1);

  GetCurrentDirectoryA(MAX_PATH + 1, str.str);

  return str;
}