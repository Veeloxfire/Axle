#include "windows_specifics.h"
#include "os.h"
#include <processthreadsapi.h>

uint32_t Windows::run_exe(const char* name) {

  PROCESS_INFORMATION proc_info ={};
  ZeroMemory(&proc_info, sizeof(PROCESS_INFORMATION));

  STARTUPINFOA start_info ={};
  ZeroMemory(&start_info, sizeof(STARTUPINFOA));
  start_info.cb = sizeof(STARTUPINFOA);


  if (CreateProcessA(name, nullptr, nullptr, nullptr, false, 0, nullptr, nullptr, &start_info, &proc_info)) {
    WaitForSingleObject( proc_info.hProcess, INFINITE );
    
    DWORD ec = 0;
    if (GetExitCodeProcess(proc_info.hProcess, &ec)) {
      ec = 0;
    }

    CloseHandle( proc_info.hProcess );
    CloseHandle( proc_info.hThread );

    return ec;
  }
  else {
    INVALID_CODE_PATH("Failed to create process");
  }
}

Array<Windows::ActiveDll> Windows::load_dlls(Program* prog) {
  Array<ActiveDll> dlls = {};

  DllImportIterator import_iter = {prog->imports.ptr};

  for (DllImportFile file = next_import_file(prog, &import_iter);
       file.file_name != nullptr;
       file = next_import_file(prog, &import_iter)) {

    dlls.insert_uninit(1);
    ActiveDll* dll = dlls.back();
    dll->lib = LoadLibraryA(file.file_name);

    ASSERT(dll->lib != 0);

    for (DllImportFunction func = next_import(prog, &file);
         func.name != nullptr;
         func = next_import(prog, &file)) {
    
      auto proc_address = GetProcAddress(dll->lib, func.name);

      ASSERT(proc_address != nullptr);

      //Load the functions
      x64_to_bytes(proc_address, func.load_to);
    }
  }

  return dlls;
}

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