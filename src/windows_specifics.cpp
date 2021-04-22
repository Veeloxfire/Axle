#include "windows_specifics.h"

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
    throw std::exception("Failed to create process");
  }
}