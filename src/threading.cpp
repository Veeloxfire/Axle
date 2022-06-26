#include "threading.h"
#include "safe_lib.h"

#include "windows_specifics.h"

bool SpinLockMutex::acquire_if_free() {
  return _InterlockedCompareExchange8(&held, '\1', '\0') == '\0';
}

void SpinLockMutex::acquire() {
  while (_InterlockedCompareExchange8(&held, '\1', '\0') != '\0') {}
}

void SpinLockMutex::release() {
  char res = _InterlockedCompareExchange8(&held, '\0', '\1');
  ASSERT(res == '\1');
}

void Signal::set() {
  _InterlockedCompareExchange8(&held, '\1', '\0');
}
void Signal::unset() {
  _InterlockedCompareExchange8(&held, '\0', '\1');
}

bool Signal::test() {
  return _InterlockedCompareExchange8(&held, '\0', '\0') == '\1';
}

struct ThreadingInfo {
  ThreadHandle* handle;
  void* data;
  THREAD_PROC proc;
};

struct ThreadHandle {
  DWORD id;
  HANDLE handle;
};

DWORD WINAPI generic_thread_proc(
  _In_ LPVOID lpParameter
) {
  ThreadingInfo* info = (ThreadingInfo*)lpParameter;

  info->proc(info->handle, info->data);

  free_destruct_single(info);
  return 0;
}

const ThreadHandle* start_thread(THREAD_PROC thread_proc, void* data) {
  ThreadHandle* handle = allocate_default<ThreadHandle>();
  ThreadingInfo* info = allocate_default<ThreadingInfo>();
  info->data = data;
  info->proc = thread_proc;

  handle->handle = CreateThread(NULL, 0, &generic_thread_proc, info, 0, &handle->id);
  return handle;
}

void wait_for_thread_end(const ThreadHandle* thread) {
  WaitForSingleObject(thread->handle, INFINITE);
  
  free_destruct_single(thread);
}