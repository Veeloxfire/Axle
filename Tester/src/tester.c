#include <tester.h>

#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>

#define SHARED_MAX_SIZE 1024
static volatile unsigned char* shared_memory = NULL;
static HANDLE shared_map = NULL;

#define shared_memory_name "tester_shared_memmap"

//Need a main to make the static values initialize correctly
BOOL WINAPI DllMain(HINSTANCE _handle, DWORD fdwReason, LPVOID _reserved) {
  (void)_handle;
  (void)_reserved;

  if (fdwReason == DLL_PROCESS_ATTACH) {
    load_shared_memory();
  }

  return TRUE;
}

__declspec(dllexport) bool load_shared_memory(void) {
  if (shared_map != NULL) {
    return false;
  }

  shared_map = CreateFileMappingA(
    INVALID_HANDLE_VALUE,
    NULL,
    PAGE_READWRITE,
    0,
    SHARED_MAX_SIZE,
    shared_memory_name);

  if (shared_map == NULL) {
    return false;
  }

  bool already_exists = (GetLastError() == ERROR_ALREADY_EXISTS);

  shared_memory = MapViewOfFile(
    shared_map,
    FILE_MAP_WRITE,
    0, 0, 0);


  if (shared_memory == NULL) {
    return false;
  }

  if (already_exists) {
    for (size_t i = 0; i < SHARED_MAX_SIZE; ++i) {
      shared_memory[i] = 0;
    }
  }

  return true;
}

__declspec(dllexport) bool unload_shared_memory(void) {
  if (shared_memory == NULL) {
    return false;
  }
  UnmapViewOfFile((const void*)shared_memory);
  shared_memory = NULL;

  if (shared_map == NULL) {
    return false;
  }
  CloseHandle(shared_map);
  shared_map = NULL;

  return true;
}

__declspec(dllexport) void write_shared_memory(const void* buffer, size_t size) {
  size_t i = 0;
  const unsigned char* b = (const unsigned char*)buffer;
  volatile unsigned char* shared = (volatile unsigned char*)shared_memory;

  if (size > SHARED_MAX_SIZE - 8) {
    size = SHARED_MAX_SIZE - 8;//TODO: inform about this
  }

  size_t prev_size = 0;
  prev_size |= (size_t)shared[0];
  prev_size |= ((size_t)shared[1] << 8);
  prev_size |= ((size_t)shared[2] << 16);
  prev_size |= ((size_t)shared[3] << 24);
  prev_size |= ((size_t)shared[4] << 32);
  prev_size |= ((size_t)shared[5] << 40);
  prev_size |= ((size_t)shared[6] << 48);
  prev_size |= ((size_t)shared[7] << 56);

  shared[0] = size & 0xff;
  shared[1] = (size >> 8) & 0xff;
  shared[2] = (size >> 16) & 0xff;
  shared[3] = (size >> 24) & 0xff;
  shared[4] = (size >> 32) & 0xff;
  shared[5] = (size >> 40) & 0xff;
  shared[6] = (size >> 48) & 0xff;
  shared[7] = (size >> 56) & 0xff;

  shared += 8;

  for (; i < size; ++i) {
    shared[i] = b[i];
  }

  for (; i < prev_size; ++i) {
    shared[i] = 0;
  }
}

__declspec(dllexport) void get_shared_memory(SHARED_MEM* shared_out) {
  size_t saved_size = 0;

  volatile unsigned char* shared = (volatile unsigned char*)shared_memory;

  saved_size |=  (size_t)shared[0];
  saved_size |= ((size_t)shared[1] << 8);
  saved_size |= ((size_t)shared[2] << 16);
  saved_size |= ((size_t)shared[3] << 24);
  saved_size |= ((size_t)shared[4] << 32);
  saved_size |= ((size_t)shared[5] << 40);
  saved_size |= ((size_t)shared[6] << 48);
  saved_size |= ((size_t)shared[7] << 56);

  shared += 8;

  shared_out->size = saved_size;
  shared_out->buffer = shared;
}
