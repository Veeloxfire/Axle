#pragma once

#ifdef __cplusplus
extern "C" {
#else
#define bool _Bool
#define true 1
#define false 0
#endif

typedef struct {
  const volatile unsigned char* buffer;
  size_t size;
} SHARED_MEM;

  __declspec(dllexport) bool load_shared_memory();
  __declspec(dllexport) bool unload_shared_memory();
  __declspec(dllexport) void write_shared_memory(const void* buffer, size_t size);
  __declspec(dllexport) void get_shared_memory(SHARED_MEM* shared);

#ifdef __cplusplus
};
#endif