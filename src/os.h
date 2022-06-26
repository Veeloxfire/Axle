#pragma once


#ifdef _WIN64
#define OS_WINDOWS
u8* virtual_alloc(usize bytes);
void virtual_free(u8* bytes);

usize get_page_size();

#else
#error Probably one of these should be defined
#endif