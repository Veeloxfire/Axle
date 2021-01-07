#pragma once
#include <stddef.h>

#define PE_SIGNATURE = {'P', 'E', '\0', '\0'}


struct COFF_file_header {
  uint8_t machine[2];
  uint8_t number_of_sections[2];
  uint8_t time_date_stamp[4];
  uint8_t pointer_to_symbol_table[4];
  uint8_t number_of_symbols[4];
  uint8_t size_of_optional_header[2];
  uint8_t characteristics[2];
};

//Not really optional. Just called that because other file formats dont include it
struct PE32_optional_header {
  uint8_t magic_number[2];
  
  uint8_t major_linker_version;// heh but this is a linker
  uint8_t minor_linker_version;// heh but this is a linker
  
  uint8_t size_of_code[4];// code/.text section
  uint8_t size_of_initialized_data[4];
  uint8_t size_of_uninitialized_data[4];
  uint8_t address_of_entry_point[4];
  
  uint8_t base_of_code[4];
  uint8_t base_of_data[4];// not in the PE32+ format  
};

struct PE32_windows_specific {
  uint8_t image_base[4];// 8 bytes in PE32+ format
  
  uint8_t section_alignment[4];
  uint8_t file_alignment[4];

  uint8_t major_os_version[2];
  uint8_t minor_os_version[2];

  uint8_t major_image_version[2];
  uint8_t minor_image_version[2];

  uint8_t major_sub_version[2];
  uint8_t minor_sub_version[2];

  uint8_t win32_version[4];
  uint8_t size_of_header[4];
  uint8_t check_sum[4];
  uint8_t subsystem[2];
  uint8_t DLL_characteristics[2];
 
  uint8_t size_of_stack_reserve[4];// 8 bytes in PE32+ format
  uint8_t size_of_stack_commit[4];// 8 bytes in PE32+ format
  uint8_t size_of_heap_reserve[4];// 8 bytes in PE32+ format
  uint8_t size_of_heap_commit[4];// 8 bytes in PE32+ format
  
  uint8_t loader_flags[4];
  uint8_t number_of_rva_and_sizes[4];
};

struct PE_file_header {
  uint8_t signature[4] = PE_SIGNATURE;
  COFF_file_header coff;
  PE32_optional_header pe32;
  PE32_windows_specific pe32_windows;
};

struct PE_File {
  PE_file_header header;
}

