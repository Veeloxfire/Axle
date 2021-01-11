#pragma once
#include <stddef.h>
#include "utilityh"

#define PE_SIGNATURE {'P', 'E', '\0', '\0'}

using PE_Address = uint32_t;

struct COFF_Characteristics {
  enum Characteristics : uint16_t {
    RELOCS_STRIPPED = 0x0001,
    EXECUTABLE_IMAGE = 0x0002,
    LINE_NUMS_STRIPPED = 0x0004,
    LOCAL_SYMS_STRIPPED = 0x0008,
    AGGRESSIVE_WS_TRIM = 0x0010,
    LARGE_ADDRESS_AWARE = 0x0020,
    //0x0040 is reserved for some reason
    BYTES_REVERSED_LO = 0x0080,
    IS_32_BIT_MACHINE = 0x0100,
    DEBUG_STRIPPED = 0x0200,
    REMOVABLE_RUN_FROM_SWAP = 0x0400,
    NET_RUN_FROM_SWAP = 0x0800,
    SYSTEM = 0x1000,
    DLL = 0x2000,
    UP_SYSTEM_ONLY = 0x4000,
    BYTES_REVERSED_HI = 0x8000,
  } 

  uint16_t mask;

  void relocs_stripped(bool enabled);
  void executable_image(bool enabled);
  void line_nums_stripped(bool enabled);
  void local_syms_stripped(bool enabled);
  void aggressive_ws_trim(bool enabled);
  void large_address_aware(bool enabled);
  void byte_reversed_lo(bool enabled);
  void is_32_bit_machine(bool enabled);
  void debug_stripped(bool enabled);
  void removable_run_from_swap(bool enabled);
  void net_run_from_swap(bool enabled);
  void system(bool enabled);
  void dll(bool enabled);
  void up_system_only(bool enabled);
  void bytes_reversed_hi(bool enabled);

  bool relocs_stripped() const { return RELOCS_STRIPPED & mask != 0; }
  bool executable_image() const { return EXECUTABLE_IMAGE & mask != 0; }
  bool line_nums_stripped() const { return LINE_NUMS_STRIPPED & mask != 0; }
  bool local_syms_stripped() const { return LOCAL_SYMS_STRIPPED & mask != 0; }
  bool aggressive_ws_trim() const { return AGGRESSIVE_WS_TRIM & mask != 0; }
  bool large_address_aware() const { return LARGE_ADDRESS_AWARE & mask != 0; }
  bool byte_reversed_lo() const { return BYTE_REVERSE_LO & mask != 0; }
  bool is_32_bit_machine() const { return IS_32_BIT_MACHINE & mask != 0; }
  bool debug_stripped() const { return DEBUG_STRIPPED & mask != 0; }
  bool removable_run_from_swap() const { return REMOVABLE_RUN_FROM_SWAP & mask != 0; }
  bool net_run_from_swap() const { return NET_RUN_FROM_SWAP & mask != 0; }
  bool system() const { return SYSTEM & mask != 0; }
  bool dll() const { return DLL & mask != 0; }
  bool up_system_only() const { return UP_SYSTEM_ONLY & mask != 0; }
  bool bytes_reversed_hi() const { return BYTE_REVERSED_HI & mask != 0; }
};

struct COFF_file_header {
  uint16_t machine;
  uint16_t number_of_sections;
  uint8_t time_date_stamp[4];// crt time_t value - do this ourselves?
  PE_Address pointer_to_symbol_table;
  uint32_t number_of_symbols;
  uint32_t size_of_optional_header;
  COFF_Characteristics characteristics;
};

enum struct PE_MAGIC_NUMBER : uint16_t {
  PE32      = 0x10b,
  PE32_PLUS = 0x20b,
}

//Not really optional. Just called that because other file formats dont include it
struct PE32_optional_header {
  PE_MAGIC_NUMBER magic_number;
  
  uint8_t major_linker_version;// heh but this is a linker
  uint8_t minor_linker_version;// heh but this is a linker
  
  uint32_t size_of_code;
  uint32_t size_of_initialized_data;
  uint32_t size_of_uninitialized_data;
  PE_Address address_of_entry_point;
  
  PE_Address base_of_code;
  PE_Address base_of_data;// not in the PE32+ format  
};

struct PE32_windows_specific {
  PE_Address image_base;// 8 bytes in PE32+ format
  
  uint32_t section_alignment;
  uint32_t file_alignment;

  uint16_t major_os_version;
  uint16_t minor_os_version;

  uint16_t major_image_version;
  uint16_t minor_image_version;

  uint16_t major_sub_version;
  uint16_t minor_sub_version;

  uint32_t win32_version = 0x00000000;// reserved to be 0
  uint32_t size_of_header;
  uint32_t check_sum;
  uint16_t subsystem;
  DLL_Characteristics DLL_characteristics;
 
  uint32_t size_of_stack_reserve;// 8 bytes in PE32+ format
  uint32_t size_of_stack_commit;// 8 bytes in PE32+ format
  uint32_t size_of_heap_reserve;// 8 bytes in PE32+ format
  uint32_t size_of_heap_commit;// 8 bytes in PE32+ format
  
  uint32_t loader_flags = 0x00000000;// reserved to be 0
  uint32_t number_of_rva_and_sizes;
};

struct PE_File_Header {
  uint8_t signature[4] = PE_SIGNATURE;
  COFF_file_header coff;
  PE32_optional_header pe32;
  PE32_windows_specific pe32_windows;
};

struct Data_directory {
  PE_Address virtual_address;
  uint32_t size;
};

struct Optional_header_directories {
  Data_directory export_table;
  Data_directory import_table;
  Data_directory resource_table;
  Data_directory exception_table;
  Data_directory certificate_table;
  Data_directory base_relocation_table;
  Data_directory debug;
  Data_directory architecture;// reserved, apparently must be all be 0
  Data_directory global_ptr;
  Data_directory tls_table;
  Data_directory load_config_table;
  Data_directory bound_import;
  Data_directory import_address_table;
  Data_directory delay_import_descriptor;
  Data_directory clr_runtime_header;
  Data_directory reserved_space;// not entirely sure why but it has to be reserved
};

struct Section_Header {
  uint8_t name[8];// 8 byte null-padded ascii string OR '/' followed by ascii decimal offset into the string table
  uint32_t virtual_size;// size of section when loaded into memory, not the same as the size_of_raw_data
  PE_Address virtual_address;
  uint32_t size_of_raw_data;
  PE_Address pointer_to_raw_data;
  PE_Address pointer_to_relocations;
  PE_Address pointer_to_line_numbers;
  uint16_t number_of_relocations;
  uint16_t number_of_line_numbers;
  Section_Header_Characteristics characteristics;
};

struct Section {
  // The end of this will be padded to reach whatever the virtual size is (in section header)
  // i.e. length of array is not size of raw data!
  Array<uint8_t> bytes;
};

struct PE_File {
  PE_File_Header pe_file_header;

  //Sections and Section Headers should be ordered by offset
  Array<Section_Header> section_headers;
  Array<Section> sections;  
};

PE_File create_portable_executable();
PE_File_Header create_pe_file_header();
Section create_pe_section();
Section_Header create_pe_section_header();

void write_portable_executable(PE_File*);
