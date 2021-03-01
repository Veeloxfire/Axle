#pragma once
#include <stddef.h>
#include "utility.h"

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
  };

  uint16_t mask;
};

enum struct MACHINE_TYPE : uint16_t {
  UNKNOWN = 0x0000,
};

struct COFF_file_header {
  uint8_t signature[4] = PE_SIGNATURE;
  MACHINE_TYPE machine = MACHINE_TYPE::UNKNOWN;
  uint16_t number_of_sections;
  uint8_t time_date_stamp[4];// crt time_t value - do this ourselves?
  PE_Address pointer_to_symbol_table;
  uint32_t number_of_symbols;
  uint16_t size_of_optional_header;
  COFF_Characteristics characteristics;
};

enum struct MAGIC_NUMBER : uint16_t {
  PE32      = 0x010b,
  PE32_PLUS = 0x020b,
  MZ        = 0x5a4d,
};

//Not really optional. Just called that because other file formats dont include it
struct PE32_optional_header {
  MAGIC_NUMBER magic_number;

  uint8_t major_linker_version;// heh but this is a linker
  uint8_t minor_linker_version;// heh but this is a linker

  uint32_t size_of_code;
  uint32_t size_of_initialized_data;
  uint32_t size_of_uninitialized_data;
  PE_Address address_of_entry_point;

  PE_Address base_of_code;
  PE_Address base_of_data;// not in the PE32+ format
};

//TODO: DLL Characteristics
struct DLL_Characteristics {};

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
  uint32_t size_of_image;
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

struct MS_DOS_Header {
  MAGIC_NUMBER magic = MAGIC_NUMBER::MZ;
  uint16_t extra_bytes = 0x0000;
  uint16_t pages = 0x0000;
  uint16_t num_relocations = 0x0000;
  uint16_t size_of_header = 0x0000;// in paragraphs
  uint16_t min_alloc = 0x0000;
  uint16_t max_alloc = 0x0000;
  uint16_t initial_ss = 0x0000;
  uint16_t initial_st = 0x0000;
  uint16_t checksum = 0x0000;
  uint16_t initial_ip = 0x0000;
  uint16_t initial_cs = 0x0000;
  uint16_t address_of_relocation_table = 0x0000;
  uint16_t overlay_number = 0x0000;
  uint16_t reserved_words[4] = {0,0,0,0};
  uint16_t oem_identifier = 0x0000;
  uint16_t oem_info = 0x0000;
  uint16_t reserved_2_electric_boogaloo[10] = {0,0,0,0,0,0,0,0,0,0};
  PE_Address actual_start_of_header;// actually needs to be calculated
};

struct MS_DOS_Stub {
  // Default is the windows stub which just says it cannot be run in MS-DOS
  uint8_t bytes[128] = { 0x0E, 0x1F, 0xBA, 0x0E, 0x00, 0xB4, 0x09, 0xCD,
                         0x21, 0xB8, 0x01, 0x4C, 0xCD, 0x21, 0x54, 0x68,
                         0x69, 0x73, 0x20, 0x70, 0x72, 0x6F, 0x67, 0x72,
                         0x61, 0x6D, 0x20, 0x63, 0x61, 0x6E, 0x6E, 0x6F,
                         0x74, 0x20, 0x62, 0x65, 0x20, 0x72, 0x75, 0x6E,
                         0x20, 0x69, 0x6E, 0x20, 0x44, 0x4F, 0x53, 0x20,
                         0x6D, 0x6F, 0x64, 0x65, 0x2E, 0x0D, 0x0D, 0x0A,
                         0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };
};

struct MS_DOS {
  MS_DOS_Header header;
  MS_DOS_Stub stub;
};

struct Data_directory {
  PE_Address virtual_address = 0x00000000;
  uint32_t size = 0x00000000;
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
  PE_Address global_ptr;
  uint32_t reserved1 = 0x00000000;
  Data_directory tls_table;
  Data_directory load_config_table;
  Data_directory bound_import;
  Data_directory import_address_table;
  Data_directory delay_import_descriptor;
  Data_directory clr_runtime_header;

  // not entirely sure why but it has to be reserved
  Data_directory reserved2;
};

struct PE_File_Header {
  MS_DOS ms_dos;// required by windows. Some programs even require it to be a specific length
  COFF_file_header coff;
  PE32_optional_header pe32;
  PE32_windows_specific pe32_windows;
  Optional_header_directories directories;
};

//TODO: Section Header Characteristics
struct Section_Header_Characteristics{};

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
  PE_File_Header header;

  //Sections and Section Headers should be ordered by offset
  Array<Section_Header> section_headers;
  Array<Section> sections;
};

PE_File        create_portable_executable();
//Section        create_pe_section();
//Section_Header create_pe_section_header();

ErrorCode write_portable_executable(const PE_File* pe_file, const char* file_name);
