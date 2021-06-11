#pragma once
#include <stddef.h>
#include "utility.h"
#include "strings.h"

#define PE_SIGNATURE {'P', 'E', '\0', '\0'}
#define DOS_STUB \
{ 0x0E, 0x1F, 0xBA, 0x0E, 0x00, 0xB4, 0x09, 0xCD,\
  0x21, 0xB8, 0x01, 0x4C, 0xCD, 0x21, 0x54, 0x68,\
  0x69, 0x73, 0x20, 0x70, 0x72, 0x6F, 0x67, 0x72,\
  0x61, 0x6D, 0x20, 0x63, 0x61, 0x6E, 0x6E, 0x6F,\
  0x74, 0x20, 0x62, 0x65, 0x20, 0x72, 0x75, 0x6E,\
  0x20, 0x69, 0x6E, 0x20, 0x44, 0x4F, 0x53, 0x20,\
  0x6D, 0x6F, 0x64, 0x65, 0x2E, 0x0D, 0x0D, 0x0A,\
  0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 }

constexpr inline uint32_t PAGE_SIZE = 4096;
constexpr inline uint32_t MS_DO_HEADER_SIZE = 64;
constexpr inline uint32_t MS_DOS_STUB_SIZE = 128;
constexpr inline uint32_t COFF_HEADER_SIZE = 20;
constexpr inline uint32_t SIGNATURE_SIZE = 4;
constexpr inline uint32_t OPTIONAL_STANDARD_FIELDS_SIZE = 24;
constexpr inline uint32_t OPTIONAL_WINDOWS_FIELDS_SIZE = 88;
constexpr inline uint32_t ALL_DATA_DIRECTORIES_NUM = 16;
constexpr inline uint32_t DATA_DIRECTORY_SIZE = 8;
constexpr inline uint32_t ALL_DATA_DIRECTORIES_SIZE = ALL_DATA_DIRECTORIES_NUM * DATA_DIRECTORY_SIZE;

constexpr inline uint32_t BASE_HEADERS_SIZE =
MS_DO_HEADER_SIZE
+ MS_DOS_STUB_SIZE
+ SIGNATURE_SIZE
+ COFF_HEADER_SIZE
+ OPTIONAL_STANDARD_FIELDS_SIZE
+ OPTIONAL_WINDOWS_FIELDS_SIZE;

constexpr inline uint32_t SINGLE_SECTION_HEADER_SIZE = 40;
constexpr inline uint32_t NT_IMAGE_BASE = 0x00400000;
constexpr inline uint32_t IMPORT_DIRECTORY_SIZE = 20;


using PE_Address = uint32_t;
using RVA = uint32_t;
using VA = uint32_t;
using FileOffset = uint32_t;

namespace COFF_Characteristics {
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
};

enum struct MACHINE_TYPE : uint16_t {
  UNKNOWN = 0x0000,
  AMD64   = 0x8664,
};

struct COFF_file_header {
  MACHINE_TYPE machine = MACHINE_TYPE::UNKNOWN;
  uint16_t number_of_sections;
  uint32_t time_date_stamp;
  PE_Address pointer_to_symbol_table;
  uint32_t number_of_symbols;
  uint16_t size_of_optional_header;
  uint16_t characteristics;
};

enum struct MAGIC_NUMBER : uint16_t {
  PE32      = 0x010b,
  PE32_PLUS = 0x020b,
  MZ        = 0x5a4d,
};

//Not really optional. Just called that because other file formats dont include it
struct PE32Plus_optional_header {
  MAGIC_NUMBER magic_number;

  uint8_t major_linker_version;// heh but this is a linker
  uint8_t minor_linker_version;// heh but this is a linker

  uint32_t size_of_code;
  uint32_t size_of_initialized_data;
  uint32_t size_of_uninitialized_data;
  PE_Address address_of_entry_point;

  PE_Address base_of_code;
};

//TODO: DLL Characteristics
struct DLL_Characteristics {};

struct PE32Plus_windows_specific {
  uint64_t image_base;// 8 bytes in PE32+ format

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
  uint32_t check_sum = 0;

  uint16_t subsystem;//TODO: More subsystems
  uint16_t DLL_characteristics;

  uint64_t size_of_stack_reserve;// 8 bytes in PE32+ format
  uint64_t size_of_stack_commit;// 8 bytes in PE32+ format
  uint64_t size_of_heap_reserve;// 8 bytes in PE32+ format
  uint64_t size_of_heap_commit;// 8 bytes in PE32+ format

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
  uint16_t reserved_words[4] ={ 0,0,0,0 };
  uint16_t oem_identifier = 0x0000;
  uint16_t oem_info = 0x0000;
  uint16_t reserved_2_electric_boogaloo[10] ={ 0,0,0,0,0,0,0,0,0,0 };
  PE_Address actual_start_of_header;
};

struct MS_DOS_Stub {
  // Default is the stub which just says it cannot be run in MS-DOS
  uint8_t bytes[MS_DOS_STUB_SIZE] = DOS_STUB;
};

struct MS_DOS {
  MS_DOS_Header header;
  MS_DOS_Stub stub;
};

struct Data_directory {
  RVA virtual_address = 0x00000000;
  uint32_t size = 0x00000000;
};

//Not all emited
struct Image_header_directories {
  Data_directory export_table ={};
  Data_directory import_table ={};
  Data_directory resource_table ={};
  Data_directory exception_table ={};
  Data_directory certificate_table ={};
  Data_directory base_relocation_table ={};
  Data_directory debug ={};
  uint64_t architecture = 0x0;// reserved, apparently must be all be 0
  PE_Address global_ptr = 0x0;
  uint32_t reserved1 = 0x0;
  Data_directory tls_table ={};
  Data_directory load_config_table ={};
  Data_directory bound_import ={};
  Data_directory import_address_table ={};
  Data_directory delay_import_descriptor ={};
  Data_directory clr_runtime_header ={};
  uint64_t reserved2 = 0x0;// not entirely sure why but it has to be reserved
};

struct PE_File_Header {
  MS_DOS_Header ms_dos;//does not include the stub
  uint8_t signature[SIGNATURE_SIZE] = PE_SIGNATURE;
  COFF_file_header coff;
  PE32Plus_optional_header pe32;
  PE32Plus_windows_specific pe32_windows;
  Image_header_directories directories;
};

namespace Section_Flags {
  enum FLAGS : uint32_t {
    CONTAINS_CODE = 0x00000020,
    CONTAINS_INITIALIZED = 0x00000040,
    EXECTUTABLE = 0x20000000,
    READABLE = 0x40000000,
    WRITABLE = 0x80000000,
  };
}

struct Section_Header {
  uint8_t name[8];// 8 byte null-padded utf8 string (if there is exactly 8 bytes then the null is ignored)
  uint32_t virtual_size;// size of section when loaded into memory, not the same as the size_of_raw_data
  RVA virtual_address;
  uint32_t size_of_raw_data;
  uint32_t pointer_to_raw_data;
  uint32_t pointer_to_relocations;
  uint32_t pointer_to_line_numbers;
  uint16_t number_of_relocations = 0x0;
  uint16_t number_of_line_numbers = 0x0;
  uint32_t characteristics;
};

struct ExportDirectoryTable {
  uint32_t export_flags = 0;
  uint32_t time_and_date;
  uint16_t major_version;
  uint16_t minor_version;
  RVA name_rva;
  uint32_t ordinal_base;
  uint32_t num_export_entries;
  uint32_t num_name_pointers;
  RVA export_table_address;
  RVA name_pointer_table_address;
  RVA ordinal_table_address;
};

union ExportAddress {
  RVA export_rva;
  const InternString* forwarder_rva;
};

struct ExportElement {
  const InternString* str;
  uint16_t ordinal;
};

struct ExportTable {
  ExportDirectoryTable directory_table ={};
  Array<ExportAddress> address_table ={};
  Array<ExportElement> element_table ={};
};

struct PEFile {
  PE_File_Header header;
  Array<Section_Header> section_headers;

  ExportTable export_table ={};
};

struct ImportDataDirectory {
  RVA import_lookup_table;
  uint32_t date_time_stamp;
  uint32_t forwarder_chain;
  RVA name_rva;
  RVA import_address_table;
};

struct CodeSection {
  const uint8_t* bytes = nullptr;
  size_t size = 0;
  size_t entry_point = 0;
};

enum struct ConstantType : uint8_t {
  //UTF8_STRING,
  STRING,
  INTEGER,
};

struct ConstantEntry {
  ConstantType type;
  union {
    const InternString* string;
    uint64_t integer;
  };

  uint32_t loaded_offset = 0;
};

struct ConstantOffset {
  size_t offset;
};

struct ConstantTable {
  Array<ConstantEntry> constants;
  RVA table_rva;
  VA table_va;

  ConstantOffset string_constant(const InternString*);
  RVA get_constant_rva(ConstantOffset offset) const;
  VA get_constant_va(ConstantOffset offset) const;
};

struct ImportNameEntry {
  const InternString* name;
  VA va;
};

struct Import {
  ConstantOffset DLL_name;
  VA lookup_table1;
  VA lookup_table2;

  Array<size_t> imported_names;
};

struct ImportTable {
  Array<ImportNameEntry> import_names;

  Array<Import> imports;
};

Import* new_import(const InternString* dll_name, ImportTable* imports, ConstantTable* constants);
void add_name_to_import(Import* import_ptr, const InternString* name_to_import, VA estimated_va, ImportTable* table);


struct PE_File_Build {
  CodeSection* code = nullptr;
  ConstantTable* constants = nullptr;
  ImportTable* imports = nullptr;
};

struct ImportantValues {
  uint32_t num_sections;

  uint32_t size_of_optional_header;
  uint32_t header_padding;
  uint32_t size_of_header;

  uint32_t size_of_image;

  RVA code_rva;
  VA code_va;
  uint32_t code_raw_size;

  RVA constants_rva;
  VA constants_va;
  uint32_t constants_raw_size;

  RVA imports_rva;
  VA imports_va;
  uint32_t imports_raw_size;
};

struct Compiler;
struct ImportedDll;
struct Span;

ErrorCode write_portable_executable_to_file(const PE_File_Build* pe_file, const char* file_name);

void load_portable_executable_from_file(Compiler* const comp,
                                        const Span& span,
                                        PEFile* pe_file,
                                        const char* file_name);

void load_portable_executable_exports(Compiler* const comp,
                                      ImportedDll* dll,
                                      const Span& span,
                                      const char* file_name);