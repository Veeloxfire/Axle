#include "PE_file_format.h"
#include "files.h"
#include <stdio.h>

//// Sizes ////

static_assert(sizeof(COFF_Characteristics) == 2, "Must be 2 bytes");
//static_assert(sizeof(MS_DOS_Header) == 128, "Must be 128 bytes");
static_assert(sizeof(MS_DOS_Stub) == 128, "Must be 128 bytes");

//// Forward Declarations ////

static PE_File_Header create_pe_file_header();
//static MS_DOS create_default_ms_dos();
static COFF_file_header create_default_coff_header();
static PE32_optional_header create_default_pe32();
//static PE32_windows_specific create_default_pe32_windows_specific();

///// Definitions /////

ErrorCode write_portable_executable(const PE_File* pe_file, const char* file_name) {

  FILE* out = FILES::open(file_name, FILES::OPEN_MODE::WRITE, FILES::DATA_MODE::BINARY);

  if(out == NULL) return ErrorCode::COULD_NOT_CREATE_FILE;

  {
    //MS-DOS header and stub
    const MS_DOS& ms = pe_file->header.ms_dos;
    FILES::write(out, ms.header);
    FILES::write(out, ms.stub);
  }

  {
    const PE_File_Header& pe_head = pe_file->header;

    //COFF Header
    {
      constexpr size_t COFF_LENGTH = 24;
      uint8_t coff_header[COFF_LENGTH];

      const COFF_file_header& coff = pe_head.coff;

      load_to_bytes(coff_header, 0, coff.signature, 4);
      load_to_bytes(coff_header, 4, coff.machine);
      load_to_bytes(coff_header, 6, coff.number_of_sections);
      load_to_bytes(coff_header, 8, coff.time_date_stamp, 4);
      load_to_bytes(coff_header, 12, coff.pointer_to_symbol_table);
      load_to_bytes(coff_header, 16, coff.number_of_symbols);
      load_to_bytes(coff_header, 20, coff.size_of_optional_header);
      load_to_bytes(coff_header, 22, coff.characteristics);

      FILES::write(out, coff_header, COFF_LENGTH);
    }

    //PE32 Optional header
    {
      constexpr size_t OPT_LENGTH = 28;
      uint8_t opt_header[OPT_LENGTH];

      const PE32_optional_header& pe32 = pe_head.pe32;

      load_to_bytes(opt_header, 0, pe32.magic_number);
      load_to_bytes(opt_header, 2, pe32.major_linker_version);
      load_to_bytes(opt_header, 3, pe32.minor_linker_version);
      load_to_bytes(opt_header, 4, pe32.size_of_code);
      load_to_bytes(opt_header, 8, pe32.size_of_initialized_data);
      load_to_bytes(opt_header, 12, pe32.size_of_uninitialized_data);
      load_to_bytes(opt_header, 16, pe32.address_of_entry_point);
      load_to_bytes(opt_header, 20, pe32.base_of_code);
      load_to_bytes(opt_header, 24, pe32.base_of_data);

      FILES::write(out, opt_header, OPT_LENGTH);
    }

    //PE32 Optional Windows specific header
    {
      constexpr size_t OPT_WIN_LENGTH = 68;
      uint8_t win_opt_header[OPT_WIN_LENGTH];

      const PE32_windows_specific& pe32_win = pe_head.pe32_windows;

      load_to_bytes(win_opt_header, 0, pe32_win.image_base);
      load_to_bytes(win_opt_header, 4, pe32_win.section_alignment);
      load_to_bytes(win_opt_header, 8, pe32_win.file_alignment);
      load_to_bytes(win_opt_header, 12, pe32_win.major_os_version);
      load_to_bytes(win_opt_header, 14, pe32_win.minor_os_version);
      load_to_bytes(win_opt_header, 16, pe32_win.major_image_version);
      load_to_bytes(win_opt_header, 18, pe32_win.minor_image_version);
      load_to_bytes(win_opt_header, 20, pe32_win.major_sub_version);
      load_to_bytes(win_opt_header, 22, pe32_win.minor_sub_version);
      load_to_bytes(win_opt_header, 24, pe32_win.win32_version);
      load_to_bytes(win_opt_header, 28, pe32_win.size_of_image);
      load_to_bytes(win_opt_header, 32, pe32_win.size_of_header);
      load_to_bytes(win_opt_header, 36, pe32_win.check_sum);
      load_to_bytes(win_opt_header, 40, pe32_win.subsystem);
      load_to_bytes(win_opt_header, 42, pe32_win.DLL_characteristics);
      load_to_bytes(win_opt_header, 44, pe32_win.size_of_stack_reserve);
      load_to_bytes(win_opt_header, 48, pe32_win.size_of_stack_commit);
      load_to_bytes(win_opt_header, 52, pe32_win.size_of_heap_reserve);
      load_to_bytes(win_opt_header, 56, pe32_win.size_of_heap_commit);
      load_to_bytes(win_opt_header, 60, pe32_win.loader_flags);
      load_to_bytes(win_opt_header, 64, pe32_win.number_of_rva_and_sizes);

      FILES::write(out, win_opt_header, OPT_WIN_LENGTH);
    }

    //Optional Header Directories
    {
      constexpr size_t OPT_HEAD_DIR_LENGTH = 128;
      uint8_t opt_head_dir[OPT_HEAD_DIR_LENGTH];

      const Optional_header_directories& dir = pe_head.directories;

      load_to_bytes(opt_head_dir, 0  , dir.export_table);
      load_to_bytes(opt_head_dir, 8  , dir.import_table);
      load_to_bytes(opt_head_dir, 16 , dir.resource_table);
      load_to_bytes(opt_head_dir, 24 , dir.exception_table);
      load_to_bytes(opt_head_dir, 32 , dir.certificate_table);
      load_to_bytes(opt_head_dir, 40 , dir.base_relocation_table);
      load_to_bytes(opt_head_dir, 48 , dir.debug);
      load_to_bytes(opt_head_dir, 56 , dir.architecture);
      load_to_bytes(opt_head_dir, 64 , dir.global_ptr);
      load_to_bytes(opt_head_dir, 68 , dir.reserved1);
      load_to_bytes(opt_head_dir, 72 , dir.tls_table);
      load_to_bytes(opt_head_dir, 80 , dir.load_config_table);
      load_to_bytes(opt_head_dir, 88 , dir.bound_import);
      load_to_bytes(opt_head_dir, 96 , dir.import_address_table);
      load_to_bytes(opt_head_dir, 104, dir.delay_import_descriptor);
      load_to_bytes(opt_head_dir, 112, dir.clr_runtime_header);
      load_to_bytes(opt_head_dir, 120, dir.reserved2);

      FILES::write(out, opt_head_dir, OPT_HEAD_DIR_LENGTH);
    }
  }

  {
    //TODO: Section Headers
  }

  {
    //TODO: Sections
  }

  return FILES::close(out);
}

PE_File create_portable_executable() {
  PE_File pe;

  pe.header = create_pe_file_header();

  return pe;
}

PE_File_Header create_pe_file_header() {
  PE_File_Header header;

  //header.ms_dos     = defualt;
  header.coff         = create_default_coff_header();
  header.pe32         = create_default_pe32();
  //header.pe32_windows = create_default_pe32_windows_specifics();

  return header;
}

COFF_file_header create_default_coff_header() {
  COFF_file_header coff;

  //TODO Set Machine type
  coff.number_of_sections = 0x0000;
  //TODO Set Time
  coff.pointer_to_symbol_table = 0x00000000;
  coff.number_of_symbols = 0x00000000;
  coff.size_of_optional_header = 0x00000000;//TODO Set to a reasonable number
  //TODO Characteristics

  return coff;
}

PE32_optional_header create_default_pe32() {
  PE32_optional_header header;

  header.magic_number = MAGIC_NUMBER::PE32;
  header.major_linker_version = 0x00;//TODO versions??
  header.minor_linker_version = 0x00;//TODO versions??

  header.size_of_code = 0x00000000;
  header.size_of_initialized_data = 0x00000000;
  header.size_of_uninitialized_data = 0x00000000;

  header.address_of_entry_point = 0x00000000;
  header.base_of_code = 0x00000000;
  header.base_of_data = 0x00000000;

  return header;
}

PE32_windows_specific create_default_pe32_windows_specifics() {
  PE32_windows_specific header;

  header.image_base = 0x00000000;
  header.section_alignment = 0x00000000;
  header.file_alignment = 0x00000000;

  header.major_os_version = 0x0000;//TODO os versions??
  header.minor_os_version = 0x0000;//TODO os versions??

  header.major_image_version = 0x0000;//TODO image version??
  header.minor_image_version = 0x00000;//TODO image version??

  header.major_sub_version = 0x0000;//TODO sub version??
  header.minor_sub_version = 0x0000;//TODO sub version??

  header.size_of_header = 0x00000000;
  header.check_sum = 0x00000000;//TODO checksum???
  header.subsystem = 0x00000000;//TODO subsystem??
  //TODO DLL characteristics

  header.size_of_stack_reserve = 0;
  header.size_of_stack_commit = 0;
  header.size_of_heap_reserve = 0;
  header.size_of_heap_commit = 0;

  header.number_of_rva_and_sizes = 0x00000000;//TODO whatever this entails

  return header;
}
