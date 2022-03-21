#include "PE_file_format.h"
#include "files.h"
#include "compiler.h"
#include <time.h>

//// Sizes ////

static_assert(sizeof(COFF_Characteristics::Characteristics) == 2, "Must be 2 bytes");
static_assert(sizeof(COFF_file_header) == 20, "Must be 20 bytes");
static_assert(COFF_HEADER_SIZE == 20, "Must be 20 bytes");

static_assert(sizeof(MS_DOS_Header) == 64, "Must be 64 bytes");
static_assert(sizeof(MS_DOS_Stub) == 128, "Must be 128 bytes");
static_assert(MS_DOS_STUB_SIZE == 128, "Must be 128 bytes");

///// Definitions /////

RVA ConstantTable::get_constant_rva(ConstantOffset offset) const {
  const ConstantEntry* const entry = constants.data + offset.offset;

  return table_rva + entry->loaded_offset;
}

VA ConstantTable::get_constant_va(ConstantOffset offset) const {
  const ConstantEntry* const entry = constants.data + offset.offset;

  return table_va + entry->loaded_offset;
}


ConstantOffset ConstantTable::string_constant(const InternString* string) {
  const size_t size = constants.size;
  size_t i = 0;

  for (; i < size; i++) {
    const ConstantEntry* const entry = constants.data + i;
    if (entry->type == ConstantType::STRING && entry->string == string) {
      //Already exists
      return ConstantOffset{ i };
    }
  }

  constants.insert_uninit(1);

  ConstantEntry* const entry = constants.data + constants.size - 1;
  entry->type = ConstantType::STRING;
  entry->string = string;

  return ConstantOffset{ i };
}

Import* new_import(const InternString* dll_name, ImportTable* imports, ConstantTable* constants) {
  imports->imports.insert_uninit(1);

  Import* new_i = imports->imports.data + imports->imports.size - 1;
  new_i->DLL_name = constants->string_constant(dll_name);

  return new_i;
}

void add_name_to_import(Import* import_ptr, const InternString* name_to_import, VA estimated_va, ImportTable* table) {
  {
    auto i = import_ptr->imported_names.begin();
    const auto end = import_ptr->imported_names.end();

    for (; i < end; i++) {
      const ImportNameEntry* entry = table->import_names.data + *i;

      if (entry->name == name_to_import) {
        //Already imported
        return;
      }
    }
  }

  {
    size_t i = 0;
    const size_t end = table->import_names.size;

    for (; i < end; i++) {
      const ImportNameEntry* entry = table->import_names.data + i;

      if (entry->name == name_to_import) {
        //Same name already used
        import_ptr->imported_names.insert(i);
        return;
      }
    }
  }

  table->import_names.insert_uninit(1);
  auto* name = table->import_names.back();
  name->name = name_to_import;
  name->va = estimated_va;

  import_ptr->imported_names.insert(table->import_names.size - 1);
}

constexpr uint32_t FILE_ALIGNMENT = 512;

static uint32_t round_to_file_alignment(uint32_t v) {
  const uint32_t v_mod = v % FILE_ALIGNMENT;

  if (v_mod == 0) {
    return v;
  }
  else {
    return v + FILE_ALIGNMENT - (v % FILE_ALIGNMENT);
  }
}

static uint32_t round_to_section_alignment(uint32_t v) {
  const uint32_t v_mod = v % PAGE_SIZE;

  if (v_mod == 0) {
    return v;
  }
  else {
    return v + PAGE_SIZE - (v % PAGE_SIZE);
  }
}

static void constants_to_bytes(const RVA rva, const VA va, ConstantTable* const constants, Array<uint8_t>& bytes) {
  auto i = constants->constants.mut_begin();
  const auto end = constants->constants.mut_end();

  constants->table_rva = rva;
  constants->table_va  = va;

  uint32_t offset = 0;

  for (; i < end; i++) {
    i->loaded_offset = offset;

    switch (i->type) {
      /*case ConstantType::UTF8_STRING: {
          TempUTF8String utf8 = ascii_to_utf8(i->string.string);

          bytes.reserve_extra(utf8.size);
          memcpy_ts(bytes.data + bytes.size, bytes.capacity - bytes.size, utf8.bytes, utf8.size);
          bytes.size += utf8.size;

          offset += (uint32_t)utf8.size;
          break;
        }*/
      case ConstantType::STRING: {
          const size_t save = bytes.size;
          const char* str = i->string->string;
          while (str[0] != '\0') {
            bytes.insert(str[0]);
            str++;
          }

          bytes.insert('\0');

          offset += (uint32_t)(bytes.size - save);
          break;
        }
      case ConstantType::INTEGER: {
          bytes.reserve_extra(8);
          x64_to_bytes(i->integer, bytes.data + bytes.size);
          bytes.size += 8;

          offset += 8;
          break;
        }
    }
  }
}

static void import_names_to_bytes(VA va, Array<ImportNameEntry>& import_names, Array<uint8_t>& bytes) {
  auto i = import_names.mut_begin();
  const auto end = import_names.mut_end();

  for (; i < end; i++) {
    i->va = va;

    size_t saved_size = bytes.size;

    bytes.reserve_extra(2);
    bytes.data[bytes.size] = 0xFF;
    bytes.data[bytes.size + 1] = 0xFF;
    bytes.size += 2;

    const char* str = i->name->string;
    while (str[0] != '\0') {
      bytes.insert(str[0]);
      str++;
    }

    bytes.insert('\0');

    if (bytes.size % 2 != 0) {
      //Padding
      bytes.insert('\0');
    }

    va += (uint32_t)(bytes.size - saved_size);
  }
}

ErrorCode write_obj_to_file(const PE_File_Build* pe_file, const char* file_name) {

  if (pe_file->imports != nullptr) {
    ASSERT(pe_file->constants != nullptr);
  }

  uint32_t TIME = time(0) & 0x0000000000000000FFFFFFFFFFFFFFFF;

  ImportantValues important_vals ={};
  Array<uint8_t> constants ={};
  Array<uint8_t> import_names ={};

  {
    //Headers

    if (pe_file->code != nullptr) {
      important_vals.num_sections++;
    }

    if (pe_file->constants != nullptr) {
      important_vals.num_sections++;
    }

    if (pe_file->imports != nullptr) {
      important_vals.num_sections++;
    }

    important_vals.size_of_optional_header = OPTIONAL_STANDARD_FIELDS_SIZE + OPTIONAL_WINDOWS_FIELDS_SIZE
      + ALL_DATA_DIRECTORIES_SIZE;

    important_vals.size_of_header =
      BASE_HEADERS_SIZE
      + ALL_DATA_DIRECTORIES_SIZE
      + SINGLE_SECTION_HEADER_SIZE * important_vals.num_sections;

    //Sections
    uint32_t section_RVA = round_to_file_alignment(important_vals.size_of_header);
    VA section_VA = round_to_section_alignment(important_vals.size_of_header);

    important_vals.header_padding = section_RVA - important_vals.size_of_header;

    if (pe_file->code != nullptr) {
      important_vals.code_raw_size = (uint32_t)pe_file->code->size;
      important_vals.code_va = section_VA;
      important_vals.code_rva = section_RVA;

      section_RVA += round_to_file_alignment(important_vals.code_raw_size);
      section_VA += round_to_section_alignment(important_vals.code_raw_size);
    }

    if (pe_file->constants != nullptr) {
      constants_to_bytes(section_RVA, section_VA, pe_file->constants, constants);

      important_vals.constants_raw_size = (uint32_t)constants.size;
      important_vals.constants_va = section_VA;
      important_vals.constants_rva = section_RVA;

      section_RVA += round_to_file_alignment(important_vals.constants_raw_size);
      section_VA += round_to_section_alignment(important_vals.constants_raw_size);
    }

    if (pe_file->imports != nullptr) {
      important_vals.imports_va = section_VA;
      important_vals.imports_rva = section_RVA;

      //1 for null termination
      const uint32_t import_directory_table_size = (1ul + (uint32_t)pe_file->imports->imports.size) * IMPORT_DIRECTORY_SIZE;

      uint32_t cumulative_import_lookup_tables = 0;

      {
        auto i = pe_file->imports->imports.mut_begin();
        const auto end = pe_file->imports->imports.end();
        for (; i < end; i++) {
          uint32_t table_size =  8ul * (1ul + (uint32_t)i->imported_names.size);

          i->lookup_table1 = import_directory_table_size + cumulative_import_lookup_tables + section_VA;
          cumulative_import_lookup_tables += table_size;

          //Identical tables
          i->lookup_table2 = import_directory_table_size + cumulative_import_lookup_tables + section_VA;
          cumulative_import_lookup_tables += table_size;
        }
      }

      import_names_to_bytes(section_VA + import_directory_table_size + cumulative_import_lookup_tables,
                            pe_file->imports->import_names, import_names);


      important_vals.imports_raw_size = cumulative_import_lookup_tables + import_directory_table_size + (uint32_t)import_names.size;

      section_RVA += round_to_file_alignment(important_vals.imports_raw_size);
      section_VA += round_to_section_alignment(important_vals.imports_raw_size);
    }

    ASSERT(section_RVA % FILE_ALIGNMENT == 0);
    ASSERT(section_VA % PAGE_SIZE == 0);

    important_vals.size_of_image = section_VA;
  }

  FILES::OpenedFile file = FILES::open(file_name, FILES::OPEN_MODE::WRITE);

  if (file.error_code != ErrorCode::OK) return file.error_code;

  FileData* const out = file.file;

  {
    //MS-DOS header and stub
    MS_DOS ms ={};

    ms.header.actual_start_of_header = MS_DO_HEADER_SIZE + MS_DOS_STUB_SIZE;

    FILES::write_obj(out, ms.header);
    FILES::write_obj(out, ms.stub);
  }

  {
    constexpr uint8_t signature[SIGNATURE_SIZE] = PE_SIGNATURE;

    FILES::write(out, signature, SIGNATURE_SIZE);
  }

  //COFF Header
  {
    COFF_file_header coff ={};

    coff.machine = MACHINE_TYPE::AMD64;
    coff.number_of_sections = important_vals.num_sections;
    coff.time_date_stamp = TIME;
    coff.pointer_to_symbol_table = 0;
    coff.number_of_symbols = 0;
    coff.size_of_optional_header = important_vals.size_of_optional_header;
    coff.characteristics = COFF_Characteristics::EXECUTABLE_IMAGE | COFF_Characteristics::LARGE_ADDRESS_AWARE;

    FILES::write_obj(out, coff);
  }

  //PE32 Optional header
  {
    PE32Plus_optional_header pe32 ={};
    pe32.magic_number = MAGIC_NUMBER::PE32_PLUS;
    pe32.major_linker_version = 14;
    pe32.minor_linker_version = 30;
    pe32.size_of_code = round_to_file_alignment(important_vals.code_raw_size);

    pe32.size_of_initialized_data
      = round_to_file_alignment(important_vals.constants_raw_size)
      + round_to_file_alignment(important_vals.imports_raw_size);

    pe32.size_of_uninitialized_data = 0;
    if (pe_file->code == nullptr) {
      pe32.address_of_entry_point = 0;
    }
    else {
      pe32.address_of_entry_point = important_vals.code_va + (uint32_t)pe_file->code->entry_point;
    }
    pe32.base_of_code = important_vals.code_rva;

    FILES::write_obj(out, pe32);
  }

  //PE32 Optional Windows specific header
  {
    PE32Plus_windows_specific pe32_win ={};

    pe32_win.image_base = NT_IMAGE_BASE;
    pe32_win.section_alignment = PAGE_SIZE;
    pe32_win.file_alignment = FILE_ALIGNMENT;

    //not sure if these versions are correct ... 

    pe32_win.major_os_version = 6;
    pe32_win.minor_os_version = 0;

    pe32_win.major_image_version = 0;
    pe32_win.minor_image_version = 0;

    pe32_win.major_sub_version = 6;
    pe32_win.minor_sub_version = 0;

    pe32_win.size_of_image = important_vals.size_of_image;
    pe32_win.size_of_header = round_to_file_alignment(important_vals.size_of_header);
    pe32_win.subsystem = 3;

    pe32_win.DLL_characteristics = DLL_Characteristics::NX_COMPAT | DLL_Characteristics::TERMINAL_SERVER_AWARE;

    pe32_win.size_of_stack_reserve = 0x100000;
    pe32_win.size_of_stack_commit = 0x1000;

    pe32_win.size_of_heap_reserve = 0x100000;
    pe32_win.size_of_heap_commit = 0x1000;

    pe32_win.number_of_rva_and_sizes = ALL_DATA_DIRECTORIES_NUM;

    FILES::write_obj(out, pe32_win);
  }

  //Optional Header Directories
  {
    Image_header_directories data_directories ={};

    if (pe_file->imports != nullptr) {
      data_directories.import_table.virtual_address = important_vals.imports_va;
      data_directories.import_table.size = important_vals.imports_raw_size;
    }

    FILES::write_obj(out, data_directories);
  }


  ///// Section headers right after main header /////

  if (pe_file->code != nullptr) {
    Section_Header code_header ={};

    //utf-8 ".text"
    code_header.name[0] = 0x2E;
    code_header.name[1] = 0x74;
    code_header.name[2] = 0x65;
    code_header.name[3] = 0x78;
    code_header.name[4] = 0x74;
    code_header.name[5] = 0x00;
    code_header.name[6] = 0x00;
    code_header.name[7] = 0x00;

    code_header.virtual_size = important_vals.code_raw_size;
    code_header.virtual_address = important_vals.code_va;
    code_header.size_of_raw_data = round_to_file_alignment(important_vals.code_raw_size);
    code_header.pointer_to_raw_data = important_vals.code_rva;

    //All irrelevant in this
    code_header.pointer_to_relocations = 0x0;
    code_header.pointer_to_line_numbers = 0x0;
    code_header.number_of_relocations = 0x0;
    code_header.number_of_line_numbers = 0x0;

    code_header.characteristics = Section_Flags::CONTAINS_CODE | Section_Flags::EXECTUTABLE | Section_Flags::READABLE;

    FILES::write_obj(out, code_header);
  }

  if (pe_file->constants != nullptr) {
    Section_Header constants_header ={};

    //utf-8 ".rdata"
    constants_header.name[0] = 0x2E;
    constants_header.name[1] = 0x72;
    constants_header.name[2] = 0x64;
    constants_header.name[3] = 0x61;
    constants_header.name[4] = 0x74;
    constants_header.name[5] = 0x61;
    constants_header.name[6] = 0x00;
    constants_header.name[7] = 0x00;

    constants_header.virtual_size = important_vals.constants_raw_size;
    constants_header.virtual_address = important_vals.constants_va;
    constants_header.size_of_raw_data = round_to_file_alignment(important_vals.constants_raw_size);
    constants_header.pointer_to_raw_data = important_vals.constants_rva;

    //All irrelevant in this
    constants_header.pointer_to_relocations = 0x0;
    constants_header.pointer_to_line_numbers = 0x0;
    constants_header.number_of_relocations = 0x0;
    constants_header.number_of_line_numbers = 0x0;

    constants_header.characteristics = Section_Flags::READABLE | Section_Flags::CONTAINS_INITIALIZED;

    FILES::write_obj(out, constants_header);
  }

  if (pe_file->imports != nullptr) {
    Section_Header imports_header ={};

    imports_header.name[0] = 0x2E;//.
    imports_header.name[1] = 0x69;//i
    imports_header.name[2] = 0x64;//d
    imports_header.name[3] = 0x61;//a
    imports_header.name[4] = 0x74;//t
    imports_header.name[5] = 0x61;//a
    imports_header.name[6] = 0x00;
    imports_header.name[7] = 0x00;

    imports_header.virtual_size = important_vals.imports_raw_size;
    imports_header.virtual_address = important_vals.imports_va;
    imports_header.size_of_raw_data = round_to_file_alignment(important_vals.imports_raw_size);
    imports_header.pointer_to_raw_data = important_vals.imports_rva;

    //All irrelevant in this
    imports_header.pointer_to_relocations = 0x0;
    imports_header.pointer_to_line_numbers = 0x0;
    imports_header.number_of_relocations = 0x0;
    imports_header.number_of_line_numbers = 0x0;

    imports_header.characteristics = Section_Flags::READABLE | Section_Flags::CONTAINS_INITIALIZED;

    FILES::write_obj(out, imports_header);
  }

  FILES::write_padding_bytes(out, '\0', important_vals.header_padding);

  ///// SECTIONS /////

  if (pe_file->code != nullptr) {
    FILES::write_aligned_array(out, pe_file->code->bytes, pe_file->code->size, FILE_ALIGNMENT);
  }

  if (pe_file->constants != nullptr) {
    FILES::write_aligned_array(out, constants, FILE_ALIGNMENT);
  }

  if (pe_file->imports != nullptr) {
    //Data directories
    {
      auto i = pe_file->imports->imports.begin();
      const auto end = pe_file->imports->imports.end();

      for (; i < end; i++) {
        ImportDataDirectory data_directory ={};

        data_directory.import_lookup_table = i->lookup_table1;
        data_directory.date_time_stamp = 0;
        data_directory.forwarder_chain = 0;
        data_directory.name_rva = pe_file->constants->get_constant_va(i->DLL_name);
        data_directory.import_address_table = i->lookup_table2;

        FILES::write_obj(out, data_directory);
      }

      //Null terminated
      FILES::write_padding_bytes(out, '\0', IMPORT_DIRECTORY_SIZE);
    }

    //Entry tables
    {
      auto i = pe_file->imports->imports.begin();
      const auto end = pe_file->imports->imports.end();

      const ImportNameEntry* entries = pe_file->imports->import_names.data;

      for (; i < end; i++) {
        for (size_t itr = 0; itr < 2; itr++) {
          //Write it out twice
          for (auto imp : i->imported_names) {
            uint64_t entry = 0;
            //31 bits for some reason
            entry |= (entries[imp].va & 0x7fffffff);
            FILES::write_obj(out, entry);
          }
          FILES::write_padding_bytes(out, '\0', 8);
        }
      }
    }

    FILES::write(out, import_names.data, import_names.size);

    size_t padding = (size_t)round_to_file_alignment(important_vals.imports_raw_size) - (size_t)important_vals.imports_raw_size;
    FILES::write_padding_bytes(out, '\0', padding);
  }

  return FILES::close(out);
}

struct RVA_Resolver {
  int32_t ptr_base = 0;
  size_t va_base = 0;

  int32_t load_rva_ptr(RVA rva) const {
    return (int32_t)(rva - va_base) + ptr_base;
  }
};

static RVA_Resolver get_rva_section_resolver(PEFile* pe_file, RVA rva) {
  auto i = pe_file->section_headers.begin();
  auto end = pe_file->section_headers.end();
  for (; i < end; i++) {
    size_t end_of_section = i->virtual_address + (uint64_t)i->virtual_size;
    if (i->virtual_address <= rva && rva < end_of_section) {
      RVA_Resolver res ={};
      res.ptr_base = i->pointer_to_raw_data;
      res.va_base = i->virtual_address;
      return res;
    }
  }

  return {};
}

void load_portable_executable_from_file(Compiler* const comp,
                                        const Span& span,
                                        PEFile* pe_file,
                                        const char* file_name) {

  FILES::OpenedFile file = FILES::open(file_name, FILES::OPEN_MODE::READ);

  if (file.error_code != ErrorCode::OK) {
    comp->report_error(ERROR_CODE::FILE_ERROR, span,
                       "Could not open file '{}'\n"
                       "Perhaps it does not exist",
                       file_name);
    return;
  }

  //Close file if it was opened
  DEFER(&) { FILES::close(file.file); };

  FILES::read(file.file, &pe_file->header.ms_dos, 1);

  if (pe_file->header.ms_dos.magic != MAGIC_NUMBER::MZ) {
    comp->report_error(ERROR_CODE::FILE_ERROR, span,
                       "File '{}' did not have correct type\n"
                       "Magic numbers did not match\n"
                       "Expected '{}'. Found '{}'",
                       file_name, MagicNumber{ MAGIC_NUMBER::MZ },
                       MagicNumber{ pe_file->header.ms_dos.magic });
    return;
  }

  FILES::seek_from_start(file.file, pe_file->header.ms_dos.actual_start_of_header);
  FILES::read(file.file, (uint8_t*)pe_file->header.signature, SIGNATURE_SIZE);

  {
    char expected_sig[SIGNATURE_SIZE] = PE_SIGNATURE;
    static_assert(SIGNATURE_SIZE == 4, "Must be 4");
    char* sig = (char*)pe_file->header.signature;

    if (memcmp_ts(expected_sig, sig, 4) != 0) {
      comp->report_error(ERROR_CODE::FILE_ERROR, span,
                         "File '{}' did not have correct signature\n"
                         "Expected: {} {} {} {}. Found: {} {} {} {}",
                         file_name,
                         DisplayChar{ expected_sig[0] }, DisplayChar{ expected_sig[1] },
                         DisplayChar{ expected_sig[2] }, DisplayChar{ expected_sig[3] },
                         DisplayChar{ sig[0] }, DisplayChar{ sig[1] },
                         DisplayChar{ sig[2] }, DisplayChar{ sig[3] });
      return;
    }
  }

  FILES::read(file.file, &pe_file->header.coff, 1);
  const size_t opt_header_pos = FILES::get_current_pos(file.file);
  FILES::read(file.file, &pe_file->header.pe32, 1);

  if (pe_file->header.pe32.magic_number != MAGIC_NUMBER::PE32_PLUS) {
    comp->report_error(ERROR_CODE::FILE_ERROR, span,
                       "File '{}' did not have correct type\n"
                       "2nd magic numbers did not match\n"
                       "Expected '{}'. Found '{}'",
                       file_name, (uint16_t)MAGIC_NUMBER::PE32_PLUS,
                       (uint16_t)pe_file->header.pe32.magic_number);
    return;
  }

  FILES::read(file.file, &pe_file->header.pe32_windows, 1);

  {
    const PE32Plus_windows_specific& win = pe_file->header.pe32_windows;

    if (win.win32_version != 0 || win.loader_flags != 0) {
      comp->report_error(ERROR_CODE::FILE_ERROR, span,
                         "Parts of file '{}' that are reserved as 0 were not 0\n"
                         "This is probably an internal reading error",
                         file_name);
      return;
    }
  }

  const size_t num_dirs = pe_file->header.pe32_windows.number_of_rva_and_sizes;
  FILES::read<Data_directory>(file.file, (Data_directory*)&pe_file->header.directories,
                              num_dirs);

  {
    size_t currpos = FILES::get_current_pos(file.file);
    size_t expected = pe_file->header.coff.size_of_optional_header + opt_header_pos;

    if (currpos != expected) {
      comp->report_error(ERROR_CODE::FILE_ERROR, span,
                         "Size mismatch in header\n"
                         "Expected optional header position '{}'. Found '{}'",
                         expected, currpos);
      return;
    }
  }

  //Load Sections
  {
    const size_t num_sections = pe_file->header.coff.number_of_sections;

    pe_file->section_headers.insert_uninit(num_sections);
    Section_Header* headers = pe_file->section_headers.data;

    FILES::read(file.file, headers, num_sections);
  }

  //Load exports
  if (num_dirs > 1 && pe_file->header.directories.export_table.virtual_address > 0) {
    //There is an export table
    const Data_directory& export_table = pe_file->header.directories.export_table;
    RVA_Resolver resolver = get_rva_section_resolver(pe_file, export_table.virtual_address);


    //Seek to export table
    FILES::seek_from_start(file.file, resolver.load_rva_ptr(export_table.virtual_address));

    //Load directory table
    FILES::read(file.file, &pe_file->export_table.directory_table, 1);

    const ExportDirectoryTable& directory_table = pe_file->export_table.directory_table;
    if (directory_table.export_flags != 0) {
      comp->report_error(ERROR_CODE::FILE_ERROR, span,
                         "Export table export flags should be '0'. Found '{}'\n"
                         "This is probably an internal error",
                         directory_table.export_flags);
      return;
    }


    //Load all exports
    FILES::seek_from_start(file.file, resolver.load_rva_ptr(directory_table.export_table_address));

    const size_t num_exports = directory_table.num_export_entries;
    pe_file->export_table.address_table.insert_uninit(num_exports);

    FILES::read(file.file, pe_file->export_table.address_table.data, num_exports);

    //Load name pointers
    FILES::seek_from_start(file.file, resolver.load_rva_ptr(directory_table.name_pointer_table_address));


    const size_t num_ordinals = directory_table.num_name_pointers;

    //Load name rvas
    Array<RVA> name_rvas ={};
    name_rvas.insert_uninit(num_ordinals);
    //Seek to names
    FILES::seek_from_start(file.file, resolver.load_rva_ptr(directory_table.name_pointer_table_address));
    //Read names
    FILES::read(file.file, name_rvas.data, num_ordinals);

    //Load ordinals
    Array<uint16_t> ordinals ={};
    ordinals.insert_uninit(num_ordinals);
    //Seek to ordinals
    FILES::seek_from_start(file.file, resolver.load_rva_ptr(directory_table.ordinal_table_address));
    //Read ordinals
    FILES::read(file.file, ordinals.data, num_ordinals);

    //Load the names and ordinals together
    {
      pe_file->export_table.element_table.reserve_extra(num_ordinals);

      Array<char> name_holder ={};

      for (size_t i = 0; i < num_ordinals; i++) {
        pe_file->export_table.element_table.insert_uninit(1);
        ExportElement* ptr = pe_file->export_table.element_table.back();

        RVA name = name_rvas.data[i];
        uint16_t ordinal = ordinals.data[i];

        ptr->ordinal = ordinal;
        if (name == 0) {
          //No name
          ptr->str = nullptr;
          continue;
        }

        //These is a name we can load!

        //Start of the string
        FILES::seek_from_start(file.file, resolver.load_rva_ptr(name));

        //load each character
        char c = FILES::read_byte(file.file);
        while (c != '\0') {
          name_holder.insert(c);
          c = FILES::read_byte(file.file);
        }

        name_holder.insert('\0');

        ptr->str = comp->services.strings->intern(name_holder.data);
        pe_file->export_table.names.insert(ptr->str);

        name_holder.clear();
      }

    }
  }
}