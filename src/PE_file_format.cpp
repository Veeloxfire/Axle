#include "PE_file_format.h"
#include "files.h"
#include <stdio.h>
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


ConstantOffset ConstantTable::string_constant(InternString string) {
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

Import* new_import(InternString dll_name, ImportTable* imports, ConstantTable* constants) {
  imports->imports.insert_uninit(1);

  Import* new_i = imports->imports.data + imports->imports.size - 1;
  new_i->DLL_name = constants->string_constant(dll_name);

  return new_i;
}

void add_name_to_import(Import* import_ptr, InternString name_to_import, ImportTable* table) {
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
  table->import_names.back()->name = name_to_import;

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
      case ConstantType::UTF8_STRING: {
          TempUTF8String utf8 = ascii_to_utf8(i->string.string);

          bytes.reserve_extra(utf8.size);
          memcpy_ts(bytes.data + bytes.size, bytes.capacity - bytes.size, utf8.bytes, utf8.size);
          bytes.size += utf8.size;

          offset += (uint32_t)utf8.size;
          break;
        }
      case ConstantType::STRING: {
          const size_t save = bytes.size;
          const char* str = i->string.string;
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

    const char* str = i->name.string;
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

ErrorCode write_portable_executable_to_file(const PE_File* pe_file, const char* file_name) {

  if (pe_file->imports != nullptr) {
    assert(pe_file->constants != nullptr);
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

    assert(section_RVA % FILE_ALIGNMENT == 0);
    assert(section_VA % PAGE_SIZE == 0);

    important_vals.size_of_image = section_VA;
  }

  FILES::OpenedFile file = FILES::open(file_name, FILES::OPEN_MODE::WRITE, FILES::DATA_MODE::BINARY);

  if (file.error_code != 0) return ErrorCode::COULD_NOT_CREATE_FILE;

  FILE* const out = file.file;

  {
    //MS-DOS header and stub
    MS_DOS ms ={};

    ms.header.actual_start_of_header = MS_DO_HEADER_SIZE + MS_DOS_STUB_SIZE;

    FILES::write(out, ms.header);
    FILES::write(out, ms.stub);
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

    FILES::write(out, coff);
  }

  //PE32 Optional header
  {
    PE32Plus_optional_header pe32 ={};
    pe32.magic_number = MAGIC_NUMBER::PE32_PLUS;
    pe32.major_linker_version = 0;
    pe32.minor_linker_version = 0;
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

    FILES::write(out, pe32);
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
    pe32_win.major_image_version = 6;
    pe32_win.minor_image_version = 0;

    pe32_win.major_sub_version = 6;
    pe32_win.minor_sub_version = 0;

    pe32_win.size_of_image = important_vals.size_of_image;
    pe32_win.size_of_header = round_to_file_alignment(important_vals.size_of_header);
    pe32_win.subsystem = 3;

    //TODO: Actually mean something
    pe32_win.DLL_characteristics = 0;// 0x8160;

    pe32_win.size_of_stack_reserve = 0x100000;
    pe32_win.size_of_stack_commit = 0x1000;

    pe32_win.size_of_heap_reserve = 0x100000;
    pe32_win.size_of_heap_commit = 0x1000;

    pe32_win.number_of_rva_and_sizes = ALL_DATA_DIRECTORIES_NUM;

    FILES::write(out, pe32_win);
  }

  //Optional Header Directories
  {
    Image_header_directories data_directories ={};

    if (pe_file->imports != nullptr) {
      data_directories.import_table.virtual_address = important_vals.imports_va;
      data_directories.import_table.size = important_vals.imports_raw_size;
    }

    FILES::write(out, data_directories);
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

    FILES::write(out, code_header);
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

    FILES::write(out, constants_header);
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

    FILES::write(out, imports_header);
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

        FILES::write(out, data_directory);
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
            FILES::write(out, entry);
          }
          FILES::write_padding_bytes(out, '\0', 8);
        }
      }
    }

    FILES::write(out, import_names.data, import_names.size);

    size_t padding = round_to_file_alignment(important_vals.imports_raw_size) - important_vals.imports_raw_size;
    FILES::write_padding_bytes(out, '\0', padding);
  }

  return FILES::close(out);
}