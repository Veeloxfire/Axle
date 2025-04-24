#include <AxleUtil/files.h>
#include <AxleUtil/io.h>

#include <Axle/backends/PE_file_format.h>
#include "compiler.h"
#include "backends.h"
#include <Axle/tracing_wrapper.h>
#include <ctime>

namespace FILES = Axle::FILES;
namespace IO = Axle::IO;

//// Sizes ////

static_assert(sizeof(COFF_Characteristics::Characteristics) == 2, "Must be 2 bytes");
static_assert(sizeof(COFF_file_header) == 20, "Must be 20 bytes");
static_assert(COFF_HEADER_SIZE == 20, "Must be 20 bytes");

static_assert(sizeof(MS_DOS_Header) == 64, "Must be 64 bytes");
static_assert(sizeof(MS_DOS_Stub) == 128, "Must be 128 bytes");
static_assert(MS_DOS_STUB_SIZE == 128, "Must be 128 bytes");

constexpr uint32_t FILE_ALIGNMENT = 512;

///// Definitions /////

static void write_code_partial(FILES::FileHandle out,
                               Backend::DataBucketIterator* code, usize end,
                               usize* const file_ptr, usize* const mem_ptr) {
  while (true) {
    usize remaining = end - code->actual_location;

    if (Backend::BUCKET_SIZE - code->bucket_counter < remaining) {
      usize write_size = Backend::BUCKET_SIZE - code->bucket_counter;
      FILES::write(out, code->bucket->arr + code->bucket_counter, write_size);

      code->actual_location += write_size;
      code->bucket_counter = 0;
      code->bucket = code->bucket->next;

      *file_ptr += write_size;
      *mem_ptr += write_size;

      ASSERT(code->bucket->next != nullptr);
      code->bucket = code->bucket->next;
      code->bucket_counter = 0;
    }
    else {
      FILES::write(out, code->bucket->arr + code->bucket_counter, remaining);
      code->actual_location += remaining;
      code->bucket_counter += remaining;
      *file_ptr += remaining;
      *mem_ptr += remaining;
      break;
    }
  }

  ASSERT(code->actual_location == end);
}

static void file_align(FILES::FileHandle out,
                       usize* const file_ptr, usize file_al) {
  usize current = *file_ptr;
  usize new_size = Axle::ceil_to_n(current, file_al);

  if (new_size != current) {
    FILES::write_padding_bytes(out, '\0', new_size - current);
    *file_ptr = new_size;
  }
}

static void mem_align(usize* const mem_ptr, usize mem_al) {
  *mem_ptr = Axle::ceil_to_n(*mem_ptr, mem_al);
}

template<typename T>
static void write_to_image(FILES::FileHandle out, const T& t, usize* const file_pos, usize* const mem_pos) {
  FILES::write_obj(out, t);
  *file_pos += sizeof(T);
  *mem_pos += sizeof(T);
}

static void write_raw_to_image(FILES::FileHandle out, const u8* bytes, usize size, usize* const file_pos, usize* const mem_pos) {
  FILES::write(out, bytes, size);
  *file_pos += size;
  *mem_pos += size;
}

namespace X64 {
  bool try_relative_disp(usize from, usize to, i32* jump);
}

struct ProgramLocation {
  u32 memory;
  u32 file;
};

void write_pe_file(CompilerThread* comp_thread,
                   const Backend::ProgramData* program,
                   const Axle::InternString* out_name, const Axle::InternString* out_folder, bool lib) {
  AXLE_TELEMETRY_FUNCTION();

  ASSERT((lib && program->dyn_exports.size != 0) || program->dyn_exports.size == 0);
  ASSERT(lib || program->entry_point != IR::NULL_GLOBAL_LABEL);

  const uint32_t TIME = time(0) & 0x0000000000000000FFFFFFFFFFFFFFFF;

  const bool has_dynamic_imports = program->dyn_imports.size > 0;
  const bool has_globals = program->globals.size > 0;

  Axle::InternHashTable<ProgramLocation> strings = {};

  FOR(program->dyn_imports, it) {
    {
      ProgramLocation* l = strings.get_or_create(it->library);
      l->memory = 0;
      l->file = 0;
    }
    {
      ProgramLocation* f = strings.get_or_create(it->function);
      f->memory = 0;
      f->file = 0;
    }
  }

  const bool has_constants = strings.used > 0 || lib;

  Axle::ViewArr<const char> suffix;
  if (lib) {
    suffix = Axle::lit_view_arr("dll");
  }
  else {
    suffix = Axle::lit_view_arr("exe");
  }

  Axle::AllocFilePath file_path = format_file_path(view_arr(out_folder), view_arr(out_name), suffix);

  FILES::OpenedFile file = FILES::replace(view_arr(file_path.raw), FILES::OPEN_MODE::WRITE);

  if (file.error_code != FILES::ErrorCode::OK) {
    comp_thread->report_error(ERROR_CODE::FILE_ERROR, Span{}, "Could not open output file \"{}\" to write", file_path.raw);
    return;
  }
  IO::format("Writing to: {}\n", file_path.raw);

  const auto& out = file.file;

  usize file_pointer = 0;
  usize memory_pointer = 0;

  {
    //MS-DOS header and stub
    MS_DOS ms = {};

    ms.header.actual_start_of_header = MS_DO_HEADER_SIZE + MS_DOS_STUB_SIZE;

    write_to_image(out, ms.header, &file_pointer, &memory_pointer);
    write_to_image(out, ms.stub, &file_pointer, &memory_pointer);
  }

  {
    constexpr uint8_t signature[SIGNATURE_SIZE] = PE_SIGNATURE;

    write_raw_to_image(out, signature, SIGNATURE_SIZE, &file_pointer, &memory_pointer);
  }

  size_t coff_header_location = file_pointer;

  {
    COFF_file_header coff = {};
    static_assert(sizeof(coff) == 20);
    write_to_image(out, coff, &file_pointer, &memory_pointer);
  }

  size_t optional_header_start = file_pointer;
  size_t pe_header_location = optional_header_start;

  {
    PE32_StandardFields pe32 = {};
    static_assert(sizeof(pe32) == 24);
    write_to_image(out, pe32, &file_pointer, &memory_pointer);
  }

  constexpr usize section_alignment = PAGE_SIZE;
  constexpr usize file_alignmnet = FILE_ALIGNMENT;

  size_t pe_plus_header_location = file_pointer;

  {
    PE32Plus_windows_specific pe32_win = {};
    static_assert(sizeof(pe32_win) == 88);
    write_to_image(out, pe32_win, &file_pointer, &memory_pointer);
  }

  size_t data_directories_location = file_pointer;

  {
    Image_header_directories data_directories = {};

    static_assert(sizeof(data_directories) == 128);
    static_assert(sizeof(data_directories) == ALL_DATA_DIRECTORIES_NUM * 8);
    write_to_image(out, data_directories, &file_pointer, &memory_pointer);
  }


  ///// Section headers right after main header /////
  const size_t section_headers_start = file_pointer;

  usize num_sections = 0;

  const size_t constants_header_location = file_pointer;

  if (has_constants) {
    Section_Header constants_header = {};
    num_sections += 1;
    static_assert(sizeof(constants_header) == 40);
    write_to_image(out, constants_header, &file_pointer, &memory_pointer);
  }

  const size_t globals_header_location = file_pointer;

  if (has_globals) {
    Section_Header globals_header = {};
    num_sections += 1;
    static_assert(sizeof(globals_header) == 40);
    write_to_image(out, globals_header, &file_pointer, &memory_pointer);
  }

  const size_t imports_header_location = file_pointer;

  if (has_dynamic_imports) {
    Section_Header import_header = {};
    num_sections += 1;
    static_assert(sizeof(import_header) == 40);
    write_to_image(out, import_header, &file_pointer, &memory_pointer);
  }

  const size_t code_header_location = file_pointer;

  {
    Section_Header code_header = {};
    num_sections += 1;
    static_assert(sizeof(code_header) == 40);
    write_to_image(out, code_header, &file_pointer, &memory_pointer);
  }

  const size_t exports_header_location = file_pointer;

  if (lib) {
    Section_Header export_header = {};
    num_sections += 1;
    static_assert(sizeof(export_header) == 40);
    write_to_image(out, export_header, &file_pointer, &memory_pointer);
  }

  ///// SECTIONS /////

  file_align(out, &file_pointer, file_alignmnet);
  mem_align(&memory_pointer, section_alignment);
  const usize end_of_headers = file_pointer;


  usize initialized_data_size = 0;
  usize lib_name_rva = 0;

  const usize constants_start = file_pointer;
  const usize constants_memory_start = memory_pointer;

  if (has_constants) {
    if (lib) {
      Axle::OwnedArr<char> lib_name = format("{}.{}", out_name, suffix);
      lib_name_rva = memory_pointer;

      write_raw_to_image(out, (const u8*)lib_name.data, lib_name.size, &file_pointer, &memory_pointer);
    }

    auto string_itr = strings.itr();

    while (string_itr.is_valid()) {
      const Axle::InternString* s = string_itr.key();
      ProgramLocation* l = string_itr.val();

      l->file = (uint32_t)file_pointer;
      l->memory = (uint32_t)memory_pointer;

      write_raw_to_image(out, (const u8*)s->string, s->len + 1, &file_pointer, &memory_pointer);

      string_itr.next();
    }
  }

  const usize constants_raw_size = file_pointer - constants_start;
  file_align(out, &file_pointer, file_alignmnet);
  mem_align(&memory_pointer, section_alignment);
  const usize constants_file_size = file_pointer - constants_start;

  initialized_data_size += constants_file_size;

  const usize globals_start = file_pointer;
  const usize globals_memory_start = memory_pointer;

  if (has_globals) {
    auto itr = program->data_store.start();
    write_code_partial(out, &itr, program->data_store.total_size, &file_pointer, &memory_pointer);
    ASSERT(itr.actual_location == program->data_store.total_size);
  }

  const usize globals_raw_size = file_pointer - constants_start;
  file_align(out, &file_pointer, file_alignmnet);
  mem_align(&memory_pointer, section_alignment);
  const usize globals_file_size = file_pointer - constants_start;

  initialized_data_size += globals_file_size;

  const usize imports_start = file_pointer;
  const usize imports_memory_start = memory_pointer;

  usize import_address_table_memory_start = 0;
  usize import_address_table_size = 0;

  usize import_directory_memory_start = 0;
  usize import_directory_size = 0;

  struct  DynImport {
    const Axle::InternString* function;
    const Axle::InternString* library;
    u32 index;
  };

  Axle::Array<usize> dyn_import_lookup = {};
  Axle::Array<DynImport> dyn_imports = {};
  if (has_dynamic_imports) {
    AXLE_TELEMETRY_SCOPE("Write Dynamic Imports");

    {
      dyn_import_lookup.insert_uninit(program->dyn_imports.size);
      dyn_imports.reserve_total(program->dyn_imports.size);

      const Backend::DynImport* p_imports_start = program->dyn_imports.begin();
      const Backend::DynImport* p_imports = p_imports_start;
      const Backend::DynImport* p_imports_end = program->dyn_imports.end();

      for (; p_imports < p_imports_end; ++p_imports) {
        DynImport copy = {};
        copy.function = p_imports->function;
        copy.library = p_imports->library;
        copy.index = (u32)(p_imports - p_imports_start);

        dyn_imports.insert(copy);
      }
    }

    Axle::sort_view(Axle::view_arr(dyn_imports),
               [](const DynImport& left, const DynImport& right) {
      if (left.library != right.library) {
        return left.library <=> right.library;
      }
      else {
        return left.function <=> right.function;
      }
    });


    struct LI {
      const Axle::InternString* lib_name;
      u32 num_funcs;
      usize lookup_table_estimate;
      usize address_table_estimate;
    };
    Axle::Array<LI> library_infos = {};

    {
      const DynImport* previous = nullptr;
      LI* li = nullptr;

      dyn_imports.remove_if([&](const DynImport& curr) {
        if (previous != nullptr && previous->library == curr.library) {
          // filter duplicates
          if (previous->function == curr.function) {
            return true;
          }

          li->num_funcs += 1;
          
          previous = &curr;
          return false;
        }
        else {
          library_infos.insert_uninit(1);
          li = library_infos.back();
          li->lib_name = curr.library;
          li->num_funcs = 1;
          
          previous = &curr;
          return false;
        }
      });
    }

    const DynImport* const dyn_imports_start = dyn_imports.begin();
    const DynImport* const dyn_imports_end = dyn_imports.end();


    usize import_tables_size = 0;
    {
      LI* li = library_infos.mut_begin();
      LI* li_end = library_infos.mut_end();

      for (; li < li_end; li += 1) {
        import_tables_size += sizeof(u64) * (li->num_funcs + 1);
      }
    }

    usize import_directory_start = file_pointer;
    import_directory_memory_start = memory_pointer;
    import_directory_size = (library_infos.size + 1) * sizeof(ImportDataDirectory);

    usize import_lookup_start_estimate = file_pointer + import_directory_size + import_tables_size;

    {
      usize import_table_acc = 0;

      LI* li = library_infos.mut_begin();
      LI* li_end = library_infos.mut_end();

      const usize base_memory_pointer = memory_pointer;

      for (; li < li_end; li += 1) {
        ImportDataDirectory idir = {};
        idir.forwarder_chain = 0;
        idir.date_time_stamp = 0;

        const ProgramLocation* pl = strings.get_val(li->lib_name);
        ASSERT(pl != nullptr);

        idir.name_rva = pl->memory;

        usize table_size = ((usize)li->num_funcs + 1) * sizeof(u64);

        li->address_table_estimate = base_memory_pointer + import_directory_size + import_table_acc;
        li->lookup_table_estimate = li->address_table_estimate + import_tables_size;
        idir.import_address_table = (RVA)li->address_table_estimate;
        idir.import_lookup_table = (RVA)li->lookup_table_estimate;
        import_table_acc += table_size;

        write_to_image(out, idir, &file_pointer, &memory_pointer);
      }

      {
        ImportDataDirectory null_idir = {};
        null_idir.import_lookup_table = 0;
        null_idir.date_time_stamp = 0;
        null_idir.forwarder_chain = 0;
        null_idir.name_rva = 0;
        null_idir.import_address_table = 0;

        write_to_image(out, null_idir, &file_pointer, &memory_pointer);
      }
    }

    ASSERT(file_pointer - import_directory_start == import_directory_size);

    usize hint_name_table_start_estimate = memory_pointer + import_tables_size * 2;
    usize hint_name_table_size = 0;

    usize import_address_table_start = file_pointer;
    import_address_table_memory_start = memory_pointer;

    {
      const DynImport* dyn_import_i = dyn_imports_start;

      const LI* li = library_infos.begin();
      const LI* li_end = library_infos.end();
      constexpr u64 NULL_ENTRY = 0;

      for (; li < li_end; li += 1) {
        ASSERT(li->address_table_estimate == memory_pointer);
        for (; dyn_import_i < dyn_imports_end && dyn_import_i->library == li->lib_name; dyn_import_i += 1) {
          u64 entry = 0;
          entry |= (hint_name_table_start_estimate + hint_name_table_size) & 0x7fffffff;

          dyn_import_lookup.data[dyn_import_i->index] = memory_pointer;
          write_to_image(out, entry, &file_pointer, &memory_pointer);

          const usize name_size = dyn_import_i->function->len + 1;

          hint_name_table_size += 2;
          hint_name_table_size += name_size;
          hint_name_table_size += name_size % 2;
        }

        write_to_image(out, NULL_ENTRY, &file_pointer, &memory_pointer);

      }

      ASSERT(dyn_import_i == dyn_imports_end);
    }

    import_address_table_size = file_pointer - import_address_table_start;
    ASSERT(import_lookup_start_estimate == file_pointer);

    {
      const DynImport* dyn_import_i = dyn_imports_start;

      const LI* li = library_infos.begin();
      const LI* li_end = library_infos.end();
      constexpr u64 NULL_ENTRY = 0;

      usize hint_name_table_acc = 0;

      for (; li < li_end; li += 1) {
        ASSERT(li->lookup_table_estimate == memory_pointer);
        for (; dyn_import_i < dyn_imports_end && dyn_import_i->library == li->lib_name; dyn_import_i += 1) {
          u64 entry = 0;
          entry |= (hint_name_table_start_estimate + hint_name_table_acc) & 0x7fffffff;

          write_to_image(out, entry, &file_pointer, &memory_pointer);

          const usize name_size = dyn_import_i->function->len + 1;

          hint_name_table_acc += 2;
          hint_name_table_acc += name_size;
          hint_name_table_acc += name_size % 2;


        }

        write_to_image(out, NULL_ENTRY, &file_pointer, &memory_pointer);

      }

      ASSERT(hint_name_table_acc == hint_name_table_size);
    }

    ASSERT(hint_name_table_start_estimate == memory_pointer);

    {
      const DynImport* dyn_import_i = dyn_imports_start;

      constexpr u16 GUESS = 0;
      constexpr u8 PAD = '\0';

      for (; dyn_import_i < dyn_imports_end; ++dyn_import_i) {
        const usize name_size = dyn_import_i->function->len + 1;

        write_to_image(out, GUESS, &file_pointer, &memory_pointer);
        write_raw_to_image(out, (const u8*)dyn_import_i->function->string, name_size, &file_pointer, &memory_pointer);
        if (name_size % 2 == 1) {
          write_to_image(out, PAD, &file_pointer, &memory_pointer);
        }
      }
    }
  }

  const usize imports_raw_size = file_pointer - imports_start;
  file_align(out, &file_pointer, file_alignmnet);
  mem_align(&memory_pointer, section_alignment);
  const usize imports_file_size = file_pointer - imports_start;

  initialized_data_size += imports_file_size;

  const usize code_start = file_pointer;
  const usize code_memory_start = memory_pointer;

  Axle::OwnedArr actual_function_locations = Axle::new_arr<ProgramLocation>(program->functions.size);

  {
    AXLE_TELEMETRY_SCOPE("Write Machine Code");
    //Write all the code

    Backend::DataBucketIterator code = program->code_store.start();

    struct SortedFunction {
      const Backend::FunctionMetadata* metadata = nullptr;
    };

    Axle::OwnedArr sorted_functions = Axle::new_arr<const Backend::FunctionMetadata*>(program->functions.size);
    {
      const Backend::FunctionMetadata* functions = program->functions.begin();
      FOR_MUT(sorted_functions, it) {
        *it = functions;
        functions += 1;
      }
    }

    Axle::sort_view(Axle::view_arr(sorted_functions),
      [](const Backend::FunctionMetadata * l, const Backend::FunctionMetadata * r) {
        return l->code_start <=> r->code_start;
      });

    auto sf_i = sorted_functions.begin();
    const auto sf_end = sorted_functions.end();

    const Backend::Relocation* reloc = program->relocations.begin();
    const Backend::Relocation* reloc_end = program->relocations.end();

    while (true) {
      bool next_is_reloc;
      usize location;

      if ((reloc < reloc_end) && (sf_i < sf_end)) {
        usize f_start = (*sf_i)->code_start;
        if (reloc->location < f_start) {
          next_is_reloc = true;
          location = reloc->location;
        }
        else {
          next_is_reloc = false;
          location = f_start;
        }
      }
      else if (reloc < reloc_end) {
        next_is_reloc = true;
        location = reloc->location;
      }
      else if (sf_i < sf_end) {
        next_is_reloc = false;
        location = (*sf_i)->code_start;
      }
      else {
        break;//nothing to look up left
      }

      ASSERT(code.actual_location <= location);

      write_code_partial(out, &code, location, &file_pointer, &memory_pointer);

      if (next_is_reloc) {
        ASSERT(reloc->location == code.actual_location);
        switch (reloc->type) {
          case Backend::RelocationType::Label: {
              ASSERT(program->functions.size >= reloc->label.label);

              usize jump_to = program->functions.data[reloc->label.label - 1].code_start;

              i32 disp = 0;
              bool can_local_jump = X64::try_relative_disp(reloc->location + 4, jump_to, &disp);
              ASSERT(can_local_jump);//TODO: handle trampolining

              write_to_image(out, disp, &file_pointer, &memory_pointer);
              code.jump_to(reloc->location + 4);
              break;
            }
          case Backend::RelocationType::LibraryLabel: {
              ASSERT(dyn_import_lookup.size > reloc->library_call);
              usize jump_to = dyn_import_lookup.data[reloc->library_call];

              i32 disp = 0;
              bool can_local_jump = X64::try_relative_disp(memory_pointer + 4, jump_to, &disp);
              ASSERT(can_local_jump);//TODO: handle trampolining

              write_to_image(out, disp, &file_pointer, &memory_pointer);
              code.jump_to(reloc->location + 4);
              break;
            }
          case Backend::RelocationType::Global: {
              const Backend::GlobalData gd = program->globals[reloc->label.label - 1];

              i32 disp = 0;
              bool can_local_jump = X64::try_relative_disp(code_memory_start + reloc->location + 4,
                                                           globals_memory_start + gd.data_index, &disp);
              ASSERT(can_local_jump);//TODO: absolute addressing in this case?


              write_to_image(out, disp, &file_pointer, &memory_pointer);
              code.jump_to(reloc->location + 4);
              break;
            }
        }

        reloc += 1;
      }
      else {
        const Backend::FunctionMetadata* func = *sf_i;
        ASSERT(func->code_start == code.actual_location);
        usize i = func - program->functions.data;
        sf_i += 1;
        ASSERT(i < actual_function_locations.size);

        ProgramLocation f = {};
        f.file = (u32)file_pointer;
        f.memory = (u32)memory_pointer;
        actual_function_locations.data[i] = f;
      }
    }

    write_code_partial(out, &code, program->code_store.total_size, &file_pointer, &memory_pointer);
  }

  const usize code_raw_size = file_pointer - code_start;

  file_align(out, &file_pointer, file_alignmnet);
  mem_align(&memory_pointer, section_alignment);

  const usize code_file_size = file_pointer - code_start;

  const usize exports_start = file_pointer;
  const usize exports_memory_start = memory_pointer;

  if (lib) {
    AXLE_TELEMETRY_SCOPE("Write Dynamic Exports");

    Axle::OwnedArr name_offsets = Axle::new_arr<u32>(program->dyn_exports.size);

    {
      const auto* exp = program->dyn_exports.begin();
      const auto* exp_end = program->dyn_exports.end();

      auto* loc = name_offsets.mut_begin();
      u32 start = 0;
      for (; exp < exp_end; (exp += 1, loc += 1)) {
        *loc = start;
        start += (u32)(exp->name->len + 1);
      }

      ASSERT(loc == name_offsets.end());
    }

    u32 count = static_cast<u32>(program->dyn_exports.size);
    
    ASSERT(has_constants && lib_name_rva != 0);

    const RVA expected_eta = static_cast<RVA>(exports_memory_start + sizeof(ExportDirectoryTable));
    const RVA expected_npta = static_cast<RVA>(expected_eta + count * sizeof(ExportAddress));
    const RVA expected_ota = static_cast<RVA>(expected_npta + count * sizeof(RVA));
    const RVA expected_nta = static_cast<RVA>(expected_ota + count * sizeof(u16));

    {
      ExportDirectoryTable export_table = {};
      export_table.time_and_date = 0xffffffff;
      export_table.major_version = 0;
      export_table.minor_version = 0;
      export_table.name_rva = (RVA)lib_name_rva;
      export_table.ordinal_base = 1;//for some reason usually 1
      export_table.num_export_entries = count;
      export_table.num_name_pointers = count;
      export_table.export_table_address = expected_eta;
      export_table.name_pointer_table_address = expected_npta;
      export_table.ordinal_table_address = expected_ota;

      write_to_image(out, export_table, &file_pointer, &memory_pointer);
    }

    ASSERT(expected_eta == memory_pointer);

    //Export Address Table
    FOR(program->dyn_exports, it) {
      const ProgramLocation& l = actual_function_locations.data[it->label.label - 1];
      ASSERT(l.file != 0);
      ASSERT(l.memory != 0);

      ExportAddress addr = {};
      addr.export_rva = l.memory;
      write_to_image(out, addr, &file_pointer, &memory_pointer);
    }

    ASSERT(expected_npta == memory_pointer);
    ASSERT(file_pointer % 4 == 0);

    //Name Pointer Table
    FOR(name_offsets, it) {
      u32 offset = *it + expected_nta;
      write_to_image<u32>(out, offset, &file_pointer, &memory_pointer);
    }

    ASSERT(expected_ota == memory_pointer);

    ASSERT(count <= UINT16_MAX);
    ASSERT(file_pointer % 2 == 0);

    //Ordinal Table
    for (u16 ord = 0; ord < static_cast<u16>(count); ++ord) {
      write_to_image<u16>(out, ord, &file_pointer, &memory_pointer);
    }

    ASSERT(expected_nta == memory_pointer);
    //Export Name Table
    {
      const auto* exp = program->dyn_exports.begin();
      const auto* exp_end = program->dyn_exports.end();

      const auto* loc = name_offsets.mut_begin();
      for (; exp < exp_end; (exp += 1, loc += 1)) {
        ASSERT(*loc + expected_nta == memory_pointer);

        write_raw_to_image(out, (const u8*)exp->name->string, exp->name->len + 1, &file_pointer, &memory_pointer);
      }

      ASSERT(loc == name_offsets.end());
    }
  }

  const usize exports_raw_size = file_pointer - exports_start;
  file_align(out, &file_pointer, file_alignmnet);
  mem_align(&memory_pointer, section_alignment);
  const usize exports_file_size = file_pointer - exports_start;

  initialized_data_size += exports_file_size;

  usize end_of_file = file_pointer;
  usize image_size = memory_pointer;
  ASSERT(end_of_file % file_alignmnet == 0);

  //Go back and fix the headers
  FILES::seek_from_start(out, coff_header_location);

  {
    COFF_file_header coff = {};

    coff.machine = MACHINE_TYPE::AMD64;
    coff.number_of_sections = (uint16_t)num_sections;
    coff.time_date_stamp = TIME;
    coff.pointer_to_symbol_table = 0;//COFF symbol table not supported in image
    coff.number_of_symbols = 0;
    coff.size_of_optional_header = (uint16_t)(section_headers_start - optional_header_start);//size of image header
    coff.characteristics = COFF_Characteristics::EXECUTABLE_IMAGE | COFF_Characteristics::LARGE_ADDRESS_AWARE;
    if (lib) {
      coff.characteristics |= COFF_Characteristics::DLL;
    }

    static_assert(sizeof(coff) == 20);
    FILES::write_obj(out, coff);
  }

  FILES::seek_from_start(out, pe_header_location);

  {
    PE32_StandardFields pe32 = {};
    pe32.magic_number = MAGIC_NUMBER::PE32_PLUS;
    pe32.major_linker_version = 14;//TODO: get these right
    pe32.minor_linker_version = 36;//TODO: get these right
    pe32.size_of_code = (uint32_t)code_file_size;
    pe32.size_of_initialized_data = (uint32_t)initialized_data_size;
    pe32.size_of_uninitialized_data = 0;

    pe32.base_of_code = (PE_Address)code_memory_start;

    if (program->entry_point != IR::NULL_GLOBAL_LABEL) {
      ASSERT(program->start_code.code_size > 0);
      pe32.address_of_entry_point = (PE_Address)program->start_code.code_start + (PE_Address)code_memory_start;
    }
    else {
      pe32.address_of_entry_point = 0;
    }

    static_assert(sizeof(pe32) == 24);
    FILES::write_obj(out, pe32);
  }

  FILES::seek_from_start(out, pe_plus_header_location);

  {
    PE32Plus_windows_specific pe32_win = {};

    if (lib) {
      pe32_win.image_base = DLL64_IMAGE_BASE;
    }
    else {
      pe32_win.image_base = EXE64_IMAGE_BASE;
    }
    pe32_win.section_alignment = section_alignment;
    pe32_win.file_alignment = file_alignmnet;

    //not sure if these versions are correct ... 

    pe32_win.major_os_version = 6;
    pe32_win.minor_os_version = 0;

    pe32_win.major_image_version = 0;
    pe32_win.minor_image_version = 0;

    pe32_win.major_sub_version = 6;
    pe32_win.minor_sub_version = 0;

    pe32_win.size_of_image = (uint32_t)image_size;
    pe32_win.size_of_header = (uint32_t)end_of_headers;
    pe32_win.subsystem = SUBSYSTEMS::CONSOLE;

    pe32_win.DLL_characteristics = DLL_Characteristics::NX_COMPAT | DLL_Characteristics::DYNAMIC_BASE;

    pe32_win.size_of_stack_reserve = 0x100000;
    pe32_win.size_of_stack_commit = 0x1000;

    pe32_win.size_of_heap_reserve = 0x100000;
    pe32_win.size_of_heap_commit = 0x1000;

    pe32_win.number_of_rva_and_sizes = ALL_DATA_DIRECTORIES_NUM;

    static_assert(sizeof(pe32_win) == 88);
    FILES::write_obj(out, pe32_win);
  }

  FILES::seek_from_start(out, data_directories_location);

  {
    Image_header_directories data_directories = {};

    if (has_dynamic_imports) {
      ASSERT(imports_raw_size > 0);
      ASSERT(imports_file_size > 0);

      data_directories.import_table.virtual_address = (RVA)import_directory_memory_start;
      data_directories.import_table.size = (u32)import_directory_size;

      data_directories.import_address_table.virtual_address = (RVA)import_address_table_memory_start;
      data_directories.import_address_table.size = (u32)import_address_table_size;
    }

    if (lib) {
      ASSERT(exports_memory_start > 0);
      ASSERT(exports_file_size > 0);
      data_directories.export_table.virtual_address = (RVA)exports_memory_start;
      data_directories.export_table.size = (u32)exports_raw_size;
    }

    static_assert(sizeof(data_directories) == 128);
    static_assert(sizeof(data_directories) == ALL_DATA_DIRECTORIES_NUM * 8);
    FILES::write_obj(out, data_directories);
  }

  if (has_constants) {
    FILES::seek_from_start(out, constants_header_location);

    Section_Header constants_header = {};

    constants_header.name[0] = '.';
    constants_header.name[1] = 'r';
    constants_header.name[2] = 'd';
    constants_header.name[3] = 'a';
    constants_header.name[4] = 't';
    constants_header.name[5] = 'a';
    constants_header.name[6] = 0x00;
    constants_header.name[7] = 0x00;

    constants_header.virtual_size = (uint32_t)constants_raw_size;
    constants_header.virtual_address = (RVA)constants_memory_start;
    constants_header.size_of_raw_data = (uint32_t)constants_file_size;
    constants_header.pointer_to_raw_data = (uint32_t)constants_start;

    constants_header.characteristics = Section_Flags::READABLE | Section_Flags::CONTAINS_INITIALIZED;

    FILES::write_obj(out, constants_header);
  }

  if (has_globals) {
    FILES::seek_from_start(out, globals_header_location);

    Section_Header globals_header = {};

    globals_header.name[0] = '.';
    globals_header.name[1] = 'd';
    globals_header.name[2] = 'a';
    globals_header.name[3] = 't';
    globals_header.name[4] = 'a';
    globals_header.name[5] = 0x00;
    globals_header.name[6] = 0x00;
    globals_header.name[7] = 0x00;

    globals_header.virtual_size = (uint32_t)globals_raw_size;
    globals_header.virtual_address = (RVA)globals_memory_start;
    globals_header.size_of_raw_data = (uint32_t)globals_file_size;
    globals_header.pointer_to_raw_data = (uint32_t)globals_start;

    globals_header.characteristics = Section_Flags::READABLE | Section_Flags::WRITABLE  | Section_Flags::CONTAINS_INITIALIZED;

    FILES::write_obj(out, globals_header);
  }

  if (has_dynamic_imports) {
    FILES::seek_from_start(out, imports_header_location);

    Section_Header imports_header = {};

    imports_header.name[0] = '.';
    imports_header.name[1] = 'i';
    imports_header.name[2] = 'd';
    imports_header.name[3] = 'a';
    imports_header.name[4] = 't';
    imports_header.name[5] = 'a';
    imports_header.name[6] = 0x00;
    imports_header.name[7] = 0x00;

    imports_header.virtual_size = (uint32_t)imports_raw_size;
    imports_header.virtual_address = (RVA)imports_memory_start;
    imports_header.size_of_raw_data = (uint32_t)imports_file_size;
    imports_header.pointer_to_raw_data = (uint32_t)imports_start;

    //All irrelevant in this
    imports_header.pointer_to_relocations = 0x0;
    imports_header.pointer_to_line_numbers = 0x0;
    imports_header.number_of_relocations = 0x0;
    imports_header.number_of_line_numbers = 0x0;

    imports_header.characteristics = Section_Flags::READABLE | Section_Flags::CONTAINS_INITIALIZED;

    FILES::write_obj(out, imports_header);
  }

  FILES::seek_from_start(out, code_header_location);

  {
    Section_Header code_header = {};

    code_header.name[0] = '.';
    code_header.name[1] = 't';
    code_header.name[2] = 'e';
    code_header.name[3] = 'x';
    code_header.name[4] = 't';
    code_header.name[5] = 0x00;
    code_header.name[6] = 0x00;
    code_header.name[7] = 0x00;

    code_header.virtual_size = (uint32_t)code_raw_size;
    code_header.virtual_address = (RVA)code_memory_start;
    code_header.size_of_raw_data = (uint32_t)code_file_size;
    code_header.pointer_to_raw_data = (uint32_t)code_start;

    code_header.pointer_to_relocations = 0;
    code_header.pointer_to_line_numbers = 0;
    code_header.number_of_relocations = 0;
    code_header.number_of_line_numbers = 0;

    code_header.characteristics = Section_Flags::CONTAINS_CODE | Section_Flags::EXECTUTABLE | Section_Flags::READABLE;

    static_assert(sizeof(code_header) == 40);
    FILES::write_obj(out, code_header);
  }

  if (lib) {
    FILES::seek_from_start(out, exports_header_location);

    Section_Header exports_header = {};

    exports_header.name[0] = '.';
    exports_header.name[1] = 'e';
    exports_header.name[2] = 'd';
    exports_header.name[3] = 'a';
    exports_header.name[4] = 't';
    exports_header.name[5] = 'a';
    exports_header.name[6] = 0x00;
    exports_header.name[7] = 0x00;

    exports_header.virtual_size = (uint32_t)exports_raw_size;
    exports_header.virtual_address = (RVA)exports_memory_start;
    exports_header.size_of_raw_data = (uint32_t)exports_file_size;
    exports_header.pointer_to_raw_data = (uint32_t)exports_start;

    //All irrelevant in this
    exports_header.pointer_to_relocations = 0x0;
    exports_header.pointer_to_line_numbers = 0x0;
    exports_header.number_of_relocations = 0x0;
    exports_header.number_of_line_numbers = 0x0;

    exports_header.characteristics = Section_Flags::READABLE | Section_Flags::CONTAINS_INITIALIZED;

    FILES::write_obj(out, exports_header);
  }
}

void output_pe_exe(CompilerThread* comp_thread,
                   const Backend::ProgramData* program,
                   const Axle::InternString* out_name, const Axle::InternString* out_folder) {
  write_pe_file(comp_thread, program, out_name, out_folder, false);
}


void output_pe_dll(CompilerThread* comp_thread,
                   const Backend::ProgramData* program,
                   const Axle::InternString* out_name, const Axle::InternString* out_folder) {
  write_pe_file(comp_thread, program, out_name, out_folder, true);
}

#if 0
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
      RVA_Resolver res = {};
      res.ptr_base = i->pointer_to_raw_data;
      res.va_base = i->virtual_address;
      return res;
    }
  }

  return {};
}

void load_portable_executable_from_file(CompilerGlobals* const comp,
                                        CompilerThread* const comp_thread,
                                        const Span& span,
                                        PEFile* pe_file,
                                        const Axle::ViewArr<const char>& file_name) {

  FILES::OpenedFile file = FILES::open(file_name, FILES::OPEN_MODE::READ);

  if (file.error_code != FILES::ErrorCode::OK) {
    comp_thread->report_error(ERROR_CODE::FILE_ERROR, span,
                              "Could not open file '{}'\n"
                              "Perhaps it does not exist",
                              file_name);
    return;
  }

  //Close file if it was opened
  DEFER(&) { FILES::close(file.file); };

  FILES::read(file.file, &pe_file->header.ms_dos, 1);

  if (pe_file->header.ms_dos.magic != MAGIC_NUMBER::MZ) {
    comp_thread->report_error(ERROR_CODE::FILE_ERROR, span,
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

    if (!memeq_ts<char>(expected_sig, sig, 4)) {
      comp_thread->report_error(ERROR_CODE::FILE_ERROR, span,
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
    comp_thread->report_error(ERROR_CODE::FILE_ERROR, span,
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
      comp_thread->report_error(ERROR_CODE::FILE_ERROR, span,
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
      comp_thread->report_error(ERROR_CODE::FILE_ERROR, span,
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
      comp_thread->report_error(ERROR_CODE::FILE_ERROR, span,
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
    Axle::Array<RVA> name_rvas = {};
    name_rvas.insert_uninit(num_ordinals);
    //Seek to names
    FILES::seek_from_start(file.file, resolver.load_rva_ptr(directory_table.name_pointer_table_address));
    //Read names
    FILES::read(file.file, name_rvas.data, num_ordinals);

    //Load ordinals
    Axle::Array<uint16_t> ordinals = {};
    ordinals.insert_uninit(num_ordinals);
    //Seek to ordinals
    FILES::seek_from_start(file.file, resolver.load_rva_ptr(directory_table.ordinal_table_address));
    //Read ordinals
    FILES::read(file.file, ordinals.data, num_ordinals);

    //Load the names and ordinals together
    {
      pe_file->export_table.element_table.reserve_extra(num_ordinals);

      Axle::Array<char> name_holder = {};

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

        ptr->str = comp->services.strings.get()->intern(name_holder.data, name_holder.size);
        pe_file->export_table.names.insert(ptr->str);

        name_holder.clear();
      }

    }
  }
}
#endif

