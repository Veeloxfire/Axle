#include "backends.h"
#include "PE_file_format.h"

#if 0

#include "compiler.h"
#include "calling_conventions.h"
#include "format.h"
#include "trace.h"


#include <stdio.h>

static void relocation_fix(uint8_t* code,
                           const uint8_t* data_base,
                           const Relocation* reloc,
                           size_t* label_indexes) {
  u8* ptr = code + reloc->value_offset;

  switch (reloc->type) {
    case RELOCATION_TYPE::U64_LABEL_OFFSET: {
        const u64 label = x64_from_bytes(ptr);
        x64_to_bytes(label_indexes[label], ptr);
        break;
      }
    case RELOCATION_TYPE::U64_DATA_OFFSET: {
        //Shouldnt be using non-const globals if we have no data
        ASSERT(data_base != nullptr);
        const u64 offset = x64_from_bytes(ptr);
        x64_to_bytes(data_base + offset, ptr);
        break;
      }
    case RELOCATION_TYPE::I32_RELATIVE_TO_NEXT: {
        const u32 label = x32_from_bytes(ptr);
        x32_to_bytes((i32)((i64)label_indexes[label] - (i64)reloc->other_offset), ptr);
        break;
      }
    default: {
        INVALID_CODE_PATH("Relocation type not supported");
      }
  }
}


void build_data_section_for_vm(Program* prog, CompilerGlobals* const comp) {
  TRACING_FUNCTION();

  InternHashTable<size_t> loaded_strings = {};

  Array<uint8_t> data = {};

  //Writes a string if its not alread written
  [[maybe_unused]] const auto write_string = [&](Array<uint8_t>& bytes, const InternString* s) -> size_t {
    if (!loaded_strings.contains(s)) {
      //doesnt contain this string
      //have to write it to data

      bytes.reserve_extra(s->len + 1);

      size_t position = bytes.size;

      memcpy_ts(bytes.data + bytes.size, bytes.capacity - bytes.size, (const uint8_t*)s->string, s->len + 1);
      bytes.size += s->len + 1;


      size_t ret_position = position;

      loaded_strings.insert(s, std::move(position));
      return ret_position;
    }
    else {
      //Already written
      return *loaded_strings.get_val(s);
    }
  };

  //Write a 64 bit number aligned to a 64 bit boundary
  const auto write_u64 = [](Array<uint8_t>& bytes, uint64_t a)->size_t {
    //Align
    const auto align_to = ceil_to_8(bytes.size);
    bytes.insert_uninit(align_to - bytes.size);

    size_t position = bytes.size;
    serialise_to_array(bytes, a);

    return position;
  };

  const auto write_num_bytes = [](Array<uint8_t>& bytes, size_t num_bytes, size_t alignment)->size_t {
    //Align
    const auto align_to = ceil_to_n(bytes.size, alignment);
    bytes.insert_uninit(align_to - bytes.size);

    size_t position = bytes.size;
    bytes.insert_uninit(num_bytes);

    return position;
  };

  //0 is an invalid position
  write_u64(data, 0);

  // Reserved Data Sections
  {
    auto i = comp->data_holders.mut_begin();
    const auto end = comp->data_holders.end();

    for (; i < end; i++) {
      i->data_index = write_num_bytes(data, i->size, i->alignment);
    }
  }

  Array<uint8_t> imports = {};

#if 0
  //Dll imports
  {
    //Should be sorted ...

    auto i_dll = comp->lib_import.begin();
    auto end_dll = comp->lib_import.end();

    const InternString* path = nullptr;

    for (; i_dll < end_dll; i_dll++) {
      if (path != i_dll->path) {
        if (path != nullptr) {
          //Null terminate for previous functions in a dll
          write_u64(imports, 0);
        }

        path = i_dll->path;

        size_t name_position = write_string(data, path);

        //write header
        write_u64(imports, name_position);
      }



      //Write the function name
      size_t name_position = write_string(data, i_dll->name);
      write_u64(imports, name_position);

      //The write the data index
      write_u64(imports, (comp->data_holders.data + i_dll->data_holder_index - 1)->data_index);
    }

    //2nd null terminate for dlls in import
    write_u64(imports, 0);
  }
#endif

  prog->data_size = data.size;
  prog->data = std::move(data);
  if (imports.size != 8) {
    prog->imports = std::move(imports);
  }
}

#if 0
void build_data_section_for_file(Array<u8>& data, Array<u8>& imports, Compiler* const comp, Array<Relocation>& relocations) {
  TRACING_FUNCTION();

  ////Write a 64 bit number aligned to a 64 bit boundary
  //const auto write_u64 = [](Array<uint8_t>& bytes, uint64_t a)->size_t {
  //  //Align
  //  const auto align_to = ceil_to_8(bytes.size);
  //  bytes.insert_uninit(align_to - bytes.size);

  //  size_t position = bytes.size;
  //  serialise_to_array(bytes, a);

  //  return position;
  //};

  //// Reserved Data Sections
  //{
  //  auto i = comp->data_holders.mut_begin();
  //  const auto end = comp->data_holders.end();

  //  for (; i < end; i++) {
  //    i->data_index = write_num_bytes(data, i->size, i->alignment);
  //  }
  //}

  //Dll imports
  {
    //Should be sorted ...

    auto i_dll = comp->lib_import.begin();
    const auto end_dll = comp->lib_import.end();

    usize base_size = 0;
    const InternString* path = nullptr;

    for (; i_dll < end_dll; i_dll++) {
      if (path != i_dll->path) {
        path = i_dll->path;
        base_size += sizeof(ImportDataDirectory);
      }
    }

    base_size += sizeof(ImportDataDirectory);

    i_dll = comp->lib_import.begin();
    path = 0;

    usize extra_size = 0;
    usize single_extra = 0;

    const auto write_single_import_directory = [&]() {
      size_t name_position = data.size;
      serialize_bytes(data, (const u8*)path->string, path->len + 1, 1);

      //write headers
      ImportDataDirectory dir = {};
      dir.import_lookup_table = base_size + extra_size;
      dir.date_time_stamp = 0;
      dir.forwarder_chain = 0;
      dir.name_rva = name_position;
      dir.import_address_table = base_size + extra_size + single_extra;

      serialize_struct(imports, &dir);
    };

    for (; i_dll < end_dll; i_dll++) {
      if (path != i_dll->path) {
        //Do this before next one starts
        if (path != nullptr) {
          //Null terminate the table
          single_extra += 8;
          write_single_import_directory();

          //Have 2 tables per dll
          extra_size += single_extra * 2;
        }

        path = i_dll->path;
      }

      single_extra += 8;
    }

    single_extra += 8;
    write_single_import_directory();

    //Null terminate all dlls
    serialize_zeros(imports, sizeof(ImportDataDirectory), alignof(ImportDataDirectory));

    ASSERT(imports.size == base_size);

    i_dll = comp->lib_import.begin();
    path = 0;

    usize hint_table_extra = 0;
    usize start = 0;

    for (; i_dll < end_dll; i_dll++) {
      if (path != i_dll->path) {
        if (path != nullptr) {
          //Copy again
          usize table_size = imports.size - start;
          imports.reserve_extra(table_size);

          for (usize i = 0; i < table_size; i++) {
            imports.data[i + start + table_size] = imports.data[i + start];
          }
        }
        start = imports.size;
        path = i_dll->path;
      }

      u64 lookup_value = 0;
      lookup_value |= ((base_size + extra_size + hint_table_extra) & 0xffffffffllu);
      serialize_struct(imports, &lookup_value);

      u64 h_size = 2 + i_dll->name->len + 1;
      hint_table_extra += h_size + (h_size & 1);
    }

    {
      //Copy again
      usize table_size = imports.size - start;
      imports.reserve_extra(table_size);

      for (usize i = 0; i < table_size; i++) {
        imports.data[i + start + table_size] = imports.data[i + start];
      }
    }

    serialize_zeros(imports, 8, 8);
    ASSERT(imports.size == (base_size + extra_size));

    for (; i_dll < end_dll; i_dll++) {
      serialize_zeros(imports, 2, 2);
      serialize_bytes(imports, (const u8*)i_dll->name->string, i_dll->name->len + 1, 1);
    }

    if (imports.size % 2 != 0) {
      imports.insert(0);
    }

    ASSERT(imports.size == (base_size + extra_size + hint_table_extra));
  }
}
#endif

#endif

u8 Backend::DataBucketIterator::read_byte() {
  if (bucket_counter == BUCKET_SIZE) {
    bucket = bucket->next;
    bucket_counter = 0;
  }

  u8 b = bucket->arr[bucket_counter];
  bucket_counter += 1;
  actual_location += 1;
  return b;
}

void Backend::DataBucketIterator::jump_to(usize l) {
  ASSERT(l >= actual_location);
  while (actual_location < l) {
    ASSERT(bucket != nullptr);

    usize distance = l - actual_location;
    usize bucket_remainng = BUCKET_SIZE - bucket_counter;
    if (distance <= bucket_remainng) {
      bucket_counter += distance;
      actual_location += distance;
      return;
    }
    else {
      bucket = bucket->next;
      bucket_counter = 0;
      actual_location += bucket_remainng;
    }
  }
  ASSERT(l == actual_location);
}

void Backend::DataBucketIterator::overwrite_arr(const u8* arr, const u8* end) {
  while (true) {
    usize remaining = end - arr;

    if (BUCKET_SIZE - bucket_counter < remaining) {
      usize bucket_remaining = BUCKET_SIZE - bucket_counter;

      for (; bucket_counter < BUCKET_SIZE; ++bucket_counter) {
        bucket->arr[bucket_counter] = *arr;
        arr += 1;
      }

      actual_location += bucket_remaining;

      ASSERT(bucket->next != nullptr);
      bucket = bucket->next;
      bucket_counter = 0;
    }
    else {
      usize top_end = bucket_counter + remaining;
      for (; bucket_counter < top_end; ++bucket_counter) {
        bucket->arr[bucket_counter] = *arr;
        arr += 1;
      }

      actual_location += remaining;

      break;
    }
  }

  ASSERT(arr == end);
}

void Backend::DataBucketStore::free_held() {
  DataBucket* curr = bottom;

  while (curr != nullptr) {
    DataBucket* next = curr->next;
    free_destruct_single(curr);
    curr = next;
  }

  top_fill = 0;
  top = nullptr;
  bottom = nullptr;
  total_size = 0;
}

void Backend::DataBucketStore::push_arr(const u8* arr, const u8* end) {
  if (bottom == nullptr) {
    bottom = allocate_default<DataBucket>();
    top = bottom;
    top_fill = 0;
    total_size = 0;
  }

  while (true) {
    usize remaining = end - arr;

    if (BUCKET_SIZE - top_fill < remaining) {
      usize bucket_remaining = BUCKET_SIZE - top_fill;

      for (; top_fill < BUCKET_SIZE; ++top_fill) {
        top->arr[top_fill] = *arr;
        arr += 1;
      }

      total_size += bucket_remaining;

      DataBucket* new_b = allocate_default<DataBucket>();
      top->next = new_b;
      top = new_b;
      top_fill = 0;
    }
    else {
      usize top_end = top_fill + remaining;
      for (; top_fill < top_end; ++top_fill) {
        top->arr[top_fill] = *arr;
        arr += 1;
      }

      total_size += remaining;

      break;
    }
  }

  ASSERT(arr == end);
}