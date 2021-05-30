#include "strings.h"
#include "safe_lib.h"

constexpr char TOMBSTONE_DATA[sizeof(InternString)] ={};
const InternString* TOMBSTONE = (const InternString*)TOMBSTONE_DATA;



Table::Table() : data(allocate_default<const InternString*>(8)), size(8) {}

const InternString** Table::find(const char* str, size_t len, uint64_t hash) const {
  uint64_t test_index = hash % size;

  const InternString** first_tombstone = nullptr;

  const InternString* el = data[test_index];
  while (el != nullptr) {

    if (el == TOMBSTONE && first_tombstone != nullptr) {
      //Tombstone space
      first_tombstone =  data + test_index;
    }
    else if (el->hash == hash && el->len == len && memcmp_ts(str, el->string, len) == 0) {
      //Success
      return data + test_index;
    }

    //Try next one
    test_index++;
    test_index %= size;
    el = data[test_index];
  }

  //Test for tombstone
  if (first_tombstone != nullptr) {
    return first_tombstone;
  }
  else {
    return data + test_index;
  }
}

const InternString** Table::find_empty(uint64_t hash) const {
  uint64_t test_index = hash % size;

  const InternString* el = data[test_index];
  while (true) {

    if (el == nullptr || el == TOMBSTONE) {
      //Empty space
      return data + test_index;
    }

    //Try next one
    test_index++;
    test_index %= size;
    el = data[test_index];
  }
}

void Table::try_resize() {
  if (num_full >= size * LOAD_FACTOR) {
    const size_t old_size = size;
    const InternString** const old_data = data;

    size <<= 1;
    data = allocate_default<const InternString*>(size);

    {
      auto i = old_data;
      const auto end = old_data + old_size;
      for (; i < end; i++) {
        const InternString* i_str = *i;

        if (i_str != nullptr && i_str != TOMBSTONE) {
          auto** place = find_empty(i_str->hash);
          *place = i_str;
        }
      }
    }

    free_no_destruct(old_data);
  }
}

const InternString* StringInterner::intern(const char* string) {
  return intern(string, strlen_ts(string));
}

const InternString* StringInterner::intern(const char* string, const size_t length) {
  assert(string != nullptr);

  const uint64_t hash = fnv1_hash(string, length);

  const InternString** const place = table.find(string, length, hash);

  const InternString* el = *place;
  if (el == nullptr || el == TOMBSTONE) {
    InternString* new_el = (InternString*)allocs.allocate_no_construct(InternString::alloc_size(length));
    new_el->hash = hash;
    new_el->len = length;

    memcpy_ts(new_el->string, length + 1, string, length);
    *place = new_el;

    new_el->string[length + 1] = '\0';

    table.num_full++;
    table.try_resize();
    return new_el;
  }
  else {
    return el;
  }
}