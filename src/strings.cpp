#include "strings.h"
#include "safe_lib.h"

#include "trace.h"

constexpr char TOMBSTONE_DATA[sizeof(InternString)] ={};
const InternString* TOMBSTONE = (const InternString*)TOMBSTONE_DATA;



Table::Table() : data(allocate_default<const InternString*>(8)), size(8) {}


Table::~Table() {
  free_destruct_n(data, size);
  data = nullptr;
  size = 0;
  num_full = 0;
}

const InternString** Table::find(const char* str, size_t len, uint64_t hash) const {
  uint64_t test_index = hash % size;

  const InternString** first_tombstone = nullptr;

  const InternString* el = data[test_index];
  while (el != nullptr) {

    if (el == TOMBSTONE && first_tombstone == nullptr) {
      //Tombstone space
      first_tombstone =  data + test_index;
    }
    else if (el->hash == hash && el->len == len && memeq_ts<char>(str, el->string, len)) {
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

    do {
      size <<= 1;
    } while (num_full >= size * LOAD_FACTOR);
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

const InternString* StringInterner::intern(const char* string, const size_t length) {
  TRACING_FUNCTION();
  ASSERT(string != nullptr);
  ASSERT(length > 0);

  const uint64_t hash = fnv1a_hash(string, length);

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

InternStringSet::~InternStringSet() {
  free_no_destruct(data);

  data = nullptr;
  el_capacity = 0;
  used = 0;
}

bool InternStringSet::contains(const InternString* key) const {
  ASSERT(key != nullptr && key != TOMBSTONE);
  if(el_capacity == 0) return false;

  size_t index = key->hash % el_capacity;

  const InternString* test_key = data[index];
  while (true) {
    if (test_key == key) {
      return true;
    }
    else if (test_key == nullptr || test_key == TOMBSTONE) {
      return false;
    }

    index++;
    index %= el_capacity;
    test_key = data[index];
  }
}

const InternString** InternStringSet::get(const InternString* key) const {
  bool found_tombstone = false;
  size_t tombstone_index = 0;

  size_t index = key->hash % el_capacity;

  const InternString* test_key = data[index];
  while (test_key != nullptr) {
    if (key == test_key) {
      return data + index;
    }
    else if (test_key == TOMBSTONE && !found_tombstone) {
      found_tombstone = true;
      tombstone_index = index;
    }

    index++;
    index %= el_capacity;
    test_key = data[index];
  }

  if (found_tombstone) {
    return data + tombstone_index;
  }
  else {
    return data + index;
  }
}

void InternStringSet::try_extend(size_t num) {
  if (needs_resize(num)) {
    const InternString** old_data = data;
    const size_t old_el_cap = el_capacity;

    do {
      el_capacity <<= 1;
    } while (needs_resize(num));

    data = allocate_default<const InternString*>(el_capacity);

    for (size_t i = 0; i < old_el_cap; i++) {
      const InternString* key = old_data[i];

      if (key != nullptr && key != TOMBSTONE) {
        const InternString** loc = get(key);
        *loc = key;
      }
    }

    free_no_destruct(old_data);
  }
}

void InternStringSet::insert(const InternString* const key) {
  ASSERT(key != nullptr && key != TOMBSTONE);

  if (el_capacity == 0) {
    ASSERT(used == 0);
    el_capacity = 8;
    data = allocate_default<const InternString*>(el_capacity);

    const InternString** loc = get(key);
    *loc = key;
    used += 1;
  }
  else {
    const InternString** loc = get(key);

    const InternString* test_key = *loc;
    if (test_key == key) return;//already contained

    ASSERT(test_key == nullptr || test_key == TOMBSTONE);

    *loc = key;
    used += 1;
    if (needs_resize(0)) {
      try_extend(0);
    }
  }
}

bool is_alphabetical_order(const InternString* l, const InternString* r) {
  size_t min_size = l->len < r->len ? l->len : r->len;

  for (size_t i = 0; i < min_size; i++) {
    char cl = l->string[i];
    char cr = r->string[i];

    if (cl == cr) {
      continue;
    }

    return cl < cr;
  }

  return l->len <= r->len;
}