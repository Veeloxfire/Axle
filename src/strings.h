#pragma once
#include "utility.h"

struct InternString {
  uint64_t hash;
  size_t len;
  char string[1];//placeholder for any length array

  constexpr bool operator==(const InternString& i)const {
    return i.hash == hash && i.len == len
      && (memcmp_ts((const char*)i.string, (const char*)string, len) == 0);
  }

  constexpr bool operator!=(const InternString& i)const {
    return !operator!=(i);
  }

  constexpr static size_t alloc_size(size_t string_len) {
    return sizeof(InternString) + string_len + 1 /* for null byte */;
  }
};

extern const InternString* TOMBSTONE;

struct Table {
  constexpr static float LOAD_FACTOR = 0.75;

  const InternString** data = nullptr;
  size_t num_full = 0;
  size_t size = 0;

  Table();
  ~Table();

  void try_resize();
  const InternString** find(const char* str, size_t len, uint64_t hash) const;
  const InternString** find_empty(uint64_t hash) const;
};

struct StringInterner {
  BumpAllocator allocs ={};
  Table table ={};

  const InternString* intern(const char* string);
  const InternString* intern(const char* string, size_t len);
};

struct TempUTF8String {
  uint8_t* bytes = nullptr;
  size_t size = 0;

  TempUTF8String() = default;

  TempUTF8String(TempUTF8String&& t_utf8) noexcept
    : bytes(std::exchange(t_utf8.bytes, nullptr)), size(std::exchange(t_utf8.size, 0)) {

  }

  ~TempUTF8String() {
    free_no_destruct(bytes);
  }
};

template<typename T>
struct InternHashTable {
  constexpr static float LOAD_FACTOR = 0.75;

  uint8_t* data      = nullptr;// ptr to data in the array
  size_t el_capacity = 0;
  size_t used        = 0;

  constexpr bool needs_resize(size_t extra) const {
    return (el_capacity * LOAD_FACTOR) <= (used + extra);
  }

  ~InternHashTable() {
    {
      const InternString** keys = (const InternString**)data;

      T* vals = (T*)(data + el_capacity * sizeof(const InternString*));

      for (size_t i = 0; i < el_capacity; i++) {
        const InternString* key = keys[i];

        if (key != nullptr && key != TOMBSTONE) {
          vals[i].~T();
        }
      }
    }

    free_no_destruct(data);

    data = nullptr;
    el_capacity = 0;
    used = 0;
  }

  bool contains(const InternString* key) const {
    if(el_capacity == 0) return false;

    const InternString** hash_arr = (const InternString**)data;

    size_t index = key->hash % el_capacity;

    const InternString* test_key = hash_arr[index];
    while (test_key != nullptr) {
      if (key == test_key) {
        return true;
      }

      index++;
      index %= el_capacity;
      test_key = hash_arr[index];
    }

    return false;
  }

  size_t get_soa_index(const InternString* key) const {
    const InternString** hash_arr = (const InternString**)data;

    bool found_tombstone = false;
    size_t tombstone_index = 0;

    size_t index = key->hash % el_capacity;

    const InternString* test_key = hash_arr[index];
    while (test_key != nullptr) {
      if (key == test_key) {
        return index;
      }
      else if (test_key == TOMBSTONE && !found_tombstone) {
        found_tombstone = true;
        tombstone_index = index;
      }

      index++;
      index %= el_capacity;
      test_key = hash_arr[index];
    }

    if (found_tombstone) {
      return tombstone_index;
    }
    else {
      return index;
    }
  }

  void try_extend(size_t num) {
    if (needs_resize(num)) {
      uint8_t* old_data = data;
      const size_t old_el_cap = el_capacity;

      do {
        el_capacity <<= 1;
      } while (needs_resize(num));

      const size_t required_alloc_bytes = el_capacity
        * (sizeof(const InternString*) + sizeof(T));

      data = allocate_default<uint8_t>(required_alloc_bytes);

      const InternString**hash_arr = (const InternString**)data;
      T* val_arr = (T*)(data + el_capacity * sizeof(const InternString*));

      const InternString**old_hash_arr = (const InternString**)old_data;
      T* old_val_arr = (T*)(old_data + old_el_cap * sizeof(const InternString*));

      for (size_t i = 0; i < old_el_cap; i++) {
        const InternString* key = old_hash_arr[i];

        if (key != nullptr && key != TOMBSTONE) {
          const size_t new_index = get_soa_index(key);

          hash_arr[new_index] = key;
          val_arr[new_index]  = std::move(old_val_arr[i]);
        }
      }

      //Destruct and free
      {
        destruct_arr(old_hash_arr, el_capacity);
        //Dont need to free old values as they've been moved

        free_no_destruct(old_data);
      }
    }
  }

  T* get_val(const InternString* const key) const {
    if(el_capacity == 0) return nullptr;

    const size_t soa_index = get_soa_index(key);

    {
      const InternString* test_key = ((const InternString**)data)[soa_index];

      if (test_key == nullptr || test_key == TOMBSTONE) {
        return nullptr;
      }
    }

    T* const vals = (T*)(data + (sizeof(const InternString*) * el_capacity));
    return vals + soa_index;
  }

  void insert(const InternString* const key, T&& val) {
    if (el_capacity == 0) {
      el_capacity = 8;
      data = allocate_default<uint8_t>(8 * (sizeof(const InternString*) + sizeof(T)));

      size_t soa_index = get_soa_index(key);

      const InternString** const keys = (const InternString**)data;
      T* const vals = (T*)(data + (sizeof(const InternString*) * el_capacity));

      used++;
      keys[soa_index] = key;
      vals[soa_index] = std::move(val);   
    }
    else {
      size_t soa_index = get_soa_index(key);

      {
        const InternString* test_key = ((const InternString**)data)[soa_index];

        if ((test_key == nullptr || test_key == TOMBSTONE) &&
            needs_resize(1)) {
          //need to resize
          try_extend(1);
          //need to reset the key
          soa_index = get_soa_index(key);
        }
      }

      const InternString** const keys = (const InternString**)data;
      T* const vals = (T*)(data + (sizeof(const InternString*) * el_capacity));

      used++;
      keys[soa_index] = key;
      vals[soa_index] = std::move(val);     
    }

     
  }
};