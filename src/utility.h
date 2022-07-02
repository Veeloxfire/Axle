#pragma once

#include <stdint.h>
#include <string.h>
#include <utility>
#include <new>

#include "safe_lib.h"
#include "threading.h"

#define FOR(name, it) \
for(auto it = (name).begin(), JOIN(__end, __LINE__) = (name).end(); \
it < JOIN(__end, __LINE__); it++)

#define FOR_MUT(name, it) \
for(auto it = (name).mut_begin(), JOIN(__end, __LINE__) = (name).mut_end(); \
it < JOIN(__end, __LINE__); it++)

constexpr inline u64 MAX_DECIMAL_U64_DIGITS = sizeof("18446744073709551615") - 1;

constexpr u64 greatest_common_divisor(u64 v1, u64 v2) {
  //Swap to be the correct way around
  if (v1 > v2) {
    const u64 temp = v1;
    v1 = v2;
    v2 = temp;
  }

  while (v2 != 0) {
    const u64 temp = v1 % v2;
    v1 = v2;
    v2 = temp;
  }

  return v1;
}

constexpr u64 lowest_common_multiple(u64 v1, u64 v2) {
  const u64 gcd = greatest_common_divisor(v1, v2);

  return (v1 * v2) / gcd;
}


constexpr inline u64 FNV1_HASH_BASE = 0xcbf29ce484222325;
constexpr inline u64 FNV1_HASH_PRIME = 0x100000001b3;

constexpr uint64_t fnv1a_hash(const char* c, size_t size) {
  uint64_t base = FNV1_HASH_BASE;

  while (size > 0) {
    base ^= *c;
    base *= FNV1_HASH_PRIME;

    c++;
    size--;
  }

  return base;
}

constexpr u64 fnv1a_hash_u16(u64 start, u16 u) {
  //1
  start ^= (u & 0xff);
  start *= FNV1_HASH_PRIME;
  u >>= 8;

  //2
  start ^= (u & 0xff);
  start *= FNV1_HASH_PRIME;

  return start;
}

constexpr u64 fnv1a_hash_u32(u64 start, u32 u) {
  //1
  start ^= (u & 0xff);
  start *= FNV1_HASH_PRIME;
  u >>= 8;

  //2
  start ^= (u & 0xff);
  start *= FNV1_HASH_PRIME;
  u >>= 8;

  //3
  start ^= (u & 0xff);
  start *= FNV1_HASH_PRIME;
  u >>= 8;

  //4
  start ^= (u & 0xff);
  start *= FNV1_HASH_PRIME;

  return start;
}

constexpr u64 fnv1a_hash_u64(u64 start, u64 u) {
  //1
  start ^= (u & 0xff);
  start *= FNV1_HASH_PRIME;
  u >>= 8;

  //2
  start ^= (u & 0xff);
  start *= FNV1_HASH_PRIME;
  u >>= 8;

  //3
  start ^= (u & 0xff);
  start *= FNV1_HASH_PRIME;
  u >>= 8;

  //4
  start ^= (u & 0xff);
  start *= FNV1_HASH_PRIME;
  u >>= 8;

  //5
  start ^= (u & 0xff);
  start *= FNV1_HASH_PRIME;
  u >>= 8;

  //6
  start ^= (u & 0xff);
  start *= FNV1_HASH_PRIME;
  u >>= 8;

  //7
  start ^= (u & 0xff);
  start *= FNV1_HASH_PRIME;
  u >>= 8;

  //8
  start ^= (u & 0xff);
  start *= FNV1_HASH_PRIME;

  return start;
}

constexpr bool can_be_from_sign_extension(uint64_t u64) {
  if ((u64 & 0xFFFFFFFF00000000ull) == 0xFFFFFFFF00000000ull) {
    //Want to extend with 1s
    //Must be a negative value
    return (u64 & 0x80000000) > 0;
  }
  else if ((u64 & 0xFFFFFFFF00000000ull) == 0x0000000000000000ull) {
    //Want to extend with 0s
    //Must be positive value
    return (u64 & 0x80000000) == 0;
  }
  else {
    return false;
  }
}

template<typename T>
constexpr T bit_fill_upper(uint8_t bits) {
  int64_t fill = ((uint64_t)1) << 63;

  fill >>= ((uint64_t)bits - (uint64_t)1 + (uint64_t)(bits == 0));

  return static_cast<T>(fill << (uint8_t)(bits == 0));
}

template<typename T>
constexpr T bit_fill_lower(uint8_t bits) {
  return ~bit_fill_upper<T>(64 - bits);
}

template<typename T, size_t i>
constexpr size_t array_size(T(&)[i]) {
  return i;
}

constexpr size_t ceil_div(size_t x, size_t y) noexcept {
  return x / y + (x % y != 0);
}

//Log 2 for uniform random 64 bit number
constexpr inline uint64_t log_2(uint64_t v) {
  if (v == 0) {
    INVALID_CODE_PATH("MATH ERROR! Cannot log of 0");
  }

  uint64_t max = 63;
  uint64_t min = 0;

  while (max >= min + 1) {
    uint64_t mid = (max + min) / 2;
    if (v > ((uint64_t)1 << mid)) {
      min = mid;
    }
    else {
      max = mid;
    }
  }

  return max;
}

constexpr inline uint64_t pow_16(uint64_t v) {
  return 1ull << (8ull * v);
}

constexpr inline uint64_t pow_10(uint64_t v) {
  if (v > 19) {
    INVALID_CODE_PATH("Power too high!");
    return 0;
  }

  constexpr uint64_t pow10[] ={
    1ull,
    10ull,
    100ull,
    1000ull,
    10000ull,
    100000ull,
    1000000ull,
    10000000ull,
    100000000ull,
    1000000000ull,
    10000000000ull,
    100000000000ull,
    1000000000000ull,
    10000000000000ull,
    100000000000000ull,
    1000000000000000ull,
    10000000000000000ull,
    100000000000000000ull,
    1000000000000000000ull,
    10000000000000000000ull
  };

  return pow10[v];
}

constexpr inline uint64_t log_10_floor(uint64_t v) {
  if (v == 0) {
    INVALID_CODE_PATH("MATH ERROR! Cannot log of 0");
  }

  //Max uint64_t = 18446744073709551615
  //Max pow 10   = 10000000000000000000ull

  auto max = 19;
  auto min = 0;

  if (v >= pow_10(max)) { return max; }

  while (max >= min + 1) {
    auto mid = (max + min) / 2;
    if (v > pow_10(mid)) {
      min = mid;
    }
    else {
      max = mid;
    }
  }

  return max;
}

//Log 2 optimised for small numbers
//Floors the output
constexpr inline uint64_t small_log_2_floor(uint64_t v) {
  if (v == 0) {
    INVALID_CODE_PATH("MATH ERROR! Cannot log of 0");
  }

  uint64_t min = 0;

  while (v > (uint64_t)1 << min) {
    min++;
  }

  return min;
}

constexpr inline uint64_t small_log_2_ceil(uint64_t v) {
  if (v == 0) {
    INVALID_CODE_PATH("MATH ERROR! Cannot log of 0");
  }

  uint8_t found1 = 0;//max value of 1
  uint64_t min = 0;

  while (v > (uint64_t)1 << min) {
    found1 |= (v >> min) & 0b1;//tests if it is a 1 
    min++;
  }

  return min + found1;
}

template<typename T>
constexpr T ceil_to_n(T val, T n) {
  T mod_n = val % n;

  if (mod_n == 0) {
    return val;
  }
  else {
    return val + n - mod_n;
  }
}

template<typename T>
constexpr T ceil_to_8(T val) {
  const T is_zero = val == 0;
  const T val2 = val - 1 + is_zero;
  const T cond8 = is_zero << 3;

  return val2 + 8 - (val2 % 8) - cond8;
}

constexpr inline uint8_t absolute(int8_t i) {
  if (i == INT32_MIN) {
    return static_cast<uint16_t>(INT8_MAX) + 1u;
  }
  else if (i < 0) {
    return static_cast<uint8_t>(-i);
  }
  else {
    return static_cast<uint8_t>(i);
  }
}

constexpr inline uint16_t absolute(int16_t i) {
  if (i == INT32_MIN) {
    return static_cast<uint16_t>(INT16_MAX) + 1u;
  }
  else if (i < 0) {
    return static_cast<uint16_t>(-i);
  }
  else {
    return static_cast<uint16_t>(i);
  }
}

constexpr inline uint32_t absolute(int32_t i) {
  if (i == INT32_MIN) {
    return static_cast<uint32_t>(INT32_MAX) + 1u;
  }
  else if (i < 0) {
    return static_cast<uint32_t>(-i);
  }
  else {
    return static_cast<uint32_t>(i);
  }
}

constexpr inline uint64_t absolute(int64_t i) {
  if (i == INT64_MIN) {
    return 0x8000000000000000ull;
  }
  else if (i < 0) {
    return static_cast<uint64_t>(-i);
  }
  else {
    return static_cast<uint64_t>(i);
  }
}

template<typename T, typename L>
size_t _sort_range_part(T* base, size_t lo, size_t hi, const L& pred) {
  size_t pivot = (hi + lo) / 2llu;

  size_t i = lo;
  size_t j = hi;

  while (pred(base[i], base[pivot])) {
    i += 1;
  }

  while (pred(base[pivot], base[j])) {
    j -= 1;
  }

  if (i >= j) return j;

  //Account for the moving things
  //means we dont have to copy
  if (i == pivot) {
    pivot = j;
  }
  else if (j == pivot) {
    pivot = i;
  }

  {
    T hold = std::move(base[i]);

    base[i] = std::move(base[j]);
    base[j] = std::move(hold);
  }

  while (true) {
    do {
      i += 1;
    } while (pred(base[i], base[pivot]));

    do {
      j -= 1;
    } while (pred(base[pivot], base[j]));

    if (i >= j) return j;

    //Account for the moving things
    //means we dont have to copy
    if (i == pivot) {
      pivot = j;
    }
    else if (j == pivot) {
      pivot = i;
    }

    {
      T hold = std::move(base[i]);

      base[i] = std::move(base[j]);
      base[j] = std::move(hold);
    }
  }

}

template<typename T, typename L>
void _sort_range_impl(T* base, size_t lo, size_t hi, const L& pred) {
  if (lo < hi) {
    size_t p = _sort_range_part(base, lo, hi, pred);
    _sort_range_impl(base, lo, p, pred);
    _sort_range_impl(base, p + 1, hi, pred);
  }
}

template<typename T, typename L>
void sort_range(T* start, T* end, const L& pred) {
  size_t num = (end - start);

  if (num != 0) {
    _sort_range_impl<T, L>(start, 0, num - 1, pred);
  }
}

template<typename T>
struct Array {
  T* data         = nullptr;// ptr to data in the array
  size_t size     = 0;// used size
  size_t capacity = 0;

  //No copy!
  Array(const Array&) = delete;

  Array() noexcept = default;
  Array(Array&& arr) noexcept : data(arr.data), size(arr.size), capacity(arr.capacity)
  {
    arr.data = nullptr;
    arr.size = 0;
    arr.capacity = 0;
  }

  //Array(size_t s) noexcept : data(allocate_default<T>(s)), size(s), capacity(s) {}

  Array& operator=(Array&& arr) noexcept {
    free();

    data = std::exchange(arr.data, nullptr);
    size = std::exchange(arr.size, 0);
    capacity = std::exchange(arr.capacity, 0);

    return *this;
  }

  void free() {
    ::free_destruct_n<T>(data, size);
    data = nullptr;
    size = 0;
    capacity = 0;
  }

  ~Array() noexcept {
    free();
  }

  const T* begin() const { return data; }
  const T* end() const { return data + size; }

  T* back() { return data + size - 1; }

  T* mut_begin() { return data; }
  T* mut_end() { return data + size; }

  T remove_at(const size_t index) {
    ASSERT(index < size);

    T t = std::move(data[index]);

    for (size_t i = index; i < size - 1; i++) {
      data[i] = std::move(data[i + 1]);
    }
    size--;
    return t;
  }

  void insert_at(const size_t index, T&& t) {
    ASSERT(index <= size);
    reserve_extra(1);

    size++;
    for (size_t i = size; (i - 1) > index; i--) {
      data[i - 1] = std::move(data[i - 2]);
    }

    data[index] = std::move(t);
  }

  template<typename L>
  void remove_if(L&& lambda) {
    size_t num_removed = 0;

    for (size_t i = 0; i < size; i++) {
      if (lambda(data[i])) {
        destruct_single(data + i);
        num_removed++;
      }
      else {
        if (num_removed > 0) {
          data[i - num_removed] = std::move(data[i]);
        }
      }
    }
    size -= num_removed;
  }

  template<typename L>
  bool any_of(L&& lambda) const {
    auto i = begin();
    const auto i_end = end();

    for (; i < i_end; i++) {
      if (lambda(i)) {
        return true;
      }
    }

    return false;
  }

  template<typename L>
  const T* find_if(L&& lambda) const {
    auto i = begin();
    const auto i_end = end();

    for (; i < i_end; i++) {
      if (lambda(i)) {
        return i;
      }
    }

    return nullptr;
  }

  void replace_a_with_b(const T& a, const T& b) {
    for (size_t i = 0; i < size; i++) {
      if (data[i] == a) {
        data[i] = b;
      }
    }
  }

  void insert(T t) noexcept {
    try_reserve_next(size + 1);

    new(data + size) T(std::move(t));
    size++;
  }

  void insert_uninit(const size_t num) noexcept {
    if (num > 0) {
      reserve_extra(num);

      default_init<T>(data + size, num);
      size += num;
    }
  }

  //Test for extra space after the size (not capacity)
  void reserve_extra(const size_t extra) noexcept {
    try_reserve_next(size + extra);
  }

  //Test for a total needed space
  void reserve_total(const size_t total) noexcept {
    try_reserve_next(total);
  }

  void try_reserve_next(const size_t total_required) noexcept {
    if (total_required < capacity) return;

    const size_t prev = capacity;

    //Min capacity should be 8
    if (capacity < 8) {
      capacity = 8;

      //8 might still be less that total_required
      while (total_required > capacity) {
        capacity <<= 1;
      }
    }
    else if (total_required > capacity) {
      do {
        capacity <<= 1;
      } while (total_required > capacity);
    }
    else {
      return;
    }

    data = reallocate_default<T>(data, prev, capacity);
  }

  void shrink() noexcept {
    if (size == 0) {
      free();
    }
    else {
      size_t old_cap = capacity;
      capacity = size;
      data = reallocate_default<T>(data, old_cap, capacity);
    }
  }

  void clear() noexcept {
    auto i = mut_begin();
    auto end = mut_end();

    for (; i < end; i++) {
      i->~T();
    }

    //Zero the memory just to be safe
    zero_init(data, capacity);

    size = 0;
  }

  void pop() noexcept {
    size--;
    (data + size)->~T();
  }

  void pop(size_t num) noexcept {
    const auto* old_end = data + size;

    if (num > size) {
      size = 0;
    }
    else {
      size -= num;
    }

    auto* i = data + size;

    for (; i < old_end; i++) {
      i->~T();
    }
  }

  bool contains(const T& t) const noexcept {
    auto i = begin();
    const auto end_i = end();

    for (; i < end_i; i++) {
      if (*i == t) {
        return true;
      }
    }

    return false;
  }

  void concat(Array<T>&& arr) noexcept {
    reserve_extra(arr.size);
    T* start = data + size;
    FOR_MUT(arr, it) {
      *start = std::move(*it);
      start += 1;
    }

    size += arr.size;
    arr.free();
  }

  void concat(const T* arr, size_t N) noexcept {
    reserve_extra(N);
    memcpy_ts(data + size, (capacity - size), arr, N);

    size += N;
  }
};

template<typename T>
struct SparseHash;

template<typename K, typename V, typename HASH = SparseHash<K>>
struct SparseHashSet {
  enum struct ENTRY_TYPE : u8 {
    FILLED, EMPTY, TOMBSTONE
  };

  using Key = K;
  using Value = V;

  struct KeyEntry {
    ENTRY_TYPE type;
    Key key;
  };

  constexpr static float LOAD_FACTOR = 0.75;

  uint8_t* data      = nullptr;// ptr to data in the array
  size_t el_capacity = 0;//number of elements
  size_t used        = 0;

  constexpr bool needs_resize(size_t extra) const {
    return (el_capacity * LOAD_FACTOR) <= (used + extra);
  }

  constexpr const KeyEntry* key_entry_arr() const {
    return (const KeyEntry*)data;
  }

  constexpr Value* val_arr() const {
    return (Value*)(data + (el_capacity * sizeof(KeyEntry)));
  }

  ~SparseHashSet() {
    {
      const KeyEntry* keys = key_entry_arr();
      Value* vals = val_arr();

      for (size_t i = 0; i < el_capacity; i++) {
        if (keys[i].type == ENTRY_TYPE::FILLED) {
          vals[i].~Value();
        }
      }
    }

    free_no_destruct(data);

    data = nullptr;
    el_capacity = 0;
    used = 0;
  }

  bool contains(const Key* key) const {
    if (el_capacity == 0) return false;

    const KeyEntry* keys = key_entry_arr();

    const auto hash_val = HASH::hash(key);

    size_t index = hash_val % el_capacity;

    const KeyEntry* test_key = keys + index;
    while (true) {
      if (test_key->type != ENTRY_TYPE::FILLED) {
        return false;
      }

      if (test_key->key == *key) {
        return true;
      }

      index++;
      index %= el_capacity;
      test_key = keys + index;
    }
  }

  size_t get_soa_index(const Key* key) const {
    const KeyEntry* hash_arr = key_entry_arr();

    bool found_tombstone = false;
    size_t tombstone_index = 0;

    const auto hash_val = HASH::hash(key);
    size_t index = hash_val % el_capacity;

    const KeyEntry* test_key = hash_arr + index;
    while (test_key->type != ENTRY_TYPE::EMPTY) {
      if (test_key->type == ENTRY_TYPE::FILLED && test_key->key == *key) {
        return index;
      }
      else if (test_key->type == ENTRY_TYPE::TOMBSTONE && !found_tombstone) {
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
        * (sizeof(KeyEntry) + sizeof(Value));

      data = allocate_default<uint8_t>(required_alloc_bytes);

      const KeyEntry* hash_arr = (const KeyEntry*)data;
      Value* val_arr = (Value*)(data + el_capacity * sizeof(KeyEntry));

      const KeyEntry* old_hash_arr = (const KeyEntry*)old_data;
      Value* old_val_arr = (Value*)(old_data + old_el_cap * sizeof(KeyEntry));

      for (size_t i = 0; i < old_el_cap; i++) {
        const KeyEntry* key = old_hash_arr + i;

        if (key->type == ENTRY_TYPE::FILLED) {
          const size_t new_index = get_soa_index(key);

          hash_arr[new_index] = key;
          val_arr[new_index]  = std::move(old_val_arr[i]);
        }
      }


      //Dont need to destruct old values as they've been moved
      free_no_destruct(old_data);
    }
  }

  Value* get_val(const Key* const key) const {
    if (el_capacity == 0) return nullptr;

    const size_t soa_index = get_soa_index(key);

    {
      const KeyEntry* test_key = key_entry_arr() + soa_index;

      if (test_key->type != ENTRY_TYPE::FILLED) {
        return nullptr;
      }
    }

    return val_arr() + soa_index;
  }

  void insert(const Key* const key, Value&& val) {
    if (el_capacity == 0) {
      el_capacity = 8;
      data = allocate_default<uint8_t>(8 * (sizeof(KeyEntry) + sizeof(Value)));

      size_t soa_index = get_soa_index(key);

      const KeyEntry* const keys = key_entry_arr();
      Value* const vals = val_arr();

      used++;
      keys[soa_index] = key;
      vals[soa_index] = std::move(val);
    }
    else {
      size_t soa_index = get_soa_index(key);

      {
        const KeyEntry* test_key = key_entry_arr() + soa_index;

        if (test_key->type != ENTRY_TYPE::FILLED && needs_resize(1)) {
          //need to resize
          try_extend(1);
          //need to reset the key
          soa_index = get_soa_index(key);
        }
      }

      const KeyEntry* const keys = key_entry_arr();
      Value* const vals = val_arr();

      used++;
      keys[soa_index] = key;
      vals[soa_index] = std::move(val);
    }
  }

  Value* insert(const Key* const key) {
    if (el_capacity == 0) {
      el_capacity = 8;
      data = allocate_default<uint8_t>(8 * (sizeof(KeyEntry) + sizeof(Value)));

      size_t soa_index = get_soa_index(key);

      const KeyEntry* const keys = key_entry_arr();

      used++;
      keys[soa_index] = *key;
    }
    else {
      size_t soa_index = get_soa_index(key);

      {
        const KeyEntry* test_key = key_entry_arr() + soa_index;

        if (test_key->type != ENTRY_TYPE::FILLED && needs_resize(1)) {
          //need to resize
          try_extend(1);
          //need to reset the key
          soa_index = get_soa_index(key);
        }
      }

      const KeyEntry* const keys = key_entry_arr();

      used++;
      keys[soa_index] = *key;
    }
  }
};

template<typename T>
struct BucketArray {
  struct BLOCK {
    constexpr static size_t BLOCK_SIZE = 32;

    size_t filled = 0;
    BLOCK* next = nullptr;

    T data[BLOCK_SIZE];

    ~BLOCK() {
      free_destruct_single<BLOCK>(next);
    }
  };

  struct Iter {
    size_t index = 0;
    BLOCK* block = nullptr;

    Iter() = default;

    void next() {
      index++;
      if (index == BLOCK::BLOCK_SIZE) {
        index = 0;
        block = block->next;
      }
      else if (index == block->filled) {
        index = 0;
        block = nullptr;
      }
    }

    T* get() {
      return block->data + index;
    }

    bool operator!=(const Iter& i) const {
      return (index != i.index) || (block != i.block);
    }
    bool operator==(const Iter& i) const {
      return (index == i.index) && (block == i.block);
    }
  };

  struct ConstIter {
    size_t index = 0;
    const BLOCK* block = nullptr;

    ConstIter() = default;

    void next() {
      index++;
      if (index == BLOCK::BLOCK_SIZE) {
        index = 0;
        block = block->next;
      }
      else if (index == block->filled) {
        index = 0;
        block = nullptr;
      }
    }

    const T* get() const {
      return block->data + index;
    }

    bool operator!=(const ConstIter& i) const {
      return (index != i.index) || (block != i.block);
    }
    bool operator==(const ConstIter& i) const {
      return (index == i.index) && (block == i.block);
    }
  };

  Iter begin_iter() {
    if (first->filled == 0) {
      return Iter();
    }
    else {
      return Iter{ 0, first };
    }
  }

  Iter end_iter() {
    return Iter();
  }

  ConstIter begin_const_iter() const {
    if (first->filled == 0) {
      return ConstIter();
    }
    else {
      return ConstIter{ 0, first };
    }
  }

  ConstIter end_const_iter() const {
    return ConstIter();
  }

  BLOCK* first = nullptr;
  BLOCK* last = nullptr;

  BucketArray() : first(allocate_default<BLOCK>()), last(first) {}
  ~BucketArray() {
    free_destruct_single<BLOCK>(first);
  }

  template<typename ... U>
  T* insert(U&& ... u) {
    if (last->filled == BLOCK::BLOCK_SIZE) {
      last->next = allocate_default<BLOCK>();
      last = last->next;
    }

    new (last->data + last->filled) T(std::forward<U>(u)...);
    last->filled++;

    return last->data + last->filled - 1;
  }
};

#define ARENA_ALLOCATOR_DEBUG

struct ArenaAllocator {
  static_assert(sizeof(void*) == sizeof(uint64_t), "Must be 8 bytes");

  struct Block {
    constexpr static size_t BLOCK_SIZE = 128;
    uint64_t data[BLOCK_SIZE] ={};

    Block* next = nullptr;

    Block() = default;
    ~Block();
  };

  struct FreeList {
    uint64_t qwords_available = 0;
    FreeList* next = nullptr;
  };

#ifdef ARENA_ALLOCATOR_DEBUG
  Array<void*> allocated;
#else
  Block* base = nullptr;
  FreeList* free_list = nullptr;
#endif

  ArenaAllocator() = default;
  ~ArenaAllocator();

  bool _debug_freelist_loops() const;
  bool _debug_valid_pointer(void* ptr) const;
  bool _debug_is_allocated_data(uint64_t* ptr, usize len) const;

  void new_block();
  void add_to_free_list(FreeList* fl);

  uint8_t* alloc_no_construct(size_t bytes);
  void free_no_destruct(void* val);

  template<typename T>
  inline T* alloc_no_construct() {
    return (T*)alloc_no_construct(sizeof(T));
  }
};

struct BumpAllocator {
  struct BLOCK {
    constexpr static size_t BLOCK_SIZE = 1024;

    size_t filled = 0;
    BLOCK* prev = nullptr;

    uint8_t data[BLOCK_SIZE] ={};
  };

  BLOCK* top = nullptr;

  BumpAllocator();

  ~BumpAllocator();

  void new_block();
  uint8_t* allocate_no_construct(size_t bytes);
};

template<typename T>
struct FreelistBlockAllocator {
  struct Element;

  struct Header {
    Element* next = nullptr;
  };

  struct Element {
    union {
      Header header ={};
      T el;
    };

    Element() : header() {}
    ~Element() {}
  };

  struct BLOCK {
    constexpr static size_t BLOCK_SIZE = 32;


    //size_t filled = 0;
    BLOCK* prev = nullptr;
    Element data[BLOCK_SIZE] ={};
  };

  BLOCK* top;
  Element* alloc_list;

  FreelistBlockAllocator()
    : top(::allocate_default<BLOCK>()) {
    alloc_list = top->data;

    for (auto i = 0; i < BLOCK::BLOCK_SIZE - 1; i++) {
      top->data[i].header.next = top->data + i + 1;
    }

    top->data[BLOCK::BLOCK_SIZE - 1].header.next = nullptr;
  }

  ~FreelistBlockAllocator() {
    while (top != nullptr) {
      BLOCK* next = top->prev;
      free_destruct_single<BLOCK>(top);
      top = next;
    }

    top = nullptr;
    alloc_list = nullptr;
  }

  void new_block() {
    BLOCK* const new_b = ::allocate_default<BLOCK>();

    new_b->prev = top;
    top = new_b;


    for (auto i = 0; i < BLOCK::BLOCK_SIZE - 1; i++) {
      new_b->data[i].header.next = new_b->data + i + 1;
    }

    new_b->data[BLOCK::BLOCK_SIZE - 1].header.next = alloc_list;
    alloc_list = new_b->data;
  }

  T* allocate() {
    if (alloc_list == nullptr) {
      new_block();
    }

    T* const new_t = (T*)alloc_list;
    alloc_list = alloc_list->header.next;

    new(new_t) T();

    return new_t;
  }

  bool _debug_valid_free_ptr(const T* const t) const {
    BLOCK* check = top;

    const uint8_t* const as_ptr = (const uint8_t*)t;

    while (check != nullptr) {
      const uint8_t* const block_ptr = (const uint8_t*)check->data;
      if (as_ptr >= block_ptr && as_ptr < (block_ptr + (sizeof(Element) * BLOCK::BLOCK_SIZE))) {
        //is it part of this block

        const size_t diff = as_ptr - block_ptr;
        if (diff % sizeof(Element) == 0) {
          //Is aligned properly
          return true;
        }
        else {
          return false;
        }

      }

      //Not in this one try next
      check = check->prev;
    }

    //Not in any
    return false;
  }

  bool _debug_all_free() const {
    size_t alloc_list_length = 0;
    {
      const Element* next_alloc = alloc_list;

      while (next_alloc != nullptr) {
        alloc_list_length++;
        next_alloc = next_alloc->header.next;
      }
    }

    size_t expected_length = 0;
    {
      const BLOCK* block = top;
      while (block != nullptr) {
        expected_length += BLOCK::BLOCK_SIZE;
        block = block->prev;
      }
    }

    return alloc_list_length == expected_length;
  }


  void free(const T* t) {
    ASSERT(_debug_valid_free_ptr(t));

    destruct_single(t);

    Element* new_e = (Element*)t;
    new_e->el.~T();
    new_e->header.next = alloc_list;

    alloc_list = new_e;
  }
};

struct SquareBitMatrix {
  //Per block: ceil(side_length / 8) bytes = (side_length / 8) + 1

  //Block length = (side_length / 8) + 1
  //Block pointer = data + (Block length * val)
  //each bit corresponds to its equivalent value at that bit index

  uint8_t* data = nullptr;
  size_t side_length = 0;
  size_t capacity = 0;

  void free();

  ~SquareBitMatrix();

  constexpr static size_t bytes_per_val_per_side(size_t side_length) {
    return side_length == 0 ? 0
      : ((side_length - 1) / 8) + 1;
  }

  bool test_a_intersects_b(size_t a, size_t b) const;
  void set_a_intersects_b(size_t a, size_t b);
  void remove_a_intersects_b(size_t a, size_t b);

  size_t new_value();
};

template<typename T>
struct Queue {
  T* holder;
  usize start;
  usize size;
  usize capacity;

  //No copy!
  Queue(const Queue&) = delete;

  Queue(Queue&& q) noexcept : holder(q.holder), start(q.start), size(q.size), capacity(q.capacity)
  {
    q.holder = nullptr;
    q.start = 0;
    q.size = 0;
    q.capacity = 0;
  }

  Queue() noexcept = default;

  Queue& operator=(Queue&& q) noexcept {
    free();

    holder = std::exchange(q.holder, nullptr);
    start = std::exchange(q.start, 0);
    size = std::exchange(q.size, 0);
    capacity = std::exchange(q.capacity, 0);

    return *this;
  }

  void free() {
    if (start + size > capacity) {
      usize temp = capacity - start;
      destruct_arr<T>(holder + start, temp);
      destruct_arr<T>(holder + start, size - temp);
    }
    else {
      destruct_arr<T>(holder + start, size);
    }

    ::free_no_destruct(holder);
    holder = nullptr;
    start = 0;
    size = 0;
    capacity = 0;
  }

  ~Queue() noexcept {
    free();
  }

  //Does the index taking into account wrapping
  usize _ptr_index(usize i) const {
    return (start + i) % capacity;
  }

  void push_front(T t) {
    if (size == capacity) {
      extend();
    }

    if (start == 0) {
      start = capacity - 1;
    }
    else {
      start--;
    }
    size++;

    new(holder + start) T(std::move(t));
  }

  T pop_front() {
    ASSERT(size > 0);

    T val = std::move(holder[start]);
    start++;
    size--;
    
    if (start >= capacity) {
      start -= capacity;
    }

    return val;
  }

  void push_back(T t) {
    if (size == capacity) {
      extend();
    }

    new(holder + _ptr_index(size)) T(std::move(t));
    size++;
  }

  T pop_back() {
    ASSERT(size > 0);

    size--;
    T val = std::move(holder[_ptr_index(size)]);

    return val;
  }

  void extend() {
    if (capacity == 0) {
      holder = allocate_default<T>(8);
      capacity = 8;
      return;
    }

    usize new_cap = capacity << 1;

    T* new_holder = allocate_default<T>(new_cap);

    usize i = 0;
    usize end_i = size + 1;
    for (; i < end_i; i++) {
      new_holder[i] = std::move(holder[_ptr_index(i)]);
    }

    free_no_destruct(holder);
    holder = new_holder;
    capacity = new_cap;

    start = 0;
  }

  void shrink() {
    if (size == 0) {
      free();
      return;
    }

    usize new_cap = size;

    T* new_holder = allocate_default<T>(new_cap);

    usize i = 0;
    usize end_i = size + 1;
    for (; i < end_i; i++) {
      new_holder[i] = std::move(holder[_ptr_index(i)]);
    }

    free_no_destruct(holder);
    holder = new_holder;
    capacity = new_cap;

    start = 0;
  }
};

template<typename T>
struct AtomicQueue {
  SpinLockMutex mutex;

  T* holder;
  usize start;
  volatile __int64 atomic_size;
  usize capacity;

  //No copy!
  AtomicQueue(const AtomicQueue&) = delete;

  AtomicQueue() noexcept = default;

  void free() {
    usize size = (usize)_InterlockedOr64(&atomic_size, 0);
    if (start + size > capacity) {
      usize temp = capacity - start;
      destruct_arr<T>(holder + start, temp);
      destruct_arr<T>(holder + start, size - temp);
    }
    else {
      destruct_arr<T>(holder + start, size);
    }

    ::free_no_destruct(holder);
    holder = nullptr;
    start = 0;
    _InterlockedExchange64(&atomic_size, 0);
    capacity = 0;
  }

  ~AtomicQueue() noexcept {
    free();
  }

  //Does the index taking into account wrapping
  usize _ptr_index(usize i) const {
    return (start + i) % capacity;
  }

  bool is_immediately_empty() {
    return (usize)_InterlockedOr64(&atomic_size, 0) == 0;
  }

  //returns true if there is a value in out_t
  bool try_pop_front(T* out_t) {
    mutex.acquire();
    if (is_immediately_empty()) {
      mutex.release();
      return false;
    }

    _InterlockedDecrement64(&atomic_size);
    *out_t = std::move(holder[start]);
    start++;

    if (start >= capacity) {
      start -= capacity;
    }

    mutex.release();
    return true;
  }

  void push_back(T t) {
    mutex.acquire();
    if ((usize)_InterlockedOr64(&atomic_size, 0) == capacity) {
      extend();
    }

    usize s = (usize)_InterlockedIncrement64(&atomic_size);

    new(holder + _ptr_index(s - 1)) T(std::move(t));
    mutex.release();
  }

  void extend() {
    if (capacity == 0) {
      holder = allocate_default<T>(8);
      capacity = 8;
      return;
    }

    usize new_cap = capacity << 1;

    T* new_holder = allocate_default<T>(new_cap);

    usize i = 0;
    usize end_i = (usize)_InterlockedOr64(&atomic_size, 0) + 1llu;
    for (; i < end_i; i++) {
      new_holder[i] = std::move(holder[_ptr_index(i)]);
    }

    free_no_destruct(holder);
    holder = new_holder;
    capacity = new_cap;

    start = 0;
  }
};

template<typename T>
struct OwnedPtr {
  T* ptr = nullptr;
  OwnedPtr(const OwnedPtr&) = delete;

  OwnedPtr() = default;

  OwnedPtr(OwnedPtr&& ptr_in) noexcept : ptr(ptr_in.ptr) {
    ptr_in.ptr = nullptr;
  }

  OwnedPtr& operator=(OwnedPtr&& ptr_in) noexcept {
    free_no_destruct();
    ptr = ptr_in.ptr;
    ptr_in.ptr = nullptr;
    return *this;
  }

  OwnedPtr(T* ptr_in) : ptr(ptr_in) {}

  OwnedPtr(Array<T>&& arr) {
    arr.shrink();
    ptr = arr.data;

    arr.data     = nullptr;
    arr.size     = 0;
    arr.capacity = 0;
  }

  ~OwnedPtr() {
    free_no_destruct();
  }

  void free_no_destruct() {
    ::free_no_destruct<T>(ptr);
    ptr = nullptr;
  }
};

//template<typename T>
//struct LinkedList {
//
//  struct LinkedNode;
//
//  T* alloc() {
//
//  }
//
//  void remove(T* t) {
//
//  }
//};

template<typename U, typename T>
OwnedPtr<U> cast_ptr(OwnedPtr<T>&& ptr) {
  OwnedPtr<U> u ={};
  u.ptr = (U*)ptr.ptr;
  ptr.ptr = nullptr;
  return u;
}

template<typename T>
void copy_array(const Array<T>& from, Array<T>& to) noexcept {
  to.clear();
  to.reserve_total(from.size);

  for (size_t i = 0; i < from.size; i++) {
    to.data[i] = from.data[i];
  }

  to.size += from.size;
}

template<typename T>
void combine_unique(const Array<T>& from, Array<T>& to) noexcept {
  auto i = from.begin();
  const auto end = from.end();

  const size_t initial_size = to.size;
  for (; i < end; i++) {
    for (size_t i_to = 0; i_to < initial_size; i_to++) {
      if (*i == to.data[i_to]) {
        goto NEXT_COMBINE;
      }
    }

    to.insert(*i);

  NEXT_COMBINE:
    continue;
  }
}

template<typename T>
void reverse_array(Array<T>& arr) noexcept {
  if (arr.size == 0) { return; }

  auto* i_beg  = arr.data;
  auto* i_back = arr.data + arr.size - 1;

  //Swap first with back - then step inwards
  while (i_beg < i_back) {
    T temp  = std::move(*i_beg);
    *i_beg  = std::move(*i_back);
    *i_back = std::move(temp);

    i_beg++;
    i_back--;
  }
}

#define ERROR_CODES_X \
modify(OK)\
modify(COULD_NOT_CREATE_FILE)\
modify(COULD_NOT_OPEN_FILE)\
modify(COULD_NOT_CLOSE_FILE)\
modify(UNDEFINED_INSTRUCTION)\
modify(STACK_OVERFLOW)

enum struct ErrorCode : uint8_t {
#define modify(NAME) NAME,
  ERROR_CODES_X
#undef modify
};

namespace ErrorCodeString {
#define modify(NAME) inline constexpr char NAME[] = #NAME;
  ERROR_CODES_X
  #undef modify
}

constexpr const char* error_code_string(const ErrorCode code) {
  switch (code) {
  #define modify(NAME) case ErrorCode:: ## NAME : return ErrorCodeString:: ## NAME ;
    ERROR_CODES_X
    #undef modify
  }

  return nullptr;
}


template<typename T>
void serialise_to_array(Array<uint8_t>& bytes, const T& t) {
  bytes.reserve_extra(sizeof(T));
  memcpy_ts<uint8_t>(bytes.data + bytes.size, bytes.capacity - bytes.size,
                     (const uint8_t*)&t, sizeof(T));
  bytes.size += sizeof(T);
}

template<typename T>
void load_to_bytes(uint8_t* bytes, size_t bytes_size, size_t offset, const T& t) {
  memcpy_ts((T*)bytes + offset, bytes_size - offset, &t, 1);
}

void load_to_bytes(Array<uint8_t>& bytes,
                   const size_t offset,
                   const uint8_t* in_bytes,
                   const size_t len);

constexpr inline void load_to_bytes(uint8_t* bytes, size_t bytes_size,
                                    const size_t offset,
                                    const uint8_t* in_bytes,
                                    const size_t len) {
  memcpy_ts(bytes + offset, bytes_size - offset, in_bytes, len);
}

namespace BIT_MASKS {
  enum BIT_MASKS : uint8_t {
    B0 = 0b0000'0001,
    B1 = 0b0000'0010,
    B2 = 0b0000'0100,
    B3 = 0b0000'1000,
    B4 = 0b0001'0000,
    B5 = 0b0010'0000,
    B6 = 0b0100'0000,
    B7 = 0b1000'0000,
  };
}

#define SET_MASK(val, mask) (val |= mask)
#define RESET_MASK(val, mask) (val &= ~mask)
#define TEST_MASK(val, mask) ((val & mask) != 0)

template<typename T>
constexpr T combine_flag(const T full, const T mask, const bool set) {
  return (full & ~mask) | (mask * set);
}

#define COMBINE_FLAG(full, mask, set) (combine_flag(full, mask, set))
#define SET_FLAG(full, mask, set) (full = COMBINE_FLAG(full, mask, set))



union X64_UNION {
  uint64_t val = 0;
  int64_t sig_val;
  double flt;
  void* vptr;

  constexpr X64_UNION() = default;
  constexpr X64_UNION(X64_UNION&& v) = default;
  constexpr X64_UNION(const X64_UNION& v) = default;

  constexpr X64_UNION& operator=(X64_UNION&& v) = default;
  constexpr X64_UNION& operator=(const X64_UNION& v) = default;

  constexpr X64_UNION(uint64_t v) : val(v) {}
  constexpr X64_UNION(int64_t v) : sig_val(v) {}
  constexpr X64_UNION(double v) : flt(v) {}

  template<typename T>
  constexpr X64_UNION(T* v) : vptr((void*)v) {}

  inline constexpr operator uint64_t() const {
    return val;
  }

  inline constexpr operator int64_t() const {
    return sig_val;
  }

  inline constexpr operator double() const {
    return flt;
  }

  template<typename T>
  inline constexpr operator T* () const {
    return (T*)vptr;
  }
};

//TODO: Big endian??
inline constexpr X64_UNION x64_from_bytes(const uint8_t* const bytes) noexcept {
  X64_UNION val;
  val.val = (static_cast<uint64_t>(bytes[7]) << 56)
    | (static_cast<uint64_t>(bytes[6]) << 48)
    | (static_cast<uint64_t>(bytes[5]) << 40)
    | (static_cast<uint64_t>(bytes[4]) << 32)
    | (static_cast<uint64_t>(bytes[3]) << 24)
    | (static_cast<uint64_t>(bytes[2]) << 16)
    | (static_cast<uint64_t>(bytes[1]) << 8)
    |  static_cast<uint64_t>(bytes[0]);

  return val;
}

inline constexpr ErrorCode x64_from_bytes_s(const uint8_t* buffer,
                                            size_t buffer_length,
                                            X64_UNION& uint,
                                            uint8_t* const bytes) noexcept {
  if (bytes >= buffer && buffer + buffer_length - 8 <= bytes) {
    //safe
    uint = x64_from_bytes(bytes);
    return ErrorCode::OK;
  }
  else {
    return ErrorCode::STACK_OVERFLOW;
  }
}

inline constexpr uint32_t x32_from_bytes(const uint8_t* const bytes) noexcept {
  return (static_cast<uint32_t>(bytes[3]) << 24)
    | (static_cast<uint32_t>(bytes[2]) << 16)
    | (static_cast<uint32_t>(bytes[1]) << 8)
    |  static_cast<uint32_t>(bytes[0]);
}

inline constexpr uint16_t x16_from_bytes(const uint8_t* const bytes) noexcept {
  return (static_cast<uint16_t>(bytes[1]) << 8)
    |  static_cast<uint16_t>(bytes[0]);
}

//TODO: Big endian??
inline constexpr void x64_to_bytes(const X64_UNION uint, uint8_t* const bytes) noexcept {
  bytes[7] = static_cast<uint8_t>(uint.val >> 56);
  bytes[6] = static_cast<uint8_t>(uint.val >> 48);
  bytes[5] = static_cast<uint8_t>(uint.val >> 40);
  bytes[4] = static_cast<uint8_t>(uint.val >> 32);
  bytes[3] = static_cast<uint8_t>(uint.val >> 24);
  bytes[2] = static_cast<uint8_t>(uint.val >> 16);
  bytes[1] = static_cast<uint8_t>(uint.val >> 8);
  bytes[0] = static_cast<uint8_t>(uint.val);
}

inline constexpr void x32_to_bytes(const uint32_t uint, uint8_t* const bytes) noexcept {
  bytes[3] = static_cast<uint8_t>(uint >> 24);
  bytes[2] = static_cast<uint8_t>(uint >> 16);
  bytes[1] = static_cast<uint8_t>(uint >> 8);
  bytes[0] = static_cast<uint8_t>(uint);
}

inline constexpr void x16_to_bytes(const uint16_t uint, uint8_t* const bytes) noexcept {
  bytes[1] = static_cast<uint8_t>(uint >> 8);
  bytes[0] = static_cast<uint8_t>(uint);
}

inline constexpr ErrorCode x64_to_bytes_s(const uint8_t* buffer,
                                          size_t buffer_length,
                                          const X64_UNION uint,
                                          uint8_t* const bytes) noexcept {
  if (bytes >= buffer && buffer + buffer_length - 8 <= bytes) {
    //safe
    x64_to_bytes(uint, bytes);
    return ErrorCode::OK;
  }
  else {
    return ErrorCode::STACK_OVERFLOW;
  }
}

template<typename RET, typename ... PARAMS>
struct FUNCTION_PTR_IMPL {
  FUNCTION_PTR_IMPL() = delete;

  using TYPE = RET(*)(PARAMS...);
};

template<typename RET, typename ... PARAMS>
using FUNCTION_PTR = typename FUNCTION_PTR_IMPL<RET, PARAMS...>::TYPE;



template<typename T>
struct MEMBER {
  MEMBER() = delete;

  template<typename RET, typename ... PARAMS>
  struct FUNCTION_PTR_IMPL {
    FUNCTION_PTR_IMPL() = delete;

    using TYPE = RET(T::*)(PARAMS...);
  };

  template<typename RET, typename ... PARAMS>
  using FUNCTION_PTR = typename FUNCTION_PTR_IMPL<RET, PARAMS...>::TYPE;
};


template<typename T>
using DESTRUCTOR = FUNCTION_PTR<void, T>;

template<typename T>
constexpr inline DESTRUCTOR<T> get_destructor() {
  return &destruct_single<T>;
}


template<typename T>
constexpr inline T square(T t) { return t * t; }

template<typename T>
constexpr inline T larger(T t1, T t2) noexcept {
  return t1 > t2 ? t1 : t2;
}

template<typename T>
constexpr inline T smaller(T t1, T t2) noexcept {
  return t1 < t2 ? t1 : t2;
}

void print_as_bytes(const uint8_t* bytes, size_t length);

template<typename T>
struct EXECUTE_AT_END {
  T t;

  constexpr EXECUTE_AT_END(T&& t_) : t(std::move(t_)) {}

  ~EXECUTE_AT_END() {
    t();
  }
};

template<typename T>
EXECUTE_AT_END(T&& t)->EXECUTE_AT_END<T>;

#define DEFER(...) EXECUTE_AT_END JOIN(defer, __LINE__) = [__VA_ARGS__]() mutable ->void 

namespace IO {
  void print_impl(const char* string);
  void print_impl(const OwnedPtr<char>& string);
  void print_impl(const char c);

  void err_print_impl(const char* string);
  void err_print_impl(const OwnedPtr<char>& string);
  void err_print_impl(const char c);

  template<typename ... T>
  void print(const T& ... t) {
    (print_impl(t), ...);
  }

  template<typename ... T>
  void err_print(const T& ... t) {
    (err_print_impl(t), ...);
  }
}

#define DO_NOTHING ((void)0)

template<typename T, typename U>
struct IS_SAME_TYPE_IMPL {
  constexpr static bool test = false;
};

template<typename T>
struct IS_SAME_TYPE_IMPL<T, T> {
  constexpr static bool test = true;
};

template<typename T, typename U>
inline constexpr bool IS_SAME_TYPE = IS_SAME_TYPE_IMPL<T, U>::test;

constexpr bool slow_string_eq(const char* str1, const char* str2) {
  while (str1[0] != '\0' && str2[0] != '\0') {
    if (str1[0] != str2[0]) { return false; }

    str1++;
    str2++;
  }

  return str1[0] == str2[0];//both are '\0'
}

#ifdef NDEBUG
#define assert_if(cond, expr) ((void)0)
#else
#define assert_if(cond, expression) if(cond) ASSERT(expression)
#endif

void serialize_bytes(Array<u8>& bytes, const u8* data, usize size, usize alignment);
void serialize_zeros(Array<u8>& bytes, usize size, usize alignment);

template<typename T>
inline void serialize_structs(Array<u8>& bytes, const T* data, usize num) {
  serialize_bytes(bytes, (u8*)data, sizeof(T) * num, alignof(T));
}

template<typename T>
inline void serialize_struct(Array<u8>& bytes, const T* data) {
  serialize_bytes(bytes, (u8*)data, sizeof(T), alignof(T));
}
