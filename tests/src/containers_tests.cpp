#include "tests.h"

constexpr int RANDOM_ARR[] = {
  65, 55, 71, 23, 92,
  57, 51, 84, 86, 30,
  1, 37, 100, 97, 86,
  14, 98, 38, 88, 45,
};

constexpr int SORTED_RANDOM_ARR[] = {
  1, 14, 23, 30, 37,
  38, 45, 51, 55, 57,
  65, 71, 84, 86, 86,
  88, 92, 97, 98, 100
};


constexpr usize RANDOM_ARR_SIZE = array_size(RANDOM_ARR);

TEST_FUNCTION(Util_Array, Insert_Remove) {
  Array<usize> a = {};

  for (usize i = 0; i < 1000; i++) {
    a.insert(i ^ (i + 1));
  }

  TEST_EQ((usize)1000, a.size);
  TEST_EQ((usize)(999 ^ 1000), *a.back());

  for (usize i = 0; i < 1000; i++) {
    TEST_EQ((i ^ (i + 1)), a.data[i]);
  }

  {
    auto b = a.begin();
    const auto end = a.end();
    for (usize i = 0; i < 1000; i++) {
      TEST_EQ((i ^ (i + 1)), *b);
      b++;
    }

    TEST_EQ(b, end);

    b = a.mut_begin();

    for (usize i = 0; i < 1000; i++) {
      TEST_EQ((i ^ (i + 1)), *b);
      b++;
    }

    TEST_EQ(end, b);
  }


  a.remove_at(500);

  TEST_EQ((usize)999, a.size);
  TEST_EQ((usize)(999 ^ 1000), a.data[998]);
  TEST_EQ((usize)(499 ^ 500), a.data[499]);
  TEST_EQ((usize)(501 ^ 502), a.data[500]);

  a.insert_at(500, (500 ^ 501));

  for (usize i = 0; i < 1000; i++) {
    TEST_EQ((i ^ (i + 1)), a.data[i]);
  }

  a.insert_at(0, 0);
  TEST_EQ((usize)0, a.data[0]);
  a.remove_at(0);
  a.insert_at(1000, 0);
  TEST_EQ((usize)0, a.data[1000]);
  a.remove_at(1000);

  for (usize i = 0; i < 1000; i++) {
    TEST_EQ((i ^ (i + 1)), a.data[i]);
  }

  for (usize i = 0; i < 1000; i++) {
    a.pop();
  }

  TEST_EQ((usize)0, a.size);
}

TEST_FUNCTION(Util_Queue, Insert_Remove) {
  Queue<usize> a = {};

  for (usize i = 0; i < 100; i++) {
    a.push_back(i ^ (i + 1));
  }

  TEST_EQ((usize)100, a.size);

  for (usize i = 0; i < 100; i++) {
    TEST_EQ((i ^ (i + 1)), a.pop_front());
  }

  for (usize i = 0; i < 100; i++) {
    a.push_front(i ^ (i + 1));
  }

  TEST_EQ((usize)100, a.size);

  TEST_EQ((usize)(0 ^ 1), a.pop_back());

  for (usize i = 1; i < 100; i++) {
    TEST_EQ((i ^ (i + 1)), a.pop_back());
  }
}

TEST_FUNCTION(Util, sort) {
  Array<int> ints = {};

  for (int i : RANDOM_ARR) {
    ints.insert(i);
  }

  sort_range(ints.mut_begin(), ints.mut_end(), [](int l, int r) { return l < r; });

  TEST_ARR_EQ(SORTED_RANDOM_ARR, RANDOM_ARR_SIZE, ints.data, ints.size);
}

struct CheckDelete {
  int* i;

  CheckDelete() = default;
  CheckDelete(int* o) : i(o) {}
  CheckDelete(CheckDelete&& o) noexcept : i(std::exchange(o.i, nullptr)) {}

  ~CheckDelete() {
    if (i != nullptr) {
      *i += 1;
    }
  }
};

TEST_FUNCTION(Util_OwnedArr, bake) {
  Array<int> ints = {};

  for (int i : RANDOM_ARR) {
    ints.insert(i);
  }

  OwnedArr<int> ints_arr = bake_arr(std::move(ints));

  static_assert(IS_SAME_TYPE<decltype(ints_arr[0]), int&>, "Must return ref");

  TEST_EQ((int*)nullptr, ints.data);
  TEST_EQ((usize)0, ints.size);
  TEST_EQ((usize)0, ints.capacity);
  TEST_ARR_EQ(RANDOM_ARR, RANDOM_ARR_SIZE, ints_arr.data, ints_arr.size);

  const OwnedArr<int> ints_arr2 = std::move(ints_arr);

  static_assert(IS_SAME_TYPE<decltype(ints_arr2[0]), int&>, "Must return ref");

  TEST_EQ((int*)nullptr, ints_arr.data);
  TEST_EQ((usize)0, ints_arr.size);
  TEST_ARR_EQ(RANDOM_ARR, RANDOM_ARR_SIZE, ints_arr2.data, ints_arr2.size);
  
}

TEST_FUNCTION(Util_OwnedArr, bake_const) {
  Array<int> ints = {};

  for (int i : RANDOM_ARR) {
    ints.insert(i);
  }
  const usize prev_size = ints.size;
  OwnedArr<const int> ints_arr = bake_const_arr(std::move(ints));

  static_assert(IS_SAME_TYPE<decltype(ints_arr[0]), const int&>, "Must return const ref");

  TEST_EQ((int*)nullptr, ints.data);
  TEST_EQ((usize)0, ints.size);
  TEST_EQ((usize)0, ints.capacity);
  TEST_ARR_EQ(RANDOM_ARR, RANDOM_ARR_SIZE, ints_arr.data, ints_arr.size);

  const OwnedArr<const int> ints_arr2 = std::move(ints_arr);

  static_assert(IS_SAME_TYPE<decltype(ints_arr2[0]), const int&>, "Must return const ref");

  TEST_EQ((const int*)nullptr, ints_arr.data);
  TEST_EQ((usize)0, ints_arr.size);
  TEST_ARR_EQ(RANDOM_ARR, RANDOM_ARR_SIZE, ints_arr2.data, ints_arr2.size);
}

TEST_FUNCTION(Util_OwnedArr, copy) {
  Array<int> ints = {};

  for (int i : RANDOM_ARR) {
    ints.insert(i);
  }

  TEST_ARR_EQ(RANDOM_ARR, RANDOM_ARR_SIZE, ints.data, ints.size);

  OwnedArr<int> arr = copy_arr(ints);

  TEST_ARR_EQ(RANDOM_ARR, RANDOM_ARR_SIZE, arr.data, arr.size);

  OwnedArr<int> arr2 = copy_arr(arr);

  TEST_ARR_EQ(RANDOM_ARR, RANDOM_ARR_SIZE, arr2.data, arr2.size);

  OwnedArr<int> arr3 = copy_arr(RANDOM_ARR, RANDOM_ARR_SIZE);

  TEST_ARR_EQ(RANDOM_ARR, RANDOM_ARR_SIZE, arr3.data, arr3.size);
}

TEST_FUNCTION(Util_OwnedArr, destruction) {
  {
    int i = 0;
    constexpr int COUNTER = 20;
    {
      OwnedArr<CheckDelete> owned;
      {
        Array<CheckDelete> deleter = {};
        for (int counter = 0; counter < COUNTER; counter += 1) {
          deleter.insert(CheckDelete{ &i });
        }

        owned = bake_arr(std::move(deleter));
      }

      TEST_EQ(0, i);

      {
        Array<CheckDelete> deleter = {};
        for (int counter = 0; counter < 20; counter += 1) {
          deleter.insert(CheckDelete{ &i });
        }

        owned = bake_arr(std::move(deleter));
      }

      TEST_EQ(COUNTER, i);
    }

    TEST_EQ(COUNTER * 2, i);
  }

  {
    int i = 0;
    constexpr int COUNTER = 20;
    {
      OwnedArr<const CheckDelete> owned;
      {
        Array<CheckDelete> deleter = {};
        for (int counter = 0; counter < COUNTER; counter += 1) {
          deleter.insert(CheckDelete{ &i });
        }

        owned = bake_arr(std::move(deleter));
      }

      TEST_EQ(0, i);

      {
        Array<CheckDelete> deleter = {};
        for (int counter = 0; counter < 20; counter += 1) {
          deleter.insert(CheckDelete{ &i });
        }

        owned = bake_arr(std::move(deleter));
      }

      TEST_EQ(COUNTER, i);
    }

    TEST_EQ(COUNTER * 2, i);
  }
}


TEST_FUNCTION(Util_ViewArr, view_Array) {
  Array<usize> arr = {};
  arr.reserve_extra(RANDOM_ARR_SIZE);

  for (int i : RANDOM_ARR) {
    arr.insert(i);
  }

  {
    ViewArr<usize> v = view_arr(arr);

    TEST_EQ(arr.data, v.data);
    TEST_EQ(arr.size, v.size);

    ViewArr<const usize> v2 = v;
    TEST_EQ((const usize*)arr.data, v2.data);
    TEST_EQ(arr.size, v2.size);
  }

  {
    ViewArr<usize> v = view_arr(arr, 3, 5);

    TEST_EQ(arr.data + 3, v.data);
    TEST_EQ((usize)5, v.size);

    ViewArr<const usize> v2 = v;
    TEST_EQ((const usize*)arr.data + 3, v2.data);
    TEST_EQ((usize)5, v2.size);
  }

  {
    ViewArr<const usize> v = const_view_arr(arr);

    TEST_EQ((const usize*)arr.data, v.data);
    TEST_EQ(arr.size, v.size);
  }

  {
    ViewArr<const usize> v = const_view_arr(arr, 3, 5);

    TEST_EQ((const usize*)arr.data + 3, v.data);
    TEST_EQ((usize)5, v.size);
  }
}

TEST_FUNCTION(Util_ViewArr, literal_view) {
  ViewArr<const char> view = lit_view_arr("hello world");

  const char expected[] = { 'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', };

  TEST_ARR_EQ(expected, array_size(expected), view.data, view.size);
}

TEST_FUNCTION(Util, freelist_block_allocator) {

  {
    FreelistBlockAllocator<int> alloc = {};
    TEST_EQ(true, alloc._debug_all_are_free());

    int* i1 = alloc.allocate();
    TEST_EQ(false, alloc._debug_all_are_free());
    *i1 = 1;
    int* i2 = alloc.allocate();
    TEST_EQ(false, alloc._debug_all_are_free());
    *i2 = 2;
    int* i3 = alloc.allocate();
    TEST_EQ(false, alloc._debug_all_are_free());
    *i3 = 3;
    int* i4 = alloc.allocate();
    TEST_EQ(false, alloc._debug_all_are_free());
    *i4 = 4;
    int* i5 = alloc.allocate();
    TEST_EQ(false, alloc._debug_all_are_free());
    *i5 = 5;


    TEST_EQ(1, *i1);
    TEST_EQ(2, *i2);
    TEST_EQ(3, *i3);
    TEST_EQ(4, *i4);
    TEST_EQ(5, *i5);

    alloc.free(i2);
    TEST_EQ(false, alloc._debug_all_are_free());

    TEST_EQ(1, *i1);
    TEST_EQ(3, *i3);
    TEST_EQ(4, *i4);
    TEST_EQ(5, *i5);

    int* i6 = alloc.allocate();
    *i6 = 6;

    TEST_EQ(1, *i1);
    TEST_EQ(3, *i3);
    TEST_EQ(4, *i4);
    TEST_EQ(5, *i5);
    TEST_EQ(6, *i6);

    alloc.free(i1);
    TEST_EQ(false, alloc._debug_all_are_free());
    alloc.free(i6);
    TEST_EQ(false, alloc._debug_all_are_free());
    alloc.free(i4);
    TEST_EQ(false, alloc._debug_all_are_free());
    alloc.free(i5);
    TEST_EQ(false, alloc._debug_all_are_free());

    TEST_EQ(3, *i3);


    alloc.free(i3);

    TEST_EQ(true, alloc._debug_all_are_free());
  }

  {
    int counter = 0;

    {
      FreelistBlockAllocator<CheckDelete> alloc = {};

      CheckDelete* i1 = alloc.allocate();
      i1->i = &counter;
      CheckDelete* i2 = alloc.allocate();
      i2->i = &counter;
      CheckDelete* i3 = alloc.allocate();
      i3->i = &counter;
      CheckDelete* i4 = alloc.allocate();
      i4->i = &counter;
      CheckDelete* i5 = alloc.allocate();
      i5->i = &counter;

      TEST_EQ(0, counter);

      alloc.free(i2);

      TEST_EQ(1, counter);

      CheckDelete* i6 = alloc.allocate();
      i6->i = &counter;

      alloc.free(i1);
      TEST_EQ(2, counter);
      alloc.free(i6);
      TEST_EQ(3, counter);
      alloc.free(i4);
      TEST_EQ(4, counter);
      alloc.free(i5);
      TEST_EQ(5, counter);
      alloc.free(i3);
      TEST_EQ(6, counter);

      TEST_EQ(true, alloc._debug_all_are_free());
    }

    TEST_EQ(6, counter);
  }
}

TEST_FUNCTION(BIT_ARRAY, insert) {


  BitArray arr = BitArray(3);

  TEST_EQ(static_cast<usize>(3), arr.length);
  TEST_EQ(static_cast<usize>(0), arr.highest_set);

  arr.set(1);
  {
    TEST_EQ(static_cast<usize>(3), arr.length);
    TEST_EQ(static_cast<usize>(1), arr.highest_set);
    TEST_EQ(true, arr.test(1));

    TEST_EQ(false, arr.test(0));
    TEST_EQ(false, arr.test(2));
    TEST_EQ(false, arr.test_all());
  }

  arr.set(2);
  {
    TEST_EQ(static_cast<usize>(3), arr.length);
    TEST_EQ(static_cast<usize>(2), arr.highest_set);
    TEST_EQ(true, arr.test(1));
    TEST_EQ(true, arr.test(2));

    TEST_EQ(false, arr.test(0));
    TEST_EQ(false, arr.test_all());
  }

  arr.set(0);
  {
    TEST_EQ(static_cast<usize>(3), arr.length);
    TEST_EQ(static_cast<usize>(2), arr.highest_set);

    TEST_EQ(true, arr.test(0));
    TEST_EQ(true, arr.test(1));
    TEST_EQ(true, arr.test(2));

    TEST_EQ(true, arr.test_all());
  }

  arr.clear();
  {
    TEST_EQ(static_cast<usize>(3), arr.length);
    TEST_EQ(static_cast<usize>(0), arr.highest_set);

    TEST_EQ(false, arr.test(0));
    TEST_EQ(false, arr.test(1));
    TEST_EQ(false, arr.test(2));
  }
}

TEST_FUNCTION(BIT_ARRAY, big_insert) {

  BitArray arr = BitArray(134);
  TEST_EQ(static_cast<usize>(134), arr.length);
  TEST_EQ(static_cast<usize>(0), arr.highest_set);

  arr.set(57);
  TEST_EQ(true, arr.test(57));
  TEST_EQ(static_cast<usize>(57), arr.highest_set);

  {
    for (usize i = 0; i < 57; ++i) {
      TEST_EQ(false, arr.test(i));
    }

    for (usize i = 58; i < 134; ++i) {
      TEST_EQ(false, arr.test(i));
    }
  }

  TEST_EQ(false, arr.test_all());
  arr.clear();
  TEST_EQ(static_cast<usize>(0), arr.highest_set);

  for (usize i = 0; i < 134; ++i) {
    TEST_EQ(false, arr.test(i));
  }
}

TEST_FUNCTION(BIT_ARRAY, intersects) {

  BitArray arr = BitArray(134);
  TEST_EQ(static_cast<usize>(134), arr.length);
  TEST_EQ(static_cast<usize>(0), arr.highest_set);

  arr.set(57);
  TEST_EQ(true, arr.test(57));
  TEST_EQ(static_cast<usize>(57), arr.highest_set);

  {
    for (usize i = 0; i < 57; ++i) {
      TEST_EQ(false, arr.test(i));
    }

    for (usize i = 58; i < 134; ++i) {
      TEST_EQ(false, arr.test(i));
    }
  }

  BitArray arr2 = BitArray(134);
  TEST_EQ(static_cast<usize>(134), arr2.length);
  TEST_EQ(static_cast<usize>(0), arr2.highest_set);

  arr2.set(12);

  TEST_EQ(true, arr2.test(12));
  TEST_EQ(static_cast<usize>(12), arr2.highest_set);

  {
    for (usize i = 0; i < 12; ++i) {
      TEST_EQ(false, arr2.test(i));
    }

    for (usize i = 13; i < 134; ++i) {
      TEST_EQ(false, arr2.test(i));
    }
  }

  TEST_EQ(false, arr2.intersects(arr));

  arr2.set(57);
  TEST_EQ(true, arr2.test(12));
  TEST_EQ(true, arr2.test(57));
  TEST_EQ(static_cast<usize>(57), arr2.highest_set);

  {
    for (usize i = 0; i < 12; ++i) {
      TEST_EQ(false, arr.test(i));
    }

    for (usize i = 13; i < 57; ++i) {
      TEST_EQ(false, arr.test(i));
    }

    for (usize i = 58; i < 134; ++i) {
      TEST_EQ(false, arr.test(i));
    }
  }

  TEST_EQ(true, arr2.intersects(arr));
}
