#include "tests.h"

TEST_FUNCTION(Util_Array_Insert_Remove) {
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

TEST_FUNCTION(Util_Queue_Insert_Remove) {
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

TEST_FUNCTION(sort) {
  Array<int> ints = {};

  for (int i : RANDOM_ARR) {
    ints.insert(i);
  }

  sort_range(ints.mut_begin(), ints.mut_end(), [](int l, int r) { return l < r; });

  TEST_ARR_EQ(SORTED_RANDOM_ARR, RANDOM_ARR_SIZE, ints.data, ints.size);
}

TEST_FUNCTION(owned_array) {
  {
    Array<int> ints = {};

    for (int i : RANDOM_ARR) {
      ints.insert(i);
    }

    OwnedArr<int> ints_arr = bake_arr(std::move(ints));

    TEST_EQ((usize)0, ints.size);
    TEST_ARR_EQ(RANDOM_ARR, RANDOM_ARR_SIZE, ints_arr.data, ints_arr.size);
  }

  {
    Array<int> ints = {};

    for (int i : RANDOM_ARR) {
      ints.insert(i);
    }
    const usize prev_size = ints.size;
    OwnedArr<const int> ints_arr = bake_const_arr(std::move(ints));

    TEST_EQ((usize)0, ints.size);
    TEST_ARR_EQ(RANDOM_ARR, RANDOM_ARR_SIZE, ints_arr.data, ints_arr.size);
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
}