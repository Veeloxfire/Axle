#include "tests.h"

TEST_FUNCTION(Util_Array_Insert_Remove) {
  Array<usize> a ={};

  for (usize i = 0; i < 1000; i++) {
    a.insert(i ^ (i + 1));
  }

  TEST_EQ(1000, a.size);
  TEST_EQ((999 ^ 1000), *a.back());

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

  TEST_EQ(999, a.size);
  TEST_EQ((999 ^ 1000), a.data[998]);
  TEST_EQ((499 ^ 500), a.data[499]);
  TEST_EQ((501 ^ 502), a.data[500]);

  a.insert_at(500, (500 ^ 501));

  for (usize i = 0; i < 1000; i++) {
    TEST_EQ((i ^ (i + 1)), a.data[i]);
  }

  a.insert_at(0, 0);
  TEST_EQ(0, a.data[0]);
  a.remove_at(0);
  a.insert_at(1000, 0);
  TEST_EQ(0, a.data[1000]);
  a.remove_at(1000);

  for (usize i = 0; i < 1000; i++) {
    TEST_EQ((i ^ (i + 1)), a.data[i]);
  }

  for (usize i = 0; i < 1000; i++) {
    a.pop();
  }

  TEST_EQ(0, a.size);
}


TEST_FUNCTION(Util_Queue_Insert_Remove) {
  Queue<usize> a ={};

  for (usize i = 0; i < 100; i++) {
    a.push_back(i ^ (i + 1));
  }

  TEST_EQ(100, a.size);

  for (usize i = 0; i < 100; i++) {
    TEST_EQ((i ^ (i + 1)), a.pop_front());
  }

  for (usize i = 0; i < 100; i++) {
    a.push_front(i ^ (i + 1));
  }

  TEST_EQ(100, a.size);

  TEST_EQ((0 ^ 1), a.pop_back());

  for (usize i = 1; i < 100; i++) {
    TEST_EQ((i ^ (i + 1)), a.pop_back());
  }
}

TEST_FUNCTION(sort) {
  Array<int> ints ={};

  ints.insert(1);
  ints.insert(2);
  ints.insert(6);
  ints.insert(7);
  ints.insert(4);
  ints.insert(3);
  ints.insert(5);

  sort_range(ints.mut_begin(), ints.mut_end(), [](int l, int r) { return l < r; });

  TEST_EQ(1, ints.data[0]);
  TEST_EQ(2, ints.data[1]);
  TEST_EQ(3, ints.data[2]);
  TEST_EQ(4, ints.data[3]);
  TEST_EQ(5, ints.data[4]);
  TEST_EQ(6, ints.data[5]);
  TEST_EQ(7, ints.data[6]);
}