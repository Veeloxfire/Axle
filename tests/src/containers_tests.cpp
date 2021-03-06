#include "tests.h"

TEST_FUNCTION(Util_Array_Insert_Remove) {
  Array<usize> a ={};

  for (usize i = 0; i < 1000; i++) {
    a.insert(i ^ (i + 1));
  }

  ASSERT(a.size == 1000);
  ASSERT(*a.back() == (999 ^1000));

  for (usize i = 0; i < 1000; i++) {
    ASSERT(a.data[i] == (i ^ (i + 1)));
  }

  {
    auto b = a.begin();
    const auto end = a.end();
    for (usize i = 0; i < 1000; i++) {
      ASSERT(*b == (i ^ (i + 1)));
      b++;
    }

    ASSERT(b == end);

    b = a.mut_begin();

    for (usize i = 0; i < 1000; i++) {
      ASSERT(*b == (i ^ (i + 1)));
      b++;
    }

    ASSERT(b == end);
  }


  a.remove_at(500);

  ASSERT(a.size == 999);
  ASSERT(a.data[998] == (999 ^ 1000));
  ASSERT(a.data[499] == (499 ^ 500));
  ASSERT(a.data[500] == (501 ^ 502));

  a.insert_at(500, (500 ^ 501));

  for (usize i = 0; i < 1000; i++) {
    ASSERT(a.data[i] == (i ^ (i + 1)));
  }

  a.insert_at(0, 0);
  ASSERT(a.data[0] == 0);
  a.remove_at(0);
  a.insert_at(1000, 0);
  ASSERT(a.data[1000] == 0);
  a.remove_at(1000);

  for (usize i = 0; i < 1000; i++) {
    ASSERT(a.data[i] == (i ^ (i + 1)));
  }

  for (usize i = 0; i < 1000; i++) {
    a.pop();
  }

  ASSERT(a.size == 0);
}


TEST_FUNCTION(Util_Queue_Insert_Remove) {
  Queue<usize> a ={};

  for (usize i = 0; i < 100; i++) {
    a.push_back(i ^ (i + 1));
  }

  ASSERT(a.size == 100);

  for (usize i = 0; i < 100; i++) {
    ASSERT(a.pop_front() == (i ^ (i + 1)));
  }

  for (usize i = 0; i < 100; i++) {
    a.push_front(i ^ (i + 1));
  }

  ASSERT(a.size == 100);

  ASSERT(a.pop_back() == (0 ^ 1));

  for (usize i = 1; i < 100; i++) {
    ASSERT(a.pop_back() == (i ^ (i + 1)));
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

  ASSERT(ints.data[0] == 1);
  ASSERT(ints.data[1] == 2);
  ASSERT(ints.data[2] == 3);
  ASSERT(ints.data[3] == 4);
  ASSERT(ints.data[4] == 5);
  ASSERT(ints.data[5] == 6);
  ASSERT(ints.data[6] == 7);
}