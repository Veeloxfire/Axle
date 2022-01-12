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