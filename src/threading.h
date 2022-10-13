#pragma once

#include <atomic>

struct SpinLockMutex {
  volatile char held;

  void acquire();
  bool acquire_if_free();
  void release();
};

struct Signal {
  mutable volatile char held;

  void set();
  void unset();

  bool test() const;
};

template<typename T>
struct AtomicLock {
  SpinLockMutex* _mutex = nullptr;
  T* _ptr = nullptr;

  T* operator->() const {
    return _ptr;
  }

  T& operator*() const {
    return *_ptr;
  }

  void release() {
    _mutex->release();
    _mutex = nullptr;
    _ptr = nullptr;
  }

  bool is_valid() const {
    return _mutex != nullptr;
  }

  ~AtomicLock() {
    if (_mutex) _mutex->release();
  }
};

template<typename T>
struct AtomicPtr {
  mutable SpinLockMutex _mutex;
  T* _ptr;

  void set(T* t) {
    _mutex.acquire();
    _ptr = t;
    _mutex.release();
  }

  AtomicLock<T> get() const {
    _mutex.acquire();
    return { &_mutex, _ptr };
  }

  void get_load(AtomicLock<T>* lock) const {
    _mutex.acquire();
    lock->_mutex = &_mutex;
    lock->_ptr = _ptr;
  }

  AtomicLock<T> get_if_free(bool* is_acquired) const {
    bool acquired = _mutex.acquire_if_free();
    if (acquired) {
      *is_acquired = true;
      return { &_mutex, _ptr };
    }
    else {
      return { nullptr, nullptr };
    }
  }
};

struct ThreadHandle;

using THREAD_PROC = void(*)(const ThreadHandle*, void*);

const ThreadHandle* start_thread(THREAD_PROC thread_proc, void* data);
void wait_for_thread_end(const ThreadHandle* thread);