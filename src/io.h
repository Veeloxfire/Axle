#pragma once

#include "safe_lib.h"
#include "formattable.h"

namespace IO_Single {
  void print_impl(const char* string, usize N);
  void print_impl(const ViewArr<char>& string);
  void print_impl(const ViewArr<const char>& string);
  void print_impl(const char c);

  template<usize N>
  void print_impl(const char(&str)[N]) {
    print_impl(str, N);
  }

  void err_print_impl(const char* string, usize N);
  void err_print_impl(const ViewArr<char>& string);
  void err_print_impl(const ViewArr<const char>& string);
  void err_print_impl(const char c);

  template<usize N>
  void err_print_impl(const char(&str)[N]) {
    err_print_impl(str, N);
  }

  template<typename ... T>
  void print(const T& ... t) {
    (print_impl(t), ...);
  }

  template<typename ... T>
  void err_print(const T& ... t) {
    (err_print_impl(t), ...);
  }

  void lock();
  void unlock();

  struct ScopeLock {
    ScopeLock() {
      lock();
    }

    ScopeLock(const ScopeLock&) = delete;
    ScopeLock(ScopeLock&&) = delete;
    ScopeLock& operator=(const ScopeLock&) = delete;
    ScopeLock& operator=(ScopeLock&&) = delete;

    ~ScopeLock() {
      unlock();
    }
  };
}

namespace Format {
  struct STPrintFormatter {
    template<usize N>
    void load_string_lit(const char(&str)[N]) {
      IO_Single::print_impl(str);
    }

    template<usize N>
    void load_string_exact(const char(&str)[N]) {
      IO_Single::print_impl(str);
    }

    inline void load_string(const char* str, usize N) {
      ASSERT(N > 0);
      IO_Single::print_impl(str, N);
    }

    inline void load_char(char c) {
      IO_Single::print_impl(c);
    }
  };

  struct STErrPrintFormatter {
    template<usize N>
    void load_string_lit(const char(&str)[N]) {
      IO_Single::err_print_impl(str);
    }

    template<usize N>
    void load_string_exact(const char(&str)[N]) {
      IO_Single::err_print_impl(str);
    }

    inline void load_string(const char* str, usize N) {
      ASSERT(N > 0);
      IO_Single::err_print_impl(str, N);
    }

    inline void load_char(char c) {
      IO_Single::err_print_impl(c);
    }
  };
}

namespace IO_Single {
  template<typename ... T>
  void format(const Format::FormatString& format, const T& ... ts) {
    Format::STPrintFormatter result = {};

    Format::format_to_formatter(result, format, ts...);
  }
}


namespace IO {
  template<typename ... T>
  void print(const T& ... t) {
    IO_Single::ScopeLock lock;
    IO_Single::print(t...);
  }

  template<typename ... T>
  void err_print(const T& ... t) {
    IO_Single::ScopeLock lock;
    IO_Single::err_print(t...);
  }

  template<typename ... T>
  void format(const Format::FormatString& format, const T& ... ts) {
    IO_Single::ScopeLock lock;
    Format::STPrintFormatter result = {};

    Format::format_to_formatter(result, format, ts...);
  }

  template<typename ... T>
  void err_format(const Format::FormatString& format, const T& ... ts) {
    IO_Single::ScopeLock lock;
    Format::STErrPrintFormatter result = {};

    Format::format_to_formatter(result, format, ts...);
  }
}
