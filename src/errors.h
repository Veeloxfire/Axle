#pragma once
#include "utility.h"
#include "comp_utilities.h"
#include "strings.h"
#include "format.h"


struct Span {
  //the span is null if full_path == nullptr
  const InternString* full_path = nullptr;
  size_t char_start = 0;
  size_t line_start = 0;

  size_t char_end = 0;
  size_t line_end = 0;
};

OwnedArr<char> load_span_from_source(const Span& span, const char* source);

struct ErrorMessage {
  ERROR_CODE type = ERROR_CODE::NO_ERRORS;
  Span span = {};
  OwnedArr<const char> message = {};
};

struct Errors {
  bool panic = false;
  Array<ErrorMessage> error_messages ={};

  template<typename ... T>
  void report_error(ERROR_CODE code, const Span& span, const Format::FormatString& f_message, const T& ... ts) {
    panic = true;

    ASSERT(f_message.arr != nullptr);
    ASSERT(f_message.len != 0);

    OwnedArr<const char> message = format(f_message, ts...);
    ASSERT(message.data != nullptr);
    ASSERT(message.size > 0);
    error_messages.insert({ code, span, std::move(message) });
  }

  inline void report_error(ERROR_CODE code, const Span& span, OwnedArr<const char>&& message) {
    panic = true;

    ASSERT(message.data != nullptr);
    ASSERT(message.size > 0);

    error_messages.insert({ code, span, std::move(message) });
  }


  inline constexpr bool is_panic() const { return panic; }
  ERROR_CODE print_all() const;
};

ERROR_CODE print_error_messages(const Array<ErrorMessage>& error_messages);