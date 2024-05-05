#pragma once
#include <AxleUtil/utility.h>
#include <AxleUtil/strings.h>
#include <AxleUtil/format.h>

#include <Axle/comp_utilities.h>

struct Span {
  //the span is null if full_path == nullptr
  const Axle::InternString* full_path = nullptr;
  size_t char_start = 0;
  size_t line_start = 0;

  size_t char_end = 0;
  size_t line_end = 0;
};

Axle::OwnedArr<char> load_span_from_source(const Span& span, const Axle::ViewArr<const char>& source);

struct ErrorMessage {
  ERROR_CODE type = ERROR_CODE::NO_ERRORS;
  Span span = {};
  Axle::OwnedArr<const char> message = {};
};

struct Errors {
  bool panic = false;
  Axle::Array<ErrorMessage> error_messages ={};

  template<typename ... T>
  void report_error(ERROR_CODE code, const Span& span, const Axle::Format::FormatString& f_message, const T& ... ts) {
    panic = true;

    ASSERT(f_message.arr != nullptr);
    ASSERT(f_message.len != 0);

    Axle::OwnedArr<const char> message = Axle::format(f_message, ts...);
    ASSERT(message.data != nullptr);
    ASSERT(message.size > 0);
    error_messages.insert({ code, span, std::move(message) });
  }

  inline void report_error(ERROR_CODE code, const Span& span, Axle::OwnedArr<const char>&& message) {
    panic = true;

    ASSERT(message.data != nullptr);
    ASSERT(message.size > 0);

    error_messages.insert({ code, span, std::move(message) });
  }


  inline constexpr bool is_panic() const { return panic; }
  ERROR_CODE print_all() const;
};

ERROR_CODE print_error_messages(const Axle::Array<ErrorMessage>& error_messages);
