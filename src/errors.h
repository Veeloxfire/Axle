#pragma once
#include "utility.h"
#include "comp_utilities.h"
#include "strings.h"
#include "format.h"

struct Span {
  //the span is null if full_path == nullptr
  const InternString* full_path ={};
  size_t line_start = 0;
  size_t line_end = 0;
  size_t char_start = 0;
  size_t char_end = 0;
};

OwnedPtr<char> load_span_from_source(const Span& span, const char* source);

struct ErrorMessage {
  ERROR_CODE type;
  Span span;
  OwnedPtr<char> message = nullptr;
};

struct Errors {
  bool panic = false;
  Array<ErrorMessage> error_messages ={};

  template<typename ... T>
  void report_error(ERROR_CODE code, const Span& span, const char* f_message, const T& ... ts) noexcept {
    panic = true;

    OwnedPtr<char> message = format(f_message, ts...);
    error_messages.insert({ code, span, std::move(message) });
  }

  ERROR_CODE print_all() const;
};