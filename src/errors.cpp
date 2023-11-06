#include "errors.h"
#include <AxleUtil/files.h>
#include <AxleUtil/io.h>

OwnedArr<char> load_span_from_source(const Span& span, const ViewArr<const char>& source) {
  Array<char> res = {};

  size_t line = 0;
  size_t i = 0;

  while (line < span.line_start && i < source.size) {
    if(source[i] == '\n') {
      i += 1;
      line += 1;
      if (i < source.size && source[i] == '\r') {
        i += 1;
      }
    }
    else if (source[i] == '\r') {
      i += 1;
      line += 1;
      if (i < source.size && source[i] == '\n') {
        i += 1;
      }
    }
    else {
      i += 1;
    }
  }

  ASSERT(line == span.line_start);
  //Tab out the first line
  res.insert(' ');
  res.insert(' ');

  const auto emit_new_line = [&] {
    line += 1;

    if (line == (span.line_start + 1)) {
      if (span.line_end == span.line_start) {
        //If its a single line span then just underline the characters

        res.reserve_extra(span.char_end + 2);

        res.insert(' ');
        res.insert(' ');

        size_t i = 0;
        for (; i < span.char_start; i++) {
          res.insert(' ');
        }

        for (; i < span.char_end; i++) {
          res.insert('^');
        }
      }
      else {
        //Do special line stuff
        //Draw this shape: /------------^

        res.reserve_extra((span.char_start + 6));

        res.insert('/');
        res.insert('-');

        for (size_t i = 0; i < span.char_start; i++) {
          res.insert('-');
        }

        res.insert('^');
        res.insert('\n');

        //Next line
        res.insert('|');
        res.insert(' ');
      }
    }
    else if (line > span.line_start && line <= span.line_end) {
      res.insert('|');
      res.insert(' ');
    }
    else if (line == (span.line_end + 1)) {
      //Do special line stuff 2.0
      //Draw this shape: \-----------^

      res.reserve_extra(span.char_end + 3);

      res.insert('\\');
      res.insert('-');

      for (size_t i = 1; i < span.char_end; i++) {
        res.insert('-');
      }

      res.insert('^');
    }
  };


  while (i < source.size && line < (span.line_end + 1)) {
    if (source[i] == '\n') {
      res.insert('\n');
      emit_new_line();
      i += 1;

      if (i < source.size && source[i] == '\r') {
        i += 1;
      }
    }
    else if (source[i] == '\r') {
      res.insert('\n');
      emit_new_line();
      i += 1;

      if (i < source.size && source[i] == '\n') {
        i += 1;
      }
    }
    else {
      if (source[i] == '\t') {
        res.insert(' ');
      }
      else {
        res.insert(source[i]);
      }

      i += 1;
    }
  }

  return bake_arr(std::move(res));
}

ERROR_CODE Errors::print_all() const {
  return print_error_messages(error_messages);
}

ERROR_CODE print_error_messages(const Array<ErrorMessage>& error_messages)
{
  ERROR_CODE ret = ERROR_CODE::NO_ERRORS;

  IO::err_print("--- Compiler Encountered A Fatal Error ---\n\n");

  auto i = error_messages.begin();
  const auto end = error_messages.end();

  InternHashTable<OwnedArr<const char>> files = {};

  for (; i < end; i++) {
    ret = i->type;

    const ViewArr<const char> message_type = error_code_string(i->type);
    IO::err_print(message_type);
    IO::err_print(":\n");

    OwnedArr<char> type_set_message = format_type_set(const_view_arr(i->message), 4, 70);

    IO::err_print(view_arr(type_set_message));
    IO::err_print("\n\n");

    if (i->span.full_path != nullptr) {
      const Span& span = i->span;

      IO::err_print("Location: ");

      if (!files.contains(i->span.full_path)) {
        auto load_file = FILES::load_file_to_string(view_arr(i->span.full_path));

        files.insert(i->span.full_path, std::move(load_file));
      }

      const OwnedArr<const char>* src_ptr = files.get_val(i->span.full_path);
      ASSERT(src_ptr != nullptr);

      IO::err_format("{} {}:{}\n\n", span.full_path, span.char_start, span.line_start);

      OwnedArr<char> string = load_span_from_source(span, view_arr(*src_ptr));
      IO::err_print(view_arr(string));
      IO::err_print('\n');
    }
  }

  IO::err_print("------------------------------------------\n");
  return ret;
}