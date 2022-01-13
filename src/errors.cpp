#include "errors.h"
#include "files.h"

OwnedPtr<char> load_span_from_source(const Span& span, const char* source) {
  Array<char> res ={};

  //size_t character = 0;
  size_t line = 0;

  bool should_emit = false;

  const auto emit_new_line = [&] {
    line++;

    if (should_emit) {
      res.insert('\n');
    }

    if (line == span.line_start) {
      should_emit = true;

      //Tab out each line
      res.insert(' ');
      res.insert(' ');
    }
    else if (line == (span.line_start + 1)) {
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

        res.reserve_extra((span.char_start + 2));

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

      res.reserve_extra(span.char_end + 2);

      res.insert('\\');
      res.insert('-');

      for (size_t i = 1; i < span.char_end; i++) {
        res.insert('-');
      }

      res.insert('^');
    }
    else if (should_emit) {
      res.insert(' ');
      res.insert(' ');
    }
  };

  if (line == span.line_start) {
    should_emit = true;

    //Tab out first line
    res.insert(' ');
    res.insert(' ');
  }

  while (true) {
    if (source[0] == '\0' || line == (span.line_end + 1)) {
      emit_new_line();
      res.insert('\0');
      break;
    }
    else if (source[0] == '\n') {
      if (source[1] == '\r') {
        source++;
        //other step will naturally happen
      }

      emit_new_line();
    }
    else if (source[0] == '\r') {
      if (source[1] == '\n') {
        source++;
        //other step will naturally happen
      }

      emit_new_line();
    }
    else if (should_emit) {
      if (source[0] == '\t') {
        res.insert(' ');
      }
      else {
        res.insert(source[0]);
      }
    }

    source++;
  }

  return res;
}

ERROR_CODE Errors::print_all() const {
  ERROR_CODE ret = ERROR_CODE::NO_ERRORS;

  IO::err_print("--- Compiler Encountered A Fatal Error ---\n\n");

  auto i = error_messages.begin();
  const auto end = error_messages.end();

  InternHashTable<OwnedPtr<const char>> files ={};

  for (; i < end; i++) {
    ret = i->type;

    const char* message_type = error_code_string(i->type);
    IO::err_print(message_type);
    IO::err_print(":\n");

    OwnedPtr<char> type_set_message = format_type_set(i->message.ptr, 4, 70);

    IO::err_print(type_set_message.ptr);
    IO::err_print('\n');

    if (i->span.full_path != nullptr) {
      const Span& span = i->span;

      IO::err_print("Location: ");

      if (!files.contains(i->span.full_path)) {
        auto load_file = FILES::load_file_to_string(i->span.full_path->string);

        files.insert(i->span.full_path, std::move(load_file));
      }

      OwnedPtr<const char>* ptr_ptr = files.get_val(i->span.full_path);
      ASSERT(ptr_ptr != nullptr);

      const char* source = ptr_ptr->ptr;
      OwnedPtr<char> string = load_span_from_source(span, source);

      IO::err_print(format("{} {}:{}\n\n", span.full_path, span.char_start, span.line_start));
      IO::err_print(string.ptr);
      IO::err_print("\n\n");
    }
  }

  IO::err_print("------------------------------------------\n");
  return ret;
}