#include "errors.h"

OwnedPtr<char> load_span_from_source(const Span& span, const char* source) {
  Array<char> res ={};

  size_t character = 0;
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