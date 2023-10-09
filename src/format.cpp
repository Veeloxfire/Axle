#include "format.h"
#include "parser.h"
#include "type.h"
#include "compiler.h"
#include "ast.h"


OwnedArr<char> format_type_set(const ViewArr<const char>& format_in, const size_t prepend_spaces, const size_t max_width) {
  Array<char> result = {};
  result.reserve_extra(format_in.size);

  const char* format_i = format_in.begin();
  const char* format_end = format_in.end();
  const char* string = format_i;
  const char* last_space = format_i;
  size_t curr_length = 0;

  const auto prepend = [&] {
    result.reserve_extra(prepend_spaces);
    for (size_t i = 0; i < prepend_spaces; i++) {
      result.insert(' ');
    }
  };

  prepend();

  while (true) {
    if (format_i == format_end) {
      result.concat(string, format_i - string);

      return bake_arr(std::move(result));
    }
    else if (curr_length == max_width && format_i[0] != '\n' && last_space > string) {
      //Need to insert a new line
      result.concat(string, last_space - string);
      result.insert('\n');
      
      prepend();
      curr_length = prepend_spaces;

      string = last_space + 1;
    }
    else if (format_i[0] == '\n') {
      format_i++;
      result.concat(string, format_i - string);
      string = format_i;
      last_space = string;

      prepend();
      curr_length = prepend_spaces;
      continue;
    }
    else if (format_i[0] == ' ') {
      last_space = format_i;
    }

    format_i++;
    curr_length++;
  }
}