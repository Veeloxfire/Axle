#pragma once

#include "safe_lib.h"

namespace clArg {
  struct ArgsList {
    usize argc;
    const char** args;

    const char** begin() const { return args; }
    const char** end() const { return args + argc; }
  };

  template<typename T>
  struct Parser {
    template<typename T>
    struct TemplateFalse {
      constexpr static bool VAL = false;
    };

    static_assert(TemplateFalse<T>::VAL, "Attempted to use unspecialized parse arg");
  };

  template<>
  struct Parser<char> {
    template<typename E>
    constexpr static bool try_parse(E&& err, const ViewArr<const char>& val,
                                    const ViewArr<const char>& name, char& c) {
      if (val.size == 0) {
        err.report_error("{}: Expected character. Was empty", name);
        return false;
      }
      else if (val.size > 1) {
        err.report_error("{}: Expected character. Was too large. Found: \"{}\"", name, val);
        return false;
      }

      c = val[0];
      return true;
    }
  };

  template<>
  struct Parser<ViewArr<const char>> {
    template<typename E>
    constexpr static bool try_parse(E&& err, const ViewArr<const char>& val,
                                    const ViewArr<const char>&, ViewArr<const char>& out) {
      out = val;
      return true;
    }
  };

  constexpr ViewArr<const char> arg_val(const char* str, const ViewArr<const char>& name) {
    if (str[0] != '-') return {};
    str += 1;

    for (usize i = 0; i < name.size; ++i) {
      if (str[i] != name[i]) return {};
    }

    if (str[name.size] != '=') return {};
    str += name.size + 1;
    const char* start = str;
    while (str[0] != '\0') str += 1;

    return { start, static_cast<usize>(str - start) };
  }

  template<typename E, typename T>
  bool parse_arg(E&& err, const ArgsList& args, const ViewArr<const char>& name, T& t) {
    FOR(args, it) {
      ViewArr<const char> v = arg_val(*it, name);
      if (v.data == nullptr) continue;

      bool r = Parser<T>::try_parse(std::forward<E>(err), v, name, t);
      if (r) return true;
    }

    err.report_error("Did not find argument: {}", name);
    return false;
  }
}