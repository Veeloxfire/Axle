#pragma once
#include <AxleUtil/strings.h>
#include <AxleUtil/safe_lib.h>
#include <AxleUtil/utility.h>
#include <AxleUtil/files.h>
#include <AxleUtil/memory.h>

#include "errors.h"

#define AXLE_TOKEN_KEYWORDS \
MODIFY(Return, "return") \
MODIFY(Function, "function") \
MODIFY(Global, "global") \
MODIFY(If, "if") \
MODIFY(While, "while") \
MODIFY(Else, "else") \
MODIFY(Cast, "cast") \
MODIFY(Struct, "struct")

#define AXLE_TOKEN_OPERATORS \
MODIFY(Add, "+") \
MODIFY(Sub, "-") \
MODIFY(Star, "*") \
MODIFY(BackSlash, "/") \
MODIFY(Percent, "%") \
MODIFY(Lesser, "<") \
MODIFY(Greater, ">") \
MODIFY(Or, "|") \
MODIFY(Xor, "^") \
MODIFY(And, "&") \
MODIFY(Equals, "=") \
MODIFY(Bang, "!") \

#define AXLE_TOKEN_STRUCTURAL \
MODIFY(Left_Bracket, "(") \
MODIFY(Right_Bracket, ")") \
MODIFY(Left_Brace, "{") \
MODIFY(Right_Brace, "}") \
MODIFY(Left_Square, "[") \
MODIFY(Right_Square, "]") \
MODIFY(Comma, ",") \
MODIFY(Semicolon, ";") \
MODIFY(Colon, ":") \
MODIFY(Full_Stop, ".")

#define AXLE_TOKEN_MODIFY \
MODIFY(End, "") \
MODIFY(Identifier, "") \
MODIFY(Intrinsic, "") \
MODIFY(Number, "") \
MODIFY(String, "") \
MODIFY(Character, "") \
AXLE_TOKEN_KEYWORDS \
AXLE_TOKEN_OPERATORS \
AXLE_TOKEN_STRUCTURAL \


//Error is for reporting lexing errors and should never been seen in the parser
enum class AxleTokenType : uint8_t {
#define MODIFY(n, str) n,
  AXLE_TOKEN_MODIFY
#undef MODIFY
};

constexpr ViewArr<const char> token_type_string(AxleTokenType t) {
  switch (t) {
#define MODIFY(tt, str) case AxleTokenType :: tt : return lit_view_arr(#tt);
    AXLE_TOKEN_MODIFY
#undef MODIFY
  }

  return lit_view_arr("UNKNOWN TOKEN TYPE");
}

namespace Format {
  template<>
  struct FormatArg<AxleTokenType> {
    template<Formatter F>
    constexpr static void load_string(F& res, AxleTokenType tt) {
      const ViewArr<const char> str = token_type_string(tt);
      res.load_string(str.data, str.size);
    }
  };

}

struct Position {
  size_t line = 0;
  size_t character = 0;
};

struct TokenPos {
  size_t line = 0;
  size_t character_start = 0;
  size_t character_end = 0;
};

struct Token {
  AxleTokenType type = AxleTokenType::End;
  bool consumed_whitespace = false;

  const InternString* string = nullptr;

  TokenPos pos ={};
};

struct Span;

void set_span_start(const InternString* file_path, const Token& token, Span& span);
void set_span_end(const Token& token, Span& span);

#define SPAN_START set_span_start(parser->full_path(), parser->current, span)
#define SPAN_END set_span_end(parser->prev, span)

Span span_of_token(const InternString* file_path, const Token& tok);


struct Lexer {
  const InternString* file_path;
  Position save_pos = {};
  Position curr_pos = {};

  const char* top = nullptr;
  const char* end = nullptr;
};

Span span_of_lex(const Lexer* lex);

struct TokenStream {
  Token* i;
  Token* end;
};

struct Namespace;

struct Parser {
  Lexer lexer = {};

  MemoryPool ast_store ={};
  Array<AST_LOCAL> eval_order = {};
  Array<AST_LOCAL> infer_order = {};

  TokenStream stream;
  Namespace* current_namespace;

  Token prev = {};
  Token current ={};
  Token next ={};

  FileLocation file_path = {};
  constexpr const InternString* full_path() const { return file_path.full_name; }
};

template<typename T>
T* ast_alloc(Parser* p) {
  static_assert(A_can_cast_to_B<T, AST>, "Only allocate ast nodes using this");
  return p->ast_store.push<T>();
}

struct CompilerGlobals;
struct CompilerThread;

void reset_parser(CompilerGlobals* comp,
                  CompilerThread* const comp_thread,
                  Parser* const parser,
                  const InternString* file_name,
                  ViewArr<const char> string);

struct FileAST;
void parse_file(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser, FileAST* const file);
