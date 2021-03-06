#pragma once
#include "strings.h"
#include "safe_lib.h"
#include "utility.h"
#include "files.h"
#include "errors.h"
#include "memory.h"

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

struct TokenTypeString {
  const char* string = nullptr;
  size_t length = 0;
};

TokenTypeString token_type_string(AxleTokenType);

struct Position {
  const InternString* full_path ={};
  size_t line = 0;
  size_t character = 0;
};

struct Token {
  AxleTokenType type = AxleTokenType::End;
  bool consumed_whitespace = false;

  const InternString* string = nullptr;

  Position pos ={};
};

struct Span;

void set_span_start(const Token& token, Span& span);
void set_span_end(const Token& token, Span& span);

#define SPAN_START set_span_start(parser->current, span)
#define SPAN_END set_span_end(parser->prev, span)

Span span_of_token(const Token& tok);


struct Lexer {
  Position save_pos = {};
  Position curr_pos = {};

  const char* top = nullptr;
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

  TokenStream stream;
  Namespace* current_namespace;

  Token prev = {};
  Token current ={};
  Token next ={};
};

#define PARSER_ALLOC(T) parser->ast_store.push<T>()

struct CompilerGlobals;
struct CompilerThread;

void reset_parser(CompilerGlobals* comp,
                  CompilerThread* const comp_thread,
                  Parser* const parser,
                  const InternString* file_name,
                  const char* string);

struct FileAST;
void parse_file(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser, FileAST* const file);

struct KeywordPair {
  const char* keyword = nullptr;
  AxleTokenType type = AxleTokenType::End;
  size_t size = 0;

  constexpr KeywordPair(const char* kw, AxleTokenType t)
    :keyword(kw), type(t), size(strlen_ts(kw))
  {}
};