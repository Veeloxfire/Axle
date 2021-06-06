#pragma once
#include "strings.h"
#include "safe_lib.h"
#include "utility.h"
#include "files.h"

struct ASTFile;

#define AXLE_TOKEN_MODIFY \
MODIFY(Error)\
MODIFY(Eof)\
MODIFY(Identifier)\
MODIFY(Number)\
MODIFY(Return)\
MODIFY(Function)\
MODIFY(If)\
MODIFY(Else)\
MODIFY(True)\
MODIFY(False)\
MODIFY(Cast)\
MODIFY(Add)\
MODIFY(Sub)\
MODIFY(Star)\
MODIFY(BackSlash)\
MODIFY(Lesser)\
MODIFY(Greater)\
MODIFY(Or)\
MODIFY(And)\
MODIFY(Equals)\
MODIFY(Left_Bracket)\
MODIFY(Right_Bracket)\
MODIFY(Left_Brace)\
MODIFY(Right_Brace)\
MODIFY(Left_Square)\
MODIFY(Right_Square)\
MODIFY(Comma)\
MODIFY(Semicolon)\
MODIFY(String)\
MODIFY(Import)\
MODIFY(As)

enum class AxleTokenType : uint8_t {
#define MODIFY(n) n,
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
  AxleTokenType type = AxleTokenType::Error;
  const InternString* string = nullptr;

  Position pos ={};
};

struct Span {
  //is null if file_name == nullptr
  const InternString* full_path ={};
  size_t line_start = 0;
  size_t line_end = 0;
  size_t char_start = 0;
  size_t char_end = 0;
};


Span span_of_token(const Token& tok);
OwnedPtr<char> load_span_from_file(const Span& span, const char* source);

struct Lexer {
  StringInterner* strings = nullptr;

  Position curr_pos ={};

  const char* top = nullptr;
};

struct Parser {
  Lexer lexer ={};

  Token current ={};

  void report_error(const char* error_message);
};

void init_parser(Parser* const parser,  const InternString* full_path, const char* source);
void parse_file(Parser* const parser, ASTFile* const file);

struct KeywordPair {
  const char* keyword = nullptr;
  AxleTokenType type = AxleTokenType::Error;
  size_t size = 0;

  constexpr KeywordPair(const char* kw, AxleTokenType t)
    :keyword(kw), type(t), size(strlen_ts(kw))
  {}
};