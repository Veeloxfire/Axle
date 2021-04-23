#pragma once
#include "strings.h"
#include "safe_lib.h"
#include "utility.h"

struct ASTFile;

#define TOKEN_TYPE_MODIFY \
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
MODIFY(Mul)\
MODIFY(Div)\
MODIFY(Lesser)\
MODIFY(Greater)\
MODIFY(Or)\
MODIFY(And)\
MODIFY(Equals)\
MODIFY(Left_Bracket)\
MODIFY(Right_Bracket)\
MODIFY(Left_Brace)\
MODIFY(Right_Brace)\
MODIFY(Comma)\
MODIFY(Semicolon)

enum class TokenType : uint8_t {
#define MODIFY(n) n,
  TOKEN_TYPE_MODIFY
#undef MODIFY
};

struct TokenTypeString {
  const char* string;
  size_t length;
};

TokenTypeString token_type_string(TokenType);

struct Token {
  TokenType type = TokenType::Error;
  InternString string ={};

  //Position
  const char* file_name = nullptr;
  size_t line = 0;
  size_t character = 0;
};

struct Lexer {
  StringInterner* strings = nullptr;

  const char* file_name = nullptr;
  size_t line = 0;
  size_t character = 0;

  const char* top = nullptr;
};

struct Parser {
  Lexer lexer;

  Token current;

  void report_error(const char* error_message);
};

void init_parser(Parser* const parser, const char* file_name, const char* source);
void parse_file(Parser* const parser, ASTFile* const file);

struct KeywordPair {
  const char* keyword;
  TokenType type;
  size_t size;

  constexpr KeywordPair(const char* kw, TokenType t)
    :keyword(kw), type(t), size(strlen_ts(kw))
  {}
};