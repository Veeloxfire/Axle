#pragma once
#include "strings.h"
#include "allocators.h"
#include "utility.h"

struct ASTFile;

enum class TokenType : uint8_t {
  //Info
  Error, Eof,
  
  //Non-builtin
  Identifier, Number,

  //Keywords
  Return, Function, If, Else, True, False,

  //Operators
  Add, Sub, Mul, Div, Lesser, Greater, Or, And, Equals,
  
  //Structure
  Left_Bracket, Right_Bracket, Left_Brace, Right_Brace,
  Comma, Semicolon
};

struct Token {
  TokenType type;
  InternString string;

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