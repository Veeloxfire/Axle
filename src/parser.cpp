#include "ast.h"
#include "parser.h"

#include <stdarg.h>
#include <stdio.h>


void Parser::report_error(const char* error_message) {
  if (current.type != TokenType::Error) {
    current.type   = TokenType::Error;
    current.string = lexer.strings->intern(error_message);
  }
}

static uint64_t string_to_uint(const char* str) {
  return atoll(str);
}

struct KeywordPair {
  const char* keyword;
  TokenType type;
};

static constexpr KeywordPair keywords[] ={
  {"return", TokenType::Return},
  {"function", TokenType::Function},
  {"if", TokenType::If},
  {"else", TokenType::Else},
  {"true", TokenType::True},
  {"false", TokenType::False},
};

static constexpr KeywordPair operators[] ={
  {"+", TokenType::Add},
  {"-", TokenType::Sub},
  {"*", TokenType::Mul},
  {"/", TokenType::Div},
  {"<", TokenType::Lesser},
  {">", TokenType::Greater},
  {"(", TokenType::Left_Bracket},
  {")", TokenType::Right_Bracket},
  {"{", TokenType::Left_Brace},
  {"}", TokenType::Right_Brace},
  {",", TokenType::Comma},
  {";", TokenType::Semicolon},
  {"|", TokenType::Or},
  {"&", TokenType::And},
  {"=", TokenType::Equals},
};

static constexpr auto num_operators = sizeof(operators) / sizeof(KeywordPair);

constexpr static  bool is_letter(const char c) {
  return ('a' <= c && c <= 'z')
    || ('A' <= c && c <= 'Z');
}

constexpr static bool is_number(const char c) {
  return '0' <= c && c <= '9';
}

constexpr static  bool is_letter_or_number(const char c) {
  return is_letter(c)
    || is_number(c);
}

constexpr static void copy_position(const Lexer* lex, Token* tok) {
  tok->file_name = lex->file_name;
  tok->line      = lex->line;
  tok->character = lex->character;
}

constexpr static Token make_token(Lexer* const lex, const TokenType type, const InternString string) {
  Token tok;

  tok.type = type;
  tok.string = string;
  copy_position(lex, &tok);

  return tok;
}

static Token error_token(Lexer* const lex, const char* string) {
  Token error ={};
  error.type = TokenType::Error;
  error.string = lex->strings->intern(string);
  copy_position(lex, &error);

  return error;
}

constexpr static void skip_whitespace(Lexer* const lex) {
  while (true) {
    const char c = lex->top[0];

    switch (c) {
      case ' ':
      case '\t':
      case '\f':
        lex->top++;
        lex->character++;
        break;
      case '\n': {
          lex->line++;
          lex->character = 0;
          const char c2 = (++lex->top)[0];
          if (c2 == '\r') {
            ++lex->top;
          }
          break;
        }
      case '\r': {
          lex->line++;
          lex->character = 0;
          const char c2 = (++lex->top)[0];
          if (c2 == '\n') {
            ++lex->top;
          }
          break;
        }
      default:
        return;
    }
  }
}

static Token lex_identifier(Lexer* const lex) {
  const char* const name_base = lex->top;

  do {
    lex->top++;
    lex->character++;
  } while (is_letter_or_number(lex->top[0]));

  const size_t ident_len = lex->top - name_base;

  Token ident ={};
  ident.type   = TokenType::Identifier;
  ident.string = lex->strings->intern(name_base, ident_len);
  copy_position(lex, &ident);

  constexpr size_t num_keywords = sizeof(keywords) / sizeof(KeywordPair);

  for (size_t i = 0; i < num_keywords; i++) {
    const KeywordPair& pair = keywords[i];
    //CONSIDER: Dont recompute every time
    const size_t kw_len = strlen(pair.keyword);

    if (kw_len == ident_len
        && memcmp(pair.keyword, ident.string.string, ident_len) == 0) {
      //Is keyword
      ident.type = pair.type;
      //Exit early
      return ident;
    }
  }

  //Not a keyword 
  return ident;
}

static Token lex_number(Lexer* const lex) {
  const char* const number_base = lex->top;

  do {
    lex->top++;
    lex->character++;
  } while (is_number(lex->top[0]));

  const size_t ident_length = lex->top - number_base;

  Token num ={};
  num.type   = TokenType::Number;
  num.string = lex->strings->intern(number_base, ident_length);

  copy_position(lex, &num);
  return num;
}

static Token make_single_char_token(Lexer* lex) {

  auto i = operators;
  auto end = operators + num_operators;

  for (; i < end; i++) {
    const KeywordPair& pair = *i;

    if (pair.keyword[0] == lex->top[0]) {
      Token tok;
      tok.type = pair.type;
      tok.string = lex->strings->intern(pair.keyword);

      copy_position(lex, &tok);

      //Operators are all 1 character at this point
      lex->top++;
      lex->character++;

      return tok;
    }
  }

  return error_token(lex, "Unlexable token");
}

static Token lex_token(Lexer* const lex) {
  skip_whitespace(lex);

  const char c = lex->top[0];

  if (is_letter(c)) {
    return lex_identifier(lex);
  }
  else if (is_number(c)) {
    return lex_number(lex);
  }
  else if (c == '\0') {
    // \0 is the end of file
    Token eof ={};
    eof.type = TokenType::Eof;
    eof.string = lex->strings->intern("End of file");
    copy_position(lex, &eof);

    return eof;
  }

  return make_single_char_token(lex);
}

static void advance(Parser* parser) {
  if (parser->current.type != TokenType::Error) {
    parser->current = lex_token(&parser->lexer);
  }
}

static bool expect(Parser* parser, const TokenType t) {
  if (parser->current.type == t) {
    advance(parser);
    return true;
  }
  else {
    parser->report_error("Unexpected Token");
    return false;
  }
}

void init_parser(Parser* const parser, const char* file_name, const char* source) {

  parser->lexer.top       = source;
  parser->lexer.file_name = file_name;

  parser->current = lex_token(&parser->lexer);
}


static constexpr uint8_t precidence_table[] ={
  2,// BINARY_OPERATOR::ADD
  2,// BINARY_OPERATOR::SUB
  3,// BINARY_OPERATOR::MUL
  3,// BINARY_OPERATOR::DIV
  1,// BINARY_OPERATOR::LESSER
  1,// BINARY_OPERATOR::GREATER
  1,// BINARY_OPERATOR::EQUIVALENT
  0,// BINARY_OPERATOR::OR
  0,// BINARY_OPERATOR::AND
};

static bool is_binary_operator(const TokenType t) {
  return TokenType::Add <= t && t < TokenType::Left_Bracket;
}

static BINARY_OPERATOR parse_binary_operator(Parser* const parser) {
  switch (parser->current.type) {
    case TokenType::Add: advance(parser); return BINARY_OPERATOR::ADD;
    case TokenType::Sub: advance(parser); return BINARY_OPERATOR::SUB;
    case TokenType::Mul: advance(parser); return BINARY_OPERATOR::MUL;
    case TokenType::Div: advance(parser); return BINARY_OPERATOR::DIV;
    case TokenType::Lesser: advance(parser); return BINARY_OPERATOR::LESSER;
    case TokenType::Greater: advance(parser); return BINARY_OPERATOR::GREATER;
    case TokenType::Equals: {
        advance(parser);
        if (parser->current.type == TokenType::Equals) {
          advance(parser);
          return BINARY_OPERATOR::EQUIVALENT;
        }
        parser->report_error("'=' is not a valid binary operator");
        return BINARY_OPERATOR::EQUIVALENT;
      }
    case TokenType::Or: advance(parser); return BINARY_OPERATOR::OR;
    case TokenType::And: advance(parser); return BINARY_OPERATOR::AND;
  }
}

static void parse_primary(Parser* const parser, ASTExpression* const expr);

static void parse_binary_operators(Parser* const parser, uint8_t precidence, BINARY_OPERATOR op, ASTExpression* const base) {

  ASTExpression* outer_left  = allocate_zerod<ASTExpression>();
  ASTExpression* outer_right       = allocate_zerod<ASTExpression>();

  *outer_left = std::move(*base);

  base->expr_type    = EXPRESSION_TYPE::BINARY_OPERATOR;
  base->bin_op.op    = op;
  base->bin_op.left  = outer_left;
  base->bin_op.right = outer_right;

  parse_primary(parser, outer_right);

  while (is_binary_operator(parser->current.type)) {

    const BINARY_OPERATOR new_op = parse_binary_operator(parser);
    const uint8_t new_prec = precidence_table[(size_t)new_op];

    if (new_prec > precidence) {
      precidence = new_prec;
      op = new_op;

      ASTExpression* const inner_left  = allocate_zerod<ASTExpression>();
      ASTExpression* const inner_right = allocate_zerod<ASTExpression>();

      *inner_left = std::move(*outer_right);

      outer_right->expr_type    = EXPRESSION_TYPE::BINARY_OPERATOR;
      outer_right->bin_op.op    = op;
      outer_right->bin_op.left  = inner_left;
      outer_right->bin_op.right = inner_right;
      outer_right = inner_right;

      parse_primary(parser, outer_right);
    }
    else {
      precidence = new_prec;
      op = new_op;

      ASTExpression* const inner_left  = allocate_zerod<ASTExpression>();
      ASTExpression* const inner_right = allocate_zerod<ASTExpression>();

      *inner_left = std::move(*base);

      base->expr_type    = EXPRESSION_TYPE::BINARY_OPERATOR;
      base->bin_op.op    = op;
      base->bin_op.left  = inner_left;
      base->bin_op.right = inner_right;

      outer_left = inner_left;
      outer_right = inner_right;

      parse_primary(parser, outer_right);
    }
  }
}

static void parse_expression(Parser* const parser, ASTExpression* const expr) {
  //Will always be a primary
  parse_primary(parser, expr);

  if (is_binary_operator(parser->current.type)) {
    const BINARY_OPERATOR op = parse_binary_operator(parser);
    const uint8_t prec = precidence_table[(size_t)op];

    parse_binary_operators(parser, prec, op, expr);
  }
}

static void parse_primary(Parser* const parser, ASTExpression* const expr) {
  const Token current = parser->current;
  advance(parser);

  switch (current.type) {
    case TokenType::Number: {
        expr->expr_type = EXPRESSION_TYPE::VALUE;
        expr->value = string_to_uint(current.string.string);
        break;
      }
    case TokenType::Identifier: {
        //Name or function call

        if (parser->current.type == TokenType::Left_Bracket) {
          expr->expr_type = EXPRESSION_TYPE::FUNCTION_CALL;
          expr->call = FunctionCallExpr();
          FunctionCallExpr& call = expr->call;

          call.function_name = current.string;

          //Advance past the left bracket
          expect(parser, TokenType::Left_Bracket);

          //Arguments
          if (parser->current.type != TokenType::Right_Bracket) {
            while (true) {
              call.arguments.insert_uninit(1);
              parse_expression(parser, call.arguments.back());

              if (parser->current.type == TokenType::Right_Bracket) {
                break;
              }
              else if (parser->current.type == TokenType::Comma) {
                advance(parser);
                continue;
              }
              else {
                //ERROR
                parser->report_error("Expected a comma");
                return;
              }
            }

            call.arguments.shrink();//reduce over allocating space
          }

          advance(parser);
        }
        else {
          expr->expr_type = EXPRESSION_TYPE::NAME;
          expr->name = current.string;
        }

        break;
      }
    case TokenType::True:
    case TokenType::False: {
        expr->expr_type       = EXPRESSION_TYPE::ENUM;
        expr->enum_value      = EnumValueExpr();
        expr->enum_value.name = current.string;
        break;
      }
    case TokenType::Left_Bracket: {
        parse_expression(parser, expr);
        expect(parser, TokenType::Right_Bracket);
        break;
      }
  }
}

static void parse_type(Parser* const parser, ASTType* const type) {
  if (parser->current.type != TokenType::Identifier) {
    parser->report_error("Expected Identifier");
    return;
  }

  type->name = parser->current.string;
  advance(parser);
}

static void parse_declaration(Parser* const parser, ASTDeclaration* const decl) {
  parse_type(parser, &decl->type);

  if (parser->current.type != TokenType::Identifier) {
    parser->report_error("Expected Identifier");
    return;
  }

  decl->name = parser->current.string;
  advance(parser);
}

static void parse_block(Parser* const parser, ASTBlock* const block);

static void parse_statement(Parser* const parser, ASTStatement* const statement) {
  switch (parser->current.type) {
    case TokenType::Left_Brace: {
        statement->type  = STATEMENT_TYPE::BLOCK;
        statement->block = ASTBlock();
        parse_block(parser, &statement->block);
        return;
      }
    case TokenType::Return: {
        statement->type = STATEMENT_TYPE::RETURN;
        statement->expression = ASTExpression();

        advance(parser);
        parse_expression(parser, &statement->expression);

        expect(parser, TokenType::Semicolon);
        return;
      }
    case TokenType::If: {
        advance(parser);
        statement->type = STATEMENT_TYPE::IF_ELSE;

        expect(parser, TokenType::Left_Bracket);
        parse_expression(parser, &statement->if_else.condition);
        expect(parser, TokenType::Right_Bracket);


        statement->if_else.if_statement = allocate_zerod<ASTStatement>();
        parse_statement(parser, statement->if_else.if_statement);

        if (parser->current.type == TokenType::Else) {
          advance(parser);
          statement->if_else.else_statement = allocate_zerod<ASTStatement>();
          parse_statement(parser, statement->if_else.else_statement);
        }

        else {
          statement->if_else.else_statement = nullptr;
        }
        return;
      }
    case TokenType::Identifier: {
        //Probs type at the moment
        statement->type = STATEMENT_TYPE::DECLARATION;
        parse_declaration(parser, &statement->declaration);

        expect(parser, TokenType::Equals);

        parse_expression(parser, &statement->declaration.expression);
        return;
      }
  }
}


static void parse_block(Parser* const parser, ASTBlock* const block) {
  expect(parser, TokenType::Left_Brace);

  while (parser->current.type != TokenType::Right_Brace) {
    if (parser->current.type == TokenType::Semicolon) {
      //Empty statement
      advance(parser);
      continue;
    }
    else if (parser->current.type == TokenType::Error) {
      return;
    }

    block->block.insert_uninit(1);
    parse_statement(parser, block->block.back());
  }

  block->block.shrink();//reduce over allocating space
  expect(parser, TokenType::Right_Brace);
}

static void parse_function(Parser* const parser, ASTFunctionDeclaration* const func) {
  expect(parser, TokenType::Function);

  //Name
  if (parser->current.type != TokenType::Identifier) {
    parser->report_error("Expected Identifier");
    return;
  }

  func->name = parser->current.string;
  advance(parser);

  //Function signature

  expect(parser, TokenType::Left_Bracket);

  //Parameters
  if (parser->current.type != TokenType::Right_Bracket) {
    while (true) {
      func->parameters.insert_uninit(1);
      parse_declaration(parser, func->parameters.back());

      if (parser->current.type == TokenType::Right_Bracket) {
        break;
      }
      else if (parser->current.type == TokenType::Comma) {
        advance(parser);
        continue;
      }
      else {
        //ERROR
        parser->report_error("Expected a comma");
        return;
      }
    }
  }

  func->parameters.shrink();//reduce over allocating space

  advance(parser);

  // ->
  expect(parser, TokenType::Sub);// -
  expect(parser, TokenType::Greater);// >

  parse_type(parser, &func->return_type);

  parse_block(parser, &func->body);
}

void parse_file(Parser* const parser, ASTFile* const file) {

  for (TokenType current = parser->current.type;
       current != TokenType::Eof && current != TokenType::Error;
       current = parser->current.type)
  {
    file->functions.insert_uninit(1);

    ASTFunctionDeclaration* const func = file->functions.data + file->functions.size - 1;
    parse_function(parser, func);
  }

  file->functions.shrink();//reduce over allocating space
}

static void print(const char* string) {
  fputs(string, stdout);
}

static void print(const char c) {
  putc(c, stdout);
}

struct Printer {
  size_t tabs = 0;

  void newline() {
    print('\n');

    for (size_t i = 0; i < tabs; i++) {
      print("  ");
    }
  }
};


static void print_ast_expression(const ASTExpression* expr) {
  switch (expr->expr_type) {
    case EXPRESSION_TYPE::FUNCTION_CALL: {
        printf("%s(", expr->call.function_name.string);
        auto i = expr->call.arguments.begin();
        const auto end = expr->call.arguments.end();

        if ((end - i) > 0) {
          for (; i < (end - 1); i++) {
            print_ast_expression(i);
            print(", ");
          }

          print_ast_expression(i);
        }

        print(')');
        break;
      }
    case EXPRESSION_TYPE::NAME:
      printf("%s", expr->name.string);
      break;
    case EXPRESSION_TYPE::VALUE:
      printf("%llu", expr->value);
      break;
    case EXPRESSION_TYPE::BINARY_OPERATOR:
      print("(");
      print_ast_expression(expr->bin_op.left);

      switch (expr->bin_op.op) {
        case BINARY_OPERATOR::ADD: print(" + "); break;
        case BINARY_OPERATOR::SUB: print(" - "); break;
        case BINARY_OPERATOR::DIV: print(" / "); break;
        case BINARY_OPERATOR::MUL: print(" * "); break;
        case BINARY_OPERATOR::LESSER: print(" < "); break;
        case BINARY_OPERATOR::GREATER: print(" > "); break;
        case BINARY_OPERATOR::EQUIVALENT: print(" == "); break;
        case BINARY_OPERATOR::OR: print(" | "); break;
        case BINARY_OPERATOR::AND: print(" & "); break;
      }

      print_ast_expression(expr->bin_op.right);
      print(")");
  }
}

static void print_ast_statement(Printer* const printer, const ASTStatement* statement) {
  printer->newline();

  switch (statement->type) {
    case STATEMENT_TYPE::DECLARATION:
      printf("%s %s = ",
             statement->declaration.type.name.string, statement->declaration.name.string);
      print_ast_expression(&statement->declaration.expression);
      print(';');
      break;
    case STATEMENT_TYPE::RETURN:
      print("return ");
      print_ast_expression(&statement->expression);
      print(';');
      break;
    case STATEMENT_TYPE::EXPRESSION:
      print_ast_expression(&statement->expression);
      print(';');
      break;
    case STATEMENT_TYPE::IF_ELSE:
      print("if(");
      print_ast_expression(&statement->if_else.condition);
      print(") ");

      if (statement->if_else.if_statement->type != STATEMENT_TYPE::BLOCK) {
        printer->tabs++;
      }

      print_ast_statement(printer, statement->if_else.if_statement);

      if (statement->if_else.if_statement->type != STATEMENT_TYPE::BLOCK) {
        printer->tabs--;
      }

      printer->newline();
      print("else ");

      if (statement->if_else.else_statement->type != STATEMENT_TYPE::BLOCK) {
        printer->tabs++;
      }

      print_ast_statement(printer, statement->if_else.else_statement);

      if (statement->if_else.else_statement->type != STATEMENT_TYPE::BLOCK) {
        printer->tabs--;
      }
      break;
    case STATEMENT_TYPE::BLOCK: {
        auto b_i = statement->block.block.begin();
        const auto b_end = statement->block.block.end();

        print('{');
        printer->tabs++;

        for (; b_i < b_end; b_i++) {
          print_ast_statement(printer, b_i);
        }

        printer->tabs--;
        printer->newline();
        print('}');
        break;
      }
  }
}

void print_ast(const ASTFile* file) {
  Printer printer ={};

  auto i = file->functions.begin();
  const auto end = file->functions.end();
  for (; i < end; i++) {
    printf("function %s(", i->name.string);

    //Parameters
    {
      auto p_i = i->parameters.begin();
      const auto p_end = i->parameters.end();

      if (p_end - p_i > 0) {
        for (; p_i < (p_end - 1); p_i++) {
          printf("%s %s,", p_i->type.name.string, p_i->name.string);
        }

        printf("%s %s", p_i->type.name.string, p_i->name.string);
      }
    }

    printf(") -> %s {", i->return_type.name.string);
    printer.tabs++;

    //Function body
    {
      auto b_i = i->body.block.begin();
      const auto b_end = i->body.block.end();

      for (; b_i < b_end; b_i++) {
        print_ast_statement(&printer, b_i);
      }
    }

    printer.tabs--;
    printer.newline();
    print('}');

    printer.newline();
    printer.newline();
  }
}