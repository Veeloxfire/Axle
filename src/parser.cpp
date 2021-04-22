#include "ast.h"
#include "parser.h"
#include "format.h"

#include <stdarg.h>
#include <stdio.h>

TokenTypeString token_type_string(TokenType t) {
  switch (t) {
  #define MODIFY(tt) case TokenType:: ## tt : return { #tt, sizeof(#tt) };
    TOKEN_TYPE_MODIFY
    #undef MODIFY
  }
}

void Parser::report_error(const char* error_message) {
  if (current.type != TokenType::Error) {
    current.type = TokenType::Error;
    current.string = lexer.strings->intern(error_message);
  }
}

static uint64_t string_to_uint(const char* str) {
  return atoll(str);
}

constexpr KeywordPair keywords[] ={
  {"return", TokenType::Return},
  {"function", TokenType::Function},
  {"if", TokenType::If},
  {"else", TokenType::Else},
  {"true", TokenType::True},
  {"false", TokenType::False},
  {"cast", TokenType::Cast},
};

constexpr KeywordPair operators[15] ={
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

constexpr static bool is_identifier_char(const char c) {
  return is_letter(c) || c == '_';
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
  tok->line = lex->line;
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
  } while (is_identifier_char(lex->top[0]) || is_number(lex->top[0]));

  const size_t ident_len = lex->top - name_base;

  Token ident ={};
  ident.type = TokenType::Identifier;
  ident.string = lex->strings->intern(name_base, ident_len);
  copy_position(lex, &ident);

  constexpr size_t num_keywords = sizeof(keywords) / sizeof(KeywordPair);

  for (size_t i = 0; i < num_keywords; i++) {
    const KeywordPair& pair = keywords[i];

    if (pair.size == ident_len
        && memcmp_ts(pair.keyword, ident.string.string, ident_len) == 0) {
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
  num.type = TokenType::Number;
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

  OwnedPtr<char> error = format("Unlexable character: '{}'", DisplayChar{ *lex->top });
  return error_token(lex, error.ptr);
}

static Token lex_token(Lexer* const lex) {
  skip_whitespace(lex);

  const char c = lex->top[0];

  if (is_identifier_char(lex->top[0])) {
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
  if (parser->current.type == TokenType::Error) {
    return false;
  }
  if (parser->current.type == t) {
    advance(parser);
    return true;
  }
  else {
    OwnedPtr<char> error = format("Unexpected Token: {}, Expected: {}", parser->current.type, t);

    parser->report_error(error.ptr);
    return false;
  }

}

void init_parser(Parser* const parser, const char* file_name, const char* source) {

  parser->lexer.top = source;
  parser->lexer.file_name = file_name;

  parser->current = lex_token(&parser->lexer);
}

//Should never be 0!
static constexpr uint8_t precidence_table[] ={
  3,// BINARY_OPERATOR::ADD
  3,// BINARY_OPERATOR::SUB
  4,// BINARY_OPERATOR::MUL
  4,// BINARY_OPERATOR::DIV
  2,// BINARY_OPERATOR::LESSER
  2,// BINARY_OPERATOR::GREATER
  2,// BINARY_OPERATOR::EQUIVALENT
  1,// BINARY_OPERATOR::OR
  1,// BINARY_OPERATOR::AND
};

static bool is_binary_operator(const TokenType t) {
  return TokenType::Add <= t && t < TokenType::Left_Bracket;
}

static void parse_type(Parser* const parser, ASTType* const type) {
  if (parser->current.type != TokenType::Identifier) {
    parser->report_error("Expected Identifier!");
    return;
  }

  type->name = parser->current.string;
  advance(parser);
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

  OwnedPtr<char> error = format("Invalid binary operator: {}", parser->current.string);

  parser->report_error(error.ptr);
  return BINARY_OPERATOR::ADD;//just return whatever and hope everything errors out
}

static void parse_unary_op(Parser* const parser, ASTExpression* const expr);

static ASTExpression* parse_binary_precidence(Parser* const parser, const uint8_t prev_prec, ASTExpression** base) {
  //precidence for this level
  uint8_t this_prec = precidence_table[(size_t)(*base)->bin_op.op];

  while (is_binary_operator(parser->current.type)) {
    const BINARY_OPERATOR new_op = parse_binary_operator(parser);
    uint8_t new_prec = precidence_table[(size_t)new_op];

    if (this_prec < new_prec) {
      //Make right the new base for the next level
      {
        ASTExpression* new_right = allocate_default<ASTExpression>();

        new_right->expr_type = EXPRESSION_TYPE::BINARY_OPERATOR;
        new_right->bin_op.op = new_op;
        new_right->bin_op.left = (*base)->bin_op.right;
        new_right->bin_op.right = allocate_default<ASTExpression>();

        (*base)->bin_op.right = new_right;
        parse_unary_op(parser, new_right->bin_op.right);
      }

      ASTExpression* new_base = parse_binary_precidence(parser, this_prec, &(*base)->bin_op.right);

      //Finished
      if (new_base == nullptr) {
        return nullptr;
      }

      new_prec = precidence_table[(size_t)new_base->bin_op.op];
      //Only return if new_base should be on the previous level
      if (new_prec <= prev_prec) {
        return new_base;
      }

      //new_base needs to replace this level
      this_prec = new_prec;
      new_base->bin_op.left = *base;
      *base = new_base;

      new_base->bin_op.right = allocate_default<ASTExpression>();
      parse_unary_op(parser, new_base->bin_op.right);
    }
    else if(new_prec <= prev_prec)
    {
      //Needs to be on the previous level
      ASTExpression* new_base = allocate_default<ASTExpression>();
      new_base->expr_type = EXPRESSION_TYPE::BINARY_OPERATOR;
      new_base->bin_op.op = new_op;

      return new_base;
    }
    else {
      this_prec = new_prec;

      //new_base needs to replace this level
      ASTExpression* new_base = allocate_default<ASTExpression>();
      new_base->expr_type = EXPRESSION_TYPE::BINARY_OPERATOR;
      new_base->bin_op.op = new_op;
      new_base->bin_op.left = *base;
      *base = new_base;

      new_base->bin_op.right = allocate_default<ASTExpression>();
      parse_unary_op(parser, new_base->bin_op.right);
    }
  }
  
  return nullptr;
}

static void parse_binary_operators(Parser* const parser, BINARY_OPERATOR op, ASTExpression* const base) {

  ASTExpression* temp_base = allocate_default<ASTExpression>();

  temp_base->expr_type    = EXPRESSION_TYPE::BINARY_OPERATOR;
  temp_base->bin_op.op    = op;
  temp_base->bin_op.left  = allocate_default<ASTExpression>();
  temp_base->bin_op.right = allocate_default<ASTExpression>();

  *temp_base->bin_op.left = std::move(*base);

  parse_unary_op(parser, temp_base->bin_op.right);

  if (is_binary_operator(parser->current.type)) {
    auto temp = parse_binary_precidence(parser, 0 /* <- will always be lowest precidence*/, &temp_base);

    //Should always return nullptr
    assert(temp == nullptr);
  }

  base->expr_type    = temp_base->expr_type;
  base->bin_op.op    = temp_base->bin_op.op;
  base->bin_op.left  = temp_base->bin_op.left;
  base->bin_op.right = temp_base->bin_op.right;

  temp_base->bin_op.left = nullptr;
  temp_base->bin_op.right = nullptr;

  free<ASTExpression>(temp_base);
}

static void parse_expression(Parser* const parser, ASTExpression* const expr) {
  parse_unary_op(parser, expr);

  if (is_binary_operator(parser->current.type)) {
    const BINARY_OPERATOR op = parse_binary_operator(parser);

    parse_binary_operators(parser, op, expr);
  }
}

static void parse_primary(Parser* const parser, ASTExpression* const expr) {
  const Token current = parser->current;
  advance(parser);

  switch (current.type) {
    case TokenType::Cast: {
        expect(parser, TokenType::Left_Bracket);
        expr->expr_type = EXPRESSION_TYPE::CAST;
        parse_type(parser, &expr->cast.type);

        expect(parser, TokenType::Comma);

        expr->cast.expr = allocate_default<ASTExpression>();
        parse_expression(parser, expr->cast.expr);

        expect(parser, TokenType::Right_Bracket);
        break;
      }
    case TokenType::Number: {
        expr->expr_type = EXPRESSION_TYPE::VALUE;
        expr->value.value = string_to_uint(current.string.string);
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
                parser->report_error("Expected a comma!");
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
        expr->expr_type = EXPRESSION_TYPE::ENUM;
        expr->enum_value = EnumValueExpr();
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

static void parse_unary_op(Parser* const parser, ASTExpression* const expr) {
  if (parser->current.type == TokenType::Sub) {
    expr->expr_type = EXPRESSION_TYPE::UNARY_OPERATOR;
    expr->un_op.op = UNARY_OPERATOR::NEG;
    advance(parser);

    expr->un_op.primary = allocate_default<ASTExpression>(1);

    parse_primary(parser, expr->un_op.primary);
  }
  else {
    parse_primary(parser, expr);
  }

}

static void parse_declaration(Parser* const parser, ASTDeclaration* const decl) {
  parse_type(parser, &decl->type);

  if (parser->current.type != TokenType::Identifier) {
    parser->report_error("Expected Identifier!");
    return;
  }

  decl->name = parser->current.string;
  advance(parser);
}

static void parse_block(Parser* const parser, ASTBlock* const block);

static void parse_statement(Parser* const parser, ASTStatement* const statement) {
  switch (parser->current.type) {
    case TokenType::Left_Brace: {
        statement->type = STATEMENT_TYPE::BLOCK;
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


        statement->if_else.if_statement = allocate_default<ASTStatement>();
        parse_statement(parser, statement->if_else.if_statement);

        if (parser->current.type == TokenType::Else) {
          advance(parser);
          statement->if_else.else_statement = allocate_default<ASTStatement>();
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
    parser->report_error("Expected Identifier!");
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
        parser->report_error("Expected a comma!");
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
      printf("%llu", expr->value.value);
      if (expr->value.suffix.string != nullptr) {
        printf("%s", expr->value.suffix.string);
      }
      break;
    case EXPRESSION_TYPE::UNARY_OPERATOR:
      print('-');
      print_ast_expression(expr->un_op.primary);
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
      break;
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