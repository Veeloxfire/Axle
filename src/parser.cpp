#include "ast.h"
#include "parser.h"
#include "format.h"

#include <stdarg.h>
#include <stdio.h>

TokenTypeString token_type_string(AxleTokenType t) {
  switch (t) {
  #define MODIFY(tt, str) case AxleTokenType:: ## tt : return { #tt, sizeof(#tt) };
    AXLE_TOKEN_MODIFY
    #undef MODIFY
  }

  return { "UNKNOWN TOKEN TYPE", sizeof("UNKNOWN TOKEN TYPE") };
}

void Parser::report_error(const char* error_message) {
  if (current.type != AxleTokenType::Error) {
    current.type = AxleTokenType::Error;
    current.string = lexer.strings->intern(error_message);
  }
}

static uint64_t string_to_uint(const char* str) {
  return atoll(str);
}

constexpr KeywordPair intrinsics[] ={
#define MODIFY(n, str) {str, AxleTokenType:: ## n},
  AXLE_TOKEN_INTRINSICS
#undef MODIFY
};


constexpr size_t num_intrinsics = sizeof(intrinsics) / sizeof(KeywordPair);

constexpr KeywordPair keywords[] ={
#define MODIFY(n, str) {str, AxleTokenType:: ## n},
  AXLE_TOKEN_KEYWORDS
#undef MODIFY
};

constexpr size_t num_keywords = sizeof(keywords) / sizeof(KeywordPair);

constexpr KeywordPair operators[] ={
#define MODIFY(n, str) {str, AxleTokenType:: ## n},
  AXLE_TOKEN_OPERATORS
  AXLE_TOKEN_STRUCTURAL
#undef MODIFY
};

constexpr size_t num_operators = sizeof(operators) / sizeof(KeywordPair);

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

constexpr static Token make_token(Lexer* const lex, const AxleTokenType type, const InternString* string) {
  Token tok;

  tok.type = type;
  tok.string = string;

  return tok;
}

static Token error_token(Lexer* const lex, const char* string) {
  Token error ={};
  error.type = AxleTokenType::Error;
  error.string = lex->strings->intern(string);

  return error;
}

constexpr static bool is_new_line(Lexer* const lex) {
  return lex->top[0] == '\n' || lex->top[0] == '\r';
}

constexpr static void skip_whitespace(Lexer* const lex) {
  while (true) {
    const char c = lex->top[0];

    switch (c) {
      case ' ':
      case '\t':
      case '\f':
        lex->top++;
        lex->curr_pos.character++;
        break;
      case '\n': {
          lex->curr_pos.line++;
          lex->curr_pos.character = 0;
          const char c2 = (++lex->top)[0];
          if (c2 == '\r') {
            ++lex->top;
          }
          break;
        }
      case '\r': {
          lex->curr_pos.line++;
          lex->curr_pos.character = 0;
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
    lex->curr_pos.character++;
  } while (is_identifier_char(lex->top[0]) || is_number(lex->top[0]));

  const size_t ident_len = lex->top - name_base;

  Token ident ={};
  ident.type = AxleTokenType::Identifier;
  ident.string = lex->strings->intern(name_base, ident_len);

  for (size_t i = 0; i < num_keywords; i++) {
    const KeywordPair& pair = keywords[i];

    if (pair.size == ident_len
        && memcmp_ts(pair.keyword, ident.string->string, ident_len) == 0) {
      //Is keyword
      ident.type = pair.type;
      //Exit early
      return ident;
    }
  }

  //Not a keyword 
  return ident;
}

static Token lex_intrinsic_identifier(Lexer* const lex) {
  const char* const name_base = lex->top;

  //First is '#'
  do {
    lex->top++;
    lex->curr_pos.character++;
    //Rest only be character
  } while (is_identifier_char(lex->top[0]));

  const size_t ident_len = lex->top - name_base;

  Token ident ={};
  ident.type = AxleTokenType::Identifier;
  ident.string = lex->strings->intern(name_base, ident_len);


  for (size_t i = 0; i < num_intrinsics; i++) {
    const KeywordPair& pair = intrinsics[i];

    if (pair.size == ident_len
        && memcmp_ts(pair.keyword, ident.string->string, ident_len) == 0) {
      //Is keyword
      ident.type = pair.type;
      //Exit early
      return ident;
    }
  }

  //Error
  return error_token(lex, "Was not a valid compiler intrinsic");
}

static Token lex_number(Lexer* const lex) {
  const char* const number_base = lex->top;

  do {
    lex->top++;
    lex->curr_pos.character++;
  } while (is_number(lex->top[0]));

  const size_t ident_length = lex->top - number_base;

  Token num ={};
  num.type = AxleTokenType::Number;
  num.string = lex->strings->intern(number_base, ident_length);

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

      lex->top += pair.size;
      lex->curr_pos.character += pair.size;

      return tok;
    }
  }

  OwnedPtr<char> error = format("Unlexable character: '{}'", DisplayChar{ *lex->top });
  return error_token(lex, error.ptr);
}

static Token lex_string(Lexer* const lex) {
  lex->top++;
  lex->curr_pos.character++;

  Array<char> out_str = {};

  while (!is_new_line(lex) && lex->top[0] != '\0' && lex->top[0] != '"') {
    out_str.insert(lex->top[0]);
    lex->top++;
    lex->curr_pos.character++;

    if (lex->top[0] == '\\') {
      //Escaped character
      lex->top++;
      lex->curr_pos.character++;

      switch (lex->top[0]) {
        case '0': out_str.insert('\0'); break;
        case '\\': out_str.insert('\\'); break;
        case 'n': out_str.insert('\n'); break;
        case 'r': out_str.insert('\r'); break;
        case 't': out_str.insert('\t'); break;
        case '"': out_str.insert('\"'); break;
        default: return error_token(lex, "Invalid escaped character");
      }

      lex->top++;
      lex->curr_pos.character++;
    }
  }

  if (lex->top[0] == '"') {
    lex->top++;
    lex->curr_pos.character++;

    Token tok;
    tok.type = AxleTokenType::String;
    tok.string = lex->strings->intern(out_str.data, out_str.size);

    return tok;
  }
  else {
    return error_token(lex, "String was not closed!");
  }
}

static Token lex_unpositioned_token(Lexer* const lex) {
  const char c = lex->top[0];

  if (is_identifier_char(lex->top[0])) {
    return lex_identifier(lex);
  }
  else if (is_number(c)) {
    return lex_number(lex);
  }
  else if (c == '#') {
    return lex_intrinsic_identifier(lex);
  }
  else if (c == '"') {
    return lex_string(lex);
  }
  else if (c == '\0') {
    // \0 is the end of file
    Token eof ={};
    eof.type = AxleTokenType::Eof;
    eof.string = lex->strings->intern("End of file");

    return eof;
  }

  return make_single_char_token(lex);
}

static Token lex_token(Lexer* const lex) {
  skip_whitespace(lex);
  Position curr_pos = lex->curr_pos;

  Token tok = lex_unpositioned_token(lex);
  tok.pos = std::move(curr_pos);

  return tok;
}

static void advance(Parser* parser) {
  if (parser->current.type != AxleTokenType::Error) {
    if (parser->prev.type != AxleTokenType::Error) {
      parser->prev = parser->current;
    }
    parser->current = lex_token(&parser->lexer);
  }
}

static bool expect(Parser* parser, const AxleTokenType t) {
  if (parser->current.type == AxleTokenType::Error) {
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

void init_parser(Parser* const parser, const InternString* full_path, const char* source) {

  parser->lexer.top = source;
  parser->lexer.curr_pos.full_path = full_path;

  parser->prev    = lex_token(&parser->lexer);
  parser->current = parser->prev;
}

static void set_span_start(const Token& token, Span& span) {
  span.full_path = token.pos.full_path;
  span.char_start = token.pos.character;
  span.line_start = token.pos.line;
}

static void set_span_end(const Token& token, Span& span) {
  assert(span.full_path == token.pos.full_path);

  span.char_end = token.pos.character + token.string->len;
  span.line_end = token.pos.line;
}

Span span_of_token(const Token& tok) {
  Span span ={};
  span.full_path = tok.pos.full_path;

  span.char_start = tok.pos.character;
  span.char_end = span.char_start + tok.string->len + 1;

  span.line_start = tok.pos.line;
  span.line_end = tok.pos.line + 1;

  return span;
}

OwnedPtr<char> load_span_from_file(const Span& span, const char* source) {
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


static void parse_type(Parser* const parser, ASTType* const type);
static void parse_unary_op(Parser* const parser, ASTExpression* const expr);

static constexpr uint8_t precidence_table[] ={
#define MODIFY(name, str, precidence) precidence,
  BIN_OP_INCS
#undef MODIFY
};

static bool is_binary_operator(const AxleTokenType t) {
  return AxleTokenType::Add <= t && t < AxleTokenType::Left_Bracket;
}

static BINARY_OPERATOR parse_binary_operator(Parser* const parser) {
  switch (parser->current.type) {
    case AxleTokenType::Add: advance(parser); return BINARY_OPERATOR::ADD;
    case AxleTokenType::Sub: advance(parser); return BINARY_OPERATOR::SUB;
    case AxleTokenType::Star: advance(parser); return BINARY_OPERATOR::MUL;
    case AxleTokenType::BackSlash: advance(parser); return BINARY_OPERATOR::DIV;
    case AxleTokenType::Lesser: {
        advance(parser);

        if (parser->current.type == AxleTokenType::Lesser) {
          advance(parser);
          return BINARY_OPERATOR::RIGHT_SHIFT;
        }

        return BINARY_OPERATOR::LESSER;
      }
    case AxleTokenType::Greater: {
        advance(parser);

        if (parser->current.type == AxleTokenType::Greater) {
          advance(parser);
          return BINARY_OPERATOR::LEFT_SHIFT;
        }

        return BINARY_OPERATOR::GREATER;
      }
    case AxleTokenType::Equals: {
        advance(parser);
        if (parser->current.type == AxleTokenType::Equals) {
          advance(parser);
          return BINARY_OPERATOR::EQUIVALENT;
        }
        parser->report_error("'=' is not a valid binary operator");
        return BINARY_OPERATOR::EQUIVALENT;
      }
    case AxleTokenType::Or: advance(parser); return BINARY_OPERATOR::OR;
    case AxleTokenType::And: advance(parser); return BINARY_OPERATOR::AND;
  }

  OwnedPtr<char> error = format("Invalid binary operator: {}", parser->current.string);

  parser->report_error(error.ptr);
  return BINARY_OPERATOR::ADD;//just return whatever and hope everything errors out
}

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

        new_right->set_union(EXPRESSION_TYPE::BINARY_OPERATOR);
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
    else if (new_prec <= prev_prec)
    {
      //Needs to be on the previous level
      ASTExpression* new_base = allocate_default<ASTExpression>();
      new_base->set_union(EXPRESSION_TYPE::BINARY_OPERATOR);
      new_base->bin_op.op = new_op;

      return new_base;
    }
    else {
      this_prec = new_prec;

      //new_base needs to replace this level
      ASTExpression* new_base = allocate_default<ASTExpression>();
      new_base->set_union(EXPRESSION_TYPE::BINARY_OPERATOR);
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
  //Has to be heap allocated
  ASTExpression* temp_base = allocate_default<ASTExpression>();
  DEFER(&) { free_destruct_single<ASTExpression>(temp_base); };

  temp_base->set_union(EXPRESSION_TYPE::BINARY_OPERATOR);
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

  *base = std::move(*temp_base);
}

static void parse_expression(Parser* const parser, ASTExpression* const expr) {
  parse_unary_op(parser, expr);

  if (is_binary_operator(parser->current.type)) {
    const BINARY_OPERATOR op = parse_binary_operator(parser);

    parse_binary_operators(parser, op, expr);
  }
}

static void parse_primary(Parser* const parser, ASTExpression* const expr) {
  //Will always be a primary so can elevate span stuff to here
  Span span ={};
  set_span_start(parser->current, span);
  DEFER(&) {
    set_span_end(parser->prev, span);
    expr->span = std::move(span);
  };

  const Token current = parser->current;
  advance(parser);

  switch (current.type) {
    case AxleTokenType::Left_Square: {
        expr->set_union(EXPRESSION_TYPE::ARRAY_EXPR);

        while (true) {
          expr->array_expr.elements.insert_uninit(1);
          auto* new_expr = expr->array_expr.elements.back();

          parse_expression(parser, new_expr);

          if (parser->current.type == AxleTokenType::Comma) {
            advance(parser);
          }
          else {
            break;
          }
        }

        expr->array_expr.elements.shrink();

        expect(parser, AxleTokenType::Right_Square);
        break;
      }
    case AxleTokenType::String: {
        expr->set_union(EXPRESSION_TYPE::ASCII_STRING);

        expr->ascii_string = current.string;

        advance(parser);
        break;
      }
    case AxleTokenType::Cast: {
        expect(parser, AxleTokenType::Left_Bracket);
        expr->set_union(EXPRESSION_TYPE::CAST);

        parse_type(parser, &expr->cast.type);

        expect(parser, AxleTokenType::Comma);

        expr->cast.expr = allocate_default<ASTExpression>();
        parse_expression(parser, expr->cast.expr);

        expect(parser, AxleTokenType::Right_Bracket);
        break;
      }
    case AxleTokenType::Number: {
        expr->set_union(EXPRESSION_TYPE::VALUE);
        expr->value.value = string_to_uint(current.string->string);
        break;
      }
    case AxleTokenType::Identifier: {
        //Name or function call

        if (parser->current.type == AxleTokenType::Left_Bracket) {
          expr->set_union(EXPRESSION_TYPE::FUNCTION_CALL);
          FunctionCallExpr& call = expr->call;

          call.function_name = current.string;

          //Advance past the left bracket
          expect(parser, AxleTokenType::Left_Bracket);

          //Arguments
          if (parser->current.type != AxleTokenType::Right_Bracket) {
            while (true) {
              call.arguments.insert_uninit(1);
              parse_expression(parser, call.arguments.back());

              if (parser->current.type == AxleTokenType::Right_Bracket) {
                break;
              }
              else if (parser->current.type == AxleTokenType::Comma) {
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
          expr->set_union(EXPRESSION_TYPE::NAME);
          expr->name = current.string;
        }

        break;
      }
    case AxleTokenType::True:
    case AxleTokenType::False: {
        expr->set_union(EXPRESSION_TYPE::ENUM);
        expr->enum_value.name = current.string;
        break;
      }
    case AxleTokenType::Nullptr: {
        expr->set_union(EXPRESSION_TYPE::NULLPTR);
        break;
      }
    case AxleTokenType::Left_Bracket: {
        parse_expression(parser, expr);
        expect(parser, AxleTokenType::Right_Bracket);
        break;
    default: parser->report_error("Unexpected Token");
      }
  }
}

static void parse_primary_and_suffix(Parser* const parser, ASTExpression* const expr) {
  parse_primary(parser, expr);

  switch (parser->current.type) {
    case AxleTokenType::Left_Square: {

        advance(parser);
        ASTExpression* const new_expr = allocate_default<ASTExpression>();
        ASTExpression* const index    = allocate_default<ASTExpression>();

        //Move the expr into the suffix thing
        *new_expr = std::move(*expr);

        //Reset expr
        default_init(expr);

        //Set span
        Span span = new_expr->span;
        set_span_start(parser->current, span);
        DEFER(&) {
          set_span_end(parser->prev, span);
          expr->span = std::move(span);
        };


        expr->set_union(EXPRESSION_TYPE::INDEX);

        expr->index.expr = new_expr;
        expr->index.index = index;

        parse_expression(parser, index);

        expect(parser, AxleTokenType::Right_Square);
        break;
      }
    default:
      //No suffix
      break;
  }
}

static void parse_unary_op(Parser* const parser, ASTExpression* const expr) {
  switch (parser->current.type) {
    case AxleTokenType::Sub: {
        Span span ={};
        set_span_start(parser->current, span);
        DEFER(&) {
          set_span_end(parser->prev, span);
          expr->span = std::move(span);
        };

        expr->set_union(EXPRESSION_TYPE::UNARY_OPERATOR);

        expr->un_op.op = UNARY_OPERATOR::NEG;
        advance(parser);

        expr->un_op.expr = allocate_default<ASTExpression>();

        parse_expression(parser, expr->un_op.expr);
        break;
      }
    case AxleTokenType::Star: {
        Span span ={};
        set_span_start(parser->current, span);
        DEFER(&) {
          set_span_end(parser->prev, span);
          expr->span = std::move(span);
        };

        expr->set_union(EXPRESSION_TYPE::UNARY_OPERATOR);

        expr->un_op.op = UNARY_OPERATOR::DEREF;
        advance(parser);

        expr->un_op.expr = allocate_default<ASTExpression>();

        parse_expression(parser, expr->un_op.expr);
        break;
      }
    case AxleTokenType::And: {
        Span span ={};
        set_span_start(parser->current, span);
        DEFER(&) {
          set_span_end(parser->prev, span);
          expr->span = std::move(span);
        };

        expr->set_union(EXPRESSION_TYPE::UNARY_OPERATOR);

        expr->un_op.op = UNARY_OPERATOR::ADDRESS;
        advance(parser);

        expr->un_op.expr = allocate_default<ASTExpression>();

        parse_expression(parser, expr->un_op.expr);
        break;
      }

    default:
      parse_primary_and_suffix(parser, expr);
      break;
  }
}

static void parse_type(Parser* const parser, ASTType* const type) {
  set_span_start(parser->current, type->span);
  DEFER(&) { set_span_end(parser->prev, type->span); };

  switch (parser->current.type) {
    case AxleTokenType::Identifier: {
        type->set_union(TYPE_TYPE::NORMAL);
        type->name = parser->current.string;
        advance(parser);
        break;
      }
    case AxleTokenType::Left_Square: {
        // [ BASE ; EXPR ]
        type->set_union(TYPE_TYPE::ARRAY);

        advance(parser);//[

        //Base Type
        type->arr.base = allocate_default<ASTType>();
        parse_type(parser, type->arr.base);

        expect(parser, AxleTokenType::Semicolon);

        //Expression
        type->arr.expr = allocate_default<ASTExpression>();
        parse_expression(parser, type->arr.expr);

        expect(parser, AxleTokenType::Right_Square);
        break;
      }
    case AxleTokenType::Star: {
        // *BASE
        type->set_union(TYPE_TYPE::PTR);

        advance(parser);//*

        //Base
        type->ptr.base = allocate_default<ASTType>();
        parse_type(parser, type->ptr.base);
        break;
      }
    default: parser->report_error("Expected Type!");
  }
}

static void parse_local(Parser* const parser, ASTLocal* const decl) {
  parse_type(parser, &decl->type);

  if (parser->current.type != AxleTokenType::Identifier) {
    parser->report_error("Expected Identifier!");
    return;
  }

  decl->name = parser->current.string;
  advance(parser);
}

static void parse_block(Parser* const parser, ASTBlock* const block);

static void parse_statement(Parser* const parser, ASTStatement* const statement) {
  Span span ={};
  set_span_start(parser->current, span);
  DEFER(&) {
    set_span_end(parser->prev, span);
    statement->span = std::move(span);
  };

  switch (parser->current.type) {
    case AxleTokenType::Left_Brace: {
        statement->set_union(STATEMENT_TYPE::BLOCK);

        parse_block(parser, &statement->block);
        return;
      }
    case AxleTokenType::Return: {
        statement->set_union(STATEMENT_TYPE::RETURN);

        advance(parser);
        parse_expression(parser, &statement->expression);

        expect(parser, AxleTokenType::Semicolon);
        break;
      }
    case AxleTokenType::If: {
        advance(parser);
        statement->set_union(STATEMENT_TYPE::IF_ELSE);

        expect(parser, AxleTokenType::Left_Bracket);
        parse_expression(parser, &statement->if_else.condition);
        expect(parser, AxleTokenType::Right_Bracket);


        statement->if_else.if_statement = allocate_default<ASTStatement>();
        parse_statement(parser, statement->if_else.if_statement);

        if (parser->current.type == AxleTokenType::Else) {
          advance(parser);
          statement->if_else.else_statement = allocate_default<ASTStatement>();
          parse_statement(parser, statement->if_else.else_statement);
        }

        else {
          statement->if_else.else_statement = nullptr;
        }
        break;
      }
    default: {
        //Ambiguous at this point - save the position
        Parser save = *parser;

        //try declaration first
        statement->set_union(STATEMENT_TYPE::LOCAL);
        parse_type(parser, &statement->local.type);

        if (parser->current.type == AxleTokenType::Identifier) {
          //Is declaration
          statement->local.name = parser->current.string;
          advance(parser);

          expect(parser, AxleTokenType::Equals);

          parse_expression(parser, &statement->local.expression);
        }
        else {
          //Not a declaration - expression
          *parser = std::move(save);
          statement->set_union(STATEMENT_TYPE::EXPRESSION);

          parse_expression(parser, &statement->expression);
        }
        break;
      }
  }
}


static void parse_block(Parser* const parser, ASTBlock* const block) {
  expect(parser, AxleTokenType::Left_Brace);

  while (parser->current.type != AxleTokenType::Right_Brace) {
    if (parser->current.type == AxleTokenType::Semicolon) {
      //Empty statement
      advance(parser);
      continue;
    }
    else if (parser->current.type == AxleTokenType::Error) {
      return;
    }

    block->block.insert_uninit(1);
    parse_statement(parser, block->block.back());
  }

  block->block.shrink();//reduce over allocating space
  expect(parser, AxleTokenType::Right_Brace);
}

static void parse_function_signature(Parser* const parser, ASTFunctionSignature* const sig) {
  //Might specifiy a convention
  if (parser->current.type == AxleTokenType::Convention) {
    advance(parser);
    expect(parser, AxleTokenType::Left_Bracket);

    if (parser->current.type != AxleTokenType::Identifier) {
      parser->report_error("Expected Identifier!");
      return;
    }

    sig->convention = parser->current.string;

    advance(parser);
    expect(parser, AxleTokenType::Right_Bracket);
  }

  //Name
  if (parser->current.type != AxleTokenType::Identifier) {
    parser->report_error("Expected Identifier!");
    return;
  }

  sig->name = parser->current.string;
  advance(parser);


  expect(parser, AxleTokenType::Left_Bracket);

  //Parameters
  if (parser->current.type != AxleTokenType::Right_Bracket) {
    while (true) {
      sig->parameters.insert_uninit(1);
      parse_local(parser, sig->parameters.back());

      if (parser->current.type == AxleTokenType::Right_Bracket) {
        break;
      }
      else if (parser->current.type == AxleTokenType::Comma) {
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

  sig->parameters.shrink();//reduce over allocating space

  advance(parser);

  // ->
  expect(parser, AxleTokenType::Sub);// -
  expect(parser, AxleTokenType::Greater);// >

  parse_type(parser, &sig->return_type);
}

static void parse_function(Parser* const parser, ASTFunctionDeclaration* const func) {
  expect(parser, AxleTokenType::Function);
  parse_function_signature(parser, &func->signature);

  if (parser->current.type == AxleTokenType::Left_Brace) {
    parse_block(parser, &func->body);

    build_expression_linked_list(&func->body);
  }
  else if (parser->current.type == AxleTokenType::Semicolon) {
    advance(parser);
  }
  else {
    OwnedPtr<char> err = format("Expected '{}' or '{}'\nFound '{}'",
                                AxleTokenType::Left_Brace, AxleTokenType::Semicolon,
                                parser->current.type);

    parser->report_error(err.ptr);
  }
}

void parse_file(Parser* const parser, ASTFile* const file) {
  if (parser->current.type == AxleTokenType::DLLHeader) {
    file->header.is_dll_header = true;

    ASTImport* imp = &file->header.dll_header;
    advance(parser);

    if (parser->current.type != AxleTokenType::String) {
      parser->report_error("Expected a string!");
      return;
    }

    imp->relative_path = parser->current.string;

    //Load the span
    Span span ={};
    set_span_start(parser->current, span);
    DEFER(&) {
      set_span_end(parser->prev, span);
      imp->span = std::move(span);
    };

    advance(parser);
    expect(parser, AxleTokenType::Semicolon);
  }

  for (AxleTokenType current = parser->current.type;
       current != AxleTokenType::Eof && current != AxleTokenType::Error;
       current = parser->current.type)
  {
    if (current == AxleTokenType::Import) {
      //Import
      file->imports.insert_uninit(1);
      ASTImport* imp = file->imports.back();

      advance(parser);

      if (parser->current.type != AxleTokenType::String) {
        parser->report_error("Expected a string!");
        return;
      }

      imp->relative_path = parser->current.string;

      //Load the span
      Span span ={};
      set_span_start(parser->current, span);
      DEFER(&) {
        set_span_end(parser->prev, span);
        imp->span = std::move(span);
      };

      advance(parser);
      expect(parser, AxleTokenType::Semicolon);
    }
    else if (current == AxleTokenType::Function) {
      file->functions.insert_uninit(1);
      ASTFunctionDeclaration* func = file->functions.back();
      parse_function(parser, func);
    }
  }

  //reduce over allocating space
  file->imports.shrink();
  file->functions.shrink();
}

struct Printer {
  size_t tabs = 0;

  void newline() {
    IO::print('\n');

    for (size_t i = 0; i < tabs; i++) {
      IO::print("  ");
    }
  }
};

static void print_type(const ASTType* type) {
  switch (type->type_type) {
    case TYPE_TYPE::NORMAL:
      IO::print(type->name->string);
      break;
    case TYPE_TYPE::ARRAY:
      IO::print('[');
      print_type(type->arr.base);
      IO::print("; ");
      print_ast_expression(type->arr.expr);
      IO::print(']');
      break;
    case TYPE_TYPE::PTR:
      IO::print('*');
      print_type(type->arr.base);
      break;
  }
}

void print_ast_expression(const ASTExpression* expr) {
  switch (expr->expr_type) {
    case EXPRESSION_TYPE::INDEX: {
        print_ast_expression(expr->index.expr);
        IO::print('[');
        print_ast_expression(expr->index.index);
        IO::print(']');
        break;
      }
    case EXPRESSION_TYPE::ARRAY_EXPR: {
        IO::print('[');
        auto i = expr->array_expr.elements.begin();
        const auto end = expr->array_expr.elements.end();

        if (i < end) {
          for (; i < (end - 1); i++) {
            print_ast_expression(i);
            IO::print(", ");
          }

          print_ast_expression(i);
          IO::print(']');
        }
        else {
          IO::print(']');
        }

        break;
      }
    case EXPRESSION_TYPE::ASCII_STRING: {
        printf("\"%s\"", expr->ascii_string->string);
        break;
      }
    case EXPRESSION_TYPE::FUNCTION_CALL: {
        printf("%s(", expr->call.function_name->string);
        auto i = expr->call.arguments.begin();
        const auto end = expr->call.arguments.end();

        if ((end - i) > 0) {
          for (; i < (end - 1); i++) {
            print_ast_expression(i);
            IO::print(", ");
          }

          print_ast_expression(i);
        }

        IO::print(')');
        break;
      }
    case EXPRESSION_TYPE::NAME:
      printf("%s", expr->name->string);
      break;
    case EXPRESSION_TYPE::NULLPTR:
      IO::print("nullptr");
      break;
    case EXPRESSION_TYPE::VALUE:
      printf("%llu", expr->value.value);
      if (expr->value.suffix != nullptr) {
        printf("%s", expr->value.suffix->string);
      }
      break;
    case EXPRESSION_TYPE::CAST: {
        IO::print("cast(");
        print_type(&expr->cast.type);
        IO::print(", ");
        print_ast_expression(expr->cast.expr);
        IO::print(')');
        break;
      }
    case EXPRESSION_TYPE::UNARY_OPERATOR:
      IO::print(UNARY_OP_STRING::get(expr->un_op.op));
      print_ast_expression(expr->un_op.expr);
      break;
    case EXPRESSION_TYPE::BINARY_OPERATOR:
      IO::print("(");
      print_ast_expression(expr->bin_op.left);

      printf(" %s ", BINARY_OP_STRING::get(expr->bin_op.op));

      print_ast_expression(expr->bin_op.right);
      IO::print(")");
      break;
  }
}

static void print_ast_statement(Printer* const printer, const ASTStatement* statement) {
  printer->newline();

  switch (statement->type) {
    case STATEMENT_TYPE::LOCAL:
      print_type(&statement->local.type);
      printf(" %s = ", statement->local.name->string);
      print_ast_expression(&statement->local.expression);
      IO::print(';');
      break;
    case STATEMENT_TYPE::RETURN:
      IO::print("return ");
      print_ast_expression(&statement->expression);
      IO::print(';');
      break;
    case STATEMENT_TYPE::EXPRESSION:
      print_ast_expression(&statement->expression);
      IO::print(';');
      break;
    case STATEMENT_TYPE::IF_ELSE:
      IO::print("if(");
      print_ast_expression(&statement->if_else.condition);
      IO::print(") ");

      if (statement->if_else.if_statement->type != STATEMENT_TYPE::BLOCK) {
        printer->tabs++;
      }

      print_ast_statement(printer, statement->if_else.if_statement);

      if (statement->if_else.if_statement->type != STATEMENT_TYPE::BLOCK) {
        printer->tabs--;
      }

      printer->newline();
      IO::print("else ");

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

        IO::print('{');
        printer->tabs++;

        for (; b_i < b_end; b_i++) {
          print_ast_statement(printer, b_i);
        }

        printer->tabs--;
        printer->newline();
        IO::print('}');
        break;
      }
  }
}

static void print_function_sig(Printer* const printer, const ASTFunctionSignature* sig) {
  if (sig->convention == nullptr) {
    printf("%s(", sig->name->string);
  }
  else {
    printf("#conv(%s) %s(", sig->convention->string, sig->name->string);
  }
  

  //Parameters
  {
    auto p_i = sig->parameters.begin();
    const auto p_end = sig->parameters.end();

    if (p_end - p_i > 0) {
      for (; p_i < (p_end - 1); p_i++) {
        print_type(&p_i->type);
        printf(" %s, ", p_i->name->string);
      }

      print_type(&p_i->type);
      printf(" %s", p_i->name->string);
    }
  }

  IO::print(") -> ");
  print_type(&sig->return_type);
}

void print_ast(const ASTFile* file) {
  Printer printer ={};

  //Header
  if (file->header.is_dll_header) {
    printf("#dll_header \"%s\"\n\n", file->header.dll_header.relative_path->string);
  }

  //Imports
  {
    auto i = file->imports.begin();
    const auto end = file->imports.end();

    for (; i < end; i++) {
      printf("#import \"%s\";\n", i->relative_path->string);
    }

    if (file->imports.size > 0) {
      IO::print('\n');//Extra new life for nice formatting
    }
  }



  //Functions
  {
    auto i = file->functions.begin();
    const auto end = file->functions.end();

    for (; i < end; i++) {
      const ASTFunctionDeclaration* func = i;
      const ASTFunctionSignature* sig = &func->signature;

      IO::print("function ");
      print_function_sig(&printer, sig);
      if (func->body.block.size == 0) {
        IO::print(';');
      }
      else {
        IO::print(" {");
        printer.tabs++;

        //Function body
        {
          auto b_i = func->body.block.begin();
          const auto b_end = func->body.block.end();

          for (; b_i < b_end; b_i++) {
            print_ast_statement(&printer, b_i);
          }
        }

        printer.tabs--;
        printer.newline();
        IO::print('}');
      }

      printer.newline();
      printer.newline();
    }
  }
}