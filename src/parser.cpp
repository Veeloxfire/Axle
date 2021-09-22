#include "ast.h"
#include "parser.h"
#include "format.h"
#include "compiler.h"

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

constexpr static bool is_dec_number(const char c) {
  return '0' <= c && c <= '9';
}

constexpr static bool is_hex_number(const char c) {
  return is_dec_number(c) || 'a' <= c && c <= 'f' || 'A' <= c && c <= 'F';
}

constexpr static bool is_any_digit(const char c) {
  return is_hex_number(c);
}

constexpr static  bool is_letter_or_number(const char c) {
  return is_letter(c)
    || is_any_digit(c);
}

constexpr static u64 num_hex_digits(const char* str) {
  const char* start = str;
  while (*str != '\0' && is_hex_number(*str)) {
    str++;
  }

  return str - start;
}

constexpr static u64 num_dec_digits(const char* str) {
  const char* start = str;
  while (*str != '\0' && is_dec_number(*str)) {
    str++;
  }

  return str - start;
}

static u64 parse_hex_uint(const char* digits, const u64 len) {
  assert(0 < len && len <= 64);

  const auto get_digit = [](const char c) -> u8 {
    assert(is_hex_number(c));

    if ('0' <= c && c <= '9') {
      return c - '0';
    }
    else if ('a' <= c && c <= 'z') {
      return (c - 'a' + 0xa);
    }
    else if ('A' <= c && c <= 'Z') {
      return (c - 'A' + 0xa);
    }
    else {
      return -1;
    }
  };

  constexpr auto base = 0x10;
  u64 shift = 1;
  u64 result = 0;
  for (u64 digit = 0; digit < len; digit++) {
    u64 digit_val = get_digit(digits[len - digit - 1]);
    assert(digit_val <= 0xF);
    digit_val *= shift;

    shift *= base;
    result += digit_val;
  }

  return result;
}

static u64 parse_dec_uint(const char* digits, const u64 len) {
  assert(0 < len && len <= MAX_DECIMAL_U64_DIGITS);

  const auto get_digit = [](const char c) -> u8 {
    assert(is_dec_number(c));
    return c - '0';
  };

  constexpr auto base = 10;
  u64 shift = 1;
  u64 result = 0;
  for (u64 digit = 0; digit < len; digit++) {
    u64 digit_val = get_digit(digits[len - digit - 1]);
    assert(digit_val <= 9);
    digit_val *= shift;

    shift *= base;
    result += digit_val;
  }

  return result;
}

static u64 string_to_uint(const char* str) {
  if (str[0] == '0' && str[1] == 'x') {
    const auto len = num_hex_digits(str + 2);
    return parse_hex_uint(str + 2, len);
  }

  const auto len = num_dec_digits(str);
  return parse_dec_uint(str, len);
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
  constexpr auto n_newline = [](Lexer* const lex) {
    lex->curr_pos.line++;
    lex->curr_pos.character = 0;
    if ((++lex->top)[0] == '\r') {
      ++lex->top;
    }
  };

  constexpr auto r_newline = [](Lexer* const lex) {
    lex->curr_pos.line++;
    lex->curr_pos.character = 0;
    if ((++lex->top)[0] == '\n') {
      ++lex->top;
    }
  };

  while (true) {
    char c = lex->top[0];

    switch (c) {
      case ' ':
      case '\t':
      case '\f':
        lex->top++;
        lex->curr_pos.character++;
        break;
      case '/': {
          if (lex->top[1] == '/') {
            //Is a comment
            lex->top += 2;
            lex->curr_pos.character += 2;

            c = lex->top[0];

            //Loop till end of line
            while (c != '\n' && c != '\r' && c != '\0') {
              lex->top++;
              lex->curr_pos.character++;

              c = lex->top[0];
            }

            if (c == '\n') {
              n_newline(lex);
            }
            else if (c == '\r') {
              r_newline(lex);
            }
            else if (c == '\0') {
              //end of file
              return;
            }

            break;
          }
          else {
            //Not a comment
            return;
          }
        }
      case '\n': {
          n_newline(lex);
          break;
        }
      case '\r': {
          r_newline(lex);
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
  } while (is_identifier_char(lex->top[0]) || is_any_digit(lex->top[0]));

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

  const auto message = format("'{}' in not a valid compiler intrinsic", ident.string);

  //Error
  return error_token(lex, message.ptr);
}

static Token lex_number(Lexer* const lex) {
  const char* const number_base = lex->top;

  if (lex->top[0] == '0' && (lex->top[1] == 'x' || lex->top[1] == 'X')) {
    lex->top += 2;
    lex->curr_pos.character += 2;

    if (!is_any_digit(lex->top[0])) {
      return error_token(lex, "0x/0X is not a valid integer");
    }
  }

  do {
    lex->top++;
    lex->curr_pos.character++;
  } while (is_any_digit(lex->top[0]));

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

  const OwnedPtr<char> error = format("Unlexable character: '{}'", DisplayChar{ *lex->top });
  return error_token(lex, error.ptr);
}

static Token lex_char(Lexer* const lex) {
  lex->top++;
  lex->curr_pos.character++;

  char out[2] ={ lex->top[0], '\0' };

  lex->top++;
  lex->curr_pos.character++;

  if (out[0] == '\\') {
    switch (lex->top[0]) {
      case '0': out[0] = '\0'; break;
      case '\\': out[0] = '\\'; break;
      case 'n': out[0] = '\n'; break;
      case 'r': out[0] = '\r'; break;
      case 't': out[0] = '\t'; break;
      case '\'': out[0] = '\''; break;
      default: return error_token(lex, "Invalid escaped character");
    }

    lex->top++;
    lex->curr_pos.character++;
  }

  if (lex->top[0] == '\'') {
    lex->top++;
    lex->curr_pos.character++;

    Token tok;
    tok.type = AxleTokenType::Character;
    tok.string = lex->strings->intern(out, 2);

    return tok;
  }
  else {
    return error_token(lex, "Character literal was not closed!");
  }
}

static Token lex_string(Lexer* const lex) {
  lex->top++;
  lex->curr_pos.character++;

  Array<char> out_str ={};

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
  else if (is_dec_number(c)) {
    return lex_number(lex);
  }
  else if (c == '#') {
    return lex_intrinsic_identifier(lex);
  }
  else if (c == '"') {
    return lex_string(lex);
  }
  else if (c == '\'') {
    return lex_char(lex);
  }
  else if (c == '\0') {
    // \0 is the end of file
    Token eof ={};
    eof.type = AxleTokenType::End;
    eof.string = lex->strings->intern("End of file");

    return eof;
  }

  return make_single_char_token(lex);
}

static Token lex_token(Lexer* const lex) {
  const auto* save_top = lex->top;
  skip_whitespace(lex);

  const bool consumed_whitespace = save_top != lex->top;
  Position curr_pos = lex->curr_pos;

  Token tok = lex_unpositioned_token(lex);
  tok.pos = std::move(curr_pos);
  tok.consumed_whitespace = consumed_whitespace;

  return tok;
}

static TokenStream next_lex_stream(Compiler* const comp) {
  Lexer* const lex = comp->services.lexer;

  constexpr size_t STREAM_LEN = 64;

  Array<Token>& stream = comp->current_stream;

  stream.clear();
  stream.reserve_extra(STREAM_LEN);

  size_t i = 0;
  for (; i < STREAM_LEN; i++) {
    auto* tok = stream.data + i;

    *tok = lex_token(lex);

    if (tok->type == AxleTokenType::End) {
      i++;
      break;
    }
    else if (tok->type == AxleTokenType::Error) {
      Span span ={};
      set_span_start(*tok, span);
      span.char_end = lex->curr_pos.character + 1;
      span.line_end = lex->curr_pos.line;

      comp->report_error(ERROR_CODE::SYNTAX_ERROR, span, tok->string->string);
      return { nullptr, nullptr };
    }
  }


  stream.size = i;

  TokenStream tok_stream ={};
  tok_stream.i = stream.begin();
  tok_stream.end = stream.end();

  return tok_stream;
}

static void check_valid_stream(Compiler* const comp, const Parser* parser) {
  if (parser->next.type == AxleTokenType::End && parser->stream.i < parser->stream.end) {
    Span span ={};
    set_span_start(parser->current, span);
    set_span_end(*parser->stream.i, span);

    comp->report_error(ERROR_CODE::FILE_ERROR, span,
                       "Found '{}' token in the middle of a token stream",
                       AxleTokenType::End);
  }
}

static void advance(Compiler* const comp, Parser* parser) {
  parser->prev = parser->current;

  if (parser->current.type != AxleTokenType::End) {
    parser->current = parser->next;
  }
  else {
    comp->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->current),
                       "Attempted to advance past the end of a file");
    return;
  }

  if (parser->next.type == AxleTokenType::End) {
    check_valid_stream(comp, parser);
  }
  else {
    if (parser->stream.i >= parser->stream.end) {
      parser->stream = next_lex_stream(comp);

      if (comp->is_panic()) {
        return;
      }
    }
    parser->next = *parser->stream.i;
    assert(parser->next.type != AxleTokenType::Error);

    parser->stream.i++;
  }
}

static void expect(Compiler* const comp, Parser* parser, const AxleTokenType t) {
  if (parser->current.type == t) {
    advance(comp, parser);
  }
  else {
    comp->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->current),
                       "Unexpected Token: {}, Expected: {}", parser->current.type, t);
  }
}

void set_span_start(const Token& token, Span& span) {
  span.full_path = token.pos.full_path;
  span.char_start = token.pos.character;
  span.line_start = token.pos.line;
}

void set_span_end(const Token& token, Span& span) {
  assert(span.full_path == token.pos.full_path);

  span.char_end = token.pos.character + token.string->len;
  span.line_end = token.pos.line;
}

void reset_parser(Compiler* const comp,
                  const InternString* file_name,
                  const char* string) {
  Lexer* const lex = comp->services.lexer;
  Parser* const parser = comp->services.parser;


  lex->strings = comp->services.strings;
  lex->top = string;

  lex->curr_pos.full_path = file_name;
  lex->curr_pos.character = 0;
  lex->curr_pos.line = 0;

  parser->stream = next_lex_stream(comp);
  if (comp->is_panic()) {
    return;
  }

  if (parser->stream.i == nullptr || parser->stream.end == nullptr) {
    comp->report_error(ERROR_CODE::INTERNAL_ERROR, Span{},
                       "Parser was passed a fully or partially null stream"
                       "Start: '{}', End: '{}'",
                       PrintPtr{ parser->stream.i }, PrintPtr{ parser->stream.end });
    return;
  }

  if (parser->stream.i >= parser->stream.end) {
    comp->report_error(ERROR_CODE::INTERNAL_ERROR, Span{},
                       "Parser was passed a stream of 0 elements");
    return;
  }

  parser->current = *parser->stream.i;
  assert(parser->current.type != AxleTokenType::Error);
  parser->stream.i++;

  if (parser->current.type == AxleTokenType::End && parser->stream.i < parser->stream.end) {
    Span span ={};
    set_span_start(parser->current, span);
    set_span_end(*parser->stream.i, span);

    comp->report_error(ERROR_CODE::FILE_ERROR, span,
                       "Found '{}' token in the middle of a token stream",
                       AxleTokenType::End);
    return;
  }

  parser->next    = *parser->stream.i;
  assert(parser->next.type != AxleTokenType::Error);

  check_valid_stream(comp, parser);
  if (comp->is_panic()) {
    return;
  }

  parser->stream.i++;

  //Dummy value for the first value for creating spans
  // The first 3 values should never be read
  parser->prev.type = AxleTokenType::End;
  parser->prev.consumed_whitespace = false;
  parser->prev.string = nullptr;

  parser->prev.pos.character = 0;
  parser->prev.pos.line = 0;
  parser->prev.pos.full_path = file_name;
}

Span span_of_token(const Token& tok) {
  Span span ={};

  set_span_start(tok, span);
  set_span_end(tok, span);

  return span;
}

static void parse_name(Compiler* const comp, Parser* const parser, const InternString** name) {
  if (parser->current.type != AxleTokenType::Identifier) {
    comp->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->current),
                       "Expected token type '{}'. Found: '{}'",
                       AxleTokenType::Identifier, parser->current.type);
    return;
  }

  *name = parser->current.string;
  advance(comp, parser);
}


static void parse_type(Compiler* const comp, Parser* const parser, ASTType* const type);
static void parse_unary_op(Compiler* const comp, Parser* const parser, ASTExpression* const expr);

static constexpr uint8_t precidence_table[] ={
#define MODIFY(name, str, precidence) precidence,
  BIN_OP_INCS
#undef MODIFY
};

static bool is_binary_operator(const Parser* const parser) {
  switch (parser->current.type) {
    case AxleTokenType::Add: return true;
    case AxleTokenType::Sub: return true;
    case AxleTokenType::Star: return true;
    case AxleTokenType::BackSlash: return true;
    case AxleTokenType::Lesser: return true;
    case AxleTokenType::Greater: return true;
    case AxleTokenType::Or: return true;
    case AxleTokenType::Xor: return true;
    case AxleTokenType::And: return true;
    case AxleTokenType::Equals:
      return parser->next.type == AxleTokenType::Equals && !parser->next.consumed_whitespace;
    case AxleTokenType::Bang:
      return parser->next.type == AxleTokenType::Equals && !parser->next.consumed_whitespace;
    default: return false;
  }
}

static BINARY_OPERATOR parse_binary_operator(Compiler* const comp, Parser* const parser) {
  switch (parser->current.type) {
    case AxleTokenType::Add: advance(comp, parser); return BINARY_OPERATOR::ADD;
    case AxleTokenType::Sub: advance(comp, parser); return BINARY_OPERATOR::SUB;
    case AxleTokenType::Star: advance(comp, parser); return BINARY_OPERATOR::MUL;
    case AxleTokenType::BackSlash: advance(comp, parser); return BINARY_OPERATOR::DIV;
    case AxleTokenType::Lesser: {
        advance(comp, parser);

        if (parser->current.type == AxleTokenType::Lesser && !parser->current.consumed_whitespace) {
          advance(comp, parser);
          return BINARY_OPERATOR::RIGHT_SHIFT;
        }

        return BINARY_OPERATOR::LESSER;
      }
    case AxleTokenType::Greater: {
        advance(comp, parser);

        if (parser->current.type == AxleTokenType::Greater && !parser->current.consumed_whitespace) {
          advance(comp, parser);
          return BINARY_OPERATOR::LEFT_SHIFT;
        }

        return BINARY_OPERATOR::GREATER;
      }
    case AxleTokenType::Equals: {
        advance(comp, parser);
        if (comp->is_panic()) {
          return BINARY_OPERATOR::ADD;//doesnt matter what
        }

        if (parser->current.type == AxleTokenType::Equals && !parser->current.consumed_whitespace) {
          advance(comp, parser);
          return BINARY_OPERATOR::EQUIVALENT;
        }
        break;
      }
    case AxleTokenType::Bang: {
        advance(comp, parser);
        if (comp->is_panic()) {
          return BINARY_OPERATOR::ADD;//doesnt matter what
        }

        if (parser->current.type == AxleTokenType::Equals && !parser->current.consumed_whitespace) {
          advance(comp, parser);
          return BINARY_OPERATOR::NOT_EQ;
        }
        break;
      }
    case AxleTokenType::Or: advance(comp, parser); return BINARY_OPERATOR::OR;
    case AxleTokenType::Xor: advance(comp, parser); return BINARY_OPERATOR::XOR;
    case AxleTokenType::And: advance(comp, parser); return BINARY_OPERATOR::AND;
  }

  comp->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->current),
                     "'{}' is not a valid binary operator", parser->current.string);
  return BINARY_OPERATOR::ADD;//just return whatever and hope everything errors out
}

static void reset_bin_op_span(ASTExpression* expr) {
  assert(expr->expr_type == EXPRESSION_TYPE::BINARY_OPERATOR);

  expr->span.full_path = expr->bin_op.left->span.full_path;
  expr->span.char_start = expr->bin_op.left->span.char_start;
  expr->span.char_end = expr->bin_op.right->span.char_end;
  expr->span.line_start = expr->bin_op.left->span.line_start;
  expr->span.line_end = expr->bin_op.right->span.line_end;
}

static ASTExpression* parse_binary_precidence(Compiler* const comp, Parser* const parser, const uint8_t prev_prec, ASTExpression** base) {
  //precidence for this level
  uint8_t this_prec = precidence_table[(size_t)(*base)->bin_op.op];

  while (!comp->is_panic() && is_binary_operator(parser)) {
    const BINARY_OPERATOR new_op = parse_binary_operator(comp, parser);
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
        parse_unary_op(comp, parser, new_right->bin_op.right);

        reset_bin_op_span(*base);
      }

      ASTExpression* new_base = parse_binary_precidence(comp, parser, this_prec, &(*base)->bin_op.right);

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
      parse_unary_op(comp, parser, new_base->bin_op.right);
      //Set next
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
      parse_unary_op(comp, parser, new_base->bin_op.right);

      reset_bin_op_span(new_base);
    }
  }

  return nullptr;
}

static void parse_binary_operators(Compiler* const comp, Parser* const parser, BINARY_OPERATOR op, ASTExpression* const base) {
  //Has to be heap allocated
  ASTExpression* temp_base = allocate_default<ASTExpression>();
  DEFER(&) { free_destruct_single<ASTExpression>(temp_base); };

  temp_base->set_union(EXPRESSION_TYPE::BINARY_OPERATOR);
  temp_base->bin_op.op    = op;
  temp_base->bin_op.left  = allocate_default<ASTExpression>();
  temp_base->bin_op.right = allocate_default<ASTExpression>();

  *temp_base->bin_op.left = std::move(*base);

  parse_unary_op(comp, parser, temp_base->bin_op.right);

  if (!comp->is_panic() && is_binary_operator(parser)) {
    auto temp = parse_binary_precidence(comp, parser, 0 /* <- will always be lowest precidence*/, &temp_base);

    //Should always return nullptr
    assert(temp == nullptr);
  }

  reset_bin_op_span(temp_base);
  *base = std::move(*temp_base);
}

static void parse_inner_expression(Compiler* const comp, Parser* const parser, ASTExpression* const expr) {
  parse_unary_op(comp, parser, expr);
  if (comp->is_panic()) {
    return;
  }

  if (is_binary_operator(parser)) {
    const BINARY_OPERATOR op = parse_binary_operator(comp, parser);
    if (comp->is_panic()) {
      return;
    }

    parse_binary_operators(comp, parser, op, expr);
    if (comp->is_panic()) {
      return;
    }
  }
}

static void parse_expression(Compiler* const comp, Parser* const parser, ASTExpression* const expr) {
  //May at some point be important to do this
  //I did it once for part of an experiment and im keeping it just because why not
  parse_inner_expression(comp, parser, expr);
}

static const Token* step_scope(const Token* tok, const Token* end);
static const Token* step_brackets(const Token* tok, const Token* end);
static const Token* step_squares(const Token* tok, const Token* end);

static const Token* step_squares(const Token* tok, const Token* end) {
  assert(tok->type == AxleTokenType::Left_Square);

  tok++;

  while (tok < end) {
    switch (tok->type) {
      case AxleTokenType::Left_Square: {
          tok = step_squares(tok, end);
          break;
        }
      case AxleTokenType::Right_Square: {
          tok++;
          return tok;
        }
      case AxleTokenType::Left_Brace: {
          tok = step_scope(tok, end);
          break;
        }
      case AxleTokenType::Right_Brace: {
          assert(false);
          break;
        }
      case AxleTokenType::Left_Bracket: {
          tok = step_brackets(tok, end);
          break;
        }
      case AxleTokenType::Right_Bracket: {
          assert(false);
          break;
        }
      default: {
          tok++;
          break;
        }
    }
  }

  return tok;
}

static const Token* step_brackets(const Token* tok, const Token* end) {
  assert(tok->type == AxleTokenType::Left_Bracket);

  tok++;

  while (tok < end) {
    switch (tok->type) {
      case AxleTokenType::Left_Square: {
          tok = step_squares(tok, end);
          break;
        }
      case AxleTokenType::Right_Square: {
          assert(false);
          break;
        }
      case AxleTokenType::Left_Brace: {
          tok = step_scope(tok, end);
          break;
        }
      case AxleTokenType::Right_Brace: {
          assert(false);
          break;
        }
      case AxleTokenType::Left_Bracket: {
          tok = step_brackets(tok, end);
          break;
        }
      case AxleTokenType::Right_Bracket: {
          tok++;
          return tok;
        }
      default: {
          tok++;
          break;
        }
    }
  }

  return tok;
}

static const Token* step_scope(const Token* tok, const Token* end) {
  assert(tok->type == AxleTokenType::Left_Brace);

  tok++;

  while (tok < end) {
    switch (tok->type) {
      case AxleTokenType::Left_Square: {
          tok = step_squares(tok, end);
          break;
        }
      case AxleTokenType::Right_Square: {
          assert(false);
          break;
        }
      case AxleTokenType::Left_Brace: {
          tok = step_scope(tok, end);
          break;
        }
      case AxleTokenType::Right_Brace: {
          assert(false);
          break;
        }
      case AxleTokenType::Left_Bracket: {
          tok = step_brackets(tok, end);
          break;
        }
      case AxleTokenType::Right_Bracket: {
          tok++;
          return tok;
        }
      default: {
          tok++;
          break;
        }
    }
  }

  return tok;
}

static void parse_structure(Compiler* const comp, Parser* const parser, ASTStructBody* const struct_decl);
static void parse_lambda(Compiler* const comp, Parser* const parser, ASTLambda* const lambda);

static void parse_primary(Compiler* const comp, Parser* const parser, ASTExpression* const expr) {
  //Will always be a primary so can elevate span stuff out of the switch
  Span span ={};
  set_span_start(parser->current, span);
  DEFER(&) {
    set_span_end(parser->prev, span);
    expr->span = std::move(span);
  };

  switch (parser->current.type) {
    case AxleTokenType::Left_Brace: {
        // { ... }

        advance(comp, parser);
        expr->set_union(EXPRESSION_TYPE::TUPLE_LIT);

        while (!comp->is_panic()) {
          expr->tuple_lit.elements.insert_uninit(1);
          auto* new_expr = expr->tuple_lit.elements.back();

          parse_inner_expression(comp, parser, new_expr);
          if (comp->is_panic()) {
            return;
          }

          if (parser->current.type == AxleTokenType::Comma) {
            advance(comp, parser);
          }
          else {
            break;
          }
        }

        if (comp->is_panic()) {
          return;
        }

        expr->tuple_lit.elements.shrink();

        expect(comp, parser, AxleTokenType::Right_Brace);
        break;
      }
    case AxleTokenType::Left_Square: {
        // [ ... ] 

        advance(comp, parser);

        expr->set_union(EXPRESSION_TYPE::ARRAY_EXPR);

        while (!comp->is_panic()) {
          expr->array_expr.elements.insert_uninit(1);
          auto* new_expr = expr->array_expr.elements.back();

          parse_inner_expression(comp, parser, new_expr);
          if (comp->is_panic()) {
            return;
          }

          if (parser->current.type == AxleTokenType::Comma) {
            advance(comp, parser);
          }
          else {
            break;
          }
        }

        if (comp->is_panic()) {
          return;
        }

        expr->array_expr.elements.shrink();

        expect(comp, parser, AxleTokenType::Right_Square);
        break;
      }
    case AxleTokenType::String: {
        // "..."
        expr->set_union(EXPRESSION_TYPE::ASCII_STRING);
        expr->ascii_string = parser->current.string;

        advance(comp, parser);
        break;
      }
    case AxleTokenType::Character: {
        expr->set_union(EXPRESSION_TYPE::ASCII_CHAR);
        expr->ascii_char = parser->current.string->string[0];

        advance(comp, parser);
        break;
      }
    case AxleTokenType::Cast: {
        // cast(... , ...)

        advance(comp, parser);

        expect(comp, parser, AxleTokenType::Left_Bracket);
        if (comp->is_panic()) {
          return;
        }

        expr->set_union(EXPRESSION_TYPE::CAST);

        parse_type(comp, parser, &expr->cast.type);
        if (comp->is_panic()) {
          return;
        }

        expect(comp, parser, AxleTokenType::Comma);
        if (comp->is_panic()) {
          return;
        }

        expr->cast.expr = allocate_default<ASTExpression>();
        parse_inner_expression(comp, parser, expr->cast.expr);
        if (comp->is_panic()) {
          return;
        }

        expect(comp, parser, AxleTokenType::Right_Bracket);
        if (comp->is_panic()) {
          return;
        }
        break;
      }
    case AxleTokenType::Number: {
        expr->set_union(EXPRESSION_TYPE::VALUE);
        expr->value.value = string_to_uint(parser->current.string->string);
        advance(comp, parser);

        if (!parser->current.consumed_whitespace && parser->current.type == AxleTokenType::Identifier) {
          expr->value.suffix = parser->current.string;
          advance(comp, parser);
        }
        break;
      }
    case AxleTokenType::Identifier: {
        //Name or function call

        if (parser->next.type == AxleTokenType::Left_Bracket) {
          //Is function call
          expr->set_union(EXPRESSION_TYPE::FUNCTION_CALL);
          FunctionCallExpr& call = expr->call;

          call.function_name = parser->current.string;

          //ident
          advance(comp, parser);
          if (comp->is_panic()) {
            return;
          }

          //left bracket
          advance(comp, parser);
          if (comp->is_panic()) {
            return;
          }

          //Arguments
          if (parser->current.type != AxleTokenType::Right_Bracket) {
            while (!comp->is_panic()) {
              call.arguments.insert_uninit(1);
              ASTExpression* arg = call.arguments.back();
              parse_inner_expression(comp, parser, arg);


              if (parser->current.type == AxleTokenType::Right_Bracket) {
                break;
              }
              else if (parser->current.type == AxleTokenType::Comma) {
                advance(comp, parser);
                continue;
              }
              else {
                //ERROR
                comp->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->current),
                                   "Expected '{}', Found '{}'",
                                   AxleTokenType::Comma, parser->current.type);
                return;
              }
            }

            if (comp->is_panic()) {
              return;
            }

            call.arguments.shrink();//reduce over allocating space
          }

          advance(comp, parser);
        }
        else {
          expr->set_union(EXPRESSION_TYPE::NAME);
          expr->name = parser->current.string;

          advance(comp, parser);
        }

        break;
      }
    case AxleTokenType::True:
    case AxleTokenType::False: {
        expr->set_union(EXPRESSION_TYPE::ENUM);
        expr->enum_value.name = parser->current.string;

        advance(comp, parser);
        break;
      }
    case AxleTokenType::Nullptr: {
        expr->set_union(EXPRESSION_TYPE::NULLPTR);
        advance(comp, parser);
        break;
      }
    case AxleTokenType::Struct: {
        expr->set_union(EXPRESSION_TYPE::STRUCT);
        parse_structure(comp, parser, &expr->struct_body);
        break;
      }
    case AxleTokenType::Left_Bracket: {
        //Function or expression

        constexpr auto parser_is_func = [](const Token* tokens, const Token* end)->bool {
          const Token* new_tok = step_brackets(tokens, end);
          return (new_tok + 1) < end
            && new_tok->type == AxleTokenType::Sub
            && (new_tok + 1)->type == AxleTokenType::Greater;
        };

        const bool is_func = parser_is_func(parser->stream.i - 2, parser->stream.end);

        if (is_func) {
          expr->set_union(EXPRESSION_TYPE::LAMBDA);
          expr->lambda.lambda = allocate_default<ASTLambda>();

          parse_lambda(comp, parser, expr->lambda.lambda);
        }
        else {
          expect(comp, parser, AxleTokenType::Left_Bracket);
          parse_inner_expression(comp, parser, expr);
          expect(comp, parser, AxleTokenType::Right_Bracket);
        }
        break;
      }
    default: comp->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->current),
                                "Unexpected Token Type '{}'", parser->current.type);
  }
}

static void parse_suffix(Compiler* const comp, Parser* const parser, ASTExpression* const expr) {
  while (!comp->is_panic()) {
    switch (parser->current.type) {
      case AxleTokenType::Left_Square: {

          advance(comp, parser);
          if (comp->is_panic()) {
            return;
          }

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

          parse_inner_expression(comp, parser, index);
          if (comp->is_panic()) {
            return;
          }

          expect(comp, parser, AxleTokenType::Right_Square);
          break;
        }
      case AxleTokenType::Full_Stop: {
          //Members
          advance(comp, parser);
          if (comp->is_panic()) {
            return;
          }
          ASTExpression* const new_expr = allocate_default<ASTExpression>();

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


          expr->set_union(EXPRESSION_TYPE::MEMBER);

          expr->member.expr = new_expr;

          parse_name(comp, parser, &expr->member.name);
          break;
        }
      default:
        //No more suffixes suffix
        return;
    }
  }
}

static void parse_primary_and_suffix(Compiler* const comp, Parser* const parser, ASTExpression* const expr) {
  parse_primary(comp, parser, expr);
  if (comp->is_panic()) {
    return;
  }
  parse_suffix(comp, parser, expr);
}

static void parse_unary_op(Compiler* const comp, Parser* const parser, ASTExpression* const expr) {
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
        advance(comp, parser);
        if (comp->is_panic()) {
          return;
        }

        expr->un_op.expr = allocate_default<ASTExpression>();

        parse_unary_op(comp, parser, expr->un_op.expr);
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
        advance(comp, parser);
        if (comp->is_panic()) {
          return;
        }

        expr->un_op.expr = allocate_default<ASTExpression>();

        parse_unary_op(comp, parser, expr->un_op.expr);
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
        advance(comp, parser);
        if (comp->is_panic()) {
          return;
        }

        expr->un_op.expr = allocate_default<ASTExpression>();

        parse_unary_op(comp, parser, expr->un_op.expr);
        break;
      }

    default:
      parse_primary_and_suffix(comp, parser, expr);
      break;
  }
}

static void parse_type(Compiler* const comp, Parser* const parser, ASTType* const type) {
  set_span_start(parser->current, type->span);
  DEFER(&) { set_span_end(parser->prev, type->span); };

  switch (parser->current.type) {
    case AxleTokenType::Identifier: {
        type->set_union(TYPE_TYPE::NORMAL);
        type->name = parser->current.string;
        advance(comp, parser);
        if (comp->is_panic()) {
          return;
        }
        break;
      }
    case AxleTokenType::Left_Square: {
        // [ BASE ; EXPR ]
        type->set_union(TYPE_TYPE::ARRAY);

        advance(comp, parser);//[
        if (comp->is_panic()) {
          return;
        }

        //Base Type
        type->arr.base = allocate_default<ASTType>();
        parse_type(comp, parser, type->arr.base);
        if (comp->is_panic()) {
          return;
        }

        expect(comp, parser, AxleTokenType::Semicolon);
        if (comp->is_panic()) {
          return;
        }

        //Expression
        type->arr.expr = allocate_default<ASTExpression>();
        parse_expression(comp, parser, type->arr.expr);
        if (comp->is_panic()) {
          return;
        }

        expect(comp, parser, AxleTokenType::Right_Square);
        if (comp->is_panic()) {
          return;
        }
        break;
      }
    case AxleTokenType::Star: {
        // *BASE
        type->set_union(TYPE_TYPE::PTR);

        advance(comp, parser);//*
        if (comp->is_panic()) {
          return;
        }

        //Base
        type->ptr.base = allocate_default<ASTType>();
        parse_type(comp, parser, type->ptr.base);
        break;
      }
    default: comp->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->current),
                                "Expected A Type! Found '{}'", parser->current.type);
  }
}

static void parse_typed_name(Compiler* const comp, Parser* const parser, ASTType* const type, const InternString** name) {
  parse_name(comp, parser, name);
  if (comp->is_panic()) {
    return;
  }

  expect(comp, parser, AxleTokenType::Colon);
  if (comp->is_panic()) {
    return;
  }

  parse_type(comp, parser, type);
  if (comp->is_panic()) {
    return;
  }
}

static void parse_block(Compiler* const comp, Parser* const parser, ASTBlock* const block);

static void parse_decl(Compiler* const comp, Parser* const parser, ASTDecl* const decl) {
  set_span_start(parser->current, decl->span);
  DEFER(&) { set_span_end(parser->prev, decl->span); };

  if (parser->current.type != AxleTokenType::Identifier) {
    comp->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->current),
                       "Expected Identifier as declarations start with identifiers");
    return;
  }

  decl->name = parser->current.string;
  advance(comp, parser);
  if (comp->is_panic()) {
    return;
  }

  expect(comp, parser, AxleTokenType::Colon);
  if (comp->is_panic()) {
    return;
  }

  //Explicit type??
  if (parser->current.type != AxleTokenType::Equals && parser->current.type != AxleTokenType::Colon) {
    decl->type = allocate_default<ASTType>();

    parse_type(comp, parser, decl->type);
    if (comp->is_panic()) {
      return;
    }
  }

  if (parser->current.type == AxleTokenType::Equals) {
    decl->compile_time_const = false;
    advance(comp, parser);
  }
  else if (parser->current.type == AxleTokenType::Colon) {
    decl->compile_time_const = true;
    advance(comp, parser);
  }
  else {
    comp->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->current),
                       "Expected ':' or '=' to continue the delaration");
    return;
  }

  if (comp->is_panic()) {
    return;
  }

  decl->expr = allocate_default<ASTExpression>();
  parse_expression(comp, parser, decl->expr);
}

static void parse_statement(Compiler* const comp, Parser* const parser, ASTStatement* const statement) {
  Span span ={};
  set_span_start(parser->current, span);
  DEFER(&) {
    set_span_end(parser->prev, span);
    statement->span = std::move(span);
  };

  switch (parser->current.type) {
    case AxleTokenType::Left_Brace: {
        statement->set_union(STATEMENT_TYPE::BLOCK);

        parse_block(comp, parser, &statement->block);
        return;
      }
    case AxleTokenType::Return: {
        statement->set_union(STATEMENT_TYPE::RETURN);

        advance(comp, parser);
        if (comp->is_panic()) {
          return;
        }

        statement->expression.expr = allocate_default<ASTExpression>();
        parse_expression(comp, parser, statement->expression.expr);
        if (comp->is_panic()) {
          return;
        }

        expect(comp, parser, AxleTokenType::Semicolon);
        break;
      }
    case AxleTokenType::If: {
        advance(comp, parser);
        if (comp->is_panic()) {
          return;
        }
        statement->set_union(STATEMENT_TYPE::IF_ELSE);

        expect(comp, parser, AxleTokenType::Left_Bracket);
        if (comp->is_panic()) {
          return;
        }

        statement->if_else.condition = allocate_default<ASTExpression>();
        parse_expression(comp, parser, statement->if_else.condition);
        if (comp->is_panic()) {
          return;
        }

        expect(comp, parser, AxleTokenType::Right_Bracket);
        if (comp->is_panic()) {
          return;
        }


        statement->if_else.if_statement = allocate_default<ASTStatement>();
        parse_statement(comp, parser, statement->if_else.if_statement);
        if (comp->is_panic()) {
          return;
        }

        if (parser->current.type == AxleTokenType::Else) {
          advance(comp, parser);
          if (comp->is_panic()) {
            return;
          }
          statement->if_else.else_statement = allocate_default<ASTStatement>();
          parse_statement(comp, parser, statement->if_else.else_statement);
        }
        else {
          statement->if_else.else_statement = nullptr;
        }
        break;
      }
    case AxleTokenType::While: {
        advance(comp, parser);
        if (comp->is_panic()) {
          return;
        }
        statement->set_union(STATEMENT_TYPE::WHILE);
        expect(comp, parser, AxleTokenType::Left_Bracket);
        if (comp->is_panic()) {
          return;
        }

        statement->while_loop.condition = allocate_default<ASTExpression>();
        parse_expression(comp, parser, statement->while_loop.condition);
        if (comp->is_panic()) {
          return;
        }

        expect(comp, parser, AxleTokenType::Right_Bracket);
        if (comp->is_panic()) {
          return;
        }

        statement->while_loop.statement = allocate_default<ASTStatement>();
        parse_statement(comp, parser, statement->while_loop.statement);
        break;
      }
    default: {
        if (parser->current.type == AxleTokenType::Identifier && parser->next.type == AxleTokenType::Colon) {
          //Declaration!
          statement->set_union(STATEMENT_TYPE::LOCAL);
          parse_decl(comp, parser, &statement->local);
          break;
        }

        //Expression or assignment

        //Get the assign to expression
        ASTExpression* expr = allocate_default<ASTExpression>();
        parse_expression(comp, parser, expr);
        if (comp->is_panic()) {
          return;
        }

        if (parser->current.type == AxleTokenType::Equals) {
          advance(comp, parser);
          if (comp->is_panic()) {
            return;
          }

          //is assignment
          statement->set_union(STATEMENT_TYPE::ASSIGN);
          statement->assign.assign_to = expr;

          //reset the expr
          expr = allocate_default<ASTExpression>();
          parse_expression(comp, parser, expr);
          if (comp->is_panic()) {
            return;
          }

          statement->assign.value = expr;
          expect(comp, parser, AxleTokenType::Semicolon);
        }
        else if (parser->current.type == AxleTokenType::Semicolon) {
          //is expressiom
          statement->set_union(STATEMENT_TYPE::EXPRESSION);
          statement->expression.expr = expr;
        }
        else {
          comp->report_error(ERROR_CODE::SYNTAX_ERROR, expr->span,
                             "Expected one of '{}' or '{}' at the end of the expression shown\n"
                             "Found '{}'",
                             AxleTokenType::Equals, AxleTokenType::Semicolon,
                             parser->current.type);
          return;
        }
        break;
      }
  }
}


static void parse_block(Compiler* const comp, Parser* const parser, ASTBlock* const block) {
  expect(comp, parser, AxleTokenType::Left_Brace);
  if (comp->is_panic()) {
    return;
  }

  while (!comp->is_panic() && parser->current.type != AxleTokenType::Right_Brace) {
    if (parser->current.type == AxleTokenType::Semicolon) {
      //Empty statement
      advance(comp, parser);
      if (comp->is_panic()) {
        return;
      }
      continue;
    }

    block->block.insert_uninit(1);
    parse_statement(comp, parser, block->block.back());
  }

  if (comp->is_panic()) {
    return;
  }

  block->block.shrink();//reduce over allocating space
  expect(comp, parser, AxleTokenType::Right_Brace);
}

static void parse_function_signature(Compiler* const comp, Parser* const parser, ASTFuncSig* const sig) {
  expect(comp, parser, AxleTokenType::Left_Bracket);
  if (comp->is_panic()) {
    return;
  }

  //Parameters
  if (parser->current.type != AxleTokenType::Right_Bracket) {
    while (!comp->is_panic()) {
      sig->parameters.insert_uninit(1);
      ASTDecl* param = sig->parameters.back();
      param->expr = nullptr;
      param->type = allocate_default<ASTType>();

      parse_typed_name(comp, parser, param->type, &param->name);

      if (parser->current.type == AxleTokenType::Right_Bracket) {
        break;
      }
      else if (parser->current.type == AxleTokenType::Comma) {
        advance(comp, parser);
        if (comp->is_panic()) {
          return;
        }
        continue;
      }
      else {
        //ERROR
        comp->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->current),
                           "Expected a comma!");
        return;
      }
    }
  }

  if (comp->is_panic()) {
    return;
  }

  sig->parameters.shrink();//reduce over allocating space

  advance(comp, parser);
  if (comp->is_panic()) {
    return;
  }

  // ->
  expect(comp, parser, AxleTokenType::Sub);// -
  if (comp->is_panic()) {
    return;
  }

  expect(comp, parser, AxleTokenType::Greater);// >
  if (comp->is_panic()) {
    return;
  }

  parse_type(comp, parser, &sig->return_type);
}

static void parse_lambda(Compiler* const comp, Parser* const parser, ASTLambda* const func) {
  parse_function_signature(comp, parser, &func->sig);

  if (parser->current.type == AxleTokenType::Left_Brace) {
    parse_block(comp, parser, &func->body);
  }
  else if (parser->current.type == AxleTokenType::Semicolon) {
    advance(comp, parser);
  }
  else {
    comp->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->current),
                       "Expected '{}' or '{}'\nFound '{}'",
                       AxleTokenType::Left_Brace, AxleTokenType::Semicolon,
                       parser->current.type);
    return;
  }

  //Load to a compilation unit
  add_comp_unit_for_lambda(comp, parser->current_namespace, func);
}

static void parse_structure(Compiler* const comp, Parser* const parser, ASTStructBody* const struct_decl) {
  expect(comp, parser, AxleTokenType::Struct);
  if (comp->is_panic()) {
    return;
  }

  expect(comp, parser, AxleTokenType::Left_Brace);
  if (comp->is_panic()) {
    return;
  }

  while (!comp->is_panic() && parser->current.type != AxleTokenType::Right_Brace) {
    struct_decl->elements.insert_uninit(1);
    ASTTypedName* tn = struct_decl->elements.back();

    parse_typed_name(comp, parser, &tn->type, &tn->name);
    if (comp->is_panic()) {
      return;
    }
    expect(comp, parser, AxleTokenType::Semicolon);
  }

  if (comp->is_panic()) {
    return;
  }

  struct_decl->elements.shrink();

  expect(comp, parser, AxleTokenType::Right_Brace);

  //Load to a compilation unit
  add_comp_unit_for_struct(comp, parser->current_namespace, struct_decl);
}

void parse_file(Compiler* const comp, Parser* const parser, ASTFile* const file) {
  if (parser->current.type == AxleTokenType::DLLHeader) {
    file->header.is_dll_header = true;

    ASTImport* imp = &file->header.dll_header;
    advance(comp, parser);
    if (comp->is_panic()) {
      return;
    }

    if (parser->current.type != AxleTokenType::String) {
      comp->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->current),
                         "Expected a string!");
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

    advance(comp, parser);

    if (comp->is_panic()) {
      return;
    }
    expect(comp, parser, AxleTokenType::Semicolon);
  }

  if (comp->is_panic()) {
    return;
  }

  for (AxleTokenType current = parser->current.type;
       !comp->is_panic() && current != AxleTokenType::End;
       current = parser->current.type)
  {
    if (parser->current.type == AxleTokenType::Import || parser->current.type == AxleTokenType::Stdlib) {
      //Import
      file->imports.insert_uninit(1);
      ASTImport* imp = file->imports.back();

      imp->std = (parser->current.type == AxleTokenType::Stdlib);

      advance(comp, parser);
      if (comp->is_panic()) {
        return;
      }

      if (parser->current.type != AxleTokenType::String) {
        comp->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->current),
                           "Expected a string!");
        return;
      }

      imp->relative_path = parser->current.string;

      //Load the span
      set_span_start(parser->current, imp->span);
      DEFER(&) {
        set_span_end(parser->prev, imp->span);
      };

      advance(comp, parser);
      if (comp->is_panic()) {
        return;
      }
      expect(comp, parser, AxleTokenType::Semicolon);
    }
    else if (parser->current.type == AxleTokenType::Identifier
             && parser->next.type == AxleTokenType::Colon) {
      //Decl
      file->decls.insert_uninit(1);
      ASTDecl* decl = file->decls.back();

      parse_decl(comp, parser, decl);
    }
    else {
      comp->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->current),
                         "Unexpected token");
    }
  }

  //reduce over allocating space
  file->imports.shrink();
  file->decls.shrink();
}

void Printer::newline() const {
  IO::print('\n');

  for (size_t i = 0; i < tabs; i++) {
    IO::print("  ");
  }
}

static void print_function_sig(Printer* const, const ASTFuncSig*);
static void print_ast_statement(Printer* const, const ASTStatement*);

static void print_type(Printer* const printer, const ASTType* type) {
  switch (type->type_type) {
    case TYPE_TYPE::NORMAL:
      IO::print(type->name->string);
      break;
    case TYPE_TYPE::ARRAY:
      IO::print('[');
      print_type(printer, type->arr.base);
      IO::print("; ");
      print_ast_expression(printer, type->arr.expr);
      IO::print(']');
      break;
    case TYPE_TYPE::PTR:
      IO::print('*');
      print_type(printer, type->arr.base);
      break;
  }
}

static void print_ast_block(Printer* const printer, const ASTBlock* block) {
  IO::print('{');

  auto i = block->block.begin();
  auto end = block->block.end();

  if (i < end) {
    printer->tabs++;

    for (; i < end; i++) {
      print_ast_statement(printer, i);
    }

    printer->tabs--;
    printer->newline();
  }

  IO::print('}');
}

void print_ast_expression(Printer* const printer, const ASTExpression* expr) {
  switch (expr->expr_type) {
    case EXPRESSION_TYPE::LAMBDA: {
        const ASTLambda* lambda = expr->lambda.lambda;

        print_function_sig(printer, &lambda->sig);
        print_ast_block(printer, &lambda->body);
        break;
      }
    case EXPRESSION_TYPE::MEMBER: {
        print_ast_expression(printer, expr->member.expr);
        printf(".%s", expr->member.name->string);
        break;
      }
    case EXPRESSION_TYPE::INDEX: {
        print_ast_expression(printer, expr->index.expr);
        IO::print('[');
        print_ast_expression(printer, expr->index.index);
        IO::print(']');
        break;
      }
    case EXPRESSION_TYPE::TUPLE_LIT: {
        IO::print("{ ");
        auto i = expr->array_expr.elements.begin();
        const auto end = expr->array_expr.elements.end();

        if (i < end) {
          print_ast_expression(printer, i);
          i++;

          for (; i < end; i++) {
            IO::print(", ");
            print_ast_expression(printer, i);
          }
        }

        IO::print(" }");
        break;
      }
    case EXPRESSION_TYPE::ARRAY_EXPR: {
        IO::print('[');
        auto i = expr->array_expr.elements.begin();
        const auto end = expr->array_expr.elements.end();

        if (i < end) {
          print_ast_expression(printer, i);
          i++;

          for (; i < end; i++) {
            IO::print(", ");
            print_ast_expression(printer, i);
          }
        }

        IO::print(']');
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
            print_ast_expression(printer, i);
            IO::print(", ");
          }

          print_ast_expression(printer, i);
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
        print_type(printer, &expr->cast.type);
        IO::print(", ");
        print_ast_expression(printer, expr->cast.expr);
        IO::print(')');
        break;
      }
    case EXPRESSION_TYPE::UNARY_OPERATOR:
      IO::print(UNARY_OP_STRING::get(expr->un_op.op));
      print_ast_expression(printer, expr->un_op.expr);
      break;
    case EXPRESSION_TYPE::BINARY_OPERATOR:
      IO::print("(");
      print_ast_expression(printer, expr->bin_op.left);

      printf(" %s ", BINARY_OP_STRING::get(expr->bin_op.op));

      print_ast_expression(printer, expr->bin_op.right);
      IO::print(")");
      break;
  }
}

static void print_ast_decl(Printer* const printer, const ASTDecl* decl) {
  if (decl->type != nullptr && decl->expr != nullptr) {
    printf("%s: ", decl->name->string);
    print_type(printer, decl->type);

    if (decl->compile_time_const) {
      IO::print(" : ");
    }
    else {
      IO::print(" = ");
    }
    print_ast_expression(printer, decl->expr);
  }
  else if (decl->expr != nullptr) {
    if (decl->compile_time_const) {
      printf("%s :: ", decl->name->string);
    }
    else {
      printf("%s := ", decl->name->string);
    }

    print_ast_expression(printer, decl->expr);
  }
  else if (decl->type != nullptr) {
    printf("%s: ", decl->name->string);
    print_type(printer, decl->type);
  }
  else {
    assert(false);//shouldnt be possible to be here
  }
}

static void print_ast_statement(Printer* const printer, const ASTStatement* statement) {
  printer->newline();

  switch (statement->type) {
    case STATEMENT_TYPE::ASSIGN:
      print_ast_expression(printer, statement->assign.assign_to);
      IO::print(" = ");
      print_ast_expression(printer, statement->assign.value);
      IO::print(';');
      break;
    case STATEMENT_TYPE::LOCAL:
      print_ast_decl(printer, &statement->local);
      IO::print(';');
      break;
    case STATEMENT_TYPE::RETURN:
      IO::print("return ");
      print_ast_expression(printer, statement->expression.expr);
      IO::print(';');
      break;
    case STATEMENT_TYPE::EXPRESSION:
      print_ast_expression(printer, statement->expression.expr);
      IO::print(';');
      break;
    case STATEMENT_TYPE::WHILE:
      IO::print("while(");
      print_ast_expression(printer, statement->while_loop.condition);
      IO::print(") ");

      if (statement->while_loop.statement->type != STATEMENT_TYPE::BLOCK) {
        printer->tabs++;
      }

      print_ast_statement(printer, statement->while_loop.statement);

      if (statement->while_loop.statement->type != STATEMENT_TYPE::BLOCK) {
        printer->tabs--;
      }
      break;
    case STATEMENT_TYPE::IF_ELSE:
      IO::print("if(");
      print_ast_expression(printer, statement->if_else.condition);
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
        print_ast_block(printer, &statement->block);
        break;
      }
  }
}

static void print_function_sig(Printer* const printer, const ASTFuncSig* sig) {
  IO::print('(');


  //Parameters
  {
    auto p_i = sig->parameters.begin();
    const auto p_end = sig->parameters.end();

    if (p_end - p_i > 0) {
      for (; p_i < (p_end - 1); p_i++) {
        print_ast_decl(printer, p_i);
        IO::print(", ");
      }

      print_ast_decl(printer, p_i);
    }
  }

  IO::print(") -> ");
  print_type(printer, &sig->return_type);
  IO::print(' ');
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
      if (i->std) {
        printf("#stdlib \"%s\";\n", i->relative_path->string);
      }
      else {
        printf("#import \"%s\";\n", i->relative_path->string);
      }
    }

    if (file->imports.size > 0) {
      IO::print('\n');//Extra new life for nice formatting
    }
  }

  //Declarations
  {
    auto i = file->decls.begin();
    const auto end = file->decls.end();

    for (; i < end; i++) {
      Printer printer ={};
      print_ast_decl(&printer, i);
      IO::print(';');
      printer.newline();
      printer.newline();
    }
  }
}