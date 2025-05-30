#include "ast.h"
#include "parser.h"
#include "compiler.h"

#include <AxleUtil/memory.h>
#include <AxleUtil/io.h>

#include <Axle/tracing_wrapper.h>

namespace IO_Single = Axle::IO_Single;

struct KeywordPair {
  Axle::ViewArr<const char> keyword;
  AxleTokenType type = AxleTokenType::End;
};

constexpr KeywordPair keywords[] = {
#define MODIFY(n, str) {Axle::lit_view_arr(str), AxleTokenType :: n},
  AXLE_TOKEN_KEYWORDS
#undef MODIFY
};

constexpr size_t num_keywords = sizeof(keywords) / sizeof(KeywordPair);

constexpr KeywordPair operators[] = {
#define MODIFY(n, str) {Axle::lit_view_arr(str), AxleTokenType :: n},
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
  return is_dec_number(c) || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F');
}

constexpr static bool is_any_digit(const char c) {
  return is_hex_number(c);
}

[[maybe_unused]] constexpr static  bool is_letter_or_number(const char c) {
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
  ASSERT(0 < len && len <= 64);

  const auto get_digit = [](const char c) -> u8 {
    ASSERT(is_hex_number(c));

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
      return '\0';
    }
  };

  constexpr auto base = 0x10;
  u64 shift = 1;
  u64 result = 0;
  for (u64 digit = 0; digit < len; digit++) {
    u64 digit_val = get_digit(digits[len - digit - 1]);
    ASSERT(digit_val <= 0xF);
    digit_val *= shift;

    shift *= base;
    result += digit_val;
  }

  return result;
}

static u64 parse_dec_uint(const char* digits, const u64 len) {
  ASSERT(0 < len && len <= Axle::MAX_DECIMAL_U64_DIGITS);

  const auto get_digit = [](const char c) -> u8 {
    ASSERT(is_dec_number(c));
    return c - '0';
  };

  constexpr auto base = 10;
  u64 shift = 1;
  u64 result = 0;
  for (u64 digit = 0; digit < len; digit++) {
    u64 digit_val = get_digit(digits[len - digit - 1]);
    ASSERT(digit_val <= 9);
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

constexpr static bool is_new_line(Lexer* const lex) {
  return lex->top[0] == '\n' || lex->top[0] == '\r';
}

constexpr static void skip_whitespace(Lexer* const lex) {
  constexpr auto n_newline = [](Lexer* const lex) {
    lex->curr_pos.line += 1;
    lex->curr_pos.character = 0;

    lex->top += 1;
    if (lex->top < lex->end && lex->top[0] == '\r') {
      lex->top += 1;
    }
  };

  constexpr auto r_newline = [](Lexer* const lex) {
    lex->curr_pos.line += 1;
    lex->curr_pos.character = 0;
    lex->top += 1;

    if (lex->top < lex->end && lex->top[0] == '\n') {
      lex->top += 1;
    }
  };

  while (lex->top < lex->end) {
    char c = lex->top[0];

    switch (c) {
      case ' ':
      case '\t':
      case '\f':
        lex->top += 1;
        lex->curr_pos.character += 1;
        break;
      case '/': {
          if (lex->top + 1 < lex->end && lex->top[1] == '/') {
            //Is a comment
            lex->top += 2;
            lex->curr_pos.character += 2;

            while (lex->top < lex->end) {
              c = lex->top[0];

              if (c == '\n') {
                n_newline(lex);
                break;
              }
              else if (c == '\r') {
                r_newline(lex);
                break;
              }
              else if (c == '\0') {
                return;
              }

              lex->top += 1;
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

static Token lex_identifier(CompilerGlobals* comp, Lexer* const lex) {
  const char* const name_base = lex->top;

  do {
    lex->top++;
    lex->curr_pos.character++;
  } while (lex->top < lex->end && (is_identifier_char(lex->top[0]) || is_any_digit(lex->top[0])));

  const size_t ident_len = lex->top - name_base;

  Token ident = {};
  ident.type = AxleTokenType::Identifier;
  ident.string = comp->services.strings.get()->intern(name_base, ident_len);

  for (size_t i = 0; i < num_keywords; i++) {
    const KeywordPair& pair = keywords[i];

    if (pair.keyword.size == ident_len
        && Axle::memeq_ts<char>(pair.keyword.data, ident.string->string, ident_len)) {
      //Is keyword
      ident.type = pair.type;
      //Exit early
      return ident;
    }
  }

  //Not a keyword 
  return ident;
}

static Token lex_number(CompilerGlobals* comp, CompilerThread* comp_thread, Lexer* const lex) {
  const char* const number_base = lex->top;

  lex->top++;
  lex->curr_pos.character++;

  if (lex->top < lex->end && (number_base[0] == '0' && (lex->top[0] == 'x' || lex->top[0] == 'X'))) {
    lex->top += 1;
    lex->curr_pos.character += 1;

    if (!(lex->top < lex->end && is_any_digit(lex->top[0]))) {
      Span span = span_of_lex(lex);
      comp_thread->report_error(ERROR_CODE::LEXING_ERROR, span, "0x/0X is not a valid integer");
      return {};
    }
    else {
      lex->top += 1;
      lex->curr_pos.character += 1;
    }
  }

  while (lex->top < lex->end && is_any_digit(lex->top[0])) {
    lex->top++;
    lex->curr_pos.character++;
  }

  const size_t ident_length = lex->top - number_base;

  Token num = {};
  num.type = AxleTokenType::Number;
  num.string = comp->services.strings.get()->intern(number_base, ident_length);

  return num;
}

static Token make_operator_token(CompilerGlobals* comp, CompilerThread* comp_thread, Lexer* lex) {
  auto i = operators;
  auto end = operators + num_operators;

  for (; i < end; i++) {
    const KeywordPair& pair = *i;

    ASSERT(pair.keyword.size == 1);

    if (pair.keyword[0] == lex->top[0]) {
      Token tok;
      tok.type = pair.type;
      tok.string = comp->services.strings.get()->intern(pair.keyword);

      lex->top += 1;
      lex->curr_pos.character += 1;

      return tok;
    }
  }

  Span span = span_of_lex(lex);
  comp_thread->report_error(ERROR_CODE::LEXING_ERROR, span, "Unlexable character: '{}'", Axle::DisplayChar{ lex->top[0] });
  return {};
}

static Token lex_char(CompilerGlobals* const comp, CompilerThread* const comp_thread, Lexer* const lex) {
  lex->top++;
  lex->curr_pos.character++;

  if (lex->top >= lex->end) {
    Span span = span_of_lex(lex);
    comp_thread->report_error(ERROR_CODE::LEXING_ERROR, span, "Invalid character");
    return {};
  }

  char out[2] = { lex->top[0], '\0' };

  lex->top++;
  lex->curr_pos.character++;

  if (out[0] == '\\') {
    if (lex->top >= lex->end) {
      Span span = span_of_lex(lex);
      comp_thread->report_error(ERROR_CODE::LEXING_ERROR, span, "Invalid character");
      return {};
    }

    switch (lex->top[0]) {
      case '0': out[0] = '\0'; break;
      case '\\': out[0] = '\\'; break;
      case 'n': out[0] = '\n'; break;
      case 'r': out[0] = '\r'; break;
      case 't': out[0] = '\t'; break;
      case '\'': out[0] = '\''; break;
      default: {
          Span span = span_of_lex(lex);
          comp_thread->report_error(ERROR_CODE::LEXING_ERROR, span, "Invalid escaped character");
          return {};
        }
    }

    lex->top++;
    lex->curr_pos.character++;
  }

  if (lex->top < lex->end && lex->top[0] == '\'') {
    lex->top++;
    lex->curr_pos.character++;

    Token tok;
    tok.type = AxleTokenType::Character;
    tok.string = comp->services.strings.get()->intern(out, 2);

    return tok;
  }
  else {
    Span span = span_of_lex(lex);
    comp_thread->report_error(ERROR_CODE::LEXING_ERROR, span, "Character literal was not closed!");
    return {};
  }
}

static Token lex_string(CompilerGlobals* const comp, CompilerThread* const comp_thread, Lexer* const lex) {
  lex->top++;
  lex->curr_pos.character++;

  Axle::Array<char> out_str = {};

  while (lex->top < lex->end && !is_new_line(lex) && lex->top[0] != '\0' && lex->top[0] != '"') {
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
        default: {
            Span span = span_of_lex(lex);
            comp_thread->report_error(ERROR_CODE::LEXING_ERROR, span, "Invalid escaped character");
            return {};
          }
      }

      lex->top++;
      lex->curr_pos.character++;
    }
  }

  if (lex->top < lex->end && lex->top[0] == '"') {
    lex->top++;
    lex->curr_pos.character++;

    Token tok;
    tok.type = AxleTokenType::String;
    tok.string = comp->services.strings.get()->intern(out_str.data, out_str.size);

    return tok;
  }
  else {
    Span span = span_of_lex(lex);

    comp_thread->report_error(ERROR_CODE::LEXING_ERROR, span, "String was not closed!");
    return {};
  }
}

Span span_of_lex(const Lexer* const lex) {
  Span span = {};
  span.full_path = lex->file_path;
  span.char_start = lex->save_pos.character;
  span.line_start = lex->save_pos.line;
  span.char_end = lex->curr_pos.character;
  span.line_end = lex->curr_pos.line;
  return span;
}

static Token lex_intrinsic(CompilerGlobals* const comp, CompilerThread* const comp_thread, Lexer* const lex) {
  ASSERT(lex->top[0] == '#');

  lex->top++;
  lex->curr_pos.character++;

  if (!is_identifier_char(lex->top[0])) {
    Span span = span_of_lex(lex);

    comp_thread->report_error(ERROR_CODE::LEXING_ERROR, span, "'#' must be accompanied by an identifier");
    return {};
  }

  const char* const name_base = lex->top;

  do {
    lex->top++;
    lex->curr_pos.character++;
  } while (is_identifier_char(lex->top[0]) || is_any_digit(lex->top[0]));

  const size_t ident_len = lex->top - name_base;

  Token ident = {};
  ident.type = AxleTokenType::Intrinsic;
  ident.string = comp->services.strings.get()->intern(name_base, ident_len);

  return ident;
}

static Token lex_unpositioned_token(CompilerGlobals* const comp, CompilerThread* comp_thread, Lexer* const lex) {
  const char c = lex->top[0];

  if (lex->top >= lex->end || c == '\0') {
    // \0 is the end of file
    Token eof = {};
    eof.type = AxleTokenType::End;
    eof.string = comp->services.strings.get()->intern(Axle::lit_view_arr("End of file"));

    return eof;
  }
  else if (is_identifier_char(lex->top[0])) {
    return lex_identifier(comp, lex);
  }
  else if (is_dec_number(c)) {
    return lex_number(comp, comp_thread, lex);
  }
  else if (c == '"') {
    return lex_string(comp, comp_thread, lex);
  }
  else if (c == '\'') {
    return lex_char(comp, comp_thread, lex);
  }
  else if (c == '#') {
    return lex_intrinsic(comp, comp_thread, lex);
  }

  return make_operator_token(comp, comp_thread, lex);
}

static Token lex_token(CompilerGlobals* const comp_globals, CompilerThread* const comp_thread, Lexer* const lex) {
  const auto* save_top = lex->top;
  skip_whitespace(lex);

  const bool consumed_whitespace = save_top != lex->top;

  lex->save_pos = lex->curr_pos;
  Token tok = lex_unpositioned_token(comp_globals, comp_thread, lex);
  tok.pos.line = lex->curr_pos.line;
  tok.pos.character_start = lex->save_pos.character;
  tok.pos.character_end = lex->curr_pos.character;
  tok.consumed_whitespace = consumed_whitespace;

  return tok;
}

static TokenStream next_lex_stream(CompilerGlobals* const comp, CompilerThread* const comp_thread, Lexer* lex, u64 free_start) {
  constexpr size_t STREAM_LEN = 64;

  Axle::Array<Token>& stream = comp_thread->current_stream;

  stream.clear();
  stream.reserve_extra(STREAM_LEN);

  size_t i = free_start;
  for (; i < STREAM_LEN; i++) {
    auto* tok = stream.data + i;

    *tok = lex_token(comp, comp_thread, lex);
    if (comp_thread->is_panic()) {
      return { nullptr, nullptr };
    }

    if (tok->type == AxleTokenType::End) {
      i++;
      break;
    }
  }


  stream.size = i;

  TokenStream tok_stream = {};
  tok_stream.i = stream.mut_begin();
  tok_stream.end = stream.mut_end();

  return tok_stream;
}

static void check_valid_stream(CompilerThread* const comp_thread, const Parser* parser) {
  if (parser->next.type == AxleTokenType::End && parser->stream.i < parser->stream.end) {
    Span span = {};
    set_span_start(parser->file_path.full_name, parser->current, span);
    set_span_end(*parser->stream.i, span);

    comp_thread->report_error(ERROR_CODE::FILE_ERROR, span,
                              "Found '{}' token in the middle of a token stream",
                              AxleTokenType::End);
  }
}

static void advance(CompilerGlobals* comp, CompilerThread* const comp_thread, Parser* parser) {
  parser->prev = parser->current;

  if (parser->current.type != AxleTokenType::End) {
    parser->current = parser->next;
  }
  else {
    comp_thread->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->full_path(), parser->current),
                              "Attempted to advance past the end of a file");
    return;
  }

  if (parser->next.type == AxleTokenType::End) {
    check_valid_stream(comp_thread, parser);
  }
  else {
    if (parser->stream.i >= parser->stream.end) {
      parser->stream = next_lex_stream(comp, comp_thread, &parser->lexer, 1);
      if (comp_thread->is_panic()) {
        return;
      }

      *parser->stream.i = parser->current;
      parser->stream.i++;
    }

    parser->next = *parser->stream.i;

    parser->stream.i++;
  }
}

static void expect(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* parser, const AxleTokenType t) {
  if (parser->current.type == t) {
    advance(comp, comp_thread, parser);
  }
  else {
    Span span = {};
    set_span_start(parser->file_path.full_name, parser->prev, span);
    set_span_end(parser->current, span);

    comp_thread->report_error(ERROR_CODE::SYNTAX_ERROR, span,
                              "Unexpected Token: {}, Expected: {}", parser->current.type, t);
  }
}

void set_span_start(const Axle::InternString* path, const Token& token, Span& span) {
  span.full_path = path;
  span.char_start = token.pos.character_start;
  span.line_start = token.pos.line;
}

void set_span_end(const Token& token, Span& span) {
  ASSERT(span.full_path != nullptr);

  span.char_end = token.pos.character_end;
  span.line_end = token.pos.line;
}

void reset_parser(CompilerGlobals* const comp,
                  CompilerThread* const comp_thread,
                  Parser* const parser,
                  const Axle::InternString* file_name,
                  Axle::ViewArr<const char> string) {
  Lexer* lex = &parser->lexer;

  lex->top = string.begin();
  lex->end = string.end();

  lex->file_path = file_name;
  lex->curr_pos.character = 0;
  lex->curr_pos.line = 0;

  parser->stream = next_lex_stream(comp, comp_thread, lex, 0);
  if (comp_thread->is_panic()) {
    return;
  }

  if (parser->stream.i == nullptr || parser->stream.end == nullptr) {
    comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, Span{},
                              "Parser was passed a fully or partially null stream"
                              "Start: '{}', End: '{}'",
                              Axle::PrintPtr{ parser->stream.i }, Axle::PrintPtr{ parser->stream.end });
    return;
  }

  if (parser->stream.i >= parser->stream.end) {
    comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, Span{},
                              "Parser was passed a stream of 0 elements");
    return;
  }

  parser->current = *parser->stream.i;
  parser->stream.i++;

  if (parser->current.type == AxleTokenType::End && parser->stream.i < parser->stream.end) {
    Span span = {};
    set_span_start(parser->file_path.full_name, parser->current, span);
    set_span_end(*parser->stream.i, span);

    comp_thread->report_error(ERROR_CODE::FILE_ERROR, span,
                              "Found '{}' token in the middle of a token stream",
                              AxleTokenType::End);
    return;
  }

  parser->next = *parser->stream.i;

  check_valid_stream(comp_thread, parser);
  if (comp_thread->is_panic()) {
    return;
  }

  parser->stream.i++;

  //Dummy value for the first value for creating spans
  // The first 3 values should never be read
  parser->prev.type = AxleTokenType::End;
  parser->prev.consumed_whitespace = false;
  parser->prev.string = nullptr;

  parser->prev.pos.character_start = 0;
  parser->prev.pos.character_end = 0;
  parser->prev.pos.line = 0;
}

Span span_of_token(const Axle::InternString* path, const Token& tok) {
  Span span = {};

  set_span_start(path, tok, span);
  set_span_end(tok, span);

  return span;
}

static const Axle::InternString* parse_name(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser) {
  if (parser->current.type != AxleTokenType::Identifier) {
    comp_thread->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->full_path(), parser->current),
                              "Expected token type '{}'. Found: '{}'",
                              AxleTokenType::Identifier, parser->current.type);
    return nullptr;
  }

  const Axle::InternString* name = parser->current.string;
  advance(comp, comp_thread, parser);
  return name;
}


static AST_LOCAL parse_type(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser);
static AST_LOCAL parse_unary_op(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser);

static constexpr uint8_t precidence_table[] = {
#define MODIFY(name, str, precidence) precidence,
  BIN_OP_INCS
#undef MODIFY
};

// putting in all switch cases even with default
#pragma warning(push)
#pragma warning(disable: 4061)

static bool is_binary_operator(const Parser* const parser) {
  switch (parser->current.type) {
    case AxleTokenType::Add: return true;
    case AxleTokenType::Sub: return true;
    case AxleTokenType::Star: return true;
    case AxleTokenType::BackSlash: return true;
    case AxleTokenType::Percent: return true;
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

static BINARY_OPERATOR parse_binary_operator(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser) {
  switch (parser->current.type) {
    case AxleTokenType::Add: advance(comp, comp_thread, parser); return BINARY_OPERATOR::ADD;
    case AxleTokenType::Sub: advance(comp, comp_thread, parser); return BINARY_OPERATOR::SUB;
    case AxleTokenType::Star: advance(comp, comp_thread, parser); return BINARY_OPERATOR::MUL;
    case AxleTokenType::BackSlash: advance(comp, comp_thread, parser); return BINARY_OPERATOR::DIV;
    case AxleTokenType::Percent: advance(comp, comp_thread, parser); return BINARY_OPERATOR::MOD;
    case AxleTokenType::Lesser: {
        advance(comp, comp_thread, parser);

        if (parser->current.type == AxleTokenType::Lesser && !parser->current.consumed_whitespace) {
          advance(comp, comp_thread, parser);
          return BINARY_OPERATOR::RIGHT_SHIFT;
        }

        return BINARY_OPERATOR::LESSER;
      }
    case AxleTokenType::Greater: {
        advance(comp, comp_thread, parser);

        if (parser->current.type == AxleTokenType::Greater && !parser->current.consumed_whitespace) {
          advance(comp, comp_thread, parser);
          return BINARY_OPERATOR::LEFT_SHIFT;
        }

        return BINARY_OPERATOR::GREATER;
      }
    case AxleTokenType::Equals: {
        advance(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return BINARY_OPERATOR::ADD;//doesnt matter what
        }

        if (parser->current.type == AxleTokenType::Equals && !parser->current.consumed_whitespace) {
          advance(comp, comp_thread, parser);
          return BINARY_OPERATOR::EQUIVALENT;
        }
        break;
      }
    case AxleTokenType::Bang: {
        advance(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return BINARY_OPERATOR::ADD;//doesnt matter what
        }

        if (parser->current.type == AxleTokenType::Equals && !parser->current.consumed_whitespace) {
          advance(comp, comp_thread, parser);
          return BINARY_OPERATOR::NOT_EQ;
        }
        break;
      }
    case AxleTokenType::Or: advance(comp, comp_thread, parser); return BINARY_OPERATOR::OR;
    case AxleTokenType::Xor: advance(comp, comp_thread, parser); return BINARY_OPERATOR::XOR;
    case AxleTokenType::And: advance(comp, comp_thread, parser); return BINARY_OPERATOR::AND;

    default: break;
  }

  comp_thread->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->full_path(), parser->current),
                            "'{}' is not a valid binary operator", parser->current.string);
  return BINARY_OPERATOR::ADD;//just return whatever and hope everything errors out
}

struct PrecidenceState {
  BINARY_OPERATOR next_op;
  AST_LOCAL expr;
};

static PrecidenceState parse_binary_precidence(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser, BINARY_OPERATOR op, AST_LOCAL lhs) {
NEW_LHS:
  AST_LOCAL pos_rhs = parse_unary_op(comp, comp_thread, parser);
  if (comp_thread->is_panic()) {
    return { op, NULL_AST_NODE };
  }

  if (is_binary_operator(parser)) {
    BINARY_OPERATOR op2 = parse_binary_operator(comp, comp_thread, parser);
    if (comp_thread->is_panic()) {
      return { op, NULL_AST_NODE };
    }

    {
      u8 prec1 = precidence_table[(usize)op];
      u8 prec2 = precidence_table[(usize)op2];

      if (prec2 > prec1) {
        PrecidenceState n = parse_binary_precidence(comp, comp_thread, parser, op2, pos_rhs);

        ASTBinaryOperatorExpr* bin_op = ast_alloc<ASTBinaryOperatorExpr>(parser);
        bin_op->ast_type = AST_TYPE::BINARY_OPERATOR;
        bin_op->left = lhs;
        bin_op->right = n.expr;
        bin_op->op = op;

        return { op, { bin_op } };
      }
      else {
        ASTBinaryOperatorExpr* bin_op = ast_alloc<ASTBinaryOperatorExpr>(parser);
        bin_op->ast_type = AST_TYPE::BINARY_OPERATOR;
        bin_op->left = lhs;
        bin_op->right = pos_rhs;
        bin_op->op = op;

        lhs = { bin_op };
        op = op2;

        goto NEW_LHS;
      }
    }
  }
  else {
    ASTBinaryOperatorExpr* bin_op = ast_alloc<ASTBinaryOperatorExpr>(parser);
    bin_op->ast_type = AST_TYPE::BINARY_OPERATOR;
    bin_op->left = lhs;
    bin_op->right = pos_rhs;
    bin_op->op = op;

    return { op, { bin_op } };
  }
}

static AST_LOCAL parse_inner_expression(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser) {
  Span span = {};
  SPAN_START;

  AST_LOCAL pos_left = parse_unary_op(comp, comp_thread, parser);
  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }

  if (is_binary_operator(parser)) {
    BINARY_OPERATOR op = parse_binary_operator(comp, comp_thread, parser);
    if (comp_thread->is_panic()) {
      return NULL_AST_NODE;
    }

    PrecidenceState s = parse_binary_precidence(comp, comp_thread, parser, op, pos_left);
    if (comp_thread->is_panic()) {
      return NULL_AST_NODE;
    }

    SPAN_END;
    s.expr.ast->node_span = span;

    return s.expr;
  }
  else {
    return pos_left;
  }
}

static AST_LOCAL parse_expression(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser) {
  //May at some point be important to do this
  //I did it once for part of an experiment and im keeping it just because why not
  return parse_inner_expression(comp, comp_thread, parser);
}

static AST_LOCAL parse_structure(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser);
static AST_LOCAL parse_lambda(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser, bool skip_first_bracket = false);

static AST_LOCAL parse_tup_literal(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser, AST_LOCAL type) {
  //Will always be a primary so can elevate span stuff out of the switch
  Span span = {};
  SPAN_START;

  expect(comp, comp_thread, parser, AxleTokenType::Left_Brace);
  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }

  Axle::Array<AST_LOCAL> temp_accumulate;

  while (!comp_thread->is_panic()) {
    AST_LOCAL expr = parse_expression(comp, comp_thread, parser);
    if (comp_thread->is_panic()) {
      return NULL_AST_NODE;
    }

    temp_accumulate.insert(expr);

    if (parser->current.type == AxleTokenType::Comma) {
      advance(comp, comp_thread, parser);
    }
    else {
      break;
    }
  }


  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }

  expect(comp, comp_thread, parser, AxleTokenType::Right_Brace);
  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }

  SPAN_END;

  Axle::ViewArr<const AST_LOCAL> arr = ast_bake_arr(parser, std::move(temp_accumulate));

  ASTTupleLitExpr* const tup_lit = ast_alloc<ASTTupleLitExpr>(parser);
  tup_lit->ast_type = AST_TYPE::TUPLE_LIT;
  tup_lit->prefix = type;
  tup_lit->elements = arr;
  tup_lit->node_span = span;

  return { tup_lit };
}

static AST_LOCAL parse_primary(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser) {
  //Will always be a primary so can elevate span stuff out of the switch
  Span span = {};
  SPAN_START;

  switch (parser->current.type) {
    case AxleTokenType::Intrinsic: {
        if (parser->current.string == comp_thread->intrinsics.dynamic_import) {

          const Axle::InternString* type = parser->current.string;

          advance(comp, comp_thread, parser);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          expect(comp, comp_thread, parser, AxleTokenType::Left_Bracket);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          AST_LOCAL it = parse_type(comp, comp_thread, parser);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          expect(comp, comp_thread, parser, AxleTokenType::Comma);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          if (parser->current.type != AxleTokenType::String) {
            comp_thread->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->full_path(), parser->current),
                                      "Expected syntax: #{}(TYPE, \"LIBRARY_NAME\", \"IMPORT_NAME\")", type);
            return NULL_AST_NODE;
          }

          const Axle::InternString* lib = parser->current.string;
          advance(comp, comp_thread, parser);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          expect(comp, comp_thread, parser, AxleTokenType::Comma);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          if (parser->current.type != AxleTokenType::String) {
            comp_thread->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->full_path(), parser->current),
                                      "Expected syntax: #{}(TYPE, \"LIBRARY_NAME\", \"IMPORT_NAME\")", type);
            return NULL_AST_NODE;
          }

          const Axle::InternString* imp = parser->current.string;
          advance(comp, comp_thread, parser);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          expect(comp, comp_thread, parser, AxleTokenType::Right_Bracket);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          SPAN_END;

          ASTLink* li = ast_alloc<ASTLink>(parser);
          li->dynamic = true;
          li->import_type = it;
          li->node_span = span;
          li->ast_type = AST_TYPE::LINK;
          li->lib_file = lib;
          li->name = imp;

          return { li };
        }
        else if (parser->current.string == comp_thread->intrinsics.type) {
          //Way of getting into the type parser
          advance(comp, comp_thread, parser);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          return parse_type(comp, comp_thread, parser);
        }
        else {
          comp_thread->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->full_path(), parser->current),
                                    "Invalid intrinsics #{} in this context",
                                    parser->current.string);
          return NULL_AST_NODE;
        }
      }
    case AxleTokenType::Left_Brace: {
        return parse_tup_literal(comp, comp_thread, parser, NULL_AST_NODE);
      }
    case AxleTokenType::Left_Square: {
        // Array expression
        // [ ... ] 

        advance(comp, comp_thread, parser);

        Axle::Array<AST_LOCAL> temp_elements = {};

        while (!comp_thread->is_panic()) {
          AST_LOCAL inner = parse_inner_expression(comp, comp_thread, parser);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          temp_elements.insert(inner);

          if (parser->current.type == AxleTokenType::Comma) {
            advance(comp, comp_thread, parser);
          }
          else {
            break;
          }
        }

        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        expect(comp, comp_thread, parser, AxleTokenType::Right_Square);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        SPAN_END;

        Axle::ViewArr<const AST_LOCAL> elements = ast_bake_arr(parser, std::move(temp_elements));

        ASTArrayExpr* arr_expr = ast_alloc<ASTArrayExpr>(parser);
        arr_expr->ast_type = AST_TYPE::ARRAY_EXPR;
        arr_expr->elements = elements;
        arr_expr->node_span = span;

        return { arr_expr };
      }
    case AxleTokenType::String: {
        // Ascii string
        // " ... "

        const Axle::InternString* str = parser->current.string;

        advance(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        SPAN_END;

        ASTAsciiString* s = ast_alloc<ASTAsciiString>(parser);
        s->ast_type = AST_TYPE::ASCII_STRING;
        s->node_span = span;
        s->string = str;

        return { s };
      }
    case AxleTokenType::Character: {
        // Ascii char
        // e.g. 'C'

        const char ch = parser->current.string->string[0];

        advance(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        SPAN_END;

        ASTAsciiChar* c = ast_alloc<ASTAsciiChar>(parser);
        c->ast_type = AST_TYPE::ASCII_CHAR;
        c->node_span = span;
        c->character = ch;

        return { c };
      }
    case AxleTokenType::Cast: {
        // cast(... , ...)

        advance(comp, comp_thread, parser);

        expect(comp, comp_thread, parser, AxleTokenType::Left_Bracket);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        AST_LOCAL ty = parse_type(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        expect(comp, comp_thread, parser, AxleTokenType::Comma);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        AST_LOCAL expr = parse_inner_expression(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        expect(comp, comp_thread, parser, AxleTokenType::Right_Bracket);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        SPAN_END;

        ASTCastExpr* cast = ast_alloc<ASTCastExpr>(parser);
        cast->ast_type = AST_TYPE::CAST;
        cast->node_span = span;
        cast->type = ty;
        cast->expr = expr;

        return { cast };
      }
    case AxleTokenType::Number: {
        u64 val = string_to_uint(parser->current.string->string);
        advance(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        const Axle::InternString* suffix = nullptr;
        if (!parser->current.consumed_whitespace && parser->current.type == AxleTokenType::Identifier) {
          suffix = parser->current.string;
          advance(comp, comp_thread, parser);

          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }
        }

        SPAN_END;

        ASTNumber* num = ast_alloc<ASTNumber>(parser);
        num->ast_type = AST_TYPE::NUMBER;
        num->node_span = span;
        num->num_value = val;
        num->suffix = suffix;

        return { num };
      }
    case AxleTokenType::Identifier: {
        const Axle::InternString* name = parser->current.string;

        advance(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        SPAN_END;

        ASTIdentifier* i = ast_alloc<ASTIdentifier>(parser);
        i->ast_type = AST_TYPE::IDENTIFIER_EXPR;
        i->node_span = span;
        i->name = name;

        return { i };
      }
    case AxleTokenType::Struct: {
        AST_LOCAL s = parse_structure(comp, comp_thread, parser);
        SPAN_END;

        ASTStructExpr* se = ast_alloc<ASTStructExpr>(parser);
        se->ast_type = AST_TYPE::STRUCT_EXPR;
        se->node_span = span;
        se->struct_body = s;

        return { se };
      }
    case AxleTokenType::Left_Bracket: {
        //Function or expression

        // (
        advance(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        // Lambdas start in 1 of 3 ways
        // () - empty brackets must be lambda star
        // (mut - expressions cannot have mut keyword in them
        // (ident : - this is the start of a declaration, must be lambda
        
        if (parser->current.type == AxleTokenType::Mut
            || (parser->current.type == AxleTokenType::Identifier
                && parser->next.type == AxleTokenType::Colon)
            || parser->current.type == AxleTokenType::Right_Bracket) {
          AST_LOCAL l = parse_lambda(comp, comp_thread, parser, true);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }
          SPAN_END;

          ASTLambdaExpr* le = ast_alloc<ASTLambdaExpr>(parser);
          le->ast_type = AST_TYPE::LAMBDA_EXPR;
          le->node_span = span;
          le->lambda = l;

          return { le };
        }
        else {
          // Is just a nested expression
          AST_LOCAL e = parse_inner_expression(comp, comp_thread, parser);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }
          expect(comp, comp_thread, parser, AxleTokenType::Right_Bracket);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          SPAN_END;
          e.ast->node_span = span;

          return e;
        }
      }
    default: comp_thread->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->full_path(), parser->current),
                                       "Unexpected Token Type '{}'", parser->current.type);
      return NULL_AST_NODE;
  }
}

static AST_LOCAL parse_primary_and_suffix(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser) {
  Span span = {};
  SPAN_START;


  AST_LOCAL current = parse_primary(comp, comp_thread, parser);
  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }

  //Parse the suffixes

  while (!comp_thread->is_panic()) {
    switch (parser->current.type) {
      case AxleTokenType::Left_Square: {

          advance(comp, comp_thread, parser);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          Axle::Array<AST_LOCAL> temp_arguments = {};

          //Arguments
          if (parser->current.type != AxleTokenType::Right_Square) {
            while (!comp_thread->is_panic()) {
              AST_LOCAL inner = parse_inner_expression(comp, comp_thread, parser);
              if (comp_thread->is_panic()) {
                return NULL_AST_NODE;
              }

              temp_arguments.insert(inner);

              if (parser->current.type == AxleTokenType::Right_Square) {
                break;
              }
              else if (parser->current.type == AxleTokenType::Comma) {
                advance(comp, comp_thread, parser);
                continue;
              }
              else {
                //ERROR
                comp_thread->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->full_path(), parser->current),
                                          "Expected '{}', Found '{}'",
                                          AxleTokenType::Comma, parser->current.type);
                return NULL_AST_NODE;
              }
            }

            if (comp_thread->is_panic()) {
              return NULL_AST_NODE;
            }
          }

          advance(comp, comp_thread, parser);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          SPAN_END;

          Axle::ViewArr<const AST_LOCAL> arguments = 
            ast_bake_arr(parser, std::move(temp_arguments));

          ASTIndexExpr* index = ast_alloc<ASTIndexExpr>(parser);
          index->ast_type = index->EXPECTED_AST_TYPE;
          index->node_span = span;
          index->expr = current;
          index->arguments = arguments;

          current = { index };

          //Loop again
          break;
        }
      case AxleTokenType::Full_Stop: {
          //Members

          advance(comp, comp_thread, parser);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          const Axle::InternString* name = parse_name(comp, comp_thread, parser);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          SPAN_END;

          ASTMemberAccessExpr* member = ast_alloc<ASTMemberAccessExpr>(parser);
          member->ast_type = AST_TYPE::MEMBER_ACCESS;
          member->expr = current;
          member->name = name;
          member->node_span = span;

          current = { member };
          //Loop again
          break;
        }
      case AxleTokenType::Left_Bracket: {
          //left bracket
          advance(comp, comp_thread, parser);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          Axle::Array<AST_LOCAL> temp_arguments = {};

          //Arguments
          if (parser->current.type != AxleTokenType::Right_Bracket) {
            while (!comp_thread->is_panic()) {
              AST_LOCAL inner = parse_inner_expression(comp, comp_thread, parser);
              if (comp_thread->is_panic()) {
                return NULL_AST_NODE;
              }

              temp_arguments.insert(inner);

              if (parser->current.type == AxleTokenType::Right_Bracket) {
                break;
              }
              else if (parser->current.type == AxleTokenType::Comma) {
                advance(comp, comp_thread, parser);
                continue;
              }
              else {
                //ERROR
                comp_thread->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->full_path(), parser->current),
                                          "Expected '{}', Found '{}'",
                                          AxleTokenType::Comma, parser->current.type);
                return NULL_AST_NODE;
              }
            }

            if (comp_thread->is_panic()) {
              return NULL_AST_NODE;
            }
          }


          advance(comp, comp_thread, parser);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          SPAN_END;

          Axle::ViewArr<const AST_LOCAL> arguments = 
            ast_bake_arr(parser, std::move(temp_arguments));

          ASTFunctionCallExpr* call = ast_alloc<ASTFunctionCallExpr>(parser);
          call->ast_type = AST_TYPE::FUNCTION_CALL;
          call->node_span = span;
          call->function = current;
          call->arguments = arguments;

          current = { call };
          //Loop again
          break;
        }
      case AxleTokenType::Left_Brace: {
          //tuple literal with named type
          current = parse_tup_literal(comp, comp_thread, parser, current);
          break;
        }
      default:
        //No more suffixes suffix
        return current;
    }
  }

  INVALID_CODE_PATH("Managed to escape loop ...");
}

static AST_LOCAL parse_unary_op(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser) {
  switch (parser->current.type) {
    case AxleTokenType::Sub: {
        Span span = {};
        SPAN_START;

        advance(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        AST_LOCAL p = parse_unary_op(comp, comp_thread, parser);

        SPAN_END;

        ASTUnaryOperatorExpr* op = ast_alloc<ASTUnaryOperatorExpr>(parser);
        op->ast_type = AST_TYPE::UNARY_OPERATOR;
        op->op = UNARY_OPERATOR::NEG;
        op->node_span = span;
        op->expr = p;

        return { op };
      }
    case AxleTokenType::Star: {
        Span span = {};
        SPAN_START;

        advance(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        AST_LOCAL p = parse_unary_op(comp, comp_thread, parser);

        SPAN_END;

        ASTUnaryOperatorExpr* op = ast_alloc<ASTUnaryOperatorExpr>(parser);
        op->ast_type = AST_TYPE::UNARY_OPERATOR;
        op->op = UNARY_OPERATOR::DEREF;
        op->node_span = span;
        op->expr = p;

        return { op };
      }
    case AxleTokenType::And: {
        Span span = {};
        SPAN_START;

        advance(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        AST_LOCAL p = parse_unary_op(comp, comp_thread, parser);

        SPAN_END;

        ASTUnaryOperatorExpr* op = ast_alloc<ASTUnaryOperatorExpr>(parser);
        op->ast_type = AST_TYPE::UNARY_OPERATOR;
        op->op = UNARY_OPERATOR::ADDRESS;
        op->node_span = span;
        op->expr = p;

        return { op };
      }

    default:
      return parse_primary_and_suffix(comp, comp_thread, parser);
  }
}

static AST_LOCAL parse_type(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser) {
  Span span = {};
  SPAN_START;

  switch (parser->current.type) {
    case AxleTokenType::Left_Bracket: {
        //Lambda or tuple signature
        //Tuple = (ty, ty, etc)
        //Lambda = (ty, ty, etc) -> ty
        //       = () -> ty
        //Can parse as if it were a tuple and then convert to lambda

        advance(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        Axle::Array<AST_LOCAL> temp_args = {};

        if (parser->current.type != AxleTokenType::Right_Bracket) {
          while (true) {
            AST_LOCAL ty = parse_type(comp, comp_thread, parser);
            if (comp_thread->is_panic()) {
              return NULL_AST_NODE;
            }

            temp_args.insert(ty);

            if (parser->current.type == AxleTokenType::Right_Bracket) {
              break;
            }
            else if (parser->current.type != AxleTokenType::Comma) {
              comp_thread->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->full_path(), parser->current),
                                        "Invalid token type '{}' in type list", parser->current.type);
              return NULL_AST_NODE;
            }

            expect(comp, comp_thread, parser, AxleTokenType::Comma);
            if (comp_thread->is_panic()) {
              return NULL_AST_NODE;
            }
          }
        }


        ASSERT(parser->current.type == AxleTokenType::Right_Bracket);
        advance(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        if (parser->current.type == AxleTokenType::Sub
            && parser->next.type == AxleTokenType::Greater) {
          if (parser->next.consumed_whitespace) {
            comp_thread->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->full_path(), parser->current),
                                      "Expected -> or nothing after type\n"
                                      "Found we the compiler thinks is '->' but the characters are separated");
            return NULL_AST_NODE;
          }

          advance(comp, comp_thread, parser);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }
          advance(comp, comp_thread, parser);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          AST_LOCAL ret = parse_type(comp, comp_thread, parser);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          SPAN_END;
          
          Axle::ViewArr<const AST_LOCAL> args = 
            ast_bake_arr(parser, std::move(temp_args));

          ASTLambdaType* l = ast_alloc<ASTLambdaType>(parser);
          l->ast_type = AST_TYPE::LAMBDA_TYPE;
          l->node_span = span;
          l->args = args;
          l->ret = ret;

          return { l };
        }
        else {
          SPAN_END;
          
          Axle::ViewArr<const AST_LOCAL> args = 
            ast_bake_arr(parser, std::move(temp_args));

          ASTTupleType* tup = ast_alloc<ASTTupleType>(parser);
          tup->ast_type = AST_TYPE::TUPLE_TYPE;
          tup->node_span = span;
          tup->types = args;

          return { tup };
        }
      }
    case AxleTokenType::Identifier: {
        const Axle::InternString* i = parser->current.string;
        advance(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        SPAN_END;

        ASTNamedType* nt = ast_alloc<ASTNamedType>(parser);
        nt->ast_type = AST_TYPE::NAMED_TYPE;
        nt->name = i;
        nt->node_span = span;

        return { nt };
      }
    case AxleTokenType::Left_Square: {
        // Array or Slice type

        advance(comp, comp_thread, parser);//[
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        if(parser->current.type == AxleTokenType::Right_Square) {
          // Slice type
          // [] (?mut) Type

          advance(comp, comp_thread, parser);
          if(comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          bool mut = false;

          if (parser->current.type == AxleTokenType::Mut) {
            advance(comp, comp_thread, parser);
            if (comp_thread->is_panic()) {
              return NULL_AST_NODE;
            }

            mut = true;
          }

          AST_LOCAL base = parse_type(comp, comp_thread, parser);
          
          SPAN_END;

          ASTSliceType* arr = ast_alloc<ASTSliceType>(parser);
          arr->ast_type = AST_TYPE::SLICE_TYPE;
          arr->node_span = span;
          arr->base = base;
          arr->mut = mut;

          return { arr };
        }
        else {
          // Array type
          // [ BASE ; EXPR ]
          //Base Type
          AST_LOCAL base = parse_type(comp, comp_thread, parser);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          expect(comp, comp_thread, parser, AxleTokenType::Semicolon);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          //Expression
          AST_LOCAL expr = parse_expression(comp, comp_thread, parser);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          expect(comp, comp_thread, parser, AxleTokenType::Right_Square);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          SPAN_END;

          ASTArrayType* arr = ast_alloc<ASTArrayType>(parser);
          arr->ast_type = AST_TYPE::ARRAY_TYPE;
          arr->node_span = span;
          arr->base = base;
          arr->expr = expr;

          return { arr };
        }
      }
    case AxleTokenType::Star: {
        // Pointer type
        // * (?mut) BASE

        advance(comp, comp_thread, parser);//*
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        bool mut = false;

        if (parser->current.type == AxleTokenType::Mut) {
          advance(comp, comp_thread, parser);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          mut = true;
        }

        //Base
        AST_LOCAL base = parse_type(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        SPAN_END;

        ASTPtrType* arr = ast_alloc<ASTPtrType>(parser);
        arr->ast_type = AST_TYPE::PTR_TYPE;
        arr->node_span = span;
        arr->base = base;
        arr->mut = mut;

        return { arr };
      }
    default: {
        comp_thread->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->full_path(), parser->current),
                                  "Expected a Type! Found '{}'", parser->current.type);
        return NULL_AST_NODE;
      }
  }
}

static AST_LOCAL parse_typed_name(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser) {
  Span span = {};
  SPAN_START;

  bool mut = false;

  if (parser->current.type == AxleTokenType::Mut) {
    advance(comp, comp_thread, parser);
    if (comp_thread->is_panic()) {
      return NULL_AST_NODE;
    }

    mut = true;
  }

  const Axle::InternString* name = parse_name(comp, comp_thread, parser);
  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }

  expect(comp, comp_thread, parser, AxleTokenType::Colon);
  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }

  AST_LOCAL ty = parse_type(comp, comp_thread, parser);
  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }

  SPAN_END;

  ASTTypedName* tn = ast_alloc<ASTTypedName>(parser);
  tn->ast_type = AST_TYPE::TYPED_NAME;
  tn->node_span = span;

  if (mut) {
    tn->mutability = ASTDeclMutability::Mutable;
  }
  else {
    tn->mutability = ASTDeclMutability::Immutable;
  }

  tn->name = name;
  tn->type_ast = ty;
  tn->local_ptr = nullptr;

  return { tn };
}

static AST_LOCAL parse_block(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser);

static AST_LOCAL parse_decl(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser, bool global) {
  Span span = {};
  SPAN_START;

  AST_LOCAL expr = NULL_AST_NODE;
  AST_LOCAL ty = NULL_AST_NODE;

  bool comptime = false;
  bool mut = false;

  if (parser->current.type == AxleTokenType::Mut) {
    advance(comp, comp_thread, parser);
    if (comp_thread->is_panic()) {
      return NULL_AST_NODE;
    }

    mut = true;

    if (parser->current.type != AxleTokenType::Identifier) {
      comp_thread->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->full_path(), parser->current),
                                "Expected Identifier to follow 'mut'");
      return NULL_AST_NODE;
    }
  }
  else {
    if (parser->current.type != AxleTokenType::Identifier) {
      comp_thread->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->full_path(), parser->current),
                                "Expected Identifier as declarations start with identifiers or 'mut'");
      return NULL_AST_NODE;
    }
  }

  const Axle::InternString* name = parser->current.string;
  advance(comp, comp_thread, parser);
  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }

  expect(comp, comp_thread, parser, AxleTokenType::Colon);
  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }

  //Explicit type??
  if (parser->current.type != AxleTokenType::Equals && parser->current.type != AxleTokenType::Colon) {
    ty = parse_type(comp, comp_thread, parser);
    if (comp_thread->is_panic()) {
      return NULL_AST_NODE;
    }
  }

  if (parser->current.type == AxleTokenType::Equals) {
    advance(comp, comp_thread, parser);
    comptime = false;
  }
  else if (parser->current.type == AxleTokenType::Colon) {
    advance(comp, comp_thread, parser);
    comptime = true;
  }
  else {
    comp_thread->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->full_path(), parser->current),
                              "Expected '{}' or '{}' to end declaration assignment syntax but found '{}'",
                              AxleTokenType::Equals, AxleTokenType::Colon, parser->current.type);
    return NULL_AST_NODE;
  }

  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }

  expr = parse_expression(comp, comp_thread, parser);
  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }

  if (expr.ast->ast_type != AST_TYPE::LAMBDA_EXPR) {
    expect(comp, comp_thread, parser, AxleTokenType::Semicolon);
    if (comp_thread->is_panic()) {
      return NULL_AST_NODE;
    }
  }
  
  SPAN_END;

  if (comptime && mut) {
    comp_thread->report_error(ERROR_CODE::CONST_ERROR, span,
                              "Declaration cannot be '{}' and '{}{}'",
                              AxleTokenType::Mut,
                              AxleTokenType::Colon, AxleTokenType::Colon);
    return NULL_AST_NODE;
  }

  ASTDecl* d = ast_alloc<ASTDecl>(parser);
  d->ast_type = AST_TYPE::DECL;
  d->node_span = span;
  d->name = name;

  if (global) {
    d->location = ASTDecl::Location::Global;
    d->global_ptr = nullptr;
  }
  else {
    d->location = ASTDecl::Location::Local;
    d->local_ptr = nullptr;
  }

  ASSERT(!(comptime && mut));
  if (mut) {
    d->mutability = ASTDeclMutability::Mutable;
  }
  else if (comptime) {
    d->mutability = ASTDeclMutability::Comptime;
  }
  else {
    d->mutability = ASTDeclMutability::Immutable;
  }

  d->type_ast = ty;
  d->expr = expr;

  return { d };
}

static AST_LOCAL parse_statement(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser) {
  Span span = {};
  SPAN_START;

  switch (parser->current.type) {
    case AxleTokenType::Left_Brace: {
        return parse_block(comp, comp_thread, parser);
      }
    case AxleTokenType::Return: {
        advance(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        AST_LOCAL expr = NULL_AST_NODE;

        if (parser->current.type != AxleTokenType::Semicolon) {
          expr = parse_expression(comp, comp_thread, parser);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          expect(comp, comp_thread, parser, AxleTokenType::Semicolon);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }
        }
        else {
          expect(comp, comp_thread, parser, AxleTokenType::Semicolon);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }
        }

        SPAN_END;

        ASTReturn* ret = ast_alloc<ASTReturn>(parser);
        ret->ast_type = AST_TYPE::RETURN;
        ret->node_span = span;
        ret->expr = expr;
        return { ret };
      }
    case AxleTokenType::If: {
        advance(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        expect(comp, comp_thread, parser, AxleTokenType::Left_Bracket);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        AST_LOCAL cond = parse_expression(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        expect(comp, comp_thread, parser, AxleTokenType::Right_Bracket);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        AST_LOCAL if_branch = parse_statement(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        AST_LOCAL else_branch = NULL_AST_NODE;
        if (parser->current.type == AxleTokenType::Else) {
          advance(comp, comp_thread, parser);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }
          else_branch = parse_statement(comp, comp_thread, parser);
        }

        SPAN_END;

        ASTIfElse* if_else = ast_alloc<ASTIfElse>(parser);
        if_else->ast_type = AST_TYPE::IF_ELSE;
        if_else->node_span = span;
        if_else->condition = cond;
        if_else->if_statement = if_branch;
        if_else->else_statement = else_branch;

        return { if_else };
      }
    case AxleTokenType::While: {
        advance(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        expect(comp, comp_thread, parser, AxleTokenType::Left_Bracket);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        AST_LOCAL cond = parse_expression(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        expect(comp, comp_thread, parser, AxleTokenType::Right_Bracket);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        AST_LOCAL loop = parse_statement(comp, comp_thread, parser);

        SPAN_END;

        ASTWhile* w = ast_alloc<ASTWhile>(parser);
        w->ast_type = AST_TYPE::WHILE;
        w->node_span = span;
        w->condition = cond;
        w->statement = loop;

        return { w };
      }
    case AxleTokenType::Mut: {
        return parse_decl(comp, comp_thread, parser, false);
      }
    default: {
        if (parser->current.type == AxleTokenType::Identifier && parser->next.type == AxleTokenType::Colon) {
          //Declaration!
          return parse_decl(comp, comp_thread, parser, false);
        }

        //Not decl means its Expression or Assignment

        //Get the assign to expression
        AST_LOCAL assign_to = parse_expression(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }

        if (parser->current.type == AxleTokenType::Equals) {
          advance(comp, comp_thread, parser);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          //reset the expr
          AST_LOCAL expr = parse_expression(comp, comp_thread, parser);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          expect(comp, comp_thread, parser, AxleTokenType::Semicolon);
          if (comp_thread->is_panic()) {
            return NULL_AST_NODE;
          }

          SPAN_END;

          ASTAssign* assign = ast_alloc<ASTAssign>(parser);
          assign->ast_type = AST_TYPE::ASSIGN;
          assign->node_span = span;
          assign->assign_to = assign_to;
          assign->value = expr;

          return { assign };
        }
        else if (parser->current.type == AxleTokenType::Semicolon) {
          //is expressiom
          return assign_to;
        }
        else {
          SPAN_END;

          comp_thread->report_error(ERROR_CODE::SYNTAX_ERROR, span,
                                    "Expected one of '{}' or '{}' at the end of the expression shown\n"
                                    "Found '{}'",
                                    AxleTokenType::Equals, AxleTokenType::Semicolon,
                                    parser->current.type);
          return NULL_AST_NODE;
        }
        break;
      }
  }
}

static AST_LOCAL parse_block(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser) {
  expect(comp, comp_thread, parser, AxleTokenType::Left_Brace);
  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }

  Axle::Array<AST_LOCAL> temp_statements;

  while (!comp_thread->is_panic() && parser->current.type != AxleTokenType::Right_Brace) {
    if (parser->current.type == AxleTokenType::Semicolon) {
      //Empty statement
      advance(comp, comp_thread, parser);
      if (comp_thread->is_panic()) {
        return NULL_AST_NODE;
      }
      continue;
    }

    AST_LOCAL stmt = parse_statement(comp, comp_thread, parser);
    temp_statements.insert(stmt);
  }


  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }

  expect(comp, comp_thread, parser, AxleTokenType::Right_Brace);
  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }

  const Axle::ViewArr<const AST_LOCAL> statements
    = ast_bake_arr(parser, std::move(temp_statements));

  ASTBlock* b = ast_alloc<ASTBlock>(parser);
  b->ast_type = AST_TYPE::BLOCK;
  b->block = statements;

  return { b };
}

static AST_LOCAL parse_function_signature(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser, bool skip_first_bracket) {
  Span span = {};

  SPAN_START;

  if (!skip_first_bracket) {
    expect(comp, comp_thread, parser, AxleTokenType::Left_Bracket);
    if (comp_thread->is_panic()) {
      return NULL_AST_NODE;
    }
  }

  Axle::Array<AST_LOCAL> temp_args = {};

  //Parameters
  if (parser->current.type != AxleTokenType::Right_Bracket) {
    while (!comp_thread->is_panic()) {
      AST_LOCAL ty = parse_typed_name(comp, comp_thread, parser);
      temp_args.insert(ty);

      if (parser->current.type == AxleTokenType::Right_Bracket) {
        break;
      }
      else if (parser->current.type == AxleTokenType::Comma) {
        advance(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return NULL_AST_NODE;
        }
        continue;
      }
      else {
        //ERROR
        comp_thread->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->full_path(), parser->current),
                                  "Expected a comma!");
        return NULL_AST_NODE;
      }
    }
  }


  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }

  advance(comp, comp_thread, parser);
  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }

  // ->
  expect(comp, comp_thread, parser, AxleTokenType::Sub);// -
  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }

  expect(comp, comp_thread, parser, AxleTokenType::Greater);// >
  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }

  AST_LOCAL ret = parse_type(comp, comp_thread, parser);
  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }

  SPAN_END;
  
  const Axle::ViewArr<const AST_LOCAL> args
    = ast_bake_arr(parser, std::move(temp_args));

  ASTFuncSig* sig = ast_alloc<ASTFuncSig>(parser);
  sig->ast_type = AST_TYPE::FUNCTION_SIGNATURE;
  sig->parameters = args;
  sig->return_type = ret;
  sig->node_span = span;

  return { sig };
}

static AST_LOCAL parse_lambda(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser, bool skip_first_bracket) {
  AST_LOCAL sig = parse_function_signature(comp, comp_thread, parser, skip_first_bracket);
  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }

  AST_LOCAL body = NULL_AST_NODE;

  if (parser->current.type == AxleTokenType::Left_Brace) {
    body = parse_block(comp, comp_thread, parser);
  }
  else if (parser->current.type == AxleTokenType::Semicolon) {
    advance(comp, comp_thread, parser);
  }
  else {
    Span span = {};
    set_span_start(parser->full_path(), parser->prev, span);
    set_span_end(parser->current, span);

    comp_thread->report_error(ERROR_CODE::SYNTAX_ERROR, span,
                              "Expected '{}' or '{}' to end a lambda declaration\nFound '{}'",
                              AxleTokenType::Left_Brace, AxleTokenType::Semicolon,
                              parser->current.type);
    return NULL_AST_NODE;
  }

  ASTFuncSig* sig_downcast = downcast_ast<ASTFuncSig>(sig);

  ASTLambda* l = ast_alloc<ASTLambda>(parser);
  l->ast_type = AST_TYPE::LAMBDA;
  l->sig = sig_downcast;
  l->body = body;

  //Load to a compilation unit
  add_comp_unit_for_lambda(comp, parser->current_namespace, l);

  return { l };
}

static AST_LOCAL parse_structure(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser) {
  expect(comp, comp_thread, parser, AxleTokenType::Struct);
  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }

  expect(comp, comp_thread, parser, AxleTokenType::Left_Brace);
  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }
  
  Axle::Array<AST_LOCAL> temp_arr = {};

  while (!comp_thread->is_panic() && parser->current.type != AxleTokenType::Right_Brace) {
    AST_LOCAL arg = parse_typed_name(comp, comp_thread, parser);
    if (comp_thread->is_panic()) {
      return NULL_AST_NODE;
    }
    
    temp_arr.insert(arg);

    expect(comp, comp_thread, parser, AxleTokenType::Semicolon);
  }

  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }

  expect(comp, comp_thread, parser, AxleTokenType::Right_Brace);
  if (comp_thread->is_panic()) {
    return NULL_AST_NODE;
  }
  
  const Axle::ViewArr<const AST_LOCAL> arr
    = ast_bake_arr(parser, std::move(temp_arr));

  ASTStructBody* s = ast_alloc<ASTStructBody>(parser);
  s->ast_type = AST_TYPE::STRUCT;
  s->elements = arr;

  //Load to a compilation unit
  add_comp_unit_for_struct(comp, parser->current_namespace, s);

  return { s };
}

Axle::ViewArr<const AST_LOCAL>
    parse_export_list(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* parser) {
  expect(comp, comp_thread, parser, AxleTokenType::Left_Brace);
  if (comp_thread->is_panic()) {
    return {};
  }

  Axle::Array<AST_LOCAL> temp_list = {};
  
  for (AxleTokenType current = parser->current.type;
       current != AxleTokenType::Right_Brace;
       current = parser->current.type) {
    Span span;
    SPAN_START;

    if (current != AxleTokenType::String) {
      comp_thread->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->full_path(), parser->current),
                                "Expected a string literal for the export name (may change to dynamic value later)");
      return {};
    }

    const Axle::InternString* name = parser->current.string;
    advance(comp, comp_thread, parser);
    if (comp_thread->is_panic()) {
      return {};
    }

    expect(comp, comp_thread, parser, AxleTokenType::Equals);
    if (comp_thread->is_panic()) {
      return {};
    }

    AST_LOCAL val = parse_expression(comp, comp_thread, parser);
    if (comp_thread->is_panic()) {
      return {};
    }

    SPAN_END;

    ASTExportSingle* e = ast_alloc<ASTExportSingle>(parser);
    e->ast_type = AST_TYPE::EXPORT_SINGLE;
    e->node_span = span;
    e->name = name;
    e->value = val;

    temp_list.insert({ e });

    if (parser->current.type == AxleTokenType::Comma) {
      advance(comp, comp_thread, parser);
      if (comp_thread->is_panic()) {
        return {};
      }
    }
    else {
      break;
    }
  }

  expect(comp, comp_thread, parser, AxleTokenType::Right_Brace);
  if (comp_thread->is_panic()) {
    return {};
  }

  return ast_bake_arr(parser, std::move(temp_list));
}

void parse_file(CompilerGlobals* const comp, CompilerThread* const comp_thread, Parser* const parser, FileAST* const file) {
  Axle::Array<AST_LOCAL> top_level_temp = {};

  for (AxleTokenType current = parser->current.type;
       current != AxleTokenType::End;
       current = parser->current.type)
  {
    if (parser->current.type == AxleTokenType::Intrinsic) {
      if (parser->current.string == comp_thread->intrinsics.import) {
        Span span = {};
        SPAN_START;

        advance(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return;
        }

        AST_LOCAL expr = parse_expression(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return;
        }

        expect(comp, comp_thread, parser, AxleTokenType::Semicolon);
        if (comp_thread->is_panic()) {
          return;
        }

        SPAN_END;

        ASTImport* imp = ast_alloc<ASTImport>(parser);
        imp->ast_type = AST_TYPE::IMPORT;
        imp->expr_location = expr;
        imp->node_span = span;

        add_comp_unit_for_import(comp, parser->current_namespace, parser->file_path, imp);

        top_level_temp.insert({ imp });
      }
      else if (parser->current.string == comp_thread->intrinsics.dynamic_export) {
        Span span = {};
        SPAN_START;

        advance(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return;
        }

        Axle::ViewArr<const AST_LOCAL>
          export_list = parse_export_list(comp, comp_thread, parser);
        if (comp_thread->is_panic()) {
          return;
        }

        SPAN_END;

        ASTExport* e = ast_alloc<ASTExport>(parser);
        e->ast_type = AST_TYPE::EXPORT;
        e->export_list = export_list;
        e->node_span = span;

        add_comp_unit_for_export(comp, parser->current_namespace, e);
        top_level_temp.insert({ e });
      }
      else {
        comp_thread->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->full_path(), parser->current),
                                  "Intrinsic '#{}' is not valid here", parser->current.string);
        return;
      }
    }
    else if (parser->current.type == AxleTokenType::Mut
             || (parser->current.type == AxleTokenType::Identifier
                 && parser->next.type == AxleTokenType::Colon)) {
      //Decl
      AST_LOCAL decl = parse_decl(comp, comp_thread, parser, true);
      if (comp_thread->is_panic()) {
        return;
      }

      add_comp_unit_for_global(comp, parser->current_namespace, downcast_ast<ASTDecl>(decl));
      if (comp_thread->is_panic()) {
        return;
      }

      top_level_temp.insert({ decl });
    }
    else {
      comp_thread->report_error(ERROR_CODE::SYNTAX_ERROR, span_of_token(parser->full_path(), parser->current),
                                "Unexpected token");
      return;
    }
  }


  file->top_level = ast_bake_arr(parser, std::move(top_level_temp));
}

// putting in all switch cases even with default
#pragma warning(pop) 

void Printer::newline() const {
  IO_Single::print('\n');

  for (size_t i = 0; i < tabs; i++) {
    IO_Single::print("  ");
  }
}

using namespace Axle::Literals;

static void print_ast(Printer* const printer, AST_LOCAL a) noexcept;

static void print_ast_comma_list(Printer* const printer,
                           Axle::ViewArr<const AST_LOCAL> arr) noexcept {
  auto c = arr.begin();
  const auto e = arr.end();
  if (c < e) {
    print_ast(printer, *c);
    c += 1;

    for (; c < e; ++c) {
      IO_Single::print(", ");
      print_ast(printer, *c);
    }
  }
}

static void print_ast_stmt_list(Printer* const printer,
                                Axle::ViewArr<const AST_LOCAL> arr) noexcept {
  auto c = arr.begin();
  const auto e = arr.end();
  if (c < e) {
    print_ast(printer, *c);
    IO_Single::print(";");
    c += 1;

    for (; c < e; ++c) {
      printer->newline();
      print_ast(printer, *c);
      IO_Single::print(";");
    }
  }
}

static void print_ast(Printer* const printer, AST_LOCAL a) noexcept {
  switch (a.ast->ast_type) {
    case AST_TYPE::INVALID: INVALID_CODE_PATH("Invalid Ast Node"); break;
    case AST_TYPE::NAMED_TYPE: {
        ASTNamedType* nt = downcast_ast<ASTNamedType>(a);
        IO_Single::format("{}", nt->name);
        return;
      }
    case AST_TYPE::ARRAY_TYPE: {
        ASTArrayType* at = downcast_ast<ASTArrayType>(a);
        IO_Single::print('[');
        print_ast(printer, at->base);
        IO_Single::print("; ");
        print_ast(printer, at->expr);
        IO_Single::print(']');
        return;
      }
    case AST_TYPE::PTR_TYPE: {
        ASTPtrType* pt = downcast_ast<ASTPtrType>(a);
        IO_Single::print('*');
        print_ast(printer, pt->base);
        return;
      }
    case AST_TYPE::SLICE_TYPE: {
        ASTSliceType* pt = downcast_ast<ASTSliceType>(a);
        IO_Single::print("[]");
        print_ast(printer, pt->base);
        return;
      }
    case AST_TYPE::LAMBDA_TYPE: {
        ASTLambdaType* lt = downcast_ast<ASTLambdaType>(a);
        IO_Single::print('(');

        print_ast_comma_list(printer, lt->args);

        IO_Single::print(") -> ");
        print_ast(printer, lt->ret);
        return;
      }
    case AST_TYPE::TUPLE_TYPE: {
        ASTTupleType* tt = downcast_ast<ASTTupleType>(a);
        IO_Single::print('(');
        print_ast_comma_list(printer, tt->types);
        IO_Single::print(')');
        return;
      }
    case AST_TYPE::CAST: {
        ASTCastExpr* cast = downcast_ast<ASTCastExpr>(a);
        IO_Single::print("cast(");
        print_ast(printer, cast->type);
        IO_Single::print(", ");
        print_ast(printer, cast->expr);
        IO_Single::print(')');
        return;
      }
    case AST_TYPE::UNARY_OPERATOR: {
        ASTUnaryOperatorExpr* un_op = downcast_ast<ASTUnaryOperatorExpr>(a);
        IO_Single::print(UNARY_OP_STRING::get(un_op->op));
        print_ast(printer, un_op->expr);
        return;
      }
    case AST_TYPE::BINARY_OPERATOR: {
        ASTBinaryOperatorExpr* bin_op = downcast_ast<ASTBinaryOperatorExpr>(a);

        print_ast(printer, bin_op->left);
        IO_Single::format(" {} ", BINARY_OP_STRING::get(bin_op->op));
        print_ast(printer, bin_op->right);
        return;
      }
    case AST_TYPE::IDENTIFIER_EXPR: {
        ASTIdentifier* i = downcast_ast<ASTIdentifier>(a);
        IO_Single::format("{}", i->name);
        return;
      }
    case AST_TYPE::NUMBER: {
        ASTNumber* n = downcast_ast<ASTNumber>(a);
        IO_Single::format("{}", n->num_value);
        if (n->suffix != nullptr) {
          IO_Single::format("{}", n->suffix);
        }
        return;
      }
    case AST_TYPE::FUNCTION_CALL: {
        ASTFunctionCallExpr* c = downcast_ast<ASTFunctionCallExpr>(a);

        print_ast(printer, c->function);

        IO_Single::print('(');
        print_ast_comma_list(printer, c->arguments);
        IO_Single::print(')');
        return;
      }
    case AST_TYPE::TUPLE_LIT: {
        ASTTupleLitExpr* t = downcast_ast<ASTTupleLitExpr>(a);

        IO_Single::print("{ ");
        print_ast_comma_list(printer, t->elements);
        IO_Single::print(" }");
        return;
      }
    case AST_TYPE::ARRAY_EXPR: {
        ASTArrayExpr* ae = downcast_ast<ASTArrayExpr>(a);

        IO_Single::print("[ ");
        print_ast_comma_list(printer, ae->elements);
        IO_Single::print(" ]");
        return;
      }
    case AST_TYPE::ASCII_STRING: {
        ASTAsciiString* as = downcast_ast<ASTAsciiString>(a);
        IO_Single::format("\"{}\"", as->string);
        return;
      }
    case AST_TYPE::ASCII_CHAR: {
        ASTAsciiChar* ac = downcast_ast<ASTAsciiChar>(a);
        IO_Single::format("'{}'", ac->character);
        return;
      }
    case AST_TYPE::INDEX_EXPR: {
        ASTIndexExpr* ie = downcast_ast<ASTIndexExpr>(a);
        print_ast(printer, ie->expr);
        IO_Single::print('[');
        print_ast_comma_list(printer, ie->arguments);
        IO_Single::print(']');
        return;
      }
    case AST_TYPE::MEMBER_ACCESS: {
        ASTMemberAccessExpr* ma = downcast_ast<ASTMemberAccessExpr>(a);
        print_ast(printer, ma->expr);
        IO_Single::format(".{}", ma->name);
        return;
      }
    case AST_TYPE::LAMBDA_EXPR: {
        ASTLambdaExpr* le = downcast_ast<ASTLambdaExpr>(a);
        print_ast(printer, le->lambda);
        return;
      }
    case AST_TYPE::LAMBDA: {
        ASTLambda* ld = downcast_ast<ASTLambda>(a);
        print_ast(printer, {ld->sig});
        print_ast(printer, ld->body);
        return;
      }
    case AST_TYPE::STRUCT_EXPR: {
        ASTStructExpr* se = downcast_ast<ASTStructExpr>(a);
        print_ast(printer, se->struct_body);
        return;
      }
    case AST_TYPE::STRUCT: {
        ASTStructBody* s = downcast_ast<ASTStructBody>(a);
        IO_Single::print("struct {");
        printer->tabs += 1;
        printer->newline();
        
        print_ast_stmt_list(printer, s->elements);

        printer->tabs -= 1;
        printer->newline();
        IO_Single::print('}');
        return;
      }
    case AST_TYPE::DECL: {
        ASTDecl* d = downcast_ast<ASTDecl>(a);

        if (d->mutability == ASTDeclMutability::Mutable) {
          IO_Single::print("mut ");
        }

        if (d->type_ast == NULL_AST_NODE) {
          IO_Single::format("{} :", d->name);
        }
        else {
          IO_Single::format("{} :", d->name);
          print_ast(printer, d->type_ast);
          IO_Single::print(" ");
        }

        if (d->mutability == ASTDeclMutability::Comptime) {
          IO_Single::print(": ");
        }
        else {
          ASSERT(d->mutability == ASTDeclMutability::Immutable);
          IO_Single::print("= ");
        }

        print_ast(printer, d->expr);
        return;
      }
    case AST_TYPE::TYPED_NAME: {
        ASTTypedName* d = downcast_ast<ASTTypedName>(a);

        ASSERT(d->mutability != ASTDeclMutability::Comptime);

        if (d->mutability == ASTDeclMutability::Mutable) {
          IO_Single::print("mut ");
        }

        IO_Single::format("{}:", d->name);
        print_ast(printer, d->type_ast);
        return;
      }
    case AST_TYPE::ASSIGN: {
        ASTAssign* as = downcast_ast<ASTAssign>(a);

        print_ast(printer, as->assign_to);
        IO_Single::print(" = ");
        print_ast(printer, as->value);
        return;
      }
    case AST_TYPE::BLOCK: {
        ASTBlock* b = downcast_ast<ASTBlock>(a);

        IO_Single::print('{');
        printer->tabs += 1;
        printer->newline();

        print_ast_stmt_list(printer, b->block);

        printer->tabs -= 1;
        printer->newline();
        IO_Single::print('}');
        return;
      }
    case AST_TYPE::IF_ELSE: {
        ASTIfElse* ie = downcast_ast<ASTIfElse>(a);

        IO_Single::print("if (");
        print_ast(printer, ie->condition);
        IO_Single::print(") ");
        print_ast(printer, ie->if_statement);

        if (ie->else_statement != NULL_AST_NODE) {
          IO_Single::print("else ");
          print_ast(printer, ie->else_statement);
        }

        return;
      }
    case AST_TYPE::WHILE: {
        ASTWhile* w = downcast_ast<ASTWhile>(a);

        IO_Single::print("while (");
        print_ast(printer, w->condition);
        IO_Single::print(") ");
        print_ast(printer, w->statement);
        return;
      }
    case AST_TYPE::RETURN: {
        ASTReturn* r = downcast_ast<ASTReturn>(a);

        if (r->expr != NULL_AST_NODE) {
          IO_Single::print("return ");
          print_ast(printer, r->expr);
        }
        else {
          IO_Single::print("return");
        }

        return;
      }
    case AST_TYPE::FUNCTION_SIGNATURE: {
        ASTFuncSig* s = downcast_ast<ASTFuncSig>(a);
        IO_Single::print('(');
        print_ast_comma_list(printer, s->parameters);
        IO_Single::print(") -> ");
        print_ast(printer, s->return_type);
        return;
      }
    case AST_TYPE::IMPORT: {
        ASTImport* i = downcast_ast<ASTImport>(a);

        IO_Single::print("#import ");
        print_ast(printer, i->expr_location);
        return;
      }
    case AST_TYPE::EXPORT: {
        ASTExport* e = downcast_ast<ASTExport>(a);

        IO_Single::print("#export {");
        printer->tabs += 1;
        printer->newline();

        for (AST_LOCAL it: e->export_list) {
          print_ast(printer, it);
          IO_Single::print(",");
          printer->newline();
        }

        printer->tabs -= 1;
        return;
      }
    case AST_TYPE::EXPORT_SINGLE: {
        ASTExportSingle* e = downcast_ast<ASTExportSingle>(a);

        IO_Single::format("\"{}\" = ", e->name);
        print_ast(printer, e->value);
        return;
      }
    case AST_TYPE::LINK: {
        ASTLink* imp = downcast_ast<ASTLink>(a);

        if (imp->dynamic) {
          IO_Single::print("#dyamic_import(");
        }
        else {
          IO_Single::print("#static_import(");
        }
        print_ast(printer, imp->import_type);

        IO_Single::format(", {}, {})", imp->lib_file, imp->name);
        return;
      }
  }

  INVALID_CODE_PATH("INVALID/UNCOVERED NODE TYPE IN PRINTER!");
}

void print_full_ast(AST_LOCAL expr) {
  Printer printer = {};
  print_ast(&printer, expr);
}

void print_full_ast(const FileAST* file) {
  AXLE_TELEMETRY_SCOPE("Print full ast");

  Printer printer = {};

  auto curr = file->top_level.begin();
  const auto end = file->top_level.end();

  if (curr < end) {
    print_ast(&printer, *curr);
    if (curr->ast->ast_type != AST_TYPE::LAMBDA) {
      IO_Single::print(';');
    }

    for (; curr < end; ++curr) {
      printer.newline();
      printer.newline();
      print_ast(&printer, *curr);
      if (curr->ast->ast_type != AST_TYPE::LAMBDA) {
        IO_Single::print(';');
      }
    }
  }
}
