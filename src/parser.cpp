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
    return error_token(lex, "Characte literal was not closed!");
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
  else if (is_number(c)) {
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
  Lexer* const lex = comp->lexer;

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
      comp->report_error(CompileCode::SYNTAX_ERROR, span_of_token(*tok),
                         tok->string->string);
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

    comp->report_error(CompileCode::FILE_ERROR, span,
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
    comp->report_error(CompileCode::SYNTAX_ERROR, span_of_token(parser->current),
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
    comp->report_error(CompileCode::SYNTAX_ERROR, span_of_token(parser->current),
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
  Lexer* const lex = comp->lexer;
  Parser* const parser = comp->parser;


  lex->strings = comp->strings;
  lex->top = string;

  lex->curr_pos.full_path = file_name;
  lex->curr_pos.character = 0;
  lex->curr_pos.line = 0;

  parser->stream = next_lex_stream(comp);
  if (comp->is_panic()) {
    return;
  }

  if (parser->stream.i == nullptr || parser->stream.end == nullptr) {
    comp->report_error(CompileCode::INTERNAL_ERROR, Span{},
                       "Parser was passed a fully or partially null stream"
                       "Start: '{}', End: '{}'",
                       PrintPtr{ parser->stream.i }, PrintPtr{ parser->stream.end });
    return;
  }

  if (parser->stream.i >= parser->stream.end) {
    comp->report_error(CompileCode::INTERNAL_ERROR, Span{},
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

    comp->report_error(CompileCode::FILE_ERROR, span,
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

static void parse_name(Compiler* const comp, Parser* const parser, const InternString** name) {
  if (parser->current.type != AxleTokenType::Identifier) {
    comp->report_error(CompileCode::SYNTAX_ERROR, span_of_token(parser->current),
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
    case AxleTokenType::And: advance(comp, parser); return BINARY_OPERATOR::AND;
  }

  comp->report_error(CompileCode::SYNTAX_ERROR, span_of_token(parser->current),
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

static void parse_primary(Compiler* const comp, Parser* const parser, ASTExpression* const expr) {
  //Will always be a primary so can elevate span stuff out of the switch
  Span span ={};
  set_span_start(parser->current, span);
  DEFER(&) {
    set_span_end(parser->prev, span);
    expr->span = std::move(span);
  };

  const Token current = parser->current;
  advance(comp, parser);
  if (comp->is_panic()) {
    return;
  }

  switch (current.type) {
    case AxleTokenType::Left_Brace: {
        // { ... }

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
        expr->ascii_string = current.string;
        break;
      }
    case AxleTokenType::Character: {
        expr->set_union(EXPRESSION_TYPE::ASCII_CHAR);
        expr->ascii_char = current.string->string[0];
        break;
      }
    case AxleTokenType::Cast: {
        // cast(... , ...)
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
        expr->value.value = string_to_uint(current.string->string);
        if (!parser->current.consumed_whitespace && parser->current.type == AxleTokenType::Identifier) {
          expr->value.suffix = parser->current.string;
          advance(comp, parser);
        }
        break;
      }
    case AxleTokenType::Identifier: {
        //Name or function call

        if (parser->current.type == AxleTokenType::Left_Bracket) {
          //Is function call
          expr->set_union(EXPRESSION_TYPE::FUNCTION_CALL);
          FunctionCallExpr& call = expr->call;

          call.function_name = current.string;

          //Advance past the left bracket
          expect(comp, parser, AxleTokenType::Left_Bracket);
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
                comp->report_error(CompileCode::SYNTAX_ERROR, span_of_token(parser->current),
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
        parse_inner_expression(comp, parser, expr);
        if (comp->is_panic()) {
          return;
        }
        expect(comp, parser, AxleTokenType::Right_Bracket);
        break;
    default: comp->report_error(CompileCode::SYNTAX_ERROR, span_of_token(current),
                                "Unexpected Token Type '{}'", current.type);
      }
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
          //Member time
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
    default: comp->report_error(CompileCode::SYNTAX_ERROR, span_of_token(parser->current),
                                "Expected A Type! Found '{}'", parser->current.type);
  }
}

static void parse_typed_name(Compiler* const comp, Parser* const parser, ASTType* const type, const InternString** name) {
  parse_type(comp, parser, type);
  if (comp->is_panic()) {
    return;
  }
  parse_name(comp, parser, name);
}

static void parse_block(Compiler* const comp, Parser* const parser, ASTBlock* const block);

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

        statement->expression = allocate_default<ASTExpression>();
        parse_expression(comp, parser, statement->expression);
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
        //Ambiguous at this point - save the position
        Parser save = *parser;

        //try declaration first
        statement->set_union(STATEMENT_TYPE::LOCAL);
        parse_type(comp, parser, &statement->local.type);
        if (comp->is_panic()) {
          return;
        }

        //This might cause some errors at the moment

        if (!comp->is_panic() && parser->current.type == AxleTokenType::Identifier) {
          //Is declaration
          parse_name(comp, parser, &statement->local.name);
          if (comp->is_panic()) {
            return;
          }

          expect(comp, parser, AxleTokenType::Equals);
          if (comp->is_panic()) {
            return;
          }

          statement->local.expression = allocate_default<ASTExpression>();
          parse_expression(comp, parser, statement->local.expression);
        }
        else {
          //Not a declaration - expression or assignment
          *parser = std::move(save);

          ASTExpression* expr = allocate_default<ASTExpression>();
          parse_expression(comp, parser, expr);

          if (comp->is_panic()) {
            return;
          }

          if (parser->current.type == AxleTokenType::Equals) {
            //Is Assign
            statement->set_union(STATEMENT_TYPE::ASSIGN);
            statement->assign.assign_to = expr;

            advance(comp, parser);
            if (comp->is_panic()) {
              return;
            }

            //Load the value expression
            statement->assign.value = allocate_default<ASTExpression>();
            parse_expression(comp, parser, statement->assign.value);
            if (comp->is_panic()) {
              return;
            }
          }
          else {
            //Is expression
            statement->set_union(STATEMENT_TYPE::EXPRESSION);
            statement->expression = expr;
          }
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

static void parse_function_signature(Compiler* const comp, Parser* const parser, ASTFunctionSignature* const sig) {
  //Might specifiy a convention
  if (parser->current.type == AxleTokenType::Convention) {
    advance(comp, parser);
    if (comp->is_panic()) {
      return;
    }

    expect(comp, parser, AxleTokenType::Left_Bracket);
    if (comp->is_panic()) {
      return;
    }

    parse_name(comp, parser, &sig->convention);
    if (comp->is_panic()) {
      return;
    }

    expect(comp, parser, AxleTokenType::Right_Bracket);
    if (comp->is_panic()) {
      return;
    }
  }

  parse_name(comp, parser, &sig->name);
  if (comp->is_panic()) {
    return;
  }

  expect(comp, parser, AxleTokenType::Left_Bracket);
  if (comp->is_panic()) {
    return;
  }

  //Parameters
  if (parser->current.type != AxleTokenType::Right_Bracket) {
    while (!comp->is_panic()) {
      sig->parameters.insert_uninit(1);
      ASTLocal* loc = sig->parameters.back();
      parse_typed_name(comp, parser, &loc->type, &loc->name);
      if (comp->is_panic()) {
        return;
      }

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
        comp->report_error(CompileCode::SYNTAX_ERROR, span_of_token(parser->current),
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

static void parse_function(Compiler* const comp, Parser* const parser, ASTFunctionDeclaration* const func) {
  expect(comp, parser, AxleTokenType::Function);
  if (comp->is_panic()) {
    return;
  }
  parse_function_signature(comp, parser, &func->signature);
  if (comp->is_panic()) {
    return;
  }

  if (parser->current.type == AxleTokenType::Left_Brace) {
    parse_block(comp, parser, &func->body);
  }
  else if (parser->current.type == AxleTokenType::Semicolon) {
    advance(comp, parser);
  }
  else {
    comp->report_error(CompileCode::SYNTAX_ERROR, span_of_token(parser->current),
                       "Expected '{}' or '{}'\nFound '{}'",
                       AxleTokenType::Left_Brace, AxleTokenType::Semicolon,
                       parser->current.type);
    return;
  }
}

static void parse_structure(Compiler* const comp, Parser* const parser, ASTStructureDeclaration* const struct_decl) {
  expect(comp, parser, AxleTokenType::Struct);
  if (comp->is_panic()) {
    return;
  }
  parse_name(comp, parser, &struct_decl->name);
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
      comp->report_error(CompileCode::SYNTAX_ERROR, span_of_token(parser->current),
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
    if (current == AxleTokenType::Import || current == AxleTokenType::Stdlib) {
      //Import
      file->imports.insert_uninit(1);
      ASTImport* imp = file->imports.back();

      imp->std = (current == AxleTokenType::Stdlib);

      advance(comp, parser);
      if (comp->is_panic()) {
        return;
      }

      if (parser->current.type != AxleTokenType::String) {
        comp->report_error(CompileCode::SYNTAX_ERROR, span_of_token(parser->current),
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
    else if (current == AxleTokenType::Function) {
      file->functions.insert_uninit(1);
      ASTFunctionDeclaration* func = file->functions.back();
      parse_function(comp, parser, func);
    }
    else if (current == AxleTokenType::Struct) {
      file->structs.insert_uninit(1);
      ASTStructureDeclaration* structure = file->structs.back();
      parse_structure(comp, parser, structure);
    }
    else if (current == AxleTokenType::Global) {
      file->globals.insert_uninit(1);
      ASTGlobalDeclaration* global = file->globals.back();

      set_span_start(parser->current, global->span);
      DEFER(&) {
        set_span_end(parser->prev, global->span);
      };

      advance(comp, parser);//global
      if (comp->is_panic()) {
        return;
      }

      parse_type(comp, parser, &global->type);
      if (comp->is_panic()) {
        return;
      }
      parse_name(comp, parser, &global->name);
      if (comp->is_panic()) {
        return;
      }

      expect(comp, parser, AxleTokenType::Equals);
      if (comp->is_panic()) {
        return;
      }

      parse_expression(comp, parser, &global->init_expr);
      if (comp->is_panic()) {
        return;
      }

      expect(comp, parser, AxleTokenType::Semicolon);
    }
    else {
      comp->report_error(CompileCode::SYNTAX_ERROR, span_of_token(parser->current),
                         "Unexpected token");
    }
  }

  //reduce over allocating space
  file->imports.shrink();
  file->functions.shrink();
  file->globals.shrink();
  file->structs.shrink();

  if (comp->is_panic()) {
    return;
  }
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
    case EXPRESSION_TYPE::MEMBER: {
        print_ast_expression(expr->member.expr);
        printf(".%s", expr->member.name->string);
        break;
      }
    case EXPRESSION_TYPE::INDEX: {
        print_ast_expression(expr->index.expr);
        IO::print('[');
        print_ast_expression(expr->index.index);
        IO::print(']');
        break;
      }
    case EXPRESSION_TYPE::TUPLE_LIT: {
        IO::print("{ ");
        auto i = expr->array_expr.elements.begin();
        const auto end = expr->array_expr.elements.end();

        if (i < end) {
          print_ast_expression(i);
          i++;

          for (; i < end; i++) {
            IO::print(", ");
            print_ast_expression(i);
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
          print_ast_expression(i);
          i++;

          for (; i < end; i++) {
            IO::print(", ");
            print_ast_expression(i);
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
    case STATEMENT_TYPE::ASSIGN:
      print_ast_expression(statement->assign.assign_to);
      IO::print(" = ");
      print_ast_expression(statement->assign.value);
      break;
    case STATEMENT_TYPE::LOCAL:
      print_type(&statement->local.type);
      printf(" %s = ", statement->local.name->string);
      print_ast_expression(statement->local.expression);
      IO::print(';');
      break;
    case STATEMENT_TYPE::RETURN:
      IO::print("return ");
      print_ast_expression(statement->expression);
      IO::print(';');
      break;
    case STATEMENT_TYPE::EXPRESSION:
      print_ast_expression(statement->expression);
      IO::print(';');
      break;
    case STATEMENT_TYPE::WHILE:
      IO::print("while(");
      print_ast_expression(statement->while_loop.condition);
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
      print_ast_expression(statement->if_else.condition);
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

  //Structures
  {
    auto i = file->structs.begin();
    const auto end = file->structs.end();

    for (; i < end; i++) {
      const ASTStructureDeclaration* str = i;

      printf("struct %s {", str->name->string);

      printer.tabs++;


      auto el_i = str->elements.begin();
      auto el_end = str->elements.end();

      for (; el_i < el_end; el_i++) {
        printer.newline();

        print_type(&el_i->type);

        printf(" %s;", el_i->name->string);
      }

      printer.tabs--;
      printer.newline();
      IO::print('}');

      printer.newline();
      printer.newline();
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