#include "ast.h"

void ASTExpression::move_from(ASTExpression&& a) noexcept {
  type = std::exchange(a.type, nullptr);

  makes_call = std::exchange(a.makes_call, false);
  call_leaf = std::exchange(a.call_leaf, false);
  comptime_eval = std::exchange(a.comptime_eval, false);

  const_val = std::exchange(a.const_val, nullptr);
  span = std::exchange(a.span, {});

  expr_type = std::exchange(a.expr_type, EXPRESSION_TYPE::UNKNOWN);

  switch (expr_type)
  {
    case EXPRESSION_TYPE::UNKNOWN:
      break;

    #define MODIFY(name, expr_name) \
    case EXPRESSION_TYPE:: ## name: expr_name = std::move(a. ## expr_name); \
    break;

      EXPRESSION_TYPE_MODIFY

    #undef MODIFY
  }
}

void ASTExpression::set_union(EXPRESSION_TYPE et) noexcept {
  destruct_union();

  expr_type = et;
  switch (expr_type) {
    case EXPRESSION_TYPE::UNKNOWN: break;

    #define MODIFY(name, expr_name) case EXPRESSION_TYPE:: ## name: \
    default_init(& expr_name); \
    break;

      EXPRESSION_TYPE_MODIFY

      #undef MODIFY
  }
}

void ASTExpression::destruct_union() noexcept {
  switch (expr_type) {
    case EXPRESSION_TYPE::UNKNOWN: break;

    #define MODIFY(name, expr_name) \
    case EXPRESSION_TYPE:: ## name: destruct_single(& expr_name); \
    break;

      EXPRESSION_TYPE_MODIFY

      #undef MODIFY
  }
}

ASTExpression::~ASTExpression() {
  destruct_union();
}

void ASTStatement::set_union(STATEMENT_TYPE st) noexcept {
  destruct_union();

  type = st;
  switch (type) {
    case STATEMENT_TYPE::UNKNOWN: break;

    #define MOD(name, expr_name) case STATEMENT_TYPE:: ## name:\
      default_init(& expr_name);\
      break;

      MOD_STATEMENTS
      #undef MOD
  }
}

void ASTStatement::destruct_union() noexcept {
  switch (type) {
    case STATEMENT_TYPE::UNKNOWN: break;

    #define MOD(name, expr_name) case STATEMENT_TYPE:: ## name:\
      destruct_single(& expr_name);\
      break;

      MOD_STATEMENTS
      #undef MOD
  }
}

ASTStatement::~ASTStatement() {
  destruct_union();
}

void ASTType::set_union(TYPE_TYPE ty) {
  destruct_union();

  type_type = ty;

  switch (type_type) {
    case TYPE_TYPE::PTR: {
        default_init(&ptr);
        break;
      }
    case TYPE_TYPE::ARRAY: {
        default_init(&arr);
        break;
      }
    case TYPE_TYPE::NORMAL: break;
  }
}

void ASTType::destruct_union() {
  switch (type_type) {
    case TYPE_TYPE::PTR: {
        ptr.~ASTPtrType();
        break;
      }
    case TYPE_TYPE::ARRAY: {
        arr.~ASTArrayType();
        break;
      }
    case TYPE_TYPE::NORMAL: break;
  }
}

ASTType::~ASTType() {
  destruct_union();
}
