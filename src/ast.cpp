#include "ast.h"

void ASTExpression::move_from(ASTExpression&& a) noexcept {
  type = std::exchange(a.type, Type{});

  makes_call = std::exchange(a.makes_call, false);
  call_leaf = std::exchange(a.call_leaf, false);
  meta_flags = std::exchange(a.meta_flags, 0);

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
  #define MOD(TYTY, name) case JOIN(TYPE_TYPE::, TYTY): default_init(&name); break;
    TYPE_TYPE_MOD;
  #undef MOD
  }
}

void ASTType::destruct_union() {
  switch (type_type) {
  #define MOD(TYTY, name) case JOIN(TYPE_TYPE::, TYTY): destruct_single(&name); break;
    TYPE_TYPE_MOD;
  #undef MOD
  }
}

void ASTType::move_from(ASTType&& a) noexcept {
  type = std::exchange(a.type, Type{});
  span = std::exchange(a.span, {});
  type_type = std::exchange(a.type_type, TYPE_TYPE::UNKNOWN);

  switch (type_type)
  {
    case TYPE_TYPE::UNKNOWN: break;

    #define MOD(name, expr_name) \
    case TYPE_TYPE:: ## name: expr_name = std::move(a. ## expr_name); \
    break;

    TYPE_TYPE_MOD

      #undef MOD
  }
}

ASTType::~ASTType() {
  destruct_union();
}
