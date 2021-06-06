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
    case EXPRESSION_TYPE::ARRAY_EXPR:
      array_expr = std::move(a.array_expr);
      break;
    case EXPRESSION_TYPE::INDEX:
      index = std::move(a.index);
      break;
    case EXPRESSION_TYPE::BINARY_OPERATOR:
      bin_op = std::move(a.bin_op);
      break;
    case EXPRESSION_TYPE::UNARY_OPERATOR:
      un_op = std::move(a.un_op);
      break;
    case EXPRESSION_TYPE::CAST:
      cast = std::move(a.cast);
      break;
    case EXPRESSION_TYPE::NAME:
    case EXPRESSION_TYPE::LOCAL:
      name = std::move(a.name);
      break;
    case EXPRESSION_TYPE::VALUE:
      value = a.value;
      break;
    case EXPRESSION_TYPE::FUNCTION_CALL:
      call = std::move(a.call);
      break;
  }
}

void ASTExpression::set_union(EXPRESSION_TYPE et) noexcept {
  destruct_union();

  expr_type = et;
  switch (expr_type) {
    case EXPRESSION_TYPE::UNKNOWN: break;

    case EXPRESSION_TYPE::BINARY_OPERATOR: {
        default_init(&bin_op);
        break;
      }
    case EXPRESSION_TYPE::UNARY_OPERATOR: {
        default_init(&un_op);
        break;
      }
    case EXPRESSION_TYPE::CAST: {
        default_init(&cast);
        break;
      }
    case EXPRESSION_TYPE::FUNCTION_CALL: {
        default_init(&call);
        break;
      }
    case EXPRESSION_TYPE::ENUM: {
        default_init(&enum_value);
        break;
      }
    case EXPRESSION_TYPE::INDEX: {
        default_init(&index);
        break;
      }
    case EXPRESSION_TYPE::ARRAY_EXPR: {
        default_init(&array_expr);
        break;
      }

    case EXPRESSION_TYPE::LOCAL:
    case EXPRESSION_TYPE::NAME:
    case EXPRESSION_TYPE::VALUE:
      break;
  }
}

void ASTExpression::destruct_union() noexcept {
  switch (expr_type) {
    case EXPRESSION_TYPE::UNKNOWN: break;

    case EXPRESSION_TYPE::BINARY_OPERATOR: {
        bin_op.~BinaryOperatorExpr();
        break;
      }
    case EXPRESSION_TYPE::UNARY_OPERATOR: {
        un_op.~UnaryOperatorExpr();
        break;
      }
    case EXPRESSION_TYPE::CAST: {
        cast.~CastExpr();
        break;
      }
    case EXPRESSION_TYPE::FUNCTION_CALL: {
        call.~FunctionCallExpr();
        break;
      }
    case EXPRESSION_TYPE::ENUM: {
        enum_value.~EnumValueExpr();
        break;
      }
    case EXPRESSION_TYPE::INDEX: {
        index.~IndexExpr();
        break;
      }
    case EXPRESSION_TYPE::ARRAY_EXPR: {
        array_expr.~ArrayExpr();
        break;
      }

    case EXPRESSION_TYPE::LOCAL:
    case EXPRESSION_TYPE::NAME:
    case EXPRESSION_TYPE::VALUE:
      break;
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
    case STATEMENT_TYPE::RETURN:
    case STATEMENT_TYPE::EXPRESSION: {
        default_init(&expression);
        break;
      }
    case STATEMENT_TYPE::LOCAL: {
        default_init(&local);
        break;
      }
    case STATEMENT_TYPE::IF_ELSE: {
        default_init(&if_else);
        break;
      }
    case STATEMENT_TYPE::BLOCK: {
        default_init(&block);
        break;
      }
  }
}

void ASTStatement::destruct_union() noexcept {
  switch (type) {
    case STATEMENT_TYPE::UNKNOWN: break;

    case STATEMENT_TYPE::RETURN:
    case STATEMENT_TYPE::EXPRESSION: {
        expression.~ASTExpression();
        break;
      }
    case STATEMENT_TYPE::LOCAL: {
        local.~ASTLocal();
        break;
      }
    case STATEMENT_TYPE::IF_ELSE: {
        if_else.~ASTIfElse();
        break;
      }
    case STATEMENT_TYPE::BLOCK: {
        block.~ASTBlock();
        break;
      }
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