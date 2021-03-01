#include "ast.h"

void ASTExpression::move_from(ASTExpression&& a) noexcept {
  type = std::exchange(a.type, nullptr);
  value_index = std::exchange(a.value_index, 0);
  expr_type = std::exchange(a.expr_type, EXPRESSION_TYPE::UNKNOWN);

  switch (expr_type)
  {
    case EXPRESSION_TYPE::UNKNOWN:
      break;
    case EXPRESSION_TYPE::BINARY_OPERATOR:
      bin_op = std::move(a.bin_op);
      break;
    case EXPRESSION_TYPE::NAME:
      name = std::move(a.name);
      break;
    case EXPRESSION_TYPE::LOCAL:
      local = std::move(a.local);
      break;
    case EXPRESSION_TYPE::VALUE:
      value = a.value;
      break;
    case EXPRESSION_TYPE::FUNCTION_CALL:
      call = std::move(a.call);
      break;
  }
}

ASTExpression::~ASTExpression() {
  switch (expr_type) {
    case EXPRESSION_TYPE::BINARY_OPERATOR: {
        bin_op.~BinaryOperatorExpr();
        break;
      }
    case EXPRESSION_TYPE::NAME: {
        name.~InternString();
        break;
      }
    case EXPRESSION_TYPE::LOCAL: {
        local.~Location();
        break;
      }
    case EXPRESSION_TYPE::FUNCTION_CALL: {
        call.~FunctionCallExpr();
        break;
      }
    case EXPRESSION_TYPE::VALUE: {
        break;
      }
  }
}

ASTStatement::~ASTStatement() {
  switch (type) {
    case STATEMENT_TYPE::EXPRESSION: {
        expression.~ASTExpression();
        break;
      }
    case STATEMENT_TYPE::DECLARATION: {
        declaration.~ASTDeclaration();
        break;
      }
  }
}