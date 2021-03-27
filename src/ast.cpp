#include "ast.h"

void ASTExpression::move_from(ASTExpression&& a) noexcept {
  type = std::exchange(a.type, nullptr);

  makes_call = std::exchange(a.makes_call, false);
  call_leaf = std::exchange(a.call_leaf, false);

  expr_type = std::exchange(a.expr_type, EXPRESSION_TYPE::UNKNOWN);

  switch (expr_type)
  {
    case EXPRESSION_TYPE::UNKNOWN:
      break;
    case EXPRESSION_TYPE::BINARY_OPERATOR:
      bin_op = std::move(a.bin_op);
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

ASTExpression::~ASTExpression() {
  switch (expr_type) {
    case EXPRESSION_TYPE::BINARY_OPERATOR: {
        bin_op.~BinaryOperatorExpr();
        break;
      }
    case EXPRESSION_TYPE::LOCAL:
    case EXPRESSION_TYPE::NAME: {
        name.~InternString();
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