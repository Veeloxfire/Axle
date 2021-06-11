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
    case EXPRESSION_TYPE::NULLPTR:
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

    case EXPRESSION_TYPE::NULLPTR:
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

    case EXPRESSION_TYPE::NULLPTR:
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
        destruct_single(&expression);
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

static void build_expression_linked_list(ASTExpression* expr);

static void build_expression_linked_list(ASTType* type) {
  switch (type->type_type) {
    case TYPE_TYPE::NORMAL: break;
    case TYPE_TYPE::ARRAY:
      build_expression_linked_list(type->arr.base);
      build_expression_linked_list(type->arr.expr);
      break;
    case TYPE_TYPE::PTR:
      build_expression_linked_list(type->ptr.base);
      break;
    default: assert(false); break;
  }
}

//Returns the first element
static void build_expression_linked_list(ASTExpression* expr) {
  assert(expr->next == nullptr);
  assert(expr->first == nullptr);

  DEFER(&) {
    assert(expr->next == nullptr);
    assert(expr->first != nullptr);
  };

  switch (expr->expr_type) {
    case EXPRESSION_TYPE::UNKNOWN: assert(false); break;
    case EXPRESSION_TYPE::CAST: {
        build_expression_linked_list(&expr->cast.type);
        build_expression_linked_list(expr->cast.expr);
        expr->first = expr->cast.expr->first;
        expr->cast.expr->next = expr;
        break;
      }
    case EXPRESSION_TYPE::UNARY_OPERATOR: {
        build_expression_linked_list(expr->un_op.expr);
        expr->first = expr->un_op.expr->first;
        expr->un_op.expr->next = expr;
        break;
      }
    case EXPRESSION_TYPE::BINARY_OPERATOR: {
        build_expression_linked_list(expr->bin_op.left);
        build_expression_linked_list(expr->bin_op.right);

        expr->first = expr->bin_op.left->first;
        expr->bin_op.left->next = expr->bin_op.right->first;
        expr->bin_op.right->next = expr;
        break;
      }
    case EXPRESSION_TYPE::INDEX: {
        build_expression_linked_list(expr->index.expr);
        build_expression_linked_list(expr->index.index);

        expr->first = expr->index.expr->first;
        expr->index.expr->next = expr->index.index->first;
        expr->index.index->next = expr;
        break;
      }
    case EXPRESSION_TYPE::NULLPTR:
    case EXPRESSION_TYPE::ASCII_STRING:
    case EXPRESSION_TYPE::VALUE:
    case EXPRESSION_TYPE::ENUM:
    case EXPRESSION_TYPE::LOCAL:
    case EXPRESSION_TYPE::NAME: {
        expr->first = expr;
        break;
      }
    case EXPRESSION_TYPE::FUNCTION_CALL: {
        auto i = expr->call.arguments.mut_begin();
        auto end = expr->call.arguments.mut_end();

        if (i < end) {
          build_expression_linked_list(i);
          expr->first = i->first;

          ASTExpression* prev = i;

          i++;//load next

          for (; i < end; i++) {
            build_expression_linked_list(i);

            prev->next = i->first;
            prev = i;
          }

          prev->next = expr;
        }
        else {
          expr->first = expr;
        }
        break;
      }
    case EXPRESSION_TYPE::ARRAY_EXPR: {
        auto i = expr->array_expr.elements.mut_begin();
        auto end = expr->array_expr.elements.mut_end();

        if (i < end) {
          build_expression_linked_list(i);
          expr->first = i->first;

          ASTExpression* prev = i;

          i++;//load next

          for (; i < end; i++) {
            build_expression_linked_list(i);

            prev->next = i->first;
            prev = i;
          }

          prev->next = expr;
        }
        else {
          expr->first = expr;
        }
        break;
      }
  }
}

void build_expression_linked_list(ASTBlock* block) {
  Array<ScopeView> scopes ={};
  scopes.insert({
    block->block.mut_begin(),
    block->block.end()
                });

  while (scopes.size > 0) {
    auto* this_scope = scopes.back();

    while (this_scope->i < this_scope->end) {
      ASTStatement* stmt = this_scope->i++;

      switch (stmt->type) {
        case STATEMENT_TYPE::UNKNOWN: assert(false); break;
        case STATEMENT_TYPE::EXPRESSION:
        case STATEMENT_TYPE::RETURN:
          build_expression_linked_list(&stmt->expression);
          break;
        case STATEMENT_TYPE::BLOCK:
          scopes.insert({
              stmt->block.block.mut_begin(),
              stmt->block.block.end()
                        });

          this_scope = scopes.back();
          goto NEW_SCOPE;
        case STATEMENT_TYPE::IF_ELSE:
          build_expression_linked_list(&stmt->if_else.condition);

          scopes.insert({
              stmt->if_else.if_statement,
              stmt->if_else.if_statement + 1
                        });

          scopes.insert({
              stmt->if_else.else_statement,
              stmt->if_else.else_statement + 1
                        });

          this_scope = scopes.back();
          goto NEW_SCOPE;
        case STATEMENT_TYPE::LOCAL:
          build_expression_linked_list(&stmt->local.type);
          build_expression_linked_list(&stmt->local.expression);
          break;
      }
    }

  NEW_SCOPE:
    while (this_scope->i >= this_scope->end) {
      scopes.pop();
      this_scope = scopes.back();
    }
  }
}