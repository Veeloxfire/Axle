#include "compiler.h"
#include "dependency_check.h"

#include <AxleUtil/format.h>
#include <AxleUtil/io.h>

namespace Format = Axle::Format;

namespace {
struct DependencyChecker {
  Namespace* available_names;
  Axle::Array<Local*> locals;

  bool generate_visit;
  Axle::Array<AstVisit> visit_array;

  Local* get_local(const Axle::InternString* name) noexcept;
  void push_visit(AST_LOCAL a, AST_VISIT_STEP v) noexcept;
};

struct EvalDependencyChecker {
};

void set_dependency(CompilerThread& comp_thread, COMPILATION_UNIT_STAGE stage, UnitID id) noexcept {
  ASSERT(id != NULL_ID);

  comp_thread.new_depends.insert({ stage, id });
}

template<typename ... T>
void set_unfound_name(CompilerThread& comp_thread,
                      UnknownName&& name,
                      ERROR_CODE code, const Span& span,
                      const Format::FormatString<T...>& f_message, const T& ... ts
                      ) noexcept {
  AXLE_TELEMETRY_FUNCTION();

  ASSERT(name.ident != nullptr);

  comp_thread.local_unfound_names.names.insert_uninit(1);
  UnfoundNameHolder* dep = comp_thread.local_unfound_names.names.back();

  dep->name = std::move(name);
  dep->dependency = nullptr;
  dep->as_error.type = code;
  dep->as_error.span = span;
  dep->as_error.message = format(f_message, ts...);
}

Local* DependencyChecker::get_local(const Axle::InternString* name) noexcept {
  AXLE_TELEMETRY_FUNCTION();
  
  auto i = locals.begin();
  const auto end = locals.end();

  for (; i < end; i++) {
    Local* l = *i;

    if (l->decl.name == name) {
      return l;
    }
  }

  return nullptr;
}

void DependencyChecker::push_visit(AST_LOCAL a, AST_VISIT_STEP v) noexcept {
  AXLE_TELEMETRY_FUNCTION();
  ASSERT(ast_visit_step_ast_type(v) == a.ast->ast_type);

  if (generate_visit) {
    visit_array.insert(AstVisit{ a, v });
  }
}

Global* test_global_type_dependency(CompilerGlobals& comp, CompilerThread& comp_thread, DependencyChecker& state, const Span& span, const Axle::InternString* ident) noexcept {
  AXLE_TELEMETRY_FUNCTION();
  
  auto names = comp.services.names.get();
  const GlobalName* name = names->find_global_name(state.available_names, ident);

  if (name == nullptr) {
    names.release();
    UnknownName unknown = {};
    unknown.ident = ident;
    unknown.ns = state.available_names;

    set_unfound_name(comp_thread, std::move(unknown),
                     ERROR_CODE::NAME_ERROR, span,
                     "Could not find name '{}'", ident);
    return nullptr;
  }
  else {
    Global* g = name->global;
    names.release();
    ASSERT(g->decl.type.is_valid());
    return g;
  }
}

void type_dependency_check_ast_node(
  CompilerGlobals& comp,
  CompilerThread& comp_thread,
  DependencyChecker& state,
  AST_LOCAL a
) noexcept {
  AXLE_TELEMETRY_FUNCTION();

  ASSERT(a != NULL_AST_NODE);

#ifdef STACKTRACE_ENABLE
  Axle::Stacktrace::ScopedExecTrace ast_trace(ast_type_string(a.ast->ast_type));
#endif

  const usize debug_check_size = state.visit_array.size;
  DEFER(&) { ASSERT(!state.generate_visit || state.visit_array.size != debug_check_size); };

  switch (a.ast->ast_type) {
    case AST_TYPE::INVALID: INVALID_CODE_PATH("Invalid node type"); break;
    case AST_TYPE::NAMED_TYPE: {
        ASTNamedType* nt = downcast_ast<ASTNamedType>(a);

        nt->global = test_global_type_dependency(comp, comp_thread, state, nt->node_span, nt->name);
        
        state.push_visit(a, AST_VISIT_STEP::NAMED_TYPE_DOWN);
        return;
      }
    case AST_TYPE::ARRAY_TYPE: {
        ASTArrayType* at = downcast_ast<ASTArrayType>(a);
        
        state.push_visit(a, AST_VISIT_STEP::ARRAY_TYPE_UP);

        type_dependency_check_ast_node(comp, comp_thread, state, at->base);
        type_dependency_check_ast_node(comp, comp_thread, state, at->expr);
        state.push_visit(a, AST_VISIT_STEP::ARRAY_TYPE_DOWN_LEN);
        
        state.push_visit(a, AST_VISIT_STEP::ARRAY_TYPE_DOWN);
        return;
      }
    case AST_TYPE::PTR_TYPE: {
        ASTPtrType* ptr = downcast_ast<ASTPtrType>(a);
        
        state.push_visit(a, AST_VISIT_STEP::PTR_TYPE_UP);

        type_dependency_check_ast_node(comp, comp_thread, state, ptr->base);
        
        state.push_visit(a, AST_VISIT_STEP::PTR_TYPE_DOWN);

        return;
      }
    case AST_TYPE::SLICE_TYPE: {
        ASTSliceType* ptr = downcast_ast<ASTSliceType>(a);

        state.push_visit(a, AST_VISIT_STEP::SLICE_TYPE_UP);

        type_dependency_check_ast_node(comp, comp_thread, state, ptr->base);
        
        state.push_visit(a, AST_VISIT_STEP::SLICE_TYPE_DOWN);

        return;
      }
    case AST_TYPE::LAMBDA_TYPE: {
        ASTLambdaType* lt = downcast_ast<ASTLambdaType>(a);
        
        state.push_visit(a, AST_VISIT_STEP::LAMBDA_TYPE_UP);

        for (AST_LOCAL ty: lt->args) {
          type_dependency_check_ast_node(comp, comp_thread, state, ty);
        }

        type_dependency_check_ast_node(comp, comp_thread, state, lt->ret);
        
        state.push_visit(a, AST_VISIT_STEP::LAMBDA_TYPE_DOWN);

        return;
      }
    case AST_TYPE::TUPLE_TYPE: {
        ASTTupleType* tt = downcast_ast<ASTTupleType>(a);
        
        state.push_visit(a, AST_VISIT_STEP::TUPLE_TYPE_UP);
        

        for (AST_LOCAL ty: tt->types) {
          type_dependency_check_ast_node(comp, comp_thread, state, ty);
        }
        
        state.push_visit(a, AST_VISIT_STEP::TUPLE_TYPE_DOWN);
        return;
      }
    case AST_TYPE::CAST: {
        ASTCastExpr* cast = downcast_ast<ASTCastExpr>(a);

        state.push_visit(a, AST_VISIT_STEP::CAST_UP_TYPE);

        type_dependency_check_ast_node(comp, comp_thread, state, cast->type);
        
        state.push_visit(a, AST_VISIT_STEP::CAST_UP_EXPR);
        
        type_dependency_check_ast_node(comp, comp_thread, state, cast->expr);
        
        state.push_visit(a, AST_VISIT_STEP::CAST_DOWN);
        return;
      }
    case AST_TYPE::UNARY_OPERATOR: {
        ASTUnaryOperatorExpr* un_op = downcast_ast<ASTUnaryOperatorExpr>(a);
        
        state.push_visit(a, AST_VISIT_STEP::UNARY_OPERATOR_UP);

        type_dependency_check_ast_node(comp, comp_thread, state, un_op->expr);
        
        state.push_visit(a, AST_VISIT_STEP::UNARY_OPERATOR_DOWN);
        return;
      }
    case AST_TYPE::BINARY_OPERATOR: {
        ASTBinaryOperatorExpr* const bin_op = downcast_ast<ASTBinaryOperatorExpr>(a);

        state.push_visit(a, AST_VISIT_STEP::BINARY_OPERATOR_UP);
        
        type_dependency_check_ast_node(comp, comp_thread, state, bin_op->left);
        type_dependency_check_ast_node(comp, comp_thread, state, bin_op->right);
        
        state.push_visit(a, AST_VISIT_STEP::BINARY_OPERATOR_DOWN);
        return;
      }
    case AST_TYPE::IDENTIFIER_EXPR: {
        ASTIdentifier* ident = downcast_ast<ASTIdentifier>(a);
        
        state.push_visit(a, AST_VISIT_STEP::IDENTIFIER_EXPR_DOWN);

        const Axle::InternString* name = ident->name;

        if (Local* local = state.get_local(name)) {
          ident->id_type = ASTIdentifier::LOCAL;
          ident->local = local;
        }
        else {
          ident->id_type = ASTIdentifier::GLOBAL;//is definitely a global
          ident->global = test_global_type_dependency(comp, comp_thread, state, ident->node_span, ident->name);
        }

        return;
      }
    case AST_TYPE::FUNCTION_CALL: {
        ASTFunctionCallExpr* const call = downcast_ast<ASTFunctionCallExpr>(a);

        state.push_visit(a, AST_VISIT_STEP::FUNCTION_CALL_UP_FUNCTION);
        
        type_dependency_check_ast_node(comp, comp_thread, state, call->function);
        
        state.push_visit(a, AST_VISIT_STEP::FUNCTION_CALL_DOWN_FUNCTION);
        
        state.push_visit(a, AST_VISIT_STEP::FUNCTION_CALL_UP_ARGS);

        for (AST_LOCAL it: call->arguments) {
          type_dependency_check_ast_node(comp, comp_thread, state, it);
        }
        
        state.push_visit(a, AST_VISIT_STEP::FUNCTION_CALL_DOWN);

        return;
      }
    case AST_TYPE::TUPLE_LIT: {
        ASTTupleLitExpr* tup = downcast_ast<ASTTupleLitExpr>(a);

        const bool known_type = tup->prefix != NULL_AST_NODE;

        if (known_type) {
          state.push_visit(a, AST_VISIT_STEP::TUPLE_LIT_UP_PREFIX);

          type_dependency_check_ast_node(comp, comp_thread, state, tup->prefix);
        }

        state.push_visit(a, AST_VISIT_STEP::TUPLE_LIT_UP_ELEMENTS);

        for (AST_LOCAL it: tup->elements) {
          type_dependency_check_ast_node(comp, comp_thread, state, it);
        }
        
        state.push_visit(a, AST_VISIT_STEP::TUPLE_LIT_DOWN);
        return;
      }
    case AST_TYPE::ARRAY_EXPR: {
        ASTArrayExpr* arr = downcast_ast<ASTArrayExpr>(a);

        if (arr->elements.size > 0) {
          auto l = arr->elements.begin();
          const auto e = arr->elements.end();
          ASSERT(l < e);

          state.push_visit(a, AST_VISIT_STEP::ARRAY_EXPR_UP_FIRST);
          type_dependency_check_ast_node(comp, comp_thread, state, *l);
          
          ++l;
          
          if (l < e) {
            state.push_visit(a, AST_VISIT_STEP::ARRAY_EXPR_UP_REST);

            do {
              type_dependency_check_ast_node(comp, comp_thread, state, *l);

              ++l;
            }while (l < e);
          }
        }

        state.push_visit(a, AST_VISIT_STEP::ARRAY_EXPR_DOWN);

        return;
      }
    case AST_TYPE::INDEX_EXPR: {
        ASTIndexExpr* index = downcast_ast<ASTIndexExpr>(a);
        
        state.push_visit(a, AST_VISIT_STEP::INDEX_EXPR_UP);

        type_dependency_check_ast_node(comp, comp_thread, state, index->expr);
        for (AST_LOCAL it: index->arguments) {
          type_dependency_check_ast_node(comp, comp_thread, state, it);
        }
        
        state.push_visit(a, AST_VISIT_STEP::INDEX_EXPR_DOWN);
        return;
      }
    case AST_TYPE::MEMBER_ACCESS: {
        ASTMemberAccessExpr* member = downcast_ast<ASTMemberAccessExpr>(a);
        
        state.push_visit(a, AST_VISIT_STEP::MEMBER_ACCESS_UP);

        type_dependency_check_ast_node(comp, comp_thread, state, member->expr);
        
        state.push_visit(a, AST_VISIT_STEP::MEMBER_ACCESS_DOWN);
        return;
      }
    case AST_TYPE::LAMBDA_EXPR: {
        ASTLambdaExpr* le = downcast_ast<ASTLambdaExpr>(a);

        ASTLambda* lambda = downcast_ast<ASTLambda>(le->lambda);

        set_dependency(comp_thread,
            COMPILATION_UNIT_STAGE::DONE, lambda->sig_unit_id);
        
        state.push_visit(a, AST_VISIT_STEP::LAMBDA_EXPR_DOWN);

        return;
      }
    case AST_TYPE::STRUCT_EXPR: {
        ASTStructExpr* se = downcast_ast<ASTStructExpr>(a);

        ASTStructBody* struct_body = downcast_ast<ASTStructBody>(se->struct_body);

        if (!(struct_body->actual_type.is_valid())) {
          //Is not valid so need to wait for that
          set_dependency(comp_thread,
              COMPILATION_UNIT_STAGE::DONE, struct_body->unit_id);
        }
        
        state.push_visit(a, AST_VISIT_STEP::STRUCT_EXPR_DOWN);

        return;
      }
    case AST_TYPE::DECL: {
        ASTDecl* decl = downcast_ast<ASTDecl>(a);

        if (decl->type_ast != NULL_AST_NODE) {
          state.push_visit(a, AST_VISIT_STEP::DECL_UP_TYPE);

          type_dependency_check_ast_node(comp, comp_thread, state, decl->type_ast);
        }

        if (decl->expr != NULL_AST_NODE) {
          state.push_visit(a, AST_VISIT_STEP::DECL_UP_EXPR);
          
          type_dependency_check_ast_node(comp, comp_thread, state, decl->expr);
        }

        if (decl->decl_type == ASTDecl::TYPE::LOCAL) {
          if (decl->local_ptr == nullptr) {
            const Local* shadowing = state.get_local(decl->name);

            if (shadowing != nullptr) {
              comp_thread.report_error(ERROR_CODE::NAME_ERROR, decl->node_span,
                                       "Attempted to shadow the local variable '{}'",
                                       decl->name);
              return;
            }


            Local* loc = comp.new_local();
            decl->local_ptr = loc;

            loc->decl.name = decl->name;
            loc->decl.span = decl->node_span;
          }

          state.locals.insert(decl->local_ptr);
        }
        
        state.push_visit(a, AST_VISIT_STEP::DECL_DOWN);

        return;
      }
    case AST_TYPE::TYPED_NAME: {
        ASTTypedName* tn = downcast_ast<ASTTypedName>(a);

        if (tn->type != NULL_AST_NODE) {
          state.push_visit(a, AST_VISIT_STEP::TYPED_NAME_UP);

          type_dependency_check_ast_node(comp, comp_thread, state, tn->type);
        }

        if (tn->local_ptr == nullptr) {
          const Local* shadowing = state.get_local(tn->name);

          if (shadowing != nullptr) {
            comp_thread.report_error(ERROR_CODE::NAME_ERROR, tn->node_span,
                                     "Attempted to shadow the variable '{}'",
                                     tn->name);
            return;
          }

          Local* loc = comp.new_local();
          tn->local_ptr = loc;

          loc->decl.name = tn->name;
          loc->decl.span = tn->node_span;
        }

        state.locals.insert(tn->local_ptr);
        state.push_visit(a, AST_VISIT_STEP::TYPED_NAME_DOWN);

        return;
      }
    case AST_TYPE::ASSIGN: {
        state.push_visit(a, AST_VISIT_STEP::ASSIGN_UP_LEFT);
        
        ASTAssign* assign = downcast_ast<ASTAssign>(a);
        type_dependency_check_ast_node(comp, comp_thread, state, assign->assign_to);
        state.push_visit(a, AST_VISIT_STEP::ASSIGN_UP_RIGHT);
        
        type_dependency_check_ast_node(comp, comp_thread, state, assign->value);


        return;
      }
    case AST_TYPE::BLOCK: {
        ASTBlock* block = downcast_ast<ASTBlock>(a);
        
        state.push_visit(a, AST_VISIT_STEP::BLOCK_UP);

        const usize count = state.locals.size;

        for (AST_LOCAL it: block->block) {
          type_dependency_check_ast_node(comp, comp_thread, state, it);
        }

        state.locals.pop_n(state.locals.size - count);

        return;
      }
    case AST_TYPE::IF_ELSE: {
        ASTIfElse* if_else = downcast_ast<ASTIfElse>(a);
        
        state.push_visit(a, AST_VISIT_STEP::IF_ELSE_UP);

        type_dependency_check_ast_node(comp, comp_thread, state, if_else->condition);

        const usize count = state.locals.size;

        type_dependency_check_ast_node(comp, comp_thread, state, if_else->if_statement);

        state.locals.pop_n(state.locals.size - count);

        if (if_else->else_statement != NULL_AST_NODE) {
          type_dependency_check_ast_node(comp, comp_thread, state, if_else->else_statement);
          state.locals.pop_n(state.locals.size - count);
        }

        return;
      }
    case AST_TYPE::WHILE: {
        ASTWhile* while_s = downcast_ast<ASTWhile>(a);
        
        state.push_visit(a, AST_VISIT_STEP::WHILE_UP);

        type_dependency_check_ast_node(comp, comp_thread, state, while_s->condition);

        const usize count = state.locals.size;
        type_dependency_check_ast_node(comp, comp_thread, state, while_s->statement);
        state.locals.pop_n(state.locals.size - count);
        return;
      }
    case AST_TYPE::RETURN: {
        ASTReturn* ret = downcast_ast<ASTReturn>(a);
        
        state.push_visit(a, AST_VISIT_STEP::RETURN_UP);

        if (ret->expr != NULL_AST_NODE) {
          type_dependency_check_ast_node(comp, comp_thread, state, ret->expr);
        }
        return;
      }
    case AST_TYPE::FUNCTION_SIGNATURE: {
        ASTFuncSig* func_sig = downcast_ast<ASTFuncSig>(a);
        
        state.push_visit(a, AST_VISIT_STEP::FUNCTION_SIGNATURE_UP);

        for (AST_LOCAL it: func_sig->parameters) {
          type_dependency_check_ast_node(comp, comp_thread, state, it);
        }

        type_dependency_check_ast_node(comp, comp_thread, state, func_sig->return_type);
        
        state.push_visit(a, AST_VISIT_STEP::FUNCTION_SIGNATURE_DOWN);

        return;
      }
    case AST_TYPE::IMPORT: {
        ASTImport* imp = downcast_ast<ASTImport>(a);
        
        state.push_visit(a, AST_VISIT_STEP::IMPORT_UP);

        type_dependency_check_ast_node(comp, comp_thread, state, imp->expr_location);
        
        state.push_visit(a, AST_VISIT_STEP::IMPORT_DOWN);
        return;
      }
    case AST_TYPE::EXPORT_SINGLE: {
        ASTExportSingle* es = downcast_ast<ASTExportSingle>(a);
        
        state.push_visit(a, AST_VISIT_STEP::EXPORT_SINGLE_UP);

        type_dependency_check_ast_node(comp, comp_thread, state, es->value);
        
        state.push_visit(a, AST_VISIT_STEP::EXPORT_SINGLE_DOWN);
        return;
      }
    case AST_TYPE::EXPORT: {
        ASTExport* e = downcast_ast<ASTExport>(a);
        
        state.push_visit(a, AST_VISIT_STEP::EXPORT_UP);

        for (AST_LOCAL it: e->export_list) {
          type_dependency_check_ast_node(comp, comp_thread, state, it);
        }
        return;
      }
    case AST_TYPE::LINK: {
        ASTLink* imp = downcast_ast<ASTLink>(a);
        
        state.push_visit(a, AST_VISIT_STEP::LINK_UP);

        type_dependency_check_ast_node(comp, comp_thread, state, imp->import_type);
        
        state.push_visit(a, AST_VISIT_STEP::LINK_DOWN);

        return;
      }

    case AST_TYPE::LAMBDA: {
        ASTLambda* l = downcast_ast<ASTLambda>(a);
        
        state.push_visit(a, AST_VISIT_STEP::LAMBDA_UP);

        ASSERT(state.locals.size == 0);

        ASSERT(l->sig->node_type.is_valid());
        const bool old_visit = state.generate_visit;
        state.generate_visit = false;
        type_dependency_check_ast_node(comp, comp_thread, state, {l->sig});
        state.generate_visit = old_visit;
        type_dependency_check_ast_node(comp, comp_thread, state, l->body);

        return;
      }
    case AST_TYPE::STRUCT: {
        ASTStructBody* s = downcast_ast<ASTStructBody>(a);
        
        state.push_visit(a, AST_VISIT_STEP::STRUCT_UP);

        for (AST_LOCAL it: s->elements) {
          type_dependency_check_ast_node(comp, comp_thread, state, it);
        }
        
        state.push_visit(a, AST_VISIT_STEP::STRUCT_DOWN);

        return;
      }

    case AST_TYPE::ASCII_CHAR: {
        state.push_visit(a, AST_VISIT_STEP::ASCII_CHAR_DOWN);
        return;
      }

    case AST_TYPE::ASCII_STRING: {
        state.push_visit(a, AST_VISIT_STEP::ASCII_STRING_DOWN);
        return;
      }

    case AST_TYPE::NUMBER: {
        state.push_visit(a, AST_VISIT_STEP::NUMBER_DOWN);
        return;
      }
  }

  comp_thread.report_error(ERROR_CODE::INTERNAL_ERROR, a.ast->node_span,
                           "Not yet implemented dependency checking for this node. Node ID: {}", (usize)a.ast->ast_type);
}

void eval_dependency_check_ast_node(
  CompilerGlobals& comp,
  CompilerThread& comp_thread,
  EvalDependencyChecker& state,
  AST_LOCAL a
) noexcept {
  AXLE_TELEMETRY_FUNCTION();

  ASSERT(a != NULL_AST_NODE);

  switch (a.ast->ast_type) {
    case AST_TYPE::INVALID: INVALID_CODE_PATH("Invalid node type"); break;
    case AST_TYPE::NAMED_TYPE:
    case AST_TYPE::ARRAY_TYPE:
    case AST_TYPE::PTR_TYPE:
    case AST_TYPE::SLICE_TYPE:
    case AST_TYPE::LAMBDA_TYPE:
    case AST_TYPE::TUPLE_TYPE: {
      return;
    }
    case AST_TYPE::CAST: {
      const ASTCastExpr* cast = downcast_ast<ASTCastExpr>(a);

      ASSERT(cast->type.ast->node_type.is_valid());
      eval_dependency_check_ast_node(comp, comp_thread, state, cast->expr);
      return;
    }
    case AST_TYPE::UNARY_OPERATOR: {
      const ASTUnaryOperatorExpr* un_op = downcast_ast<ASTUnaryOperatorExpr>(a);

      eval_dependency_check_ast_node(comp, comp_thread, state, un_op->expr);
      return;
    }
  case AST_TYPE::BINARY_OPERATOR: {
      const ASTBinaryOperatorExpr* const bin_op = downcast_ast<ASTBinaryOperatorExpr>(a);
      
      eval_dependency_check_ast_node(comp, comp_thread, state, bin_op->left);
      eval_dependency_check_ast_node(comp, comp_thread, state, bin_op->right);
      return;
    }
    case AST_TYPE::IDENTIFIER_EXPR: {
      return;
    }
    case AST_TYPE::FUNCTION_CALL: {
        ASTFunctionCallExpr* const call = downcast_ast<ASTFunctionCallExpr>(a);
        
        eval_dependency_check_ast_node(comp, comp_thread, state, call->function);

        ASSERT(call->label != IR::NULL_GLOBAL_LABEL);

        GlobalLabelInfo label_info = comp.get_label_info(call->label);

        ASSERT(VC::is_comptime(label_info.get_call_category()));
        
        UnitID depend_id = label_info.dependency;
        ASSERT(depend_id != NULL_ID);

        set_dependency(comp_thread, COMPILATION_UNIT_STAGE::EMIT, depend_id);
        
        for (AST_LOCAL it: call->arguments) {
          eval_dependency_check_ast_node(comp, comp_thread, state, it);
        }
        
        return;
      }
    case AST_TYPE::TUPLE_LIT: {
        ASTTupleLitExpr* tup = downcast_ast<ASTTupleLitExpr>(a);

        const bool known_type = tup->prefix != NULL_AST_NODE;

        if (known_type) {
          eval_dependency_check_ast_node(comp, comp_thread, state, tup->prefix);
        }

        for (AST_LOCAL it: tup->elements) {
          eval_dependency_check_ast_node(comp, comp_thread, state, it);
        }
        
        return;
      }
    case AST_TYPE::ARRAY_EXPR: {
        ASTArrayExpr* arr = downcast_ast<ASTArrayExpr>(a);

        for (AST_LOCAL it: arr->elements) {
          eval_dependency_check_ast_node(comp, comp_thread, state, it);
        }

        return;
      }
    case AST_TYPE::INDEX_EXPR: {
        ASTIndexExpr* index = downcast_ast<ASTIndexExpr>(a);
        
        eval_dependency_check_ast_node(comp, comp_thread, state, index->expr);
        for (AST_LOCAL it: index->arguments) {
          eval_dependency_check_ast_node(comp, comp_thread, state, it);
        }
        
        return;
      }
    case AST_TYPE::MEMBER_ACCESS: {
        ASTMemberAccessExpr* member = downcast_ast<ASTMemberAccessExpr>(a);
        
        eval_dependency_check_ast_node(comp, comp_thread, state, member->expr);
        return;
      }
    case AST_TYPE::LAMBDA_EXPR: {
        ASTLambdaExpr* le = downcast_ast<ASTLambdaExpr>(a);

        ASTLambda* lambda = downcast_ast<ASTLambda>(le->lambda);
        ASSERT(lambda->label != IR::NULL_GLOBAL_LABEL);
        return;
      }
    case AST_TYPE::STRUCT_EXPR: {
        ASTStructExpr* se = downcast_ast<ASTStructExpr>(a);
        ASTStructBody* struct_body = downcast_ast<ASTStructBody>(se->struct_body);

        ASSERT(struct_body->actual_type.is_valid());
        return;
      }
    case AST_TYPE::DECL: {
        ASTDecl* decl = downcast_ast<ASTDecl>(a);

        if (decl->type_ast != NULL_AST_NODE) {
          eval_dependency_check_ast_node(comp, comp_thread, state, decl->type_ast);
        }

        if (decl->expr != NULL_AST_NODE) {
          eval_dependency_check_ast_node(comp, comp_thread, state, decl->expr);
        }
        return;
      }
    case AST_TYPE::TYPED_NAME: {
        ASTTypedName* tn = downcast_ast<ASTTypedName>(a);

        if (tn->type != NULL_AST_NODE) {
          eval_dependency_check_ast_node(comp, comp_thread, state, tn->type);
        }
        return;
      }
    case AST_TYPE::ASSIGN: {
        ASTAssign* assign = downcast_ast<ASTAssign>(a);
        eval_dependency_check_ast_node(comp, comp_thread, state, assign->assign_to);
        eval_dependency_check_ast_node(comp, comp_thread, state, assign->value);
        return;
      }
    case AST_TYPE::BLOCK: {
        ASTBlock* block = downcast_ast<ASTBlock>(a);
        
        for (AST_LOCAL it: block->block) {
          eval_dependency_check_ast_node(comp, comp_thread, state, it);
        }

        return;
      }
    case AST_TYPE::IF_ELSE: {
        ASTIfElse* if_else = downcast_ast<ASTIfElse>(a);

        eval_dependency_check_ast_node(comp, comp_thread, state, if_else->condition);

        eval_dependency_check_ast_node(comp, comp_thread, state, if_else->if_statement);

        if (if_else->else_statement != NULL_AST_NODE) {
          eval_dependency_check_ast_node(comp, comp_thread, state, if_else->else_statement);
        }

        return;
      }
    case AST_TYPE::WHILE: {
        ASTWhile* while_s = downcast_ast<ASTWhile>(a);
        
        eval_dependency_check_ast_node(comp, comp_thread, state, while_s->condition);

        eval_dependency_check_ast_node(comp, comp_thread, state, while_s->statement);
        return;
      }
    case AST_TYPE::RETURN: {
        ASTReturn* ret = downcast_ast<ASTReturn>(a);
        
        if (ret->expr != NULL_AST_NODE) {
          eval_dependency_check_ast_node(comp, comp_thread, state, ret->expr);
        }
        return;
      }
    case AST_TYPE::FUNCTION_SIGNATURE: {
        ASTFuncSig* func_sig = downcast_ast<ASTFuncSig>(a);
        
        for (AST_LOCAL it: func_sig->parameters) {
          eval_dependency_check_ast_node(comp, comp_thread, state, it);
        }

        eval_dependency_check_ast_node(comp, comp_thread, state, func_sig->return_type);
        
        return;
      }
    case AST_TYPE::IMPORT: {
        ASTImport* imp = downcast_ast<ASTImport>(a);
        
        eval_dependency_check_ast_node(comp, comp_thread, state, imp->expr_location);
        return;
      }
    case AST_TYPE::EXPORT_SINGLE: {
        ASTExportSingle* es = downcast_ast<ASTExportSingle>(a);
        
        eval_dependency_check_ast_node(comp, comp_thread, state, es->value);
        return;
      }
    case AST_TYPE::EXPORT: {
        ASTExport* e = downcast_ast<ASTExport>(a);
        
        for (AST_LOCAL it: e->export_list) {
          eval_dependency_check_ast_node(comp, comp_thread, state, it);
        }
        return;
      }
    case AST_TYPE::LINK: {
        ASTLink* imp = downcast_ast<ASTLink>(a);
        
        eval_dependency_check_ast_node(comp, comp_thread, state, imp->import_type);
        
        return;
      }

    case AST_TYPE::LAMBDA: {
        ASTLambda* l = downcast_ast<ASTLambda>(a);
        
        ASSERT(l->sig->node_type.is_valid());
        eval_dependency_check_ast_node(comp, comp_thread, state, {l->sig});
        eval_dependency_check_ast_node(comp, comp_thread, state, l->body);

        return;
      }
    case AST_TYPE::STRUCT: {
        ASTStructBody* s = downcast_ast<ASTStructBody>(a);
        
        for (AST_LOCAL it: s->elements) {
          eval_dependency_check_ast_node(comp, comp_thread, state, it);
        }

        return;
      }

    case AST_TYPE::ASCII_CHAR: {
        return;
      }

    case AST_TYPE::ASCII_STRING: {
        return;
      }

    case AST_TYPE::NUMBER: {
        return;
      }
  }

  comp_thread.report_error(ERROR_CODE::INTERNAL_ERROR, a.ast->node_span,
                           "Not yet implemented dependency checking for this node. Node ID: {}", (usize)a.ast->ast_type);
}
}

Axle::OwnedArr<AstVisit> DC::type_dependency_check_ast(
  CompilerGlobals* const comp,
  CompilerThread* const comp_thread,
  Namespace* const available_names,
  AST_LOCAL a
) noexcept {
  DependencyChecker checker = {};
  checker.generate_visit = true;
  checker.available_names = available_names;

  type_dependency_check_ast_node(*comp, *comp_thread, checker, a);

#if 0
  {
    Axle::IO_Single::ScopeLock scope;

    Axle::IO_Single::print("--------------\n");

    for (const auto& pair: checker.visit_array) {
      Axle::IO_Single::format("{}\n", ast_visit_step_string(pair.step));
    }
    
    Axle::IO_Single::print("--------------\n");
  }
#endif

  return Axle::bake_arr(std::move(checker.visit_array));
}

void DC::eval_dependency_check_ast(
  CompilerGlobals* const comp,
  CompilerThread* const comp_thread,
  AST_LOCAL a
) noexcept {
  EvalDependencyChecker checker = {};

  eval_dependency_check_ast_node(*comp, *comp_thread, checker, a);
}
