#include <AxleUtil/format.h>

#include "compiler.h"
#include "dependency_check.h"

namespace Format = Axle::Format;

namespace {
struct DependencyChecker {
  Namespace* available_names;
  u32 num_locals;
  Axle::Array<Local*> locals;

  Local* get_local(const Axle::InternString* name);
};

void set_dependency(CompilerThread& comp_thread, UnitID id) {
  ASSERT(id != NULL_ID);

  comp_thread.new_depends.insert(id);
}

template<typename ... T>
void set_unfound_name(CompilerThread& comp_thread,
                      UnknownName&& name,
                      ERROR_CODE code, const Span& span,
                      const Format::FormatString<T...>& f_message, const T& ... ts) {
  TELEMETRY_FUNCTION();

  ASSERT(name.ident != nullptr);

  comp_thread.local_unfound_names.names.insert_uninit(1);
  UnfoundNameHolder* dep = comp_thread.local_unfound_names.names.back();

  dep->name = std::move(name);
  dep->dependency = nullptr;
  dep->as_error.type = code;
  dep->as_error.span = span;
  dep->as_error.message = format(f_message, ts...);
}

Local* DependencyChecker::get_local(const Axle::InternString* name) {
  TELEMETRY_FUNCTION();
  
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

Global* test_global_dependency(CompilerGlobals& comp, CompilerThread& comp_thread, DependencyChecker& state, const Span& span, const Axle::InternString* ident) {
  TELEMETRY_FUNCTION();
  
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
    ASSERT(name->global->decl.type.is_valid());
    return g;
  }
}

void dependency_check_ast_node(CompilerGlobals& comp,
                               CompilerThread& comp_thread,
                               DependencyChecker& state,
                               AST_LOCAL a) {
  TELEMETRY_FUNCTION();

  ASSERT(a != nullptr);

  switch (a->ast_type) {
    case AST_TYPE::INVALID: INVALID_CODE_PATH("Invalid node type"); break;
    case AST_TYPE::NAMED_TYPE: {
        ASTNamedType* nt = downcast_ast<ASTNamedType>(a);

        nt->global = test_global_dependency(comp, comp_thread, state, nt->node_span, nt->name);
        return;
      }
    case AST_TYPE::ARRAY_TYPE: {
        ASTArrayType* at = downcast_ast<ASTArrayType>(a);

        dependency_check_ast_node(comp, comp_thread, state, at->base);
        dependency_check_ast_node(comp, comp_thread, state, at->expr);
        return;
      }
    case AST_TYPE::PTR_TYPE: {
        ASTPtrType* ptr = downcast_ast<ASTPtrType>(a);

        dependency_check_ast_node(comp, comp_thread, state, ptr->base);

        return;
      }
    case AST_TYPE::SLICE_TYPE: {
        ASTSliceType* ptr = downcast_ast<ASTSliceType>(a);

        dependency_check_ast_node(comp, comp_thread, state, ptr->base);

        return;
      }
    case AST_TYPE::LAMBDA_TYPE: {
        ASTLambdaType* lt = downcast_ast<ASTLambdaType>(a);

        FOR_AST(lt->args, ty) {
          dependency_check_ast_node(comp, comp_thread, state, ty);
        }

        dependency_check_ast_node(comp, comp_thread, state, lt->ret);

        return;
      }
    case AST_TYPE::TUPLE_TYPE: {
        ASTTupleType* tt = downcast_ast<ASTTupleType>(a);

        FOR_AST(tt->types, ty) {
          dependency_check_ast_node(comp, comp_thread, state, ty);
        }

        return;
      }
    case AST_TYPE::CAST: {
        ASTCastExpr* cast = downcast_ast<ASTCastExpr>(a);

        dependency_check_ast_node(comp, comp_thread, state, cast->type);
        dependency_check_ast_node(comp, comp_thread, state, cast->expr);
        return;
      }
    case AST_TYPE::UNARY_OPERATOR: {
        ASTUnaryOperatorExpr* un_op = downcast_ast<ASTUnaryOperatorExpr>(a);

        dependency_check_ast_node(comp, comp_thread, state, un_op->expr);
        return;
      }
    case AST_TYPE::BINARY_OPERATOR: {
        ASTBinaryOperatorExpr* const bin_op = downcast_ast<ASTBinaryOperatorExpr>(a);

        dependency_check_ast_node(comp, comp_thread, state, bin_op->left);
        dependency_check_ast_node(comp, comp_thread, state, bin_op->right);
        return;
      }
    case AST_TYPE::IDENTIFIER_EXPR: {
        ASTIdentifier* ident = downcast_ast<ASTIdentifier>(a);

        const Axle::InternString* name = ident->name;

        {
          Local* local = state.get_local(name);

          if (local != nullptr) {
            ident->id_type = ASTIdentifier::LOCAL;
            ident->local = local;

            return;
          }
        }

        ident->id_type = ASTIdentifier::GLOBAL;//is definitely a global
        ident->global = test_global_dependency(comp, comp_thread, state, ident->node_span, ident->name);
        return;
      }
    case AST_TYPE::FUNCTION_CALL: {
        ASTFunctionCallExpr* const call = downcast_ast<ASTFunctionCallExpr>(a);

        dependency_check_ast_node(comp, comp_thread, state, call->function);

        FOR_AST(call->arguments, it) {
          dependency_check_ast_node(comp, comp_thread, state, it);
        }

        return;
      }
    case AST_TYPE::TUPLE_LIT: {
        ASTTupleLitExpr* tup = downcast_ast<ASTTupleLitExpr>(a);

        if (tup->prefix != nullptr) {
          dependency_check_ast_node(comp, comp_thread, state, tup->prefix);
        }

        FOR_AST(tup->elements, it) {
          dependency_check_ast_node(comp, comp_thread, state, it);
        }
        return;
      }
    case AST_TYPE::ARRAY_EXPR: {
        ASTArrayExpr* arr = downcast_ast<ASTArrayExpr>(a);

        FOR_AST(arr->elements, it) {
          dependency_check_ast_node(comp, comp_thread, state, it);
        }
        return;
      }
    case AST_TYPE::INDEX_EXPR: {
        ASTIndexExpr* index = downcast_ast<ASTIndexExpr>(a);

        dependency_check_ast_node(comp, comp_thread, state, index->expr);
        FOR_AST(index->arguments, it) {
          dependency_check_ast_node(comp, comp_thread, state, it);
        }
        return;
      }
    case AST_TYPE::MEMBER_ACCESS: {
        ASTMemberAccessExpr* member = downcast_ast<ASTMemberAccessExpr>(a);

        dependency_check_ast_node(comp, comp_thread, state, member->expr);
        return;
      }
    case AST_TYPE::LAMBDA_EXPR: {
        ASTLambdaExpr* le = downcast_ast<ASTLambdaExpr>(a);

        ASTLambda* lambda = downcast_ast<ASTLambda>(le->lambda);
        ASTFuncSig* sig = downcast_ast<ASTFuncSig>(lambda->sig);

        if (sig->sig->sig_struct == nullptr) {
          set_dependency(comp_thread, lambda->function->sig_unit_id);
        }

        return;
      }
    case AST_TYPE::STRUCT_EXPR: {
        ASTStructExpr* se = downcast_ast<ASTStructExpr>(a);

        ASTStructBody* struct_body = downcast_ast<ASTStructBody>(se->struct_body);

        if (!(struct_body->actual_type.is_valid())) {
          //Is not valid so need to wait for that
          set_dependency(comp_thread, struct_body->unit_id);
        }

        return;
      }
    case AST_TYPE::DECL: {
        ASTDecl* decl = downcast_ast<ASTDecl>(a);

        if (decl->type_ast != 0) {
          dependency_check_ast_node(comp, comp_thread, state, decl->type_ast);
        }

        if (decl->expr != 0) {
          dependency_check_ast_node(comp, comp_thread, state, decl->expr);
        }

        if (decl->decl_type == ASTDecl::TYPE::LOCAL) {
          if (decl->local_ptr == nullptr) {
            const Local* shadowing = state.get_local(decl->name);

            if (shadowing != nullptr) {
              comp_thread.report_error(ERROR_CODE::NAME_ERROR, a->node_span,
                                       "Attempted to shadow the local variable '{}'",
                                       decl->name);
              return;
            }


            state.num_locals += 1;
            Local* loc = comp.new_local();
            decl->local_ptr = loc;

            loc->decl.name = decl->name;
            loc->decl.span = decl->node_span;
          }

          state.locals.insert(decl->local_ptr);
        }

        return;
      }
    case AST_TYPE::TYPED_NAME: {
        ASTTypedName* tn = downcast_ast<ASTTypedName>(a);

        if (tn->type != 0) {
          dependency_check_ast_node(comp, comp_thread, state, tn->type);
        }

        if (tn->local_ptr == nullptr) {
          const Local* shadowing = state.get_local(tn->name);

          if (shadowing != nullptr) {
            comp_thread.report_error(ERROR_CODE::NAME_ERROR, a->node_span,
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

        return;
      }
    case AST_TYPE::ASSIGN: {
        ASTAssign* assign = downcast_ast<ASTAssign>(a);
        dependency_check_ast_node(comp, comp_thread, state, assign->assign_to);
        dependency_check_ast_node(comp, comp_thread, state, assign->value);

        return;
      }
    case AST_TYPE::BLOCK: {
        ASTBlock* block = downcast_ast<ASTBlock>(a);

        const usize count = state.locals.size;

        FOR_AST(block->block, it) {
          dependency_check_ast_node(comp, comp_thread, state, it);
        }

        state.locals.pop_n(state.locals.size - count);

        return;
      }
    case AST_TYPE::IF_ELSE: {
        ASTIfElse* if_else = downcast_ast<ASTIfElse>(a);

        dependency_check_ast_node(comp, comp_thread, state, if_else->condition);

        const usize count = state.locals.size;

        dependency_check_ast_node(comp, comp_thread, state, if_else->if_statement);

        state.locals.pop_n(state.locals.size - count);

        if (if_else->else_statement != 0) {
          dependency_check_ast_node(comp, comp_thread, state, if_else->else_statement);
          state.locals.pop_n(state.locals.size - count);
        }

        return;
      }
    case AST_TYPE::WHILE: {
        ASTWhile* while_s = downcast_ast<ASTWhile>(a);

        dependency_check_ast_node(comp, comp_thread, state, while_s->condition);

        const usize count = state.locals.size;
        dependency_check_ast_node(comp, comp_thread, state, while_s->statement);
        state.locals.pop_n(state.locals.size - count);
        return;
      }
    case AST_TYPE::RETURN: {
        ASTReturn* ret = downcast_ast<ASTReturn>(a);

        if (ret->expr != nullptr) {
          dependency_check_ast_node(comp, comp_thread, state, ret->expr);
        }
        return;
      }
    case AST_TYPE::FUNCTION_SIGNATURE: {
        ASTFuncSig* func_sig = downcast_ast<ASTFuncSig>(a);

        FOR_AST(func_sig->parameters, it) {
          dependency_check_ast_node(comp, comp_thread, state, it);
        }

        dependency_check_ast_node(comp, comp_thread, state, func_sig->return_type);

        return;
      }
    case AST_TYPE::IMPORT: {
        ASTImport* imp = downcast_ast<ASTImport>(a);

        dependency_check_ast_node(comp, comp_thread, state, imp->expr_location);
        return;
      }
    case AST_TYPE::EXPORT_SINGLE: {
        ASTExportSingle* es = downcast_ast<ASTExportSingle>(a);

        dependency_check_ast_node(comp, comp_thread, state, es->value);
        return;
      }
    case AST_TYPE::EXPORT: {
        ASTExport* e = downcast_ast<ASTExport>(a);

        FOR_AST(e->export_list, it) {
          dependency_check_ast_node(comp, comp_thread, state, it);
        }
        return;
      }
    case AST_TYPE::LINK: {
        ASTLink* imp = downcast_ast<ASTLink>(a);

        dependency_check_ast_node(comp, comp_thread, state, imp->import_type);

        return;
      }

    case AST_TYPE::LAMBDA: {
        ASTLambda* l = downcast_ast<ASTLambda>(a);

        ASSERT(state.locals.size == 0);
        ASSERT(state.num_locals == 0);

        dependency_check_ast_node(comp, comp_thread, state, l->sig);
        dependency_check_ast_node(comp, comp_thread, state, l->body);

        return;
      }
    case AST_TYPE::STRUCT: {
        ASTStructBody* s = downcast_ast<ASTStructBody>(a);

        FOR_AST(s->elements, it) {
          dependency_check_ast_node(comp, comp_thread, state, it);
        }

        return;
      }

    case AST_TYPE::ASCII_CHAR:
    case AST_TYPE::ASCII_STRING:
    case AST_TYPE::NUMBER: {
        //No dependencies :)
        return;
      }
  }

  comp_thread.report_error(ERROR_CODE::INTERNAL_ERROR, a->node_span,
                           "Not yet implemented dependency checking for this node. Node ID: {}", (usize)a->ast_type);
}
}

void DC::dependency_check_ast(CompilerGlobals* const comp,
                              CompilerThread* const comp_thread,
                              Namespace* const available_names,
                              AST_LOCAL a) {
  DependencyChecker checker = {};
  checker.available_names = available_names;

  dependency_check_ast_node(*comp, *comp_thread, checker, a);
}
