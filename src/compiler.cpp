#include "compiler.h"
#include "type.h"
#include "ast.h"
#include "parser.h"
#include "format.h"
#include "operators.h"
#include "files.h"
#include "backends.h"
#include "ir.h"

#include "trace.h"

CompilationUnit* CompilationUnitStore::allocate_unit() {
  CompilationUnit* unit = compilation_units.allocate();

  unit->id = ++comp_unit_counter;

  active_units.insert(unit);

  return unit;
}

void CompilationUnitStore::free_unit(CompilationUnit* unit) {
  active_units.remove_if([id = unit->id](CompilationUnit* u) { return u->id == id; });

  compilation_units.free(unit);
}

CompilationUnit* CompilationUnitStore::get_unit_if_exists(u64 id) const {
  FOR(active_units, it) {
    CompilationUnit* u = *it;
    if (u->id == id) {
      return u;
    }
  }

  return nullptr;
}

IR::GlobalLabel CompilerGlobals::next_function_label(const SignatureStructure* s) {
  label_mutex.acquire();
  IR::GlobalLabel label = { label_signature_table.size + 1 };
  label_signature_table.insert(s);
  label_mutex.release();

  return label;
}

const SignatureStructure* CompilerGlobals::get_label_signature(IR::GlobalLabel label) {
  ASSERT(label.label != 0);
  label_mutex.acquire();
  const SignatureStructure* s = label_signature_table.data[label.label - 1];
  label_mutex.release();

  return s;
}

IR::Builder* CompilerGlobals::new_ir(IR::GlobalLabel label) {
  ir_mutex.acquire();
  IR::Builder* builder = ir_builders_single_threaded.insert();
  ir_mutex.release();

  builder->global_label = label;
  builder->signature = get_label_signature(label);

  return builder;
}

IR::Function* CompilerGlobals::new_function() {
  functions_mutex.acquire();
  IR::Function* func = functions_single_threaded.insert();
  functions_mutex.release();
  return func;
}

Local* CompilerGlobals::new_local() {
  locals_mutex.acquire();
  Local* loc = locals_single_threaded.insert();
  locals_mutex.release();
  return loc;
}

Global* CompilerGlobals::new_global() {
  globals_mutex.acquire();
  Global* glob = globals_single_threaded.insert();
  globals_mutex.release();
  return glob;
}

Namespace* CompilerGlobals::new_namespace() {
  namespaces_mutex.acquire();
  Namespace* names = namespaces_single_threaded.insert();
  namespaces_mutex.release();

  return names;
}

CompilationUnit* new_compilation_unit(Compilation* const comp,
                                      COMPILATION_EMIT_TYPE emit_type,
                                      AST_LOCAL ast,
                                      Namespace* names,
                                      void* extra,
                                      bool print) {
  CompilationUnit* unit = comp->store.allocate_unit();
  unit->waiting_on_count = 0;
  unit->dependency_list = nullptr;
  unit->insert_to = nullptr;

  unit->emit = emit_type;
  unit->ast = ast;
  unit->available_names = names;
  unit->extra = extra;

  comp->in_flight_units += 1;

  if (print) {
    format_print("Started Comp Unit {}       | Active = {}, In flight = {}\n",
                 unit->id, comp->store.active_units.size, comp->in_flight_units);
  }


  return unit;
}

void set_dependency(CompilerThread* const comp_thread, Context* const context, UnitID id) {
  ASSERT(id != NULL_ID);
  ASSERT(context->dependency_load_pipe != nullptr);
  ASSERT(context->current_unit != nullptr);

  comp_thread->new_depends.insert(id);
}

void set_external_dependency(CompilationUnit* now_waiting) {
  ASSERT(now_waiting->insert_to != nullptr);
  now_waiting->waiting_on_count += 1;
}

template<typename ... T>
void set_unfound_name(CompilerThread* const comp_thread,
                      Context* context, UnknownName&& name,
                      ERROR_CODE code, const Span& span,
                      const char* f_message, T&& ... ts) {

  ASSERT(name.ident != nullptr);

  comp_thread->local_unfound_names.names.insert_uninit(1);
  UnfoundNameHolder* dep = comp_thread->local_unfound_names.names.back();

  CompilationUnit* unit = context->current_unit;

  if (comp_thread->print_options.comp_units) {
    format_print("Comp unit {} waiting on name \"{}\"\n",
                 unit->id, name.ident);
  }

  dep->name = std::move(name);
  dep->dependency = unit;
  dep->as_error.type = code;
  dep->as_error.span = span;
  dep->as_error.message = format(f_message, std::forward<T>(ts)...);

  set_external_dependency(unit);
}

Local* DependencyChecker::get_local(const InternString* name) {
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

const ArrayStructure* find_or_make_array_structure(Structures* const structures,
                                                   StringInterner* strings,
                                                   const Type& base, size_t length) {
  TRACING_FUNCTION();

  {
    auto i = structures->structures.begin();
    const auto end = structures->structures.end();

    for (; i < end; i++) {
      const Structure* s = *i;
      if (s->type == STRUCTURE_TYPE::FIXED_ARRAY) {
        //Is array
        const ArrayStructure* as = static_cast<const ArrayStructure*>(s);
        if (as->base == base
            && as->length == length) {
          //Is same
          return as;
        }
      }
    }
  }

  //Doesnt exist - need to make new type
  return STRUCTS::new_array_structure(structures, strings, base, length);
}

static u32 new_dynamic_init_object(CompilerGlobals* const comp, const InternString* name, u32 size, u32 alignment,
                                   IR::Builder* ir_builder) {
  DynamicInitData holder = {};
  holder.name = name;
  holder.size = size;
  holder.alignment = alignment;
  holder.init_expr_label = ir_builder->global_label;

  comp->dynamic_inits.insert(holder);
  return (u32)comp->dynamic_inits.size;
}

const PointerStructure* find_or_make_pointer_structure(Structures* const structures, StringInterner* strings,
                                                       usize ptr_size, const Type& base) {
  TRACING_FUNCTION();

  {
    auto i = structures->structures.begin();
    const auto end = structures->structures.end();

    for (; i < end; i++) {
      const Structure* s = *i;
      if (s->type == STRUCTURE_TYPE::POINTER) {
        //Is pointer
        const PointerStructure* ps = static_cast<const PointerStructure*>(s);
        if (ps->base == base) {
          //Is same
          return ps;
        }
      }
    }
  }

  //Doesnt exist - need to make new type
  return STRUCTS::new_pointer_structure(structures, strings, ptr_size, base);
}

const TupleStructure* find_or_make_tuple_structure(Structures* const structures, StringInterner* strings, Array<Type>&& els) {
  TRACING_FUNCTION();

  {
    auto i = structures->structures.begin();
    const auto end = structures->structures.end();

    for (; i < end; i++) {
      const Structure* s = *i;
      if (s->type == STRUCTURE_TYPE::TUPLE) {
        const TupleStructure* tls = static_cast<const TupleStructure*>(s);

        //Not same size
        if (els.size != tls->elements.size) { continue; }

        //empty
        if (els.size == 0) { return tls; }

        auto el_i = els.begin();
        auto tl_i = tls->elements.begin();

        const auto el_end = els.end();

        for (; el_i < el_end; tl_i++, el_i++) {
          if (*el_i != tl_i->type) {
            goto NOT_SAME;
          }
        }

        return tls;
      }

    NOT_SAME:
      continue;
    }
  }


  //Doesnt exist - need to make new type
  return STRUCTS::new_tuple_structure(structures, strings, std::move(els));
}

const SignatureStructure* find_or_make_lamdba_structure(Structures* const structures,
                                                        StringInterner* strings,
                                                        usize ptr_size,
                                                        const CallingConvention* conv,
                                                        Array<Type>&& params,
                                                        Type ret_type) {
  TRACING_FUNCTION();

  {
    auto i = structures->structures.begin();
    auto end = structures->structures.end();

    for (; i < end; i++) {
      const Structure* s = *i;
      if (s->type != STRUCTURE_TYPE::LAMBDA) { continue; }

      const SignatureStructure* sig_struct = (const SignatureStructure*)s;
      if (sig_struct->calling_convention != conv) { continue; }
      if (sig_struct->return_type != ret_type) { continue; }
      if (sig_struct->parameter_types.size != params.size) { continue; }

      {
        auto p_i = sig_struct->parameter_types.begin();
        auto p_end = sig_struct->parameter_types.end();
        auto pin_i = params.begin();

        for (; p_i < p_end; p_i++, pin_i++) {
          if (*p_i != *pin_i) {
            goto NOT_SAME;
          }
        }
      }

      //Is same!
      return sig_struct;

    NOT_SAME:
      continue;
    }
  }

  return STRUCTS::new_lambda_structure(structures, strings, ptr_size, conv, std::move(params), ret_type);
}

inline constexpr bool is_global_depend(const GlobalName* g) {
  return (g->global == nullptr || (g->global->constant_value == nullptr && g->global->dynamic_init_index == 0));
}

void test_global_dependency(CompilerGlobals* const comp, CompilerThread* const comp_thread, DependencyChecker* const state, const Span& span, const InternString* ident) {
  auto names = comp->services.names.get();
  const GlobalName* name = names->find_global_name(state->current_unit->available_names, ident);

  if (name == nullptr) {
    names.release();
    UnknownName unknown = {};
    unknown.ident = ident;
    unknown.ns = state->current_unit->available_names;

    state->current_unit->insert_to = state->dependency_load_pipe;

    set_unfound_name(comp_thread, state, std::move(unknown),
                     ERROR_CODE::NAME_ERROR, span,
                     "Could not find name '{}'", ident);
  }
  else if (is_global_depend(name)) {
    UnitID unit_id = name->unit_id;
    names.release();
    set_dependency(comp_thread, state, unit_id);
  }
}

void dependency_check_ast_node(CompilerGlobals* const comp,
                               CompilerThread* const comp_thread,
                               DependencyChecker* const state,
                               AST_LOCAL a) {
  TRACING_FUNCTION();

  ASSERT(a != nullptr);

  switch (a->ast_type) {
    case AST_TYPE::INVALID: INVALID_CODE_PATH("Invalid node type"); break;
    case AST_TYPE::NAMED_TYPE: {
        ASTNamedType* nt = (ASTNamedType*)a;

        nt->meta_flags |= META_FLAG::CONST;
        nt->meta_flags |= META_FLAG::COMPTIME;


        test_global_dependency(comp, comp_thread, state, nt->node_span, nt->name);
        return;
      }
    case AST_TYPE::ARRAY_TYPE: {
        ASTArrayType* at = (ASTArrayType*)a;
        at->meta_flags |= META_FLAG::CONST;
        at->meta_flags |= META_FLAG::COMPTIME;

        dependency_check_ast_node(comp, comp_thread, state, at->base);
        dependency_check_ast_node(comp, comp_thread, state, at->expr);

        comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, at->node_span, "Currently don't support compile time values");

#if 0
        UnitID id = compile_and_execute(comp, state->current_unit->available_names, at->expr,
                                        comp_thread->builtin_types->t_u64, &at->array_length);

        set_dependency(comp_thread, state, id);
#endif

        return;
      }
    case AST_TYPE::PTR_TYPE: {
        ASTPtrType* ptr = (ASTPtrType*)a;
        ptr->meta_flags |= META_FLAG::CONST;
        ptr->meta_flags |= META_FLAG::COMPTIME;

        dependency_check_ast_node(comp, comp_thread, state, ptr->base);

        return;
      }
    case AST_TYPE::LAMBDA_TYPE: {
        ASTLambdaType* lt = (ASTLambdaType*)a;
        lt->meta_flags |= META_FLAG::CONST;
        lt->meta_flags |= META_FLAG::COMPTIME;

        FOR_AST(lt->args, ty) {
          dependency_check_ast_node(comp, comp_thread, state, ty);
        }

        dependency_check_ast_node(comp, comp_thread, state, lt->ret);

        return;
      }
    case AST_TYPE::TUPLE_TYPE: {
        ASTTupleType* tt = (ASTTupleType*)a;
        tt->meta_flags |= META_FLAG::CONST;
        tt->meta_flags |= META_FLAG::COMPTIME;

        FOR_AST(tt->types, ty) {
          dependency_check_ast_node(comp, comp_thread, state, ty);
        }

        return;
      }
    case AST_TYPE::CAST: {
        ASTCastExpr* cast = (ASTCastExpr*)a;
        cast->meta_flags |= META_FLAG::CONST;
        cast->meta_flags |= META_FLAG::COMPTIME;

        dependency_check_ast_node(comp, comp_thread, state, cast->type);
        dependency_check_ast_node(comp, comp_thread, state, cast->expr);
        return;
      }
    case AST_TYPE::UNARY_OPERATOR: {
        ASTUnaryOperatorExpr* un_op = (ASTUnaryOperatorExpr*)a;
        un_op->meta_flags |= META_FLAG::CONST;
        un_op->meta_flags |= META_FLAG::COMPTIME;

        dependency_check_ast_node(comp, comp_thread, state, un_op->expr);
        return;
      }
    case AST_TYPE::BINARY_OPERATOR: {
        ASTBinaryOperatorExpr* const bin_op = (ASTBinaryOperatorExpr*)a;
        bin_op->meta_flags |= META_FLAG::CONST;
        bin_op->meta_flags |= META_FLAG::COMPTIME;

        dependency_check_ast_node(comp, comp_thread, state, bin_op->left);
        dependency_check_ast_node(comp, comp_thread, state, bin_op->right);
        return;
      }
    case AST_TYPE::IDENTIFIER_EXPR: {
        ASTIdentifier* ident = (ASTIdentifier*)a;
        ident->meta_flags |= META_FLAG::CONST;
        //ident->meta_flags |= META_FLAG::COMPTIME; TODO: get this working
        ident->meta_flags |= META_FLAG::ASSIGNABLE;

        const InternString* name = ident->name;

        {
          Local* local = state->get_local(name);

          if (local != nullptr) {
            ident->id_type = ASTIdentifier::LOCAL;
            ident->local = local;

            return;
          }
        }

        {
          auto names = comp->services.names.get();
          const GlobalName* non_local = names->find_global_name(state->current_unit->available_names, name);

          if (non_local != nullptr) {
            if (!non_local->global->decl.type.is_valid()) {
              UnitID id = non_local->unit_id;
              names.release();
              //Need to wait for this if its not ready
              set_dependency(comp_thread, state, id);
            }

            ident->id_type = ASTIdentifier::GLOBAL;
            ident->global = non_local->global;
            return;
          }
        }

        UnknownName unknown = {};
        unknown.ident = name;
        unknown.ns = state->current_unit->available_names;

        state->current_unit->insert_to = state->dependency_load_pipe;

        set_unfound_name(comp_thread, state, std::move(unknown),
                         ERROR_CODE::UNFOUND_DEPENDENCY, a->node_span,
                         "'{}' was used but a it has no matching declaration",
                         name);


        return;
      }
    case AST_TYPE::FUNCTION_CALL: {
        ASTFunctionCallExpr* const call = (ASTFunctionCallExpr*)a;

        call->meta_flags |= META_FLAG::CONST;
        //call->meta_flags |= META_FLAG::COMPTIME; TODO: get this working


        //TODO: Local functions
        test_global_dependency(comp, comp_thread, state, call->node_span, call->function_name);

        FOR_AST(call->arguments, it) {
          dependency_check_ast_node(comp, comp_thread, state, it);
        }

        return;
      }
    case AST_TYPE::TUPLE_LIT: {
        ASTTupleLitExpr* tup = (ASTTupleLitExpr*)a;

        tup->meta_flags |= META_FLAG::CONST;
        tup->meta_flags |= META_FLAG::COMPTIME;

        if (tup->name != nullptr) {
          test_global_dependency(comp, comp_thread, state, tup->node_span, tup->name);
        }

        FOR_AST(tup->elements, it) {
          dependency_check_ast_node(comp, comp_thread, state, it);
        }
        return;
      }
    case AST_TYPE::ARRAY_EXPR: {
        ASTArrayExpr* arr = (ASTArrayExpr*)a;
        arr->meta_flags |= META_FLAG::CONST;
        arr->meta_flags |= META_FLAG::COMPTIME;

        FOR_AST(arr->elements, it) {
          dependency_check_ast_node(comp, comp_thread, state, it);
        }
        return;
      }

    case AST_TYPE::INDEX_EXPR: {
        ASTIndexExpr* index = (ASTIndexExpr*)a;
        index->meta_flags |= META_FLAG::CONST;
        index->meta_flags |= META_FLAG::COMPTIME;
        index->meta_flags |= META_FLAG::ASSIGNABLE;

        dependency_check_ast_node(comp, comp_thread, state, index->expr);
        dependency_check_ast_node(comp, comp_thread, state, index->index);
        return;
      }
    case AST_TYPE::MEMBER_ACCESS: {
        ASTMemberAccessExpr* member = (ASTMemberAccessExpr*)a;
        member->meta_flags |= META_FLAG::CONST;
        member->meta_flags |= META_FLAG::COMPTIME;
        member->meta_flags |= META_FLAG::ASSIGNABLE;

        dependency_check_ast_node(comp, comp_thread, state, member->expr);
        return;
      }
    case AST_TYPE::LAMBDA_EXPR: {
        ASTLambdaExpr* le = (ASTLambdaExpr*)a;
        le->meta_flags |= META_FLAG::CONST;
        le->meta_flags |= META_FLAG::COMPTIME;

        ASTLambda* lambda = (ASTLambda*)le->lambda;
        ASTFuncSig* sig = (ASTFuncSig*)lambda->sig;

        if (sig->sig->sig_struct == nullptr) {
          set_dependency(comp_thread, state, lambda->function->sig_unit_id);
        }

        return;
      }
    case AST_TYPE::STRUCT_EXPR: {
        ASTStructExpr* se = (ASTStructExpr*)a;
        se->meta_flags |= META_FLAG::CONST;
        se->meta_flags |= META_FLAG::COMPTIME;

        ASTStructBody* struct_body = (ASTStructBody*)se->struct_body;

        if (!(struct_body->actual_type.is_valid())) {
          //Is not valid so need to wait for that
          set_dependency(comp_thread, state, struct_body->unit_id);
        }

        return;
      }
    case AST_TYPE::LOCAL_DECL: {
        ASTLocalDecl* decl = (ASTLocalDecl*)a;
        decl->meta_flags |= META_FLAG::CONST;
        decl->meta_flags |= META_FLAG::COMPTIME;

        if (decl->type_ast != 0) {
          dependency_check_ast_node(comp, comp_thread, state, decl->type_ast);
        }

        if (decl->expr != 0) {
          dependency_check_ast_node(comp, comp_thread, state, decl->expr);
        }

        if (decl->local_ptr == nullptr) {
          const Local* shadowing = state->get_local(decl->name);

          if (shadowing != nullptr) {
            comp_thread->report_error(ERROR_CODE::NAME_ERROR, a->node_span,
                                      "Attempted to shadow the local variable '{}'",
                                      decl->name);
            return;
          }

          Local* loc = comp->new_local();
          decl->local_ptr = loc;

          loc->decl.name = decl->name;
          loc->decl.span = decl->node_span;

        }



        state->locals.insert(decl->local_ptr);

        return;
      }
    case AST_TYPE::GLOBAL_DECL: {
        ASTGlobalDecl* decl = (ASTGlobalDecl*)a;
        decl->meta_flags |= META_FLAG::CONST;
        decl->meta_flags |= META_FLAG::COMPTIME;

        if (decl->type_ast != 0) {
          dependency_check_ast_node(comp, comp_thread, state, decl->type_ast);
        }

        if (decl->expr != 0) {
          dependency_check_ast_node(comp, comp_thread, state, decl->expr);
        }

        return;
      }
    case AST_TYPE::TYPED_NAME: {
        ASTTypedName* tn = (ASTTypedName*)a;
        tn->meta_flags |= META_FLAG::CONST;
        tn->meta_flags |= META_FLAG::COMPTIME;

        if (tn->type != 0) {
          dependency_check_ast_node(comp, comp_thread, state, tn->type);
        }

        if (tn->local_ptr == nullptr) {
          const Local* shadowing = state->get_local(tn->name);

          if (shadowing != nullptr) {
            comp_thread->report_error(ERROR_CODE::NAME_ERROR, a->node_span,
                                      "Attempted to shadow the variable '{}'",
                                      tn->name);
            return;
          }

          Local* loc = comp->new_local();
          tn->local_ptr = loc;

          loc->decl.name = tn->name;
          loc->decl.span = tn->node_span;
        }

        state->locals.insert(tn->local_ptr);

        return;
      }
    case AST_TYPE::ASSIGN: {
        ASTAssign* assign = (ASTAssign*)a;

        dependency_check_ast_node(comp, comp_thread, state, assign->assign_to);
        dependency_check_ast_node(comp, comp_thread, state, assign->value);

        return;
      }
    case AST_TYPE::BLOCK: {
        ASTBlock* block = (ASTBlock*)a;
        block->meta_flags |= META_FLAG::CONST;
        block->meta_flags |= META_FLAG::COMPTIME;

        const usize count = state->locals.size;

        FOR_AST(block->block, it) {
          dependency_check_ast_node(comp, comp_thread, state, it);
        }

        state->locals.pop(state->locals.size - count);

        return;
      }
    case AST_TYPE::IF_ELSE: {
        ASTIfElse* if_else = (ASTIfElse*)a;
        if_else->meta_flags |= META_FLAG::CONST;
        if_else->meta_flags |= META_FLAG::COMPTIME;

        dependency_check_ast_node(comp, comp_thread, state, if_else->condition);

        const usize count = state->locals.size;

        dependency_check_ast_node(comp, comp_thread, state, if_else->if_statement);

        state->locals.pop(state->locals.size - count);

        if (if_else->else_statement != 0) {
          dependency_check_ast_node(comp, comp_thread, state, if_else->else_statement);
          state->locals.pop(state->locals.size - count);
        }

        return;
      }
    case AST_TYPE::WHILE: {
        ASTWhile* while_s = (ASTWhile*)a;
        //index->meta_flags |= META_FLAG::CONST;
        while_s->meta_flags |= META_FLAG::COMPTIME;

        dependency_check_ast_node(comp, comp_thread, state, while_s->condition);

        const usize count = state->locals.size;
        dependency_check_ast_node(comp, comp_thread, state, while_s->statement);
        state->locals.pop(state->locals.size - count);
        return;
      }
    case AST_TYPE::RETURN: {
        ASTReturn* ret = (ASTReturn*)a;
        ret->meta_flags |= META_FLAG::CONST;
        ret->meta_flags |= META_FLAG::COMPTIME;

        if (ret->expr != nullptr) {
          dependency_check_ast_node(comp, comp_thread, state, ret->expr);
        }
        return;
      }
    case AST_TYPE::FUNCTION_SIGNATURE: {
        ASTFuncSig* func_sig = (ASTFuncSig*)a;

        func_sig->meta_flags |= META_FLAG::CONST;
        func_sig->meta_flags |= META_FLAG::COMPTIME;

        FOR_AST(func_sig->parameters, it) {
          dependency_check_ast_node(comp, comp_thread, state, it);
        }

        dependency_check_ast_node(comp, comp_thread, state, func_sig->return_type);

        return;
      }
    case AST_TYPE::IMPORT: {
        ASTImport* imp = (ASTImport*)a;

        imp->meta_flags |= META_FLAG::CONST;
        imp->meta_flags |= META_FLAG::COMPTIME;

        dependency_check_ast_node(comp, comp_thread, state, imp->expr_location);
        return;
      }
    case AST_TYPE::LINK: {
        ASTLink* imp = (ASTLink*)a;

        imp->meta_flags |= META_FLAG::CONST;

        if (imp->dynamic) {
          imp->meta_flags |= META_FLAG::COMPTIME;
        }

        dependency_check_ast_node(comp, comp_thread, state, imp->import_type);

        return;
      }

    case AST_TYPE::LAMBDA: {
        ASTLambda* l = (ASTLambda*)a;

        l->meta_flags |= META_FLAG::CONST;
        l->meta_flags |= META_FLAG::COMPTIME;

        ASSERT(state->locals.size == 0);

        dependency_check_ast_node(comp, comp_thread, state, l->sig);
        dependency_check_ast_node(comp, comp_thread, state, l->body);

        return;
      }
    case AST_TYPE::STRUCT: {
        ASTStructBody* s = (ASTStructBody*)a;

        s->meta_flags |= META_FLAG::CONST;
        s->meta_flags |= META_FLAG::COMPTIME;

        FOR_AST(s->elements, it) {
          dependency_check_ast_node(comp, comp_thread, state, it);
        }

        return;
      }

    case AST_TYPE::ASCII_CHAR:
    case AST_TYPE::ASCII_STRING:
    case AST_TYPE::NUMBER: {
        a->meta_flags |= META_FLAG::CONST;
        a->meta_flags |= META_FLAG::COMPTIME;

        //No dependencies :)
        return;
      }
  }

  comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, a->node_span,
                            "Not yet implemented dependency checking for this node. Node ID: {}", (usize)a->ast_type);
}

struct UntypedNode {
  AST_LOCAL node;
  Type infer;
};

struct Typer {
  Array<UntypedNode> new_untyped_stack = {};
  Array<UntypedNode> untyped_stack = {};

  Type return_type;

  void push_node(AST_LOCAL loc, Type infer);
  void load_new_nodes();
};

void Typer::push_node(AST_LOCAL loc, Type infer) {
  ASSERT(!loc->node_type.is_valid());

  UntypedNode n = {};
  n.node = loc;
  n.infer = infer;

  new_untyped_stack.insert(std::move(n));
}

void Typer::load_new_nodes() {
  untyped_stack.reserve_extra(new_untyped_stack.size);

  while (new_untyped_stack.size > 0) {
    untyped_stack.insert(new_untyped_stack.take());
  }
}

//Overload for taking address
void type_check_take_address(CompilerGlobals* comp,
                             CompilerThread* comp_thread,
                             ASTUnaryOperatorExpr* expr) {
  AtomicLock<Structures> structures = {};
  AtomicLock<StringInterner> strings = {};
  comp->services.get_multiple(&structures, &strings);

  const Structure* ptr = find_or_make_pointer_structure(structures._ptr, strings._ptr,
                                                        comp->platform_interface.ptr_size, expr->expr->node_type);
  expr->node_type = to_type(ptr);
  expr->emit_info = { expr->node_type, &UnOpArgs::emit_address };

  //Current cant do these at comptime
  expr->meta_flags &= ~META_FLAG::COMPTIME;

  expr->can_be_constant = false;
}

//Overload for dereferencing
void type_check_deref(CompilerGlobals* comp,
                      CompilerThread* comp_thread,
                      ASTUnaryOperatorExpr* expr) {

  AST_LOCAL prim = expr->expr;

  if (prim->node_type.struct_type() == STRUCTURE_TYPE::POINTER) {
    const auto* ptr = prim->node_type.unchecked_base<PointerStructure>();

    expr->node_type = ptr->base;
    expr->emit_info = { expr->node_type, &UnOpArgs::emit_deref_ptr };
  }
  else {
    const char* const op_string = UNARY_OP_STRING::get(expr->op);

    comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->node_span,
                              "No unary operator '{}' exists for type: '{}'",
                              op_string, prim->node_type.name);
  }
}

static bool type_check_unary_operator(CompilerGlobals* const comp,
                                      CompilerThread* const comp_thread,
                                      Typer* const typer,
                                      UntypedNode* this_untyped) {
  TRACING_FUNCTION();

  ASSERT(this_untyped->node->ast_type == AST_TYPE::UNARY_OPERATOR);
  ASTUnaryOperatorExpr* expr = (ASTUnaryOperatorExpr*)this_untyped->node;
  AST_LOCAL prim = expr->expr;

  Type infer_type = this_untyped->infer;

  switch (expr->op) {
    case UNARY_OPERATOR::NEG: {
        if (!prim->node_type.is_valid()) {
          pass_meta_flags_up(&expr->meta_flags, &prim->meta_flags);

          if (infer_type.is_valid()) {
            if (infer_type.struct_type() == STRUCTURE_TYPE::INTEGER) {
              const IntegerStructure* is = infer_type.unchecked_base<IntegerStructure>();

              if (is->is_signed) {
                //Can infer

                if (!prim->node_type.is_valid()) {
                  typer->push_node(prim, infer_type);
                  return false;
                }
              }
            }
          }

          typer->push_node(prim, {});
          return false;
        }

        pass_meta_flags_down(&expr->meta_flags, &prim->meta_flags);
        Type ty = prim->node_type;

        if (ty.struct_type() != STRUCTURE_TYPE::INTEGER) {
          comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, expr->node_span,
                                    "Cannot negate type '{}'. It was not an integer",
                                    ty.name);
          return false;
        }

        const IntegerStructure* is = ty.unchecked_base<IntegerStructure>();

        if (!is->is_signed) {
          comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, expr->node_span,
                                    "Cannot negate type '{}'. It was unsigned!",
                                    ty.name);
          return false;
        }

        expr->node_type = ty;
        expr->emit_info = { expr->node_type, &UnOpArgs::emit_neg_int };
        return true;
      }
    case UNARY_OPERATOR::ADDRESS:
      //TODO: can we infer anything here??
      if (!prim->node_type.is_valid()) {
        pass_meta_flags_up(&expr->meta_flags, &prim->meta_flags);
        typer->push_node(prim, {});
        return false;
      }

      //Expects type checked expr
      pass_meta_flags_down(&expr->meta_flags, &prim->meta_flags);
      type_check_take_address(comp, comp_thread, expr);
      return true;
    case UNARY_OPERATOR::DEREF:
      //TODO: can we infer anything here
      if (!prim->node_type.is_valid()) {
        pass_meta_flags_up(&expr->meta_flags, &prim->meta_flags);
        typer->push_node(prim, {});
        return false;
      }

      pass_meta_flags_down(&expr->meta_flags, &prim->meta_flags);
      type_check_deref(comp, comp_thread, expr);
      return true;
    default: {
        const char* name = UNARY_OP_STRING::get(expr->op);

        comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, expr->node_span,
                                  "Type checking is not implemented for unary operator '{}'",
                                  name);
        return false;
      }
  }

  INVALID_CODE_PATH("Should have returned by now");
}


void type_check_binary_operator(CompilerGlobals* comp,
                                CompilerThread* comp_thread,
                                Typer* typer,
                                UntypedNode* this_untyped) {
  ASSERT(this_untyped->node->ast_type == AST_TYPE::BINARY_OPERATOR);

  ASTBinaryOperatorExpr* expr = (ASTBinaryOperatorExpr*)(this_untyped->node);

  AST_LOCAL left_ast = expr->left;
  AST_LOCAL right_ast = expr->right;

  const Type& left = left_ast->node_type;
  const Type& right = right_ast->node_type;

  switch (expr->op) {
    case BINARY_OPERATOR::ADD: {
        switch (left.struct_type()) {
          case STRUCTURE_TYPE::INTEGER: {
              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    if (left == right) {
                      expr->node_type = left;
                      expr->emit_info.main_side = MainSide::LEFT;
                      expr->emit_info.dest_type = expr->node_type;
                      expr->emit_info.func = &BinOpArgs::emit_add_ints;
                      return;
                    }
                    break;
                  }

                case STRUCTURE_TYPE::POINTER: {
                    const auto* int_l = right.unchecked_base<IntegerStructure>();
                    if (!int_l->is_signed && int_l->size == 8) {
                      expr->node_type = right;
                      expr->emit_info.main_side = MainSide::RIGHT;
                      expr->emit_info.dest_type = expr->node_type;
                      expr->emit_info.func = &BinOpArgs::emit_add_int_to_ptr;
                      return;
                    }
                    break;
                  }

                default: break;
              }

              break;
            }

          case STRUCTURE_TYPE::POINTER: {
              if (right.struct_type() == STRUCTURE_TYPE::INTEGER) {
                const auto* int_r = right.unchecked_base<IntegerStructure>();
                if (!int_r->is_signed && int_r->size == 8) {
                  expr->node_type = left;
                  expr->emit_info.main_side = MainSide::LEFT;
                  expr->emit_info.dest_type = expr->node_type;
                  expr->emit_info.func = &BinOpArgs::emit_add_int_to_ptr;
                  return;
                }
              }

              break;
            }

          default: break;
        }

        break;
      }

    case BINARY_OPERATOR::SUB: {
        switch (left.struct_type()) {
          case STRUCTURE_TYPE::INTEGER: {
              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    if (left == right) {
                      expr->node_type = left;
                      expr->emit_info.main_side = MainSide::LEFT;
                      expr->emit_info.dest_type = expr->node_type;
                      expr->emit_info.func = &BinOpArgs::emit_sub_ints;
                      return;
                    }
                    break;
                  }

                default: break;
              }

              break;
            }

          case STRUCTURE_TYPE::POINTER: {
              switch (right.struct_type()) {
                case STRUCTURE_TYPE::POINTER: {
                    if (left == right) {
                      expr->node_type = comp->builtin_types->t_u64;
                      expr->emit_info.main_side = MainSide::LEFT;
                      expr->emit_info.dest_type = expr->node_type;
                      expr->emit_info.func = &BinOpArgs::emit_sub_ptrs;
                      return;
                    }
                    break;
                  }

                default: break;
              }

              break;
            }

          default: break;
        }

        break;
      }

    case BINARY_OPERATOR::MUL: {
        switch (left.struct_type()) {
          case STRUCTURE_TYPE::INTEGER: {
              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    if (left == right) {
                      expr->node_type = left;
                      expr->emit_info.main_side = MainSide::LEFT;
                      expr->emit_info.dest_type = expr->node_type;
                      expr->emit_info.func = &BinOpArgs::emit_mul_ints;
                      return;
                    }
                    break;
                  }

                default: break;
              }

              break;
            }

          default: break;
        }

        break;
      }

    case BINARY_OPERATOR::DIV: {
        switch (left.struct_type()) {
          case STRUCTURE_TYPE::INTEGER: {
              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    if (left == right) {
                      expr->node_type = left;
                      expr->emit_info.main_side = MainSide::LEFT;
                      expr->emit_info.dest_type = expr->node_type;
                      expr->emit_info.func = &BinOpArgs::emit_div_ints;

                      return;
                    }
                    break;
                  }

                default: break;
              }

              break;
            }

          default: break;
        }

        break;
      }

    case BINARY_OPERATOR::MOD: {
        switch (left.struct_type()) {
          case STRUCTURE_TYPE::INTEGER: {
              const auto* int_l = left.unchecked_base<IntegerStructure>();

              if (int_l->is_signed) break;//currently not supported;

              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    if (left == right) {
                      expr->node_type = left;
                      expr->emit_info.main_side = MainSide::LEFT;
                      expr->emit_info.dest_type = expr->node_type;
                      expr->emit_info.func = &BinOpArgs::emit_mod_ints;
                      return;
                    }
                    break;
                  }

                default: break;
              }

              break;
            }

          default: break;
        }

        break;
      }

    case BINARY_OPERATOR::EQUIVALENT: {
        switch (left.struct_type()) {
          case STRUCTURE_TYPE::ASCII_CHAR: {
              if (right.struct_type() == STRUCTURE_TYPE::ASCII_CHAR) {
                expr->node_type = comp_thread->builtin_types->t_bool;
                expr->emit_info.main_side = MainSide::LEFT;
                expr->emit_info.dest_type = expr->node_type;
                expr->emit_info.func = &BinOpArgs::emit_eq_ints;
                return;
              }

              break;
            }

          case STRUCTURE_TYPE::ENUM: {
              if (left == right) {
                const auto* en = left.extract_base<EnumStructure>();
                ASSERT(en->base.struct_type() == STRUCTURE_TYPE::INTEGER);

                expr->node_type = comp_thread->builtin_types->t_bool;
                expr->emit_info.main_side = MainSide::LEFT;
                expr->emit_info.dest_type = expr->node_type;
                expr->emit_info.func = &BinOpArgs::emit_eq_ints;
                return;
              }

              break;
            }

          case STRUCTURE_TYPE::INTEGER: {
              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    if (left == right) {
                      expr->node_type = comp_thread->builtin_types->t_bool;
                      expr->emit_info.main_side = MainSide::LEFT;
                      expr->emit_info.dest_type = expr->node_type;
                      expr->emit_info.func = &BinOpArgs::emit_eq_ints;
                      return;
                    }
                    break;
                  }

                default: break;
              }

              break;
            }

          case STRUCTURE_TYPE::POINTER: {
              switch (right.struct_type()) {
                case STRUCTURE_TYPE::POINTER: {
                    if (left == right) {
                      expr->node_type = comp_thread->builtin_types->t_bool;
                      expr->emit_info.main_side = MainSide::LEFT;
                      expr->emit_info.dest_type = expr->node_type;
                      expr->emit_info.func = &BinOpArgs::emit_eq_ints;
                      return;
                    }
                    break;
                  }

                default: break;
              }

              break;
            }

          default: break;
        }

        break;
      }

    case BINARY_OPERATOR::NOT_EQ: {
        switch (left.struct_type()) {
          case STRUCTURE_TYPE::ASCII_CHAR: {
              if (left == right) {
                expr->node_type = comp_thread->builtin_types->t_bool;
                expr->emit_info.main_side = MainSide::LEFT;
                expr->emit_info.dest_type = expr->node_type;
                expr->emit_info.func = &BinOpArgs::emit_neq_ints;
                return;
              }

              break;
            }

          case STRUCTURE_TYPE::ENUM: {
              if (left == right) {
                const auto* en = left.extract_base<EnumStructure>();
                ASSERT(en->base.struct_type() == STRUCTURE_TYPE::INTEGER);


                expr->node_type = comp_thread->builtin_types->t_bool;
                expr->emit_info.main_side = MainSide::LEFT;
                expr->emit_info.dest_type = expr->node_type;
                expr->emit_info.func = &BinOpArgs::emit_neq_ints;
                return;
              }

              break;
            }

          case STRUCTURE_TYPE::INTEGER: {
              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    if (left == right) {
                      expr->node_type = comp_thread->builtin_types->t_bool;
                      expr->emit_info.main_side = MainSide::LEFT;
                      expr->emit_info.dest_type = expr->node_type;
                      expr->emit_info.func = &BinOpArgs::emit_neq_ints;
                      return;
                    }
                    break;
                  }

                default: break;
              }

              break;
            }

          case STRUCTURE_TYPE::POINTER: {
              switch (right.struct_type()) {
                case STRUCTURE_TYPE::POINTER: {
                    if (left == right) {
                      expr->node_type = comp_thread->builtin_types->t_bool;
                      expr->emit_info.main_side = MainSide::LEFT;
                      expr->emit_info.dest_type = expr->node_type;
                      expr->emit_info.func = &BinOpArgs::emit_neq_ints;
                      return;
                    }
                    break;
                  }

                default: break;
              }

              break;
            }

          default: break;
        }

        break;
      }

    case BINARY_OPERATOR::LESSER: {
        switch (left.struct_type()) {
          case STRUCTURE_TYPE::INTEGER: {
              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {

                    expr->node_type = comp_thread->builtin_types->t_bool;
                    expr->emit_info.main_side = MainSide::LEFT;
                    expr->emit_info.dest_type = expr->node_type;
                    expr->emit_info.func = &BinOpArgs::emit_lesser_ints;

                    return;
                  }

                default: break;
              }

              break;
            }

          default: break;
        }

        break;
      }

    case BINARY_OPERATOR::GREATER: {
        switch (left.struct_type()) {
          case STRUCTURE_TYPE::INTEGER: {
              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    if (left == right) {
                      expr->node_type = comp_thread->builtin_types->t_bool;
                      expr->emit_info.main_side = MainSide::LEFT;
                      expr->emit_info.dest_type = expr->node_type;
                      expr->emit_info.func = &BinOpArgs::emit_greater_ints;

                      return;
                    }
                  }

                default: break;
              }

              break;
            }

          default: break;
        }

        break;
      }

    case BINARY_OPERATOR::OR: {
        switch (left.struct_type()) {
          case STRUCTURE_TYPE::ENUM: {
              if (left == right) {
                const auto* en = left.extract_base<EnumStructure>();
                ASSERT(en->base.struct_type() == STRUCTURE_TYPE::INTEGER);

                expr->node_type = en->base;
                expr->emit_info.main_side = MainSide::LEFT;
                expr->emit_info.dest_type = expr->node_type;
                expr->emit_info.func = &BinOpArgs::emit_or_ints;
                return;
              }

              break;
            }

          case STRUCTURE_TYPE::INTEGER: {
              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    if (left == right) {
                      expr->node_type = left;
                      expr->emit_info.main_side = MainSide::LEFT;
                      expr->emit_info.dest_type = expr->node_type;
                      expr->emit_info.func = &BinOpArgs::emit_or_ints;
                      return;
                    }
                  }

                default: break;
              }

              break;
            }

          default: break;
        }

        break;
      }

    case BINARY_OPERATOR::XOR: {
        switch (left.struct_type()) {
          case STRUCTURE_TYPE::INTEGER: {
              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    if (left == right) {
                      expr->node_type = left;
                      expr->emit_info.main_side = MainSide::LEFT;
                      expr->emit_info.dest_type = expr->node_type;
                      expr->emit_info.func = &BinOpArgs::emit_xor_ints;
                      return;
                    }
                  }

                default: break;
              }

              break;
            }

          default: break;
        }

        break;
      }

    case BINARY_OPERATOR::AND: {
        switch (left.struct_type()) {
          case STRUCTURE_TYPE::ENUM: {
              if (left == right) {
                const auto* en = left.extract_base<EnumStructure>();
                ASSERT(en->base.struct_type() == STRUCTURE_TYPE::INTEGER);

                expr->node_type = en->base;
                expr->emit_info.main_side = MainSide::LEFT;
                expr->emit_info.dest_type = expr->node_type;
                expr->emit_info.func = &BinOpArgs::emit_and_ints;
                return;
              }

              break;
            }

          case STRUCTURE_TYPE::INTEGER: {
              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    if (left == right) {
                      expr->node_type = left;
                      expr->emit_info.main_side = MainSide::LEFT;
                      expr->emit_info.dest_type = expr->node_type;
                      expr->emit_info.func = &BinOpArgs::emit_and_ints;
                      return;
                    }
                  }

                default: break;
              }

              break;
            }

          default: break;
        }

        break;
      }
#if 0
    case BINARY_OPERATOR::LEFT_SHIFT: {
        switch (left.struct_type()) {
          case STRUCTURE_TYPE::INTEGER: {
              const auto* int_l = left.unchecked_base<IntegerStructure>();

              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    const auto* int_r = left.unchecked_base<IntegerStructure>();


                    if (int_l->size == 8 && int_r->size == 1) {
                      expr->node_type = left;
                      expr->emit_info.main_op = MainOp::LEFT;
                      expr->emit_info.main_type = left;
                      expr->emit_info.func = &BinOpArgs::emit_shift_l_64_by_8;
                      return;
                    }
                    break;
                  }

                default: break;
              }

              break;
            }

          default: break;
        }

        break;
      }

    case BINARY_OPERATOR::RIGHT_SHIFT: {
        switch (left.struct_type()) {
          case STRUCTURE_TYPE::INTEGER: {
              const auto* int_l = left.unchecked_base<IntegerStructure>();

              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    const auto* int_r = left.unchecked_base<IntegerStructure>();


                    if (int_l->size == 8 && int_r->size == 1) {
                      expr->node_type = left;
                      expr->emit_info.main_op = MainOp::LEFT;
                      expr->emit_info.main_type = left;

                      if (int_l->is_signed) {
                        expr->emit_info.func = &BinOpArgs::emit_shift_r_i64_by_8;
                      }
                      else {
                        expr->emit_info.func = &BinOpArgs::emit_shift_r_u64_by_8;
                      }
                      return;
                    }
                    break;
                  }

                default: break;
              }

              break;
            }

          default: break;
        }

        break;
      }
#endif
  }

  const char* const op_string = BINARY_OP_STRING::get(expr->op);

  comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->node_span,
                            "No binary operator '{}' exists for left type: '{}', and right type: '{}'",
                            op_string, left.name, right.name);
  return;
}

static bool test_function_overload(const CallSignature* sig, const SignatureStructure* sig_struct) {
  TRACING_FUNCTION();

  //Correct name and number of args
  if (sig->arguments.size == sig_struct->parameter_types.size) {
    auto p_call = sig->arguments.begin();
    const auto end_call = sig->arguments.end();

    auto p_func = sig_struct->parameter_types.begin();

    while (p_call < end_call) {
      if (p_call->type == *p_func) {
        //Do nothing
      }
      else {
        //Escape out
        return false;
      }

      p_call++;
      p_func++;
    }

    return true;
  }

  return false;
}

static void compile_find_function_call(CompilerGlobals* const comp,
                                       CompilerThread* const comp_thread,
                                       Context* const context,
                                       ASTFunctionCallExpr* const call) {
  TRACING_FUNCTION();

  //TODO: local functions

  CallSignature sig = {};

  sig.name = call->function_name;
  sig.arguments.reserve_total(call->arguments.count);

  //Load all the types
  FOR_AST(call->arguments, it) {
    ASSERT(it->node_type.is_valid());
    sig.arguments.insert({ it->meta_flags, it->node_type });
  }

  sig.arguments.shrink();
  const Global* global;
  {
    auto names = comp->services.names.get();
    GlobalName* name = names->find_global_name(context->current_unit->available_names, sig.name);
    ASSERT(name != nullptr);

    global = name->global;
    ASSERT(name->global != nullptr);
  }

  ASSERT(global->decl.type.is_valid());

  if (global->decl.type.struct_type() == STRUCTURE_TYPE::LAMBDA) {
    const auto* sig_struct = global->decl.type.unchecked_base<SignatureStructure>();

    if (test_function_overload(&sig, sig_struct)) {
      ASSERT(global->constant_value != nullptr);

      call->sig = sig_struct;
      call->label = *(IR::GlobalLabel*)global->constant_value;
      return;
    }
    else {
      comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, call->node_span,
                                "Arguments mismatch\nArgs: {}\nDecl: {}", sig, PrintSignatureType{ sig_struct });
      return;
    }
  }

  comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, call->node_span,
                            "No visible function matches signature '{}'",
                            sig);
}

//Gets the underlying type of a type-node e.g. if its an array type node it gets the cached array type
//Errors if it was not a type
static Type get_type_value(CompilerThread* const comp_thread, AST_LOCAL a) {
  switch (a->ast_type) {
    case AST_TYPE::NAMED_TYPE: return static_cast<ASTNamedType*>(a)->actual_type;
    case AST_TYPE::ARRAY_TYPE: return static_cast<ASTArrayType*>(a)->actual_type;
    case AST_TYPE::PTR_TYPE: return static_cast<ASTPtrType*>(a)->actual_type;
    case AST_TYPE::LAMBDA_TYPE: return static_cast<ASTLambdaType*>(a)->actual_type;
    case AST_TYPE::TUPLE_TYPE: return static_cast<ASTTupleType*>(a)->actual_type;
    case AST_TYPE::STRUCT: return static_cast<ASTStructBody*>(a)->actual_type;
    case AST_TYPE::STRUCT_EXPR: {
        AST_LOCAL body = static_cast<ASTStructExpr*>(a)->struct_body;
        ASSERT(body->ast_type == AST_TYPE::STRUCT);
        return static_cast<ASTStructBody*>(body)->actual_type;
      }

    default: {
        comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, a->node_span,
                                  "Invalid type node: {}", ast_type_string(a->ast_type));
        return {};
      }
  }
}

static void cast_operator_type(CompilerGlobals* const comp,
                               CompilerThread* const comp_thread,
                               ASTCastExpr* const cast) {
  TRACING_FUNCTION();

  const Type cast_to = get_type_value(comp_thread, cast->type);
  if (comp_thread->is_panic()) {
    return;
  }
  ASSERT(cast_to.is_valid());

  META_FLAGS from_flags = cast->expr->meta_flags;
  const Type cast_from = cast->expr->node_type;
  ASSERT(cast_from.is_valid());

  DEFER(&) { if (!comp_thread->is_panic()) ASSERT(cast->emit != nullptr); };

  const auto emit_cast_func = [from_flags, cast_to](ASTCastExpr* cast, CASTS::CAST_FUNCTION cast_fn) {
    cast->emit = cast_fn;
    cast->node_type = cast_to;
    cast->meta_flags = from_flags;
  };

  switch (cast_from.struct_type()) {
    case STRUCTURE_TYPE::ASCII_CHAR: {
        if (cast_to.struct_type() == STRUCTURE_TYPE::INTEGER) {
          const auto* to_int = cast_to.unchecked_base<IntegerStructure>();

          if (to_int->size == 1 && !to_int->is_signed) {
            //Can cast ascii to u8
            emit_cast_func(cast, CASTS::no_op);
            return;
          }
        }
        break;
      }

    case STRUCTURE_TYPE::ENUM: {
        const auto* en = cast_from.unchecked_base<EnumStructure>();

        if (cast_to == en->base) {
          emit_cast_func(cast, CASTS::no_op);
          return;
        }

        break;
      }
    case STRUCTURE_TYPE::FIXED_ARRAY: {
        const auto* from_arr = cast_from.unchecked_base<ArrayStructure>();

        /*if (cast_to.struct_type() == STRUCTURE_TYPE::FIXED_ARRAY) {
          const auto* to_arr = cast_to.unchecked_base<ArrayStructure>();

          if (TYPE_TESTS::can_implicit_cast(from_arr->base, to_arr->base) && from_arr->size() == to_arr->size()) {
            emit_cast_func(expr, CASTS::no_op);
            return;
          }
        }
        else */
        if (cast_to.struct_type() == STRUCTURE_TYPE::POINTER) {
          const auto* to_ptr = cast_to.unchecked_base<PointerStructure>();

          if (from_arr->base == to_ptr->base) {
            //Cannot be a constant to cast like this
            cast->expr->can_be_constant = false;
            cast->can_be_constant = false;
            emit_cast_func(cast, CASTS::take_address);
            return;
          }
        }

        break;
      }
    case STRUCTURE_TYPE::INTEGER: {
        const auto* from_int = cast_from.unchecked_base<IntegerStructure>();

        if (cast_to.struct_type() == STRUCTURE_TYPE::INTEGER) {
          emit_cast_func(cast, CASTS::int_to_int);
          return;
        }
        else if (cast_to.struct_type() == STRUCTURE_TYPE::ASCII_CHAR) {
          if (from_int->size == 1) {
            emit_cast_func(cast, CASTS::no_op);
            return;
          }

          comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, cast->node_span,
                                    "Cannot cast type '{}' to type '{}'\n"
                                    "First cast to a smaller int type ({})",
                                    cast_from.name, cast_to.name, comp_thread->builtin_types->t_u8.name);
          return;
        }

        break;
      }
    case STRUCTURE_TYPE::POINTER: {
        const auto* from_ptr = cast_from.unchecked_base<PointerStructure>();
        if (cast_to.struct_type() == STRUCTURE_TYPE::POINTER) {
          const auto* to_ptr = cast_to.unchecked_base<PointerStructure>();

          // can always cast to and from *void
          if (from_ptr->base.struct_type() == STRUCTURE_TYPE::VOID
              || to_ptr->base.struct_type() == STRUCTURE_TYPE::VOID) {
            emit_cast_func(cast, CASTS::no_op);
            return;
          }

          if (TYPE_TESTS::match_sizes(from_ptr->base, to_ptr->base)) {
            emit_cast_func(cast, CASTS::no_op);
            return;
          }
        }

        break;
      }
    case STRUCTURE_TYPE::VOID: {
        comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, cast->node_span,
                                  "Cannot cast '{}' to any type\n"
                                  "Attempted to cast '{}' to '{}'",
                                  cast_from.name, cast_from.name, cast_to.name);
        break;
      }

    default: break;
  }

  comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, cast->node_span,
                            "Cannot cast type '{}' to type '{}'",
                            cast_from.name, cast_to.name);
  return;
}

//Return true on success, false on fail/wait
bool type_check_single_node(CompilerGlobals* const comp,
                            CompilerThread* const comp_thread,
                            Context* const context,
                            Typer* const typer,
                            UntypedNode* this_untyped) {

  //For debugging - will always abort because destructors can't throw
  //DEFER(comp, a) {
  //  if (!comp->is_panic()) {
  //    ASSERT(a->node_type.is_valid());
  //  }
  //};

  AST_LOCAL a = this_untyped->node;
  Type infer_type = this_untyped->infer;

  TRACING_FUNCTION();
  switch (a->ast_type) {
    case AST_TYPE::INVALID: INVALID_CODE_PATH("Invalid node type"); break;

    case AST_TYPE::NAMED_TYPE: {
        ASTNamedType* nt = (ASTNamedType*)a;

        //TODO: local types
        const Global* global;
        {
          auto names = comp->services.names.get();
          GlobalName* g = names->find_global_name(context->current_unit->available_names, nt->name);

          ASSERT(g != nullptr);

          global = g->global;
          ASSERT(global != nullptr);
        }

        if (global->decl.type.struct_type() != STRUCTURE_TYPE::TYPE) {
          comp_thread->report_error(ERROR_CODE::NAME_ERROR, nt->node_span,
                                    "'{}' was not a type",
                                    nt->name);
          return false;
        }

        ASSERT(global->constant_value != nullptr);
        /*if (global->constant_value.ptr == nullptr) {
          ASSERT(g->unit != nullptr);
          comp->set_dep(context, g->unit);
          return;
        }*/

        nt->can_be_constant = true;
        nt->actual_type = *(const Type*)global->constant_value;
        nt->node_type = comp_thread->builtin_types->t_type;

        return true;
      }
    case AST_TYPE::ARRAY_TYPE: {
        ASTArrayType* at = (ASTArrayType*)a;

        if (!at->base->node_type.is_valid()) {
          typer->push_node(at->base, comp_thread->builtin_types->t_type);
          return false;
        }

        Type base_type = get_type_value(comp_thread, at->base);
        if (comp_thread->is_panic()) {
          return false;
        }

        ASSERT(at->expr != nullptr && TYPE_TESTS::is_int(at->expr->node_type));

        if (at->array_length == 0) {
          comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, at->expr->node_span,
                                    "Length of array must be larger than 0");
          return false;
        }

        const Structure* s;
        {
          AtomicLock<Structures> structures = {};
          AtomicLock<StringInterner> strings = {};
          comp->services.get_multiple(&structures, &strings);

          s = find_or_make_array_structure(structures._ptr,
                                           strings._ptr,
                                           base_type, at->array_length);
        }

        at->actual_type = to_type(s);
        at->node_type = comp_thread->builtin_types->t_type;

        return true;
      }
    case AST_TYPE::PTR_TYPE: {
        ASTPtrType* ptr = (ASTPtrType*)a;

        if (!ptr->base->node_type.is_valid()) {
          typer->push_node(ptr->base, comp_thread->builtin_types->t_type);
          return false;
        }

        Type base_type = get_type_value(comp_thread, ptr->base);
        if (comp_thread->is_panic()) {
          return false;
        }

        const Structure* s;
        {
          AtomicLock<Structures> structures = {};
          AtomicLock<StringInterner> strings = {};
          comp->services.get_multiple(&structures, &strings);

          s = find_or_make_pointer_structure(structures._ptr,
                                             strings._ptr,
                                             comp->platform_interface.ptr_size, base_type);
        }

        ptr->actual_type = to_type(s);
        ptr->node_type = comp_thread->builtin_types->t_type;
        return true;
      }
    case AST_TYPE::LAMBDA_TYPE: {
        ASTLambdaType* lt = (ASTLambdaType*)a;

        {
          bool pushed_any = false;

          FOR_AST(lt->args, ty) {
            if (!ty->node_type.is_valid()) {
              typer->push_node(ty, comp_thread->builtin_types->t_type);
              pushed_any = true;
            }
          }

          if (!lt->ret->node_type.is_valid()) {
            typer->push_node(lt->ret, comp_thread->builtin_types->t_type);
            pushed_any = true;
          }

          if (pushed_any) {
            return false;
          }
        }

        Array<Type> args = {};

        FOR_AST(lt->args, i) {
          Type i_type = get_type_value(comp_thread, i);
          if (comp_thread->is_panic()) {
            return false;
          }

          args.insert(i_type);
        }

        Type ret = get_type_value(comp_thread, lt->ret);
        if (comp_thread->is_panic()) {
          return false;
        }

        const SignatureStructure* s;
        {
          AtomicLock<Structures> structures = {};
          AtomicLock<StringInterner> strings = {};
          comp->services.get_multiple(&structures, &strings);
          s = find_or_make_lamdba_structure(structures._ptr,
                                            strings._ptr,
                                            comp->platform_interface.ptr_size,
                                            comp_thread->build_options.default_calling_convention,
                                            std::move(args), ret);
        }

        lt->actual_type = to_type(static_cast<const Structure*>(s));
        lt->node_type = comp_thread->builtin_types->t_type;
        return true;
      }
    case AST_TYPE::TUPLE_TYPE: {
        ASTTupleType* tt = (ASTTupleType*)a;

        {
          bool pushed_any = false;
          FOR_AST(tt->types, ty) {
            if (!ty->node_type.is_valid()) {
              typer->push_node(ty, comp_thread->builtin_types->t_type);
              pushed_any = true;
            }
          }

          if (pushed_any) {
            return false;
          }
        }

        Array<Type> args = {};

        FOR_AST(tt->types, i) {
          Type i_type = get_type_value(comp_thread, i);
          if (comp_thread->is_panic()) {
            return false;
          }

          args.insert(i_type);
        }

        const Structure* s;
        {
          AtomicLock<Structures> structures = {};
          AtomicLock<StringInterner> strings = {};
          comp->services.get_multiple(&structures, &strings);

          s = find_or_make_tuple_structure(structures._ptr,
                                           strings._ptr,
                                           std::move(args));
        }

        tt->actual_type = to_type(s);
        tt->node_type = comp_thread->builtin_types->t_type;
        return true;
      }
    case AST_TYPE::STRUCT_EXPR: {
        ASTStructExpr* se = (ASTStructExpr*)a;
        ASTStructBody* struct_body = (ASTStructBody*)se->struct_body;

        ASSERT(struct_body->actual_type.is_valid());

        a->node_type = comp_thread->builtin_types->t_type;
        return true;
      }
    case AST_TYPE::LAMBDA_EXPR: {
        //Never type the actual lambda here, its typed elsewhere
        ASTLambdaExpr* le = (ASTLambdaExpr*)a;
        ASTLambda* lambda = (ASTLambda*)le->lambda;
        ASTFuncSig* sig = (ASTFuncSig*)lambda->sig;

        ASSERT(sig->sig->sig_struct != nullptr);

        SET_MASK(le->meta_flags, META_FLAG::COMPTIME);
        a->node_type = to_type(sig->sig->sig_struct);
        return true;
      }
    case AST_TYPE::FUNCTION_SIGNATURE: {
        ASTFuncSig* ast_sig = (ASTFuncSig*)a;

        {
          bool pushed_any = false;

          FOR_AST(ast_sig->parameters, i) {
            if (!i->node_type.is_valid()) {
              typer->push_node(i, {});
              pushed_any = true;
            }
          }

          if (!ast_sig->return_type->node_type.is_valid()) {
            typer->push_node(ast_sig->return_type, comp_thread->builtin_types->t_type);
            pushed_any = true;
          }

          if (pushed_any) {
            return false;
          }
        }

        Array<Type> params = {};
        params.reserve_total(ast_sig->parameters.count);

        FOR_AST(ast_sig->parameters, i) {
          params.insert(i->node_type);
        }

        Type ret_type = get_type_value(comp_thread, ast_sig->return_type);
        if (comp_thread->is_panic()) {
          return false;
        }

        const SignatureStructure* sig_struct;
        {
          AtomicLock<Structures> structures = {};
          AtomicLock<StringInterner> strings = {};
          comp->services.get_multiple(&structures, &strings);

          sig_struct = find_or_make_lamdba_structure(structures._ptr,
                                                     strings._ptr,
                                                     comp->platform_interface.ptr_size,
                                                     ast_sig->convention,
                                                     std::move(params), ret_type);
        }

        ast_sig->sig->label = comp->next_function_label(sig_struct);

        ast_sig->node_type = comp_thread->builtin_types->t_void;
        ast_sig->sig->sig_struct = sig_struct;


        return true;
      }

    case AST_TYPE::LAMBDA: {
        ASTLambda* lambda = (ASTLambda*)a;
        ASTFuncSig* ast_sig = (ASTFuncSig*)lambda->sig;

        const SignatureStructure* sig_struct = ast_sig->sig->sig_struct;

        ASSERT(sig_struct != nullptr);//should be done in the signature unit

        typer->return_type = sig_struct->return_type;

        if (!lambda->body->node_type.is_valid()) {
          typer->push_node(lambda->body, {});
          return false;
        }

        typer->return_type = {};
        lambda->node_type = to_type(sig_struct);
        return true;
      }

    case AST_TYPE::MEMBER_ACCESS: {
        ASTMemberAccessExpr* member = (ASTMemberAccessExpr*)a;
        AST_LOCAL base = member->expr;

        if (!base->node_type.is_valid()) {
          pass_meta_flags_up(&a->meta_flags, &base->meta_flags);
          typer->push_node(base, {});
          return false;
        }

        pass_meta_flags_down(&a->meta_flags, &base->meta_flags);

        ASSERT(base->node_type.is_valid());

        STRUCTURE_TYPE struct_type = base->node_type.struct_type();


        if (struct_type == STRUCTURE_TYPE::COMPOSITE) {
          const Type& cmp_t = base->node_type;

          const auto* cs = cmp_t.unchecked_base<CompositeStructure>();

          auto i = cs->elements.begin();
          auto end = cs->elements.end();

          a->node_type.structure = nullptr;//reset

          for (; i < end; i++) {
            if (i->name == member->name) {
              member->node_type = i->type;
              member->offset = i->offset;
              break;
            }
          }

          if (!a->node_type.is_valid()) {
            comp_thread->report_error(ERROR_CODE::NAME_ERROR, a->node_span,
                                      "Type '{}' has no member '{}'",
                                      cmp_t.name, member->name);
            return false;
          }
        }
        else if (struct_type == STRUCTURE_TYPE::FIXED_ARRAY) {
          const Type& arr_t = base->node_type;

          const ArrayStructure* as = arr_t.unchecked_base<ArrayStructure>();

          if (member->name == comp_thread->important_names.ptr) {
            {
              AtomicLock<Structures> structures = {};
              AtomicLock<StringInterner> strings = {};
              comp->services.get_multiple(&structures, &strings);
              a->node_type = to_type(find_or_make_pointer_structure(structures._ptr,
                                                                    strings._ptr,
                                                                    comp->platform_interface.ptr_size, as->base));
            }

            //TODO: do this outside of memory
            a->can_be_constant = false;
          }
          else  if (member->name == comp_thread->important_names.len) {
            a->node_type = comp_thread->builtin_types->t_u64;

            //TODO: make this a constant
            //set_runtime_flags(base, state, false, (u8)RVT::CONST);
          }
          else {
            comp_thread->report_error(ERROR_CODE::NAME_ERROR, a->node_span,
                                      "Type '{}' has no member '{}'",
                                      arr_t.name, member->name);
            return false;
          }
        }
        else {
          comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, a->node_span,
                                    "Type '{}' does not have any members (it is not a composite type)",
                                    base->node_type.name);
          return false;
        }

        ASSERT(a->node_type.is_valid());
        return true;
      }
    case AST_TYPE::INDEX_EXPR: {
        ASTIndexExpr* index_expr = (ASTIndexExpr*)a;
        AST_LOCAL base = index_expr->expr;
        AST_LOCAL index = index_expr->index;

        if (!base->node_type.is_valid()) {
          pass_meta_flags_up(&index_expr->meta_flags, &base->meta_flags);
          typer->push_node(base, {});
          return false;
        }

        if (!TYPE_TESTS::can_index(base->node_type)) {
          comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, base->node_span,
                                    "Cannot take index of type: {}",
                                    base->node_type.name);
          return false;
        }

        if (!index->node_type.is_valid()) {
          pass_meta_flags_down(&a->meta_flags, &base->meta_flags);
          pass_meta_flags_up(&a->meta_flags, &index->meta_flags);

          typer->push_node(index, comp_thread->builtin_types->t_u64);
          return false;
        }

        pass_meta_flags_down(&a->meta_flags, &index->meta_flags);

        ASSERT(index->node_type == comp_thread->builtin_types->t_u64);

        if (!TEST_MASK(index->meta_flags, META_FLAG::COMPTIME)) {
          comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, base->node_span,
                                    "Currently not implemented runtime indexing");
          return false;
        }


        a->node_type = base->node_type.unchecked_base<ArrayStructure>()->base;

        return true;
      }
    case AST_TYPE::TUPLE_LIT: {
        ASTTupleLitExpr* tup = (ASTTupleLitExpr*)a;

        if (!tup->named_type.is_valid() && tup->name != nullptr) {
          //Need to check that the types match
          const Global* ty_g;
          {
            auto names = comp->services.names.get();
            const GlobalName* nm = names->find_global_name(context->current_unit->available_names, tup->name);
            ASSERT(nm != nullptr);
            ty_g = nm->global;
            ASSERT(ty_g != nullptr);
          }

          if (ty_g->decl.type != comp_thread->builtin_types->t_type) {
            comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, tup->node_span,
                                      "'{}' was not a type", tup->name);
            return  false;
          }

          ASSERT(ty_g->constant_value != nullptr);
          memcpy_ts(&tup->named_type, 1, (const Type*)ty_g->constant_value, 1);
        }
        else if (infer_type.is_valid()) {
          tup->named_type = infer_type;
        }

        if (tup->named_type.is_valid()) {
          STRUCTURE_TYPE st = tup->named_type.struct_type();

          if (st == STRUCTURE_TYPE::COMPOSITE) {
            const CompositeStructure* cs = tup->named_type.unchecked_base<CompositeStructure>();

            if (cs->elements.size != tup->elements.count) {
              comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, tup->node_span,
                                        "'{}' expected {} elements. Received: {}",
                                        tup->name, cs->elements.size, tup->elements.count);
              return false;
            }

            auto cs_i = cs->elements.begin();
            const auto cs_end = cs->elements.end();

            bool pushed_any = false;

            FOR_AST(tup->elements, it) {
              ASSERT(cs_i != cs_end);

              if (!it->node_type.is_valid()) {
                typer->push_node(it, cs_i->type);
                pushed_any = true;
              }

              cs_i++;
            }

            ASSERT(cs_i == cs_end);

            if (pushed_any) {
              return false;
            }

          }
          else if (st == STRUCTURE_TYPE::TUPLE) {
            const TupleStructure* ts = tup->named_type.unchecked_base<TupleStructure>();

            if (ts->elements.size != tup->elements.count) {
              comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, tup->node_span,
                                        "'{}' expected {} elements. Received: {}",
                                        tup->name, ts->elements.size, tup->elements.count);
              return  false;
            }

            auto ts_i = ts->elements.begin();
            const auto ts_end = ts->elements.end();

            bool pushed_any = false;

            FOR_AST(tup->elements, it) {
              ASSERT(ts_i != ts_end);

              if (!it->node_type.is_valid()) {
                typer->push_node(it, ts_i->type);
                pushed_any = true;
              }

              ts_i++;
            }

            ASSERT(ts_i == ts_end);

            if (pushed_any) {
              return false;
            }

          }
          else {
            comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, tup->node_span,
                                      "Could not build composite literal for type: '{}'",
                                      tup->named_type.name);
            return false;
          }

          a->node_type = tup->named_type;
        }
        else {
          bool pushed_any = false;

          FOR_AST(tup->elements, it) {
            if (!it->node_type.is_valid()) {
              pass_meta_flags_up(&a->meta_flags, &it->meta_flags);
              typer->push_node(it, {});
              pushed_any = true;
            }
          }

          if (pushed_any) {
            return false;
          }

          Array<Type> element_types = {};

          FOR_AST(tup->elements, it) {
            pass_meta_flags_down(&a->meta_flags, &it->meta_flags);
            element_types.insert(it->node_type);
          }

          const Structure* ts;
          {
            AtomicLock<Structures> structures = {};
            AtomicLock<StringInterner> strings = {};
            comp->services.get_multiple(&structures, &strings);
            ts = find_or_make_tuple_structure(structures._ptr,
                                              strings._ptr,
                                              std::move(element_types));
          }

          a->node_type = to_type(ts);
        }

        ASSERT(a->node_type.is_valid());
        return true;
      }
    case AST_TYPE::ARRAY_EXPR: {
        ASTArrayExpr* arr_expr = (ASTArrayExpr*)a;

        if (infer_type.is_valid()) {
          if (infer_type.struct_type() != STRUCTURE_TYPE::FIXED_ARRAY) {
            comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, a->node_span,
                                      "Tried to infer an array literal as a non-array type: {}",
                                      infer_type.name);
            return false;
          }

          const ArrayStructure* as = infer_type.unchecked_base<ArrayStructure>();

          if (as->length != arr_expr->elements.count) {
            comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, a->node_span,
                                      "Array expected size {}. Actual size: {}",
                                      as->length, arr_expr->elements.count);
            return false;
          }

          Type base = as->base;



          bool pushed_any = false;
          FOR_AST(arr_expr->elements, it) {
            if (!it->node_type.is_valid()) {
              pass_meta_flags_up(&a->meta_flags, &it->meta_flags);
              typer->push_node(it, base);
              pushed_any = true;
            }
          }

          if (pushed_any) {
            return false;
          }

          a->node_type = infer_type;
        }
        else {

          AST_LINKED* l = arr_expr->elements.start;

          Type base = {};

          if (l) {
            AST_LOCAL base_test = l->curr;


            if (!base_test->node_type.is_valid()) {
              pass_meta_flags_up(&a->meta_flags, &base_test->meta_flags);
              typer->push_node(base_test, {});
              return false;
            }

            base = base_test->node_type;

            for (; l; l = l->next) {
              AST_LOCAL it = l->curr;

              if (!it->node_type.is_valid()) {
                pass_meta_flags_up(&a->meta_flags, &it->meta_flags);
                typer->push_node(it, base);
                return false;
              }
            }
          }

          if (!base.is_valid()) {
            comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, a->node_span,
                                      "Array type could not inferred\n"
                                      "This is probably because the array was empty (i.e. [])");
            return false;
          }

          const Structure* arr_s;
          {
            AtomicLock<Structures> structures = {};
            AtomicLock<StringInterner> strings = {};
            comp->services.get_multiple(&structures, &strings);

            arr_s = find_or_make_array_structure(structures._ptr,
                                                 strings._ptr,
                                                 base, arr_expr->elements.count);
          }

          //Create the type
          a->node_type = to_type(arr_s);
        }

        FOR_AST(arr_expr->elements, it) {
          pass_meta_flags_down(&a->meta_flags, &it->meta_flags);
        }

        ASSERT(a->node_type.is_valid());
        return true;
      }
    case AST_TYPE::ASCII_CHAR: {
        a->node_type = comp_thread->builtin_types->t_ascii;
        return true;
      }
    case AST_TYPE::ASCII_STRING: {
        ASTAsciiString* ascii = (ASTAsciiString*)a;
        const size_t len = ascii->string->len + 1;

        const Structure* s;
        {
          AtomicLock<Structures> structures = {};
          AtomicLock<StringInterner> strings = {};
          comp->services.get_multiple(&structures, &strings);

          s = find_or_make_array_structure(structures._ptr,
                                           strings._ptr,
                                           comp_thread->builtin_types->t_ascii, len);
        }

        a->node_type = to_type(s);
        return true;
      }
    case AST_TYPE::NUMBER: {
        ASTNumber* num = (ASTNumber*)a;

        if (num->suffix == nullptr) {
          if (infer_type.is_valid()) {
            if (infer_type.struct_type() != STRUCTURE_TYPE::INTEGER) {
              comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, a->node_span,
                                        "Can not infer a number as type '{}'",
                                        infer_type.name);
              return false;
            }

            const IntegerStructure* is = infer_type.unchecked_base<IntegerStructure>();

            u64 max_val = 0;
            if (is->is_signed) {
              max_val = bit_fill_lower<u64>((is->size * 8) - 1);
            }
            else {
              max_val = bit_fill_lower<u64>(is->size * 8);
            }

            if (num->num_value > max_val) {
              comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, a->node_span,
                                        "'{}' is too small to contain '{}'",
                                        infer_type.name, num->num_value);
              return false;
            }

            a->node_type = infer_type;
          }
          else {
            a->node_type = comp_thread->builtin_types->t_u64;
          }
        }
        else {
          if (num->suffix == comp_thread->builtin_types->t_i32.name) {
            a->node_type = comp_thread->builtin_types->t_i32;
          }
          else if (num->suffix == comp_thread->builtin_types->t_u32.name) {
            a->node_type = comp_thread->builtin_types->t_u32;
          }
          else if (num->suffix == comp_thread->builtin_types->t_i64.name) {
            a->node_type = comp_thread->builtin_types->t_i64;
          }
          else if (num->suffix == comp_thread->builtin_types->t_u64.name) {
            a->node_type = comp_thread->builtin_types->t_u64;
          }
          else {
            comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, a->node_span,
                                      "Invalid integer literal suffix type '{}'",
                                      num->suffix);
            return false;
          }
        }

        ASSERT(a->node_type.is_valid());
        return true;
      }

    case AST_TYPE::LINK: {
        ASTLink* imp = (ASTLink*)a;

        if (!imp->import_type->node_type.is_valid()) {
          typer->push_node(imp->import_type, comp_thread->builtin_types->t_type);
          return false;
        }

        imp->node_type = get_type_value(comp_thread, imp->import_type);
        if (comp_thread->is_panic()) {
          return false;
        }

        //FileLocation loc;
        //{
        //  auto strings = comp->services.strings.get();
        //
        //  loc = parse_file_location(comp_thread->build_options.lib_folder->string, imp->lib_file->string,
        //                            strings._ptr);
        //}

        ASSERT(imp->dynamic);//TODO: only support static links
        if (imp->dynamic) {
          FOR(comp->dyn_lib_imports, it) {
            if (it->path == imp->lib_file
                && imp->name == it->name) {
              //Already imported
              goto ALREADY_IMPORTED;
            }
          }



          {
            const Type& t = imp->node_type;
            ASSERT(t.struct_type() == STRUCTURE_TYPE::LAMBDA);

            IR::DynLibraryImport import_lib = {};
            import_lib.name = imp->name;
            import_lib.path = imp->lib_file;
            import_lib.label = comp->next_function_label(t.extract_base<SignatureStructure>());

            comp->dyn_lib_imports.insert(std::move(import_lib));
            imp->import_index = comp->dyn_lib_imports.size;
          }

        ALREADY_IMPORTED:
          ASSERT(a->node_type.is_valid());
        }

        return true;
      }

    case AST_TYPE::IDENTIFIER_EXPR: {
        //Because shadowing isn't allowed this should always work?
        ASTIdentifier* ident = (ASTIdentifier*)a;

        //TODO: Make this work
        //Currently fails because the declaration constant doesnt load in time
        //Might work for globals? Definitely doesnt work for locals
        ASSERT((ident->meta_flags & META_FLAG::COMPTIME) == 0);

        if (ident->id_type == ASTIdentifier::LOCAL) {
          Local* local = ident->local;

          ASSERT(local->decl.type.is_valid());

          a->node_type = local->decl.type;
          pass_meta_flags_down(&a->meta_flags, &local->decl.meta_flags);
        }
        else if (ident->id_type == ASTIdentifier::GLOBAL) {
          Global* glob = ident->global;

          a->node_type = glob->decl.type;
          pass_meta_flags_down(&a->meta_flags, &glob->decl.meta_flags);
        }
        else {
          comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, a->node_span,
                                    "Identifer type was invalid");
          return false;
        }

        ASSERT(a->node_type.is_valid());
        return true;
      }
    case AST_TYPE::CAST: {
        ASTCastExpr* cast_expr = (ASTCastExpr*)a;
        AST_LOCAL cast = cast_expr->expr;
        AST_LOCAL ty = cast_expr->type;

        if (!ty->node_type.is_valid()) {
          typer->push_node(ty, comp_thread->builtin_types->t_type);
          return false;
        }

        if (!cast->node_type.is_valid()) {
          pass_meta_flags_up(&a->meta_flags, &cast->meta_flags);
          typer->push_node(cast, {});
          return false;
        }

        //may set comptime to false
        //Sets the type
        cast_operator_type(comp, comp_thread, cast_expr);
        if (comp_thread->is_panic()) {
          return false;
        }

        pass_meta_flags_down(&a->meta_flags, &cast->meta_flags);
        return true;
      }
    case AST_TYPE::UNARY_OPERATOR: {
        return type_check_unary_operator(comp, comp_thread, typer, this_untyped);
      }
    case AST_TYPE::BINARY_OPERATOR: {
        ASTBinaryOperatorExpr* const bin_op = (ASTBinaryOperatorExpr*)a;

        AST_LOCAL left = bin_op->left;
        AST_LOCAL right = bin_op->right;

        //TODO: Can we do type inference?

        {
          bool pushed_any = false;

          if (!left->node_type.is_valid()) {
            pass_meta_flags_up(&a->meta_flags, &left->meta_flags);
            typer->push_node(left, {});
            pushed_any = true;
          }

          if (!right->node_type.is_valid()) {
            pass_meta_flags_up(&a->meta_flags, &right->meta_flags);
            typer->push_node(right, {});
            pushed_any = true;
          }

          if (pushed_any) {
            return false;
          }
        }

        pass_meta_flags_down(&a->meta_flags, &right->meta_flags);
        pass_meta_flags_down(&a->meta_flags, &left->meta_flags);

        //TODO: load constants early
#if 0
        if (!TEST_MASK(a->meta_flags, META_FLAG::COMPTIME)) {
          if (left->can_be_constant) {
            UnitID id = compile_and_execute(comp, context->current_unit->available_names, left, {}, );

            set_dependency(comp_thread, context, id);
          }
          else if (right->can_be_constant) {
            UnitID id = compile_and_execute(comp, context->current_unit->available_names, right, {});

            set_dependency(comp_thread, context, id);
          }
        }
#endif

        //TODO: inference
        type_check_binary_operator(comp, comp_thread, typer, this_untyped);
        if (comp_thread->is_panic()) {
          return false;
        }

        return true;
      }
    case AST_TYPE::IMPORT: {
        ASTImport* imp = (ASTImport*)a;
        AST_LOCAL expr = imp->expr_location;

        if (!expr->node_type.is_valid()) {
          typer->push_node(expr, {});
          return false;
        }

        if (expr->node_type.struct_type() != STRUCTURE_TYPE::FIXED_ARRAY) {
          comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->node_span,
                                    "#{} expression must be a character array\n"
                                    "Instead found: {}",
                                    comp_thread->intrinsics.import, expr->node_type.name);
          return false;
        }

        const auto* array_type = expr->node_type.unchecked_base<ArrayStructure>();

        if (array_type->base != comp_thread->builtin_types->t_ascii) {
          comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->node_span,
                                    "#{} expression must be a character array\n"
                                    "Expected base type: {}\n"
                                    "Instead found: {}",
                                    comp_thread->intrinsics.import, comp_thread->builtin_types->t_ascii.name, array_type->base.name);
          return false;
        }

        if (!TEST_MASK(expr->meta_flags, META_FLAG::COMPTIME)) {
          comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->node_span,
                                    "#{} expression must be a compile time constant",
                                    comp_thread->intrinsics.import);
          return false;
        }

        a->node_type = comp_thread->builtin_types->t_void;
        return true;
      }
    case AST_TYPE::FUNCTION_CALL: {
        ASTFunctionCallExpr* const call = (ASTFunctionCallExpr*)a;

        //TODO: Allow function execution at compile time
        //Means we need to have a way to know which functions to load
        a->meta_flags |= META_FLAG::MAKES_CALL;

        //TODO: Try to find the function call first? Then infer arguments

        FOR_AST(call->arguments, it) {

          if (!it->node_type.is_valid()) {
            pass_meta_flags_up(&a->meta_flags, &it->meta_flags);
            it->meta_flags |= META_FLAG::CALL_LEAF;

            typer->push_node(it, {});
            return false;
          }

        }

        FOR_AST(call->arguments, it) {
          pass_meta_flags_down(&a->meta_flags, &it->meta_flags);
        }

        compile_find_function_call(comp, comp_thread, context, call);
        if (comp_thread->is_panic()) {
          return false;
        }

        const auto* sig = call->sig;
        ASSERT(sig);

        const size_t size = sig->parameter_types.size;

        if (call->arguments.count != size) {
          comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, a->node_span,
                                    "Compiler linked a function with {} parameters for a call with {} arguments!",
                                    size, call->arguments.count);
          return false;
        }

        /*if (!TEST_MASK(a->meta_flags, META_FLAG::COMPTIME)) {
          FOR_AST(call->arguments, it) {
            if (can_compile_const_value(it)) {
              ConstantExprUnit* unit = comp->new_const_expr_unit(context->current_namespace);
              unit->expr = it;
              unit->stage = EXPR_COMP_STAGE::TYPED;

              comp->set_dep(context, unit);
            }
          }
        }*/

        //Last thing to do it set return type
        a->node_type = sig->return_type;

        ASSERT(a->node_type.is_valid());
        return true;
      }
    case AST_TYPE::ASSIGN: {
        ASTAssign* assign = (ASTAssign*)a;
        AST_LOCAL assign_to = assign->assign_to;
        AST_LOCAL value = assign->value;

        if (!assign_to->node_type.is_valid()) {
          typer->push_node(assign_to, {});
          return false;
        }

        if (!TEST_MASK(assign_to->meta_flags, META_FLAG::ASSIGNABLE)) {
          comp_thread->report_error(ERROR_CODE::CONST_ERROR, assign_to->node_span,
                                    "Cannot assign to non-assignable expression");
          return false;
        }

        if (!value->node_type.is_valid()) {
          typer->push_node(value, assign_to->node_type);
          return false;
        }

        a->node_type = comp_thread->builtin_types->t_void;
        return true;
      }
    case AST_TYPE::IF_ELSE: {
        ASTIfElse* const if_else = (ASTIfElse*)a;

        if (!if_else->condition->node_type.is_valid()) {
          typer->push_node(if_else->condition, comp_thread->builtin_types->t_bool);
          return false;
        }

        if (!if_else->if_statement->node_type.is_valid()) {
          typer->push_node(if_else->if_statement, {});
          return false;
        }

        if (if_else->else_statement != 0) {
          if (!if_else->else_statement->node_type.is_valid()) {
            typer->push_node(if_else->else_statement, {});
            return false;
          }
        }

        a->node_type = comp_thread->builtin_types->t_void;
        return true;
      }
    case AST_TYPE::WHILE: {
        ASTWhile* const while_loop = (ASTWhile*)a;

        if (!while_loop->condition->node_type.is_valid()) {
          typer->push_node(while_loop->condition, comp_thread->builtin_types->t_bool);
          return false;
        }

        if (!while_loop->statement->node_type.is_valid()) {
          typer->push_node(while_loop->statement, {});
          return false;
        }

        a->node_type = comp_thread->builtin_types->t_void;
        return true;
      }
    case AST_TYPE::BLOCK: {
        ASTBlock* block = (ASTBlock*)a;

        bool pushed_any = false;
        FOR_AST(block->block, it) {
          if (!it->node_type.is_valid()) {
            typer->push_node(it, {});
            pushed_any = true;
          }
        }

        if (pushed_any) {
          return false;
        }

        a->node_type = comp_thread->builtin_types->t_void;
        return true;
      }

    case AST_TYPE::GLOBAL_DECL: {
        ASTGlobalDecl* const decl = (ASTGlobalDecl*)a;

        AST_LOCAL decl_expr = decl->expr;

        if (decl->type_ast != nullptr) {
          if (!decl->type_ast->node_type.is_valid()) {
            typer->push_node(decl->type_ast, comp_thread->builtin_types->t_type);
            return false;
          }

          Type type = get_type_value(comp_thread, decl->type_ast);
          if (comp_thread->is_panic()) {
            return false;
          }

          if (!decl_expr->node_type.is_valid()) {
            typer->push_node(decl_expr, type);
            return false;
          }

          decl->type = type;
        }
        else {
          if (!decl_expr->node_type.is_valid()) {
            typer->push_node(decl_expr, {});
            return false;
          }

          decl->type = decl->expr->node_type;
        }

        ASSERT(decl->type.is_valid());

        if (decl->compile_time_const
            && (decl_expr == nullptr || !TEST_MASK(decl_expr->meta_flags, META_FLAG::COMPTIME))) {
          comp_thread->report_error(ERROR_CODE::CONST_ERROR, decl->node_span,
                                    "Compile time declaration '{}' must be initialized by a compile time expression",
                                    decl->name);
          return false;
        }

        //TODO: Do globals need to check for shadowing?
        ASSERT(decl->global_ptr != nullptr);
        decl->global_ptr->decl.type = decl->type;
        Global* g = decl->global_ptr;

#if 0
        if (decl_expr->can_be_constant) {
          g->is_constant = true;
          g->constant_value = comp->new_constant(decl->type.size());
          UnitID id = compile_and_execute(comp, context->current_unit->available_names, decl_expr, decl->type, g->constant_value);

          set_dependency(comp_thread, context, id);
        }
#endif



        a->node_type = comp_thread->builtin_types->t_void;
        return true;
      }
    case AST_TYPE::LOCAL_DECL: {
        ASTLocalDecl* const decl = (ASTLocalDecl*)a;

        AST_LOCAL decl_expr = decl->expr;

        if (decl->type_ast != nullptr) {
          if (!decl->type_ast->node_type.is_valid()) {
            typer->push_node(decl->type_ast, comp_thread->builtin_types->t_type);
            return false;
          }

          Type type = get_type_value(comp_thread, decl->type_ast);
          if (comp_thread->is_panic()) {
            return false;
          }

          if (!decl_expr->node_type.is_valid()) {
            typer->push_node(decl_expr, type);
            return false;
          }

          decl->type = type;
        }
        else {
          if (!decl_expr->node_type.is_valid()) {
            typer->push_node(decl_expr, {});
            return false;
          }

          decl->type = decl->expr->node_type;
        }

        ASSERT(decl->type.is_valid());

        if (decl->compile_time_const
            && (decl_expr == nullptr || !TEST_MASK(decl_expr->meta_flags, META_FLAG::COMPTIME))) {
          comp_thread->report_error(ERROR_CODE::CONST_ERROR, decl->node_span,
                                    "Compile time declaration '{}' must be initialized by a compile time expression",
                                    decl->name);
          return false;
        }

        ASSERT(decl->type.is_valid());

        Local* const loc = decl->local_ptr;
        loc->decl.type = decl->type;
        if (decl->compile_time_const) {
          loc->decl.meta_flags |= META_FLAG::COMPTIME;
          loc->decl.meta_flags |= META_FLAG::CONST;
          loc->decl.meta_flags &= ~META_FLAG::ASSIGNABLE;
        }
        else {
          loc->decl.meta_flags &= ~META_FLAG::COMPTIME;
          loc->decl.meta_flags |= META_FLAG::ASSIGNABLE;
        }

        ASSERT(loc->decl.type.is_valid());

        if (TEST_MASK(loc->decl.meta_flags, META_FLAG::COMPTIME) && !TEST_MASK(decl_expr->meta_flags, META_FLAG::COMPTIME)) {
          comp_thread->report_error(ERROR_CODE::CONST_ERROR, decl->node_span,
                                    "Cannot initialize a compile time constant with "
                                    "a non compile time constant value");
          return false;
        }

        if (TEST_MASK(loc->decl.meta_flags, META_FLAG::COMPTIME)) {
          ASSERT(TEST_MASK(decl_expr->meta_flags, META_FLAG::COMPTIME));
          ASSERT(decl_expr->can_be_constant);

          comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, decl_expr->node_span, "Currently don't support compile time values");
#if 0
          loc->is_constant = true;
          loc->constant = comp->new_constant(decl->type.size());

          UnitID id = compile_and_execute(comp, context->current_unit->available_names, decl_expr, decl->type, loc->constant);

          set_dependency(comp_thread, context, id);
#endif
        }

        a->node_type = comp_thread->builtin_types->t_void;
        return true;
      }
    case AST_TYPE::RETURN: {
        ASTReturn* ret = (ASTReturn*)a;

        ASSERT(typer->return_type.is_valid());

        if (ret->expr != nullptr) {
          if (!ret->expr->node_type.is_valid()) {
            typer->push_node(ret->expr, typer->return_type);
            return false;
          }
        }
        else {
          if (typer->return_type != comp_thread->builtin_types->t_void) {
            comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, ret->node_span,
                                      "Return type was {}, yet the expression was empty\n",
                                      typer->return_type.name);
            return false;
          }
        }

        a->node_type = comp_thread->builtin_types->t_void;
        return true;
      }

    case AST_TYPE::STRUCT: {
        ASTStructBody* body = (ASTStructBody*)a;

        bool pushed_any = false;
        FOR_AST(body->elements, it) {
          if (!it->node_type.is_valid()) {
            typer->push_node(it, {});
            pushed_any = true;
          }
        }

        if (pushed_any) {
          return  false;
        }

        //Build the new structure
        {
          CompositeStructure* cmp_s;
          {
            AtomicLock<Structures> structures = {};
            AtomicLock<StringInterner> strings = {};
            comp->services.get_multiple(&structures, &strings);
            cmp_s = STRUCTS::new_composite_structure(structures._ptr,
                                                     strings._ptr);
          }
          uint32_t current_size = 0;
          uint32_t current_alignment = 0;

          FOR_AST(body->elements, it) {
            ASTTypedName* tn = (ASTTypedName*)it;

            cmp_s->elements.insert_uninit(1);
            auto* b = cmp_s->elements.back();

            b->type = get_type_value(comp_thread, tn->type);
            if (comp_thread->is_panic()) {
              return false;
            }
            b->name = tn->name;
            b->offset = current_size;

            uint32_t this_align = b->type.structure->alignment;

            current_size = (uint32_t)ceil_to_n(current_size, this_align);
            current_size += b->type.structure->size;

            current_alignment = larger(this_align, current_alignment);
          }

          cmp_s->declaration = body;
          cmp_s->size = current_size;
          cmp_s->alignment = current_alignment;

          body->actual_type = to_type(cmp_s);
        }

        body->node_type = comp_thread->builtin_types->t_type;

        return true;
      }
    case AST_TYPE::TYPED_NAME: {
        ASTTypedName* name = (ASTTypedName*)a;

        if (!name->type->node_type.is_valid()) {
          typer->push_node(name->type, comp_thread->builtin_types->t_type);
          return false;
        }

        name->node_type = get_type_value(comp_thread, name->type);
        if (comp_thread->is_panic()) {
          return false;
        }

        Local* loc = name->local_ptr;

        loc->decl.type = name->node_type;
        loc->decl.meta_flags |= META_FLAG::ASSIGNABLE;
        ASSERT(a->node_type.is_valid());
        return true;
      }
  }


  comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, a->node_span,
                            "Not yet implemented type checking for this node. Node ID: {}", (usize)a->ast_type);
  return false;
}

static void type_check_ast(CompilerGlobals* comp,
                           CompilerThread* comp_thread,
                           Context* const context,
                           AST_LOCAL ast) {
  Typer typer = {};

  typer.push_node(ast, {});
  typer.load_new_nodes();

  while (typer.untyped_stack.size > 0) {
    UntypedNode* n = typer.untyped_stack.back();

    bool finished = type_check_single_node(comp, comp_thread, context, &typer, n);
    if (comp_thread->is_panic()) {
      return;
    }

    if (finished) {
      ASSERT(typer.new_untyped_stack.size == 0);
      typer.untyped_stack.pop();
    }
    else {
      typer.load_new_nodes();
    }
  }

  ASSERT(ast->node_type.is_valid());
  ASSERT(typer.new_untyped_stack.size == 0);
  ASSERT(typer.untyped_stack.size == 0);
}

static IR::RuntimeReference compile_bytecode(CompilerGlobals* const comp,
                                             CompilerThread* const comp_thread,
                                             Context* const context,
                                             IR::Builder* const builder,
                                             AST_LOCAL expr);

static IR::RuntimeReference compile_function_call(CompilerGlobals* const comp,
                                                  CompilerThread* const comp_thread,
                                                  Context* const context,
                                                  IR::Builder* const builder,
                                                  const ASTFunctionCallExpr* const call) {
  TRACING_FUNCTION();

  const auto* sig_struct = call->sig;

  bool has_return = sig_struct->return_type != comp_thread->builtin_types->t_void;

  Array<IR::SingleVal> values;
  values.reserve_total(call->arguments.count + (usize)has_return);

  FOR_AST(call->arguments, it) {
    IR::RuntimeReference val = compile_bytecode(comp, comp_thread, context, builder, it);
    if (comp_thread->is_panic()) {
      return IR::HELPERS::no_value();
    }

    if (val.is_constant) {
      val = IR::HELPERS::copy_constant(builder, val.constant, val.type);
    }

    ASSERT(!val.is_constant);
    values.insert(IR::SingleVal{
      val.base,
        val.offset,
        val.type.struct_format(),
    });
  }

  ASSERT(values.size == call->arguments.count);


  if (has_return) {
    IR::RuntimeReference ref = builder->new_temporary(sig_struct->return_type);

    values.insert(IR::SingleVal{
      ref.base,
        ref.offset,
        ref.type.struct_format(),
    });
  }

  {
    IR::Types::Call args = {};
    args.label = call->label;
    args.n_values = (u32)values.size;
    args.values = (const u8*)values.data;

    IR::Emit::Call(builder->ir_bytecode, args);
  }

  if (has_return) {
    IR::SingleVal* sv = values.back();
    IR::RuntimeReference return_value;

    return_value.base = sv->v;
    return_value.is_constant = false;
    return_value.offset = sv->v_offset;
    return_value.type = sig_struct->return_type;

    return return_value;
  }
  else {
    return IR::HELPERS::no_value();
  }
}

template<typename T>
static T to_ir_V(const IR::RuntimeReference& val) {
  ASSERT(!val.is_constant);

  T v = {};
  v.val = val.base;
  v.offset = val.offset;
  v.format = val.type.struct_format();
  return v;
}

template<typename T>
static T to_ir_V_IM32(const IR::RuntimeReference& val, u32 im) {
  ASSERT(!val.is_constant);

  T v = {};
  v.val = val.base;
  v.offset = val.offset;
  v.format = val.type.struct_format();
  v.im32 = im;
  return v;
}

template<typename T>
static T to_ir_V_V(const IR::RuntimeReference& to, const IR::RuntimeReference& from) {
  ASSERT(!to.is_constant);
  ASSERT(!from.is_constant);

  T vv = {};
  vv.to = to.base;
  vv.t_offset = to.offset;
  vv.t_format = to.type.struct_format();
  vv.from = from.base;
  vv.f_offset = from.offset;
  vv.f_format = from.type.struct_format();
  return vv;
}

template<typename T>
static T to_ir_V_C(const IR::RuntimeReference& to, const IR::RuntimeReference& from) {
  ASSERT(!to.is_constant);
  ASSERT(from.is_constant);

  T vc = {};
  vc.to = to.base;
  vc.t_offset = to.offset;
  vc.t_size = to.type.size();
  vc.data = static_cast<u8*>(from.constant) + from.offset;
  vc.d_size = from.type.size();
  return vc;
}

IR::RuntimeReference CASTS::int_to_int(IR::Builder* const builder,
                                       const Type& from, const Type& to,
                                       const IR::RuntimeReference& val) {
  ASSERT(to.is_valid() && to.struct_type() == STRUCTURE_TYPE::INTEGER);
  ASSERT(from.is_valid() && from.struct_type() == STRUCTURE_TYPE::INTEGER);

  if (to == from) {
    return val;
  }

  IR::RuntimeReference temp = builder->new_temporary(from);
  IR::HELPERS::copycast_value(builder, temp, val);
  IR::RuntimeReference r = builder->new_temporary(to);
  IR::HELPERS::copycast_value(builder, r, temp);
  return r;
}

IR::RuntimeReference CASTS::no_op(IR::Builder* const builder,
                                  const Type& from, const Type& to,
                                  const IR::RuntimeReference& val) {
  return val;
}

IR::RuntimeReference CASTS::take_address(IR::Builder* const builder,
                                         const Type& from, const Type& to,
                                         const IR::RuntimeReference& val) {
  ASSERT(from.is_valid());
  return IR::HELPERS::take_address(builder, val, to);
}

IR::RuntimeReference load_data_memory(CompilerGlobals* comp, IR::Builder* builder, const Global* global) {
  builder->new_global_reference({ global->decl.type, global->dynamic_init_index });
  const PointerStructure* ps;
  {
    AtomicLock<Structures> structures = {};
    AtomicLock<StringInterner> strings = {};
    comp->services.get_multiple(&structures, &strings);

    ps = find_or_make_pointer_structure(structures._ptr, strings._ptr, comp->platform_interface.ptr_size, global->decl.type);
  }

  IR::RuntimeReference v = builder->new_temporary(to_type(ps));

  {
    IR::Types::GlobalAddress args = {};
    args.val = v.base;
    args.offset = v.offset;
    args.format = v.type.struct_format();
    args.im32 = global->dynamic_init_index;

    IR::Emit::GlobalAddress(builder->ir_bytecode, args);
  }

  return IR::HELPERS::dereference(builder, v, global->decl.type);
}

//Note: Recursive 
static IR::RuntimeReference compile_bytecode(CompilerGlobals* const comp,
                                             CompilerThread* const comp_thread,
                                             Context* const context,
                                             IR::Builder* const builder,
                                             AST_LOCAL expr) {
  TRACING_FUNCTION();

  switch (expr->ast_type) {
    case AST_TYPE::NAMED_TYPE: {
        ASTNamedType* nt = (ASTNamedType*)expr;

        ASSERT(context->comptime_compilation);

        Type* struct_c = comp->new_constant<Type>();
        memcpy_ts(struct_c, 1, &nt->actual_type, 1);

        return IR::HELPERS::as_constant(struct_c, comp_thread->builtin_types->t_type);
      }
    case AST_TYPE::ARRAY_TYPE: {
        ASTArrayType* nt = (ASTArrayType*)expr;

        ASSERT(context->comptime_compilation);

        Type* struct_c = comp->new_constant<Type>();
        memcpy_ts(struct_c, 1, &nt->actual_type, 1);

        return IR::HELPERS::as_constant(struct_c, comp_thread->builtin_types->t_type);
      }
    case AST_TYPE::PTR_TYPE: {
        ASTPtrType* nt = (ASTPtrType*)expr;

        ASSERT(context->comptime_compilation);

        Type* struct_c = comp->new_constant<Type>();
        memcpy_ts(struct_c, 1, &nt->actual_type, 1);

        return IR::HELPERS::as_constant(struct_c, comp_thread->builtin_types->t_type);
      }
    case AST_TYPE::LAMBDA_TYPE: {
        ASTLambdaType* nt = (ASTLambdaType*)expr;

        ASSERT(context->comptime_compilation);

        Type* struct_c = comp->new_constant<Type>();
        memcpy_ts(struct_c, 1, &nt->actual_type, 1);

        return IR::HELPERS::as_constant(struct_c, comp_thread->builtin_types->t_type);
      }
    case AST_TYPE::TUPLE_TYPE: {
        ASTTupleType* nt = (ASTTupleType*)expr;

        ASSERT(context->comptime_compilation);

        Type* struct_c = comp->new_constant<Type>();
        memcpy_ts(struct_c, 1, &nt->actual_type, 1);

        return IR::HELPERS::as_constant(struct_c, comp_thread->builtin_types->t_type);
      }
    case AST_TYPE::STRUCT_EXPR: {
        ASTStructExpr* se = (ASTStructExpr*)expr;
        ASTStructBody* s = (ASTStructBody*)se->struct_body;

        ASSERT(context->comptime_compilation);

        Type* struct_c = comp->new_constant<Type>();
        memcpy_ts(struct_c, 1, &s->actual_type, 1);

        return IR::HELPERS::as_constant(struct_c, comp_thread->builtin_types->t_type);
      }
    case AST_TYPE::LAMBDA_EXPR: {
        ASTLambdaExpr* le = (ASTLambdaExpr*)expr;
        ASTLambda* l = (ASTLambda*)le->lambda;

        ASSERT(context->comptime_compilation);

        IR::GlobalLabel* label = comp->new_constant<IR::GlobalLabel>();
        *label = l->function->signature.label;

        ASSERT(le->node_type.struct_type() == STRUCTURE_TYPE::LAMBDA);

        return IR::HELPERS::as_constant(label, le->node_type);
      }
    case AST_TYPE::MEMBER_ACCESS: {
        ASTMemberAccessExpr* member_e = (ASTMemberAccessExpr*)expr;
        AST_LOCAL m_base = member_e->expr;

        STRUCTURE_TYPE st = m_base->node_type.struct_type();
        if (st == STRUCTURE_TYPE::COMPOSITE) {
          IR::RuntimeReference obj = compile_bytecode(comp, comp_thread,
                                                      context, builder, m_base);
          if (comp_thread->is_panic()) {
            return IR::HELPERS::no_value();
          }

          return IR::HELPERS::sub_object(obj, member_e->offset, expr->node_type);
        }
        else if (st == STRUCTURE_TYPE::FIXED_ARRAY) {
          if (member_e->name == comp->important_names.ptr) {
            IR::RuntimeReference obj = compile_bytecode(comp, comp_thread,
                                                        context, builder, m_base);
            Type ptr = member_e->node_type;
            ASSERT(ptr.struct_type() == STRUCTURE_TYPE::POINTER);

            return IR::HELPERS::arr_to_ptr(builder, obj, ptr);
          }
          else if (member_e->name == comp->important_names.len) {
            const ArrayStructure* as = m_base->node_type.unchecked_base<ArrayStructure>();

            u64 val = as->length;

            const size_t size = sizeof(val);

            uint8_t* val_c = comp->new_constant(size);
            memcpy_ts(val_c, size, (uint8_t*)&val, size);

            return IR::HELPERS::as_constant(val_c, comp_thread->builtin_types->t_u64);
          }
          else {
            comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, expr->node_span, "No semantics supported for the member \"{}\" on an array", member_e->name);
            return IR::HELPERS::no_value();
          }
        }
        else {
          comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, expr->node_span, "Type \"{}\" does not appear to support member access", m_base->node_type.name);
          return IR::HELPERS::no_value();
        }
      }
    case AST_TYPE::INDEX_EXPR: {
        const Type base_type = expr->node_type;
        const size_t base_size = base_type.size();

        ASTIndexExpr* index = (ASTIndexExpr*)expr;
        AST_LOCAL index_expr = index->expr;
        AST_LOCAL index_index = index->index;

        ASSERT(TYPE_TESTS::can_index(index_expr->node_type));

        IR::RuntimeReference arr = compile_bytecode(comp, comp_thread,
                                                    context, builder, index_expr);
        if (comp_thread->is_panic()) {
          return IR::HELPERS::no_value();
        }

        IR::RuntimeReference index_val = compile_bytecode(comp, comp_thread,
                                                          context, builder, index_index);
        if (comp_thread->is_panic()) {
          return IR::HELPERS::no_value();
        }

        if (index_val.is_constant && TYPE_TESTS::is_array(arr.type)) {
          ASSERT(index_val.type == comp_thread->builtin_types->t_u64);
          u64 offset = x64_from_bytes(index_val.constant);
          offset *= base_size;
          return IR::HELPERS::sub_object(arr, offset, expr->node_type);
        }
        else {
          //Do the sizeing at runtime
          if (TYPE_TESTS::is_array(arr.type)) {
            const PointerStructure* p;

            {
              auto structs = comp->services.structures.get();
              auto strings = comp->services.strings.get();
              p = find_or_make_pointer_structure(structs._ptr, strings._ptr, comp->platform_interface.ptr_size, base_type);
            }

            arr = IR::HELPERS::take_address(builder, arr, to_type(p));
          }

          //arr is now always a pointer!

          const BinOpEmitInfo add_info{
            MainSide::LEFT,
            arr.type,
            &BinOpArgs::emit_add_ints,
          };

          BinOpArgs add_args = {
            &add_info,
            comp,
            builder,
            arr,
            index_val,
          };

          IR::RuntimeReference moved_ptr = add_args.emit();

          return IR::HELPERS::dereference(builder, moved_ptr, base_type);
        }
      }
    case AST_TYPE::TUPLE_LIT: {
        ASSERT(expr->node_type.struct_type() == STRUCTURE_TYPE::COMPOSITE);
        const auto* cpst = expr->node_type.unchecked_base<CompositeStructure>();

        ASTTupleLitExpr* lit = (ASTTupleLitExpr*)expr;

        IR::RuntimeReference tup_lit = {};

        if (expr->can_be_constant) {
          tup_lit = IR::HELPERS::as_constant(comp->constants_single_threaded.alloc_no_construct(cpst->size),
                                             expr->node_type);
        }
        else {
          tup_lit = builder->new_temporary(expr->node_type);
        }

        auto i_t = cpst->elements.begin();

        FOR_AST(lit->elements, it) {
          IR::RuntimeReference v = compile_bytecode(comp, comp_thread, context, builder, it);
          if (comp_thread->is_panic()) {
            return IR::HELPERS::no_value();
          }

          if (tup_lit.is_constant) {
            ASSERT(v.is_constant);
            ASSERT(v.type == i_t->type);
            memcpy_s(tup_lit.constant + i_t->offset, i_t->type.size(), v.constant, v.type.size());
          }
          else {
            IR::RuntimeReference member = IR::HELPERS::sub_object(tup_lit, i_t->offset, i_t->type);
            IR::HELPERS::copycast_value(builder, member, v);
          }

          ++i_t;
        }
        return tup_lit;
      }
    case AST_TYPE::ARRAY_EXPR: {
        const auto* const arr_type = expr->node_type.unchecked_base<ArrayStructure>();

        const Type base_type = arr_type->base;

        //const size_t full_align = arr_type->alignment;
        const size_t full_size = arr_type->size;

        ASTArrayExpr* arr_expr = (ASTArrayExpr*)expr;

        IR::RuntimeReference arr = {};

        if (expr->can_be_constant) {
          arr = IR::HELPERS::as_constant(comp->constants_single_threaded.alloc_no_construct(arr_type->size),
                                         expr->node_type);
        }
        else {
          arr = builder->new_temporary(expr->node_type);
        }

        usize count = 0;

        FOR_AST(arr_expr->elements, it) {
          IR::RuntimeReference el = compile_bytecode(comp, comp_thread, context, builder, it);
          if (comp_thread->is_panic()) {
            return IR::HELPERS::no_value();
          }

          ASSERT(el.type == base_type);
          usize offset = count * base_type.size();

          if (arr.is_constant) {
            ASSERT(el.is_constant);
            ASSERT(el.type == arr_type->base);
            memcpy_s(arr.constant + offset, base_type.size(), el.constant, base_type.size());
          }
          else {
            IR::RuntimeReference element_ref = IR::HELPERS::sub_object(arr, offset, base_type);
            IR::HELPERS::copycast_value(builder, element_ref, el);
          }

          count += 1;
        }

        return arr;
      }
    case AST_TYPE::ASCII_CHAR: {
        ASTAsciiChar* ch = (ASTAsciiChar*)expr;

        char* char_c = comp->new_constant<char>();
        *char_c = ch->character;

        return IR::HELPERS::as_constant(char_c, ch->node_type);
      }
    case AST_TYPE::ASCII_STRING: {
        ASTAsciiString* st = (ASTAsciiString*)expr;

        const auto* const arr_type = expr->node_type.unchecked_base<ArrayStructure>();

        const size_t size = arr_type->size;
        char* string_c = (char*)comp->new_constant(size);
        memcpy_ts(string_c, size, st->string->string, size);

        return IR::HELPERS::as_constant(string_c, st->node_type);
      }
    case AST_TYPE::NUMBER: {
        ASTNumber* num = (ASTNumber*)expr;

        const size_t size = expr->node_type.structure->size;

        uint8_t* val_c = comp->new_constant(size);
        memcpy_ts(val_c, size, (uint8_t*)&num->num_value, size);

        return IR::HELPERS::as_constant(val_c, num->node_type);
      }
    case AST_TYPE::IDENTIFIER_EXPR: {
        ASTIdentifier* ident = (ASTIdentifier*)expr;

        if (ident->id_type == ASTIdentifier::LOCAL) {
          Local* local = ident->local;

          if (local->is_constant) {
            return IR::HELPERS::as_constant(local->constant, local->decl.type);
          }
          else {
            return IR::HELPERS::as_reference(local->val, local->decl.type);
          }
        }
        else if (ident->id_type == ASTIdentifier::GLOBAL) {
          const Global* glob = ident->global;
          ASSERT(glob != nullptr);

          if (glob->is_constant) {
            return IR::HELPERS::as_constant(glob->constant_value, glob->decl.type);
          }
          else {
            return load_data_memory(comp, builder, glob);
          }
        }

        comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, expr->node_span,
                                  "Invalid or missing identifier type: {}", ident->id_type);
        return IR::HELPERS::no_value();
      }
    case AST_TYPE::LINK: {
        ASTLink* li = (ASTLink*)expr;

        IR::DynLibraryImport* imp = comp->dyn_lib_imports.data + (li->import_index - 1);

        IR::GlobalLabel* label_holder = comp->new_constant<IR::GlobalLabel>();
        *label_holder = imp->label;

        ASSERT(li->node_type.struct_type() == STRUCTURE_TYPE::LAMBDA);

        return IR::HELPERS::as_constant(label_holder, li->node_type);
      }
    case AST_TYPE::CAST: {
        const ASTCastExpr* const cast = (ASTCastExpr*)expr;
        IR::RuntimeReference ref = compile_bytecode(comp, comp_thread,
                                                    context, builder, cast->expr);
        if (comp_thread->is_panic()) {
          return IR::HELPERS::no_value();
        }

        return cast->emit(builder, cast->expr->node_type, cast->node_type, ref);;
      }
    case AST_TYPE::UNARY_OPERATOR: {
        const ASTUnaryOperatorExpr* const un_op = (ASTUnaryOperatorExpr*)expr;

        IR::RuntimeReference ref = compile_bytecode(comp, comp_thread,
                                                    context, builder, un_op->expr);
        if (comp_thread->is_panic()) {
          return IR::HELPERS::no_value();
        }

        UnOpArgs args = {
          &un_op->emit_info,
          comp,
          builder,
          ref,
        };

        return args.emit();
      }
    case AST_TYPE::BINARY_OPERATOR: {
        const ASTBinaryOperatorExpr* const bin_op = (ASTBinaryOperatorExpr*)expr;
        AST_LOCAL left = bin_op->left;
        AST_LOCAL right = bin_op->right;

        IR::RuntimeReference temp_left = compile_bytecode(comp, comp_thread,
                                                          context, builder, left);
        if (comp_thread->is_panic()) {
          return IR::HELPERS::no_value();
        }

        IR::RuntimeReference temp_right = compile_bytecode(comp, comp_thread,
                                                           context, builder, right);
        if (comp_thread->is_panic()) {
          return IR::HELPERS::no_value();
        }

        BinOpArgs args = {
          &bin_op->emit_info,
          comp,
          builder,
          temp_left,
          temp_right,
        };


        return args.emit();
      }
    case AST_TYPE::FUNCTION_CALL: {
        return compile_function_call(comp, comp_thread, context, builder, (ASTFunctionCallExpr*)expr);
      }
    default: {
        //Invalid enum type
        //probably just didnt get around to supporting it
        comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, expr->node_span,
                                  "Invalid expression type: {}", ast_type_string(expr->ast_type));
        return IR::HELPERS::no_value();
      }
  }
}

void compile_bytecode_of_statement(CompilerGlobals* const comp,
                                   CompilerThread* const comp_thread,
                                   Context* const context,
                                   IR::Builder* const builder,
                                   AST_LOCAL const statement) {
  switch (statement->ast_type) {
    case AST_TYPE::ASSIGN: {
        ASTAssign* assign = (ASTAssign*)statement;

        builder->start_expression();
        IR::RuntimeReference assign_to = compile_bytecode(comp, comp_thread,
                                                          context, builder, assign->assign_to);
        if (comp_thread->is_panic()) {
          return;
        }

        IR::RuntimeReference v = compile_bytecode(comp, comp_thread,
                                                  context, builder, assign->value);
        if (comp_thread->is_panic()) {
          return;
        }

        IR::HELPERS::copycast_value(builder, assign_to, v);
        builder->end_expression();

        return;
      }
    case AST_TYPE::BLOCK: {
        ASTBlock* block = (ASTBlock*)statement;

        FOR_AST(block->block, it) {
          compile_bytecode_of_statement(comp, comp_thread, context, builder, it);
          if (comp_thread->is_panic()) {
            return;
          }
        }

        return;
      }
    case AST_TYPE::RETURN: {
        ASTReturn* ret = (ASTReturn*)statement;

        if (ret->expr != nullptr) {
          builder->start_expression();
          IR::RuntimeReference r = compile_bytecode(comp, comp_thread,
                                                    context, builder, ret->expr);
          if (comp_thread->is_panic()) {
            return;
          }

          IR::Emit::Return(builder->ir_bytecode, to_ir_V<IR::Types::Return>(r));
          builder->end_expression();
        }

        return;
      }
    case AST_TYPE::WHILE: {
        ASTWhile* const while_loop = (ASTWhile*)statement;

        IR::LocalLabel loop_label = builder->new_control_block();
        IR::LocalLabel escape_label = builder->new_control_block();

        builder->end_control_block();

        {
          builder->start_control_block(loop_label);
          builder->start_expression();

          IR::RuntimeReference cond = compile_bytecode(comp, comp_thread,
                                                       context, builder, while_loop->condition);
          if (comp_thread->is_panic()) {
            return;
          }

          ASSERT(cond.type == comp_thread->builtin_types->t_bool);

          IR::Types::IfSplit vll;
          vll.val = cond.base;
          vll.offset = cond.offset;
          vll.format = cond.type.struct_format();
          vll.label_if = loop_label;
          vll.label_else = escape_label;

          //Conditional jump out of the loop
          IR::Emit::IfSplit(builder->ir_bytecode, vll);

          builder->end_expression();
        }


        //loop branch
        compile_bytecode_of_statement(comp, comp_thread, context, builder, while_loop->statement);
        if (comp_thread->is_panic()) {
          return;
        }

        IR::Emit::Jump(builder->ir_bytecode, IR::Types::Jump{ loop_label });

        builder->end_control_block();
        builder->start_control_block(escape_label);
        return;
      }
    case AST_TYPE::IF_ELSE: {
        ASTIfElse* const if_else = (ASTIfElse*)statement;

        bool has_else = if_else->else_statement != nullptr;

        IR::LocalLabel if_label = builder->new_control_block();
        IR::LocalLabel escape_label = builder->new_control_block();

        IR::LocalLabel else_label;
        if (has_else) {
          else_label = builder->new_control_block();
        }

        {
          builder->start_expression();

          IR::RuntimeReference cond = compile_bytecode(comp, comp_thread,
                                                       context, builder, if_else->condition);
          if (comp_thread->is_panic()) {
            return;
          }

          ASSERT(cond.type == comp_thread->builtin_types->t_bool);

          IR::Types::IfSplit vll;
          vll.val = cond.base;
          vll.offset = cond.offset;
          vll.format = cond.type.struct_format();
          vll.label_if = if_label;

          if (has_else) {
            vll.label_else = else_label;
          }
          else {
            vll.label_else = escape_label;
          }

          IR::Emit::IfSplit(builder->ir_bytecode, vll);

          builder->end_expression();

          builder->end_control_block();
        }

        {
          builder->start_control_block(if_label);

          //If branch
          compile_bytecode_of_statement(comp, comp_thread, context, builder, if_else->if_statement);
          if (comp_thread->is_panic()) {
            return;
          }

          IR::Emit::Jump(builder->ir_bytecode, IR::Types::Jump{ escape_label });

          builder->end_control_block();
        }

        if (if_else->else_statement != 0) {
          builder->start_control_block(else_label);

          compile_bytecode_of_statement(comp, comp_thread, context, builder, if_else->else_statement);
          if (comp_thread->is_panic()) {
            return;
          }

          IR::Emit::Jump(builder->ir_bytecode, IR::Types::Jump{ escape_label });

          builder->end_control_block();
        }

        builder->start_control_block(escape_label);
        return;
      }
    case AST_TYPE::GLOBAL_DECL: {
        //TODO: actually do this;
        INVALID_CODE_PATH("TODO: move global decl compilation to the correct place");
      }
    case AST_TYPE::LOCAL_DECL: {
        ASTLocalDecl* const decl = (ASTLocalDecl*)statement;

        Local* const local = decl->local_ptr;

        if (!TEST_MASK(local->decl.meta_flags, META_FLAG::COMPTIME)) {
          ASSERT(!local->is_constant);

          builder->start_expression();
          IR::RuntimeReference r = compile_bytecode(comp, comp_thread,
                                                    context, builder, decl->expr);
          if (comp_thread->is_panic()) {
            return;
          }

          local->val = builder->new_variable(local->decl.type);

          IR::RuntimeReference local_ref = IR::HELPERS::as_reference(local->val, local->decl.type);
          IR::HELPERS::copycast_value(builder, local_ref, r);

          builder->end_expression();
        }
        else {
          ASSERT(local->is_constant && local->constant != nullptr);
        }

        return;
      }
    default: {
        AST_LOCAL expr = statement;

        builder->start_expression();
        [[maybe_unused]] auto _ = compile_bytecode(comp, comp_thread, context, builder, expr);
        builder->end_expression();
        return;
      }
  }
}

void start_ir(IR::Builder* builder, AST_ARR params) {
  ASSERT(builder->signature != nullptr);

  IR::LocalLabel l = builder->new_control_block();
  builder->start_control_block(l);

  builder->start_expression();


  const Type* parameters = builder->signature->parameter_types.begin();
  const Type* const parameters_end = builder->signature->parameter_types.end();

  FOR_AST(params, p) {
    ASSERT(parameters < parameters_end);
    ASSERT(p->node_type == *parameters);
    ASSERT(p->ast_type == AST_TYPE::TYPED_NAME);
    ASTTypedName* n = (ASTTypedName*)p;

    n->local_ptr->val = builder->new_variable(*parameters);
    parameters += 1;
  }

  ASSERT(parameters == parameters_end);
  IR::Emit::StartFunc(builder->ir_bytecode, IR::Types::StartFunc{});

  builder->end_expression();
}

void finish_ir(CompilerGlobals* comp, IR::Builder* builder) {
  if (comp->print_options.finished_ir) {
    IR::print_ir(builder);
  }

  comp->finished_irs.push_back(builder);
}

void compile_expression_as_func_ir(CompilerGlobals* const comp,
                                   CompilerThread* const comp_thread,
                                   Context* const context,
                                   IR::Builder* const builder,
                                   AST_LOCAL expr) {
  TRACING_FUNCTION();

  const SignatureStructure* s = nullptr;
  {
    Array<Type> args = {};
    AtomicLock<Structures> structures = {};
    AtomicLock<StringInterner> strings = {};
    comp->services.get_multiple(&structures, &strings);
    s = find_or_make_lamdba_structure(structures._ptr,
                                      strings._ptr,
                                      comp->platform_interface.ptr_size,
                                      comp_thread->build_options.default_calling_convention,
                                      std::move(args), expr->node_type);
  }



  start_ir(builder, AST_ARR{ 0,0 });

  builder->start_expression();
  IR::RuntimeReference r = compile_bytecode(comp, comp_thread,
                                            context, builder, expr);
  if (comp_thread->is_panic()) {
    return;
  }

  IR::Emit::Return(builder->ir_bytecode, to_ir_V<IR::Types::Return>(r));

  builder->end_expression();

  builder->end_control_block();

  finish_ir(comp, builder);
}

void compile_function_ir(CompilerGlobals* const comp,
                         CompilerThread* const comp_thread,
                         Context* const context,
                         ASTLambda* const ast_lambda,
                         IR::Function* const func) {
  TRACING_FUNCTION();

  ASSERT(ast_lambda->sig->ast_type == AST_TYPE::FUNCTION_SIGNATURE);
  ASTFuncSig* sig = (ASTFuncSig*)ast_lambda->sig;

  IR::Builder* builder = comp->new_ir(func->signature.label);

  start_ir(builder, sig->parameters);

  {
    AST_ARR arr = ((ASTBlock*)ast_lambda->body)->block;
    FOR_AST(arr, i) {
      compile_bytecode_of_statement(comp, comp_thread, context, builder, i);
      if (comp_thread->is_panic()) {
        return;
      }
    }
  }

  builder->end_control_block();

  finish_ir(comp, builder);
}

static void compile_import_file(CompilerGlobals* comp, CompilerThread* comp_thread, const FileLocation* src_loc, ASTImport* imp, Namespace* import_to) {
  TRACING_FUNCTION();

  const char* path = nullptr;

  AST_LOCAL expr = imp->expr_location;

  if (expr->ast_type == AST_TYPE::ASCII_STRING) {
    ASTAsciiString* str = (ASTAsciiString*)expr;
    path = str->string->string;
  }
  else {
    comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, imp->node_span, "Cannot import from non-string literals yet");
  }

  ASSERT(path != nullptr);

  FileLocation loc;
  {
    auto strings = comp->services.strings.get();
    loc = parse_file_location(src_loc->directory->string, path,
                              strings._ptr);
  }

  const auto is_correct_file = [&loc](const FileAST* f) {
    return f->file_loc == loc;
  };

  const FileAST* imported_file = comp->parsed_files.find_if(is_correct_file);

  if (imported_file != nullptr) {
    auto names = comp->services.names.get();

    names->add_global_import(comp_thread, import_to, imported_file->ns, imp->node_span);
  }
  else {
    FileImport file_import = {};
    file_import.file_loc = std::move(loc);
    file_import.ns = comp->new_namespace();
    file_import.span = imp->node_span;

    {
      auto names = comp->services.names.get();

      //Might error but its probs fine to just leave it as the error will be caught at some point
      names->add_global_import(comp_thread, import_to, file_import.ns, imp->node_span);
    }

    comp->services.file_loader.get()->unparsed_files.insert(std::move(file_import));
  }
}

void compile_init_expr_of_global(CompilerGlobals* comp, CompilerThread* comp_thread, Context* context,
                                 ASTDecl* decl, Global* global) {
  TRACING_FUNCTION();
  AST_LOCAL decl_expr = decl->expr;
  ASSERT(global->decl.type.is_valid());

  if (TEST_MASK(global->decl.meta_flags, META_FLAG::COMPTIME)) {
    IR::Builder temporary = {};

    temporary.signature = (const SignatureStructure*)comp->builtin_types->t_void_call.structure;

    start_ir(&temporary, AST_ARR{});

    context->comptime_compilation = true;

    temporary.start_expression();
    IR::RuntimeReference init_expr = compile_bytecode(comp, comp_thread, context, &temporary, decl->expr);
    if (comp_thread->is_panic()) {
      return;
    }
    ASSERT(init_expr.type == global->decl.type);

    if (!init_expr.is_constant) {
      comp_thread->report_error(ERROR_CODE::CONST_ERROR, global->decl.span, "Cannot initialize a compile time global with a runtime value");
      return;
    }

    ASSERT(init_expr.type == global->decl.type);
    global->constant_value = init_expr.constant;

    if (global->decl.name == comp->build_options.entry_point) {
      if (global->decl.type.struct_type() != STRUCTURE_TYPE::LAMBDA) {
        comp_thread->report_error(ERROR_CODE::LINK_ERROR, global->decl.span, "Entry point must be a function");
        return;
      }

      if (comp->entry_point_label.label != 0) {
        comp_thread->report_error(ERROR_CODE::LINK_ERROR, global->decl.span, "Found a second entry point");
        return;
      }

      comp->entry_point_label = *(IR::GlobalLabel*)global->constant_value;
    }
#if 0
    if (global->constant_value == nullptr) {
      global->constant_value = comp->new_constant(global->decl.type.size());

      UnitID id = compile_and_execute(comp, context->current_unit->available_names, decl->expr, global->decl.type, global->constant_value);

      set_dependency(comp_thread, context, id);
      return;
    }
#endif
  }
  else {
    IR::GlobalLabel label = comp->next_function_label((const SignatureStructure*)comp->builtin_types->t_void_call.structure);

    IR::Builder* builder = comp->new_ir(label);

    start_ir(builder, AST_ARR{});

    global->dynamic_init_index = new_dynamic_init_object(comp, global->decl.name, global->decl.type.structure->size, global->decl.type.structure->alignment, builder);

    builder->start_expression();
    IR::RuntimeReference glob_ref = load_data_memory(comp, builder, global);
    IR::RuntimeReference init_expr = compile_bytecode(comp, comp_thread, context, builder, decl->expr);
    if (comp_thread->is_panic()) {
      return;
    }

    IR::HELPERS::copycast_value(builder, glob_ref, init_expr);
    builder->end_expression();
    builder->end_control_block();

    finish_ir(comp, builder);
  }
}

void compile_current_unparsed_files(CompilerGlobals* const comp,
                                    CompilerThread* const comp_thread,
                                    FileLoader* file_loader) {
  TRACING_FUNCTION();

  while (file_loader->unparsed_files.size > 0) {
    //still have files to parse

    //parse the last file
    const FileImport* file_import = file_loader->unparsed_files.back();
    const InternString* full_path = file_import->file_loc.full_name;

    if (comp->print_options.file_loads) {
      IO::print("Loading file \"", full_path->string, "\" ...\n");
    }

    //Just a sanity check - should alread have been set
    if (file_import->file_loc.extension == comp->important_names.axl
        || file_import->file_loc.extension == nullptr) {
      //Load a source file

      OwnedPtr<const char> text_source = FILES::load_file_to_string(full_path->string);

      if (text_source.ptr == nullptr) {
        comp_thread->report_error(ERROR_CODE::FILE_ERROR, file_loader->unparsed_files.back()->span,
                                  "File '{}' could not be opened, perhaps it does not exist",
                                  full_path);
        return;
      }



      Parser parser = {};
      parser.file_path = file_import->file_loc;

      //Loaded file can pop of the file stack thing
      //It will shortly be loaded into the parsed files
      file_loader->unparsed_files.pop();

      //Reset the parser for this file
      reset_parser(comp, comp_thread, &parser, full_path, text_source.ptr);

      // ^ Can error (in the lexing)
      if (comp_thread->is_panic()) {
        return;
      }

      //Parse
      comp->parsed_files.insert_uninit(1);
      FileAST* ast_file = comp->parsed_files.back();
      ast_file->file_loc = file_import->file_loc;

      ast_file->ns = file_import->ns;
      ast_file->ns->imported.insert(comp->builtin_namespace);

      {
        TRACING_SCOPE("Parsing");

        parser.current_namespace = file_import->ns;

        parse_file(comp, comp_thread, &parser, ast_file);
        if (comp_thread->is_panic()) {
          return;
        }

        //Should no longer be needed now - can free the file
        text_source.free_no_destruct();
      }

      if (comp->print_options.ast) {
        IO_Single::lock();
        IO_Single::print("\n=== Print Parsed AST ===\n\n");
        print_full_ast(ast_file);
        IO_Single::print("\n========================\n\n");
        IO_Single::unlock();
      }

      if (comp_thread->is_panic()) {
        return;
      }
    }
    else {
      comp_thread->report_error(ERROR_CODE::FILE_ERROR, file_import->span,
                                "'{}' is not a loadable file extension",
                                file_import->file_loc.extension);
      return;
    }
  }
}

void add_comp_unit_for_import(CompilerGlobals* const comp, Namespace* ns, const FileLocation& src_loc, ASTImport* imp) noexcept {

  CompilationUnit* imp_unit;
  {
    auto compilation = comp->services.compilation.get();
    ImportExtra* extra = compilation->import_extras.allocate();
    extra->src_loc = src_loc;

    imp_unit = new_compilation_unit(compilation._ptr,
                                    COMPILATION_EMIT_TYPE::IMPORT, imp, ns,
                                    (void*)extra, comp->print_options.comp_units);
  }

  ASSERT(imp_unit != nullptr);
  comp->pipelines.depend_check.push_back(imp_unit);
}

void add_comp_unit_for_global(CompilerGlobals* const comp, CompilerThread* const comp_thread, Namespace* ns, ASTGlobalDecl* global) noexcept {

  Global* glob = comp->new_global();

  CompilationUnit* glob_unit;
  {
    auto compilation = comp->services.compilation.get();
    GlobalExtra* extra = compilation->global_extras.allocate();
    extra->global = glob;

    glob_unit = new_compilation_unit(compilation._ptr,
                                     COMPILATION_EMIT_TYPE::GLOBAL, global, ns,
                                     (void*)extra, comp->print_options.comp_units);
  }

  //i->global_ptr = glob;

  glob->decl.name = global->name;
  glob->decl.span = global->node_span;

  global->global_ptr = glob;

  {
    auto names = comp->services.names.get();

    names->add_global_name(comp_thread, ns, global->name, glob_unit->id, glob);
  }
  if (comp_thread->is_panic()) {
    return;
  }

  ASSERT(glob_unit != nullptr);
  comp->pipelines.depend_check.push_back(glob_unit);
}

void add_comp_unit_for_lambda(CompilerGlobals* const comp, CompilerThread* const comp_thread, Namespace* ns, ASTLambda* lambda) noexcept {
  //Setup the function object
  IR::Function* const func = comp->new_function();
  lambda->function = func;

  func->declaration = lambda;

  ASTFuncSig* func_sig = (ASTFuncSig*)(lambda->sig);
  func_sig->sig = &func->signature;
  func_sig->convention = comp->build_options.default_calling_convention;

  //Set the compilation units
  CompilationUnit* sig_unit;
  CompilationUnit* body_unit;
  {
    auto compilation = comp->services.compilation.get();

    sig_unit = new_compilation_unit(compilation._ptr,
                                    COMPILATION_EMIT_TYPE::FUNC_SIG, lambda->sig, ns,
                                    nullptr, comp->print_options.comp_units);

    FuncBodyExtra* func_extra = compilation->func_body_extras.allocate();
    func_extra->func = func;

    body_unit = new_compilation_unit(compilation._ptr,
                                     COMPILATION_EMIT_TYPE::FUNC_BODY, lambda, ns,
                                     func_extra, comp->print_options.comp_units);
    compilation->in_flight_units -= 1;//it never actually starts
  }

  body_unit->insert_to = &comp->pipelines.depend_check;

  {
    auto compilation = comp->services.compilation.get();

    compilation->dependencies.add_dependency_to(body_unit, sig_unit);
    if (comp_thread->print_options.comp_units) {
      format_print("Comp unit {} is function body of {} (so it waiting)\n", body_unit->id, sig_unit->id);
    }

  }
  //Last thing to do (stops certian threading bugs)
  ASSERT(sig_unit != nullptr);
  comp->pipelines.depend_check.push_back(sig_unit);

  func->sig_unit_id = sig_unit->id;
  func->body_unit_id = body_unit->id;
}

void add_comp_unit_for_struct(CompilerGlobals* const comp, Namespace* ns, ASTStructBody* struct_body) noexcept {
  CompilationUnit* unit;
  {
    auto compilation = comp->services.compilation.get();

    unit = new_compilation_unit(compilation._ptr,
                                COMPILATION_EMIT_TYPE::STRUCTURE, struct_body, ns,
                                nullptr, comp->print_options.comp_units);

    struct_body->unit_id = unit->id;
  }

  ASSERT(unit != nullptr);
  comp->pipelines.depend_check.push_back(unit);
}

void DependencyManager::remove_dependency_from(CompilationUnit* ptr) {
  ASSERT(ptr != nullptr);


  ptr->waiting_on_count -= 1;

  if (ptr->waiting_on_count == 0) {
    free_dependencies.push_back(ptr);
  }
}

void DependencyManager::add_dependency_to(CompilationUnit* now_waiting, CompilationUnit* waiting_on) {
  ASSERT(now_waiting != nullptr);
  ASSERT(waiting_on != nullptr);

  now_waiting->waiting_on_count += 1;

  DependencyListSingle* old_top = waiting_on->dependency_list;
  waiting_on->dependency_list = dependency_list_entry.allocate();

  waiting_on->dependency_list->waiting = now_waiting;
  waiting_on->dependency_list->next = old_top;
}

void DependencyManager::close_dependency(CompilationUnit* ptr) {
  ASSERT(ptr != nullptr);
  DependencyListSingle* dep_single = ptr->dependency_list;

  while (dep_single != nullptr) {
    CompilationUnit* waiting = dep_single->waiting;
    DependencyListSingle* next = dep_single->next;

    dependency_list_entry.free(dep_single);
    dep_single = next;

    remove_dependency_from(waiting);
  }
}

void launch_free_dependencies(CompilerThread* comp_thread, Compilation* const compilation) {
  while (compilation->dependencies.free_dependencies.size > 0) {
    CompilationUnit* unit = compilation->dependencies.free_dependencies.pop_front();

    ASSERT(unit != nullptr);

    ASSERT(unit->waiting_on_count == 0);
    compilation->in_flight_units += 1;

    ASSERT(unit->insert_to != nullptr);
    Pipe* p = unit->insert_to;
    unit->insert_to = nullptr;

    if (comp_thread->print_options.comp_units) {
      format_print("Comp unit started again {} -> {}\n",
                   unit->id, p->_debug_name);
    }

    p->push_back(unit);
  }

  ASSERT(compilation->in_flight_units <= compilation->store.active_units.size);
}

//Might be that dependencies were already dispatched
//Return true if depended
bool try_dispatch_dependencies(CompilerGlobals* comp, CompilerThread* comp_thread, Context* context) {
  ASSERT(context->current_unit != nullptr);
  ASSERT(context->dependency_load_pipe != nullptr);

  auto compilation = comp->services.compilation.get();

  auto unit = context->current_unit;

  bool depended = false;
  FOR(comp_thread->new_depends, id) {
    CompilationUnit* u = compilation->store.get_unit_if_exists(*id);
    if (u != nullptr) {
      if (comp_thread->print_options.comp_units) {
        format_print("Comp unit {} waiting on {}\n",
                     unit->id, u->id);
      }

      depended = true;
      compilation->dependencies.add_dependency_to(unit, u);
    }
  }

  if (comp_thread->local_unfound_names.names.size > 0) {
    depended = true;
    compilation->unfound_names.names.concat(std::move(comp_thread->local_unfound_names.names));
    comp_thread->local_unfound_names.names = {};
  }

  if (depended) {
    unit->insert_to = context->dependency_load_pipe;
    compilation->in_flight_units -= 1;
    if (comp_thread->print_options.comp_units) {
      format_print("Comp unit {} now waiting   | Active = {}, In flight = {}\n",
                   unit->id, compilation->store.active_units.size, compilation->in_flight_units);
    }
  }

  comp_thread->new_depends.clear();
  return depended;
}

void close_compilation_unit(CompilerThread* comp_thread, Compilation* compilation, CompilationUnit* unit) {
  TRACING_FUNCTION();

  UnitID id = unit->id;

  compilation->dependencies.close_dependency(unit);

  ASSERT(compilation->in_flight_units != 0);
  compilation->in_flight_units -= 1;

  compilation->store.free_unit(unit);

  if (comp_thread->print_options.comp_units) {
    format_print("Close Comp unit {}         | Active = {}, In flight = {}\n",
                 id, compilation->store.active_units.size, compilation->in_flight_units);
  }
}

void run_compiler_pipes(CompilerGlobals* const comp, CompilerThread* const comp_thread) {

  {
    bool acquired = comp->services.file_loader._mutex.acquire_if_free();

    if (acquired) {
      FileLoader* file_loader = comp->services.file_loader._ptr;

      DEFER(comp) {
        comp->services.file_loader._mutex.release();
      };

      if (file_loader->unparsed_files.size > 0) {
        thead_doing_work(comp, comp_thread);

        compile_current_unparsed_files(comp, comp_thread, file_loader);
        if (comp_thread->is_panic()) {
          return;
        }

        ASSERT(!comp_thread->is_depends());
      }


    }
  }

  {
    bool acquired = comp->services.out_program._mutex.acquire_if_free();

    if (acquired) {
      DEFER(comp) {
        comp->services.out_program._mutex.release();
      };
      Backend::Program* p = comp->services.out_program._ptr;

      const IR::Builder* ir = nullptr;
      if (comp->finished_irs.try_pop_front(&ir)) {
        thead_doing_work(comp, comp_thread);

        comp_thread->platform_interface.emit_function(comp,
                                                      comp_thread, ir,
                                                      comp->build_options.default_calling_convention,
                                                      p);
        if (comp_thread->is_panic()) {
          return;
        }

        ASSERT(!comp_thread->is_depends());
      }
    }
  }

  CompilationUnit* unit = nullptr;

  if (comp->pipelines.emit_import.try_pop_front(&unit)) {
    TRACING_SCOPE("Import loop");
    thead_doing_work(comp, comp_thread);

    ASSERT(!comp_thread->is_depends() && !comp_thread->is_panic());

    ASSERT(unit->waiting_on_count == 0);

    Context context = {};
    context.comptime_compilation = false;
    context.dependency_load_pipe = &comp->pipelines.emit_import;
    context.current_unit = unit;

    ASSERT(unit->ast != nullptr);
    ASSERT(unit->ast->ast_type == AST_TYPE::IMPORT);

    ASTImport* imp = (ASTImport*)unit->ast;


    AST_LOCAL expr = imp->expr_location;
    ASSERT(expr != nullptr);

    ImportExtra* imp_extra = (ImportExtra*)unit->extra;

    compile_import_file(comp, comp_thread, &imp_extra->src_loc, imp, unit->available_names);
    if (comp_thread->is_panic()) {
      return;
    }

    //Finished
    auto compilation = comp->services.compilation.get();

    compilation->import_extras.free(imp_extra);
    close_compilation_unit(comp_thread, compilation._ptr, unit);
    return;
  }

  if (comp->pipelines.emit_global.try_pop_front(&unit)) {
    TRACING_SCOPE("Emit Global");
    thead_doing_work(comp, comp_thread);

    ASSERT(!comp_thread->is_depends() && !comp_thread->is_panic());

    ASSERT(unit->waiting_on_count == 0);

    Context context = {};
    context.dependency_load_pipe = &comp->pipelines.emit_global;
    context.current_unit = unit;

    ASSERT(unit->ast != nullptr);
    ASSERT(unit->ast->ast_type == AST_TYPE::GLOBAL_DECL);

    ASTDecl* decl = (ASTDecl*)unit->ast;
    GlobalExtra* global_extra = (GlobalExtra*)unit->extra;
    Global* global = global_extra->global;

    global->decl.type = decl->type;
    if (decl->compile_time_const) {
      global->decl.meta_flags |= META_FLAG::COMPTIME;
    }
    else {
      global->decl.meta_flags |= META_FLAG::ASSIGNABLE;
    }

    ASSERT(global->decl.type.is_valid());

    compile_init_expr_of_global(comp, comp_thread, &context, decl, global);
    if (comp_thread->is_panic()) {
      return;
    }

    //Finished
    auto compilation = comp->services.compilation.get();

    compilation->global_extras.free(global_extra);
    close_compilation_unit(comp_thread, compilation._ptr, unit);
    return;
  }

  if (comp->pipelines.emit_function.try_pop_front(&unit)) {
    TRACING_SCOPE("Emit Function Body");
    thead_doing_work(comp, comp_thread);

    ASSERT(!comp_thread->is_depends() && !comp_thread->is_panic());

    ASSERT(unit->waiting_on_count == 0);

    Context context = {};
    context.dependency_load_pipe = &comp->pipelines.emit_function;
    context.current_unit = unit;

    ASTLambda* lambda = (ASTLambda*)unit->ast;
    FuncBodyExtra* func_body_extra = (FuncBodyExtra*)unit->extra;
    IR::Function* func = func_body_extra->func;

    compile_function_ir(comp, comp_thread,
                        &context,
                        lambda,
                        func);
    if (comp_thread->is_panic()) {
      return;
    }

    //Finished
    auto compilation = comp->services.compilation.get();

    compilation->func_body_extras.free(func_body_extra);
    close_compilation_unit(comp_thread, compilation._ptr, unit);
    return;
  }

  if (comp->pipelines.exec_ir.try_pop_front(&unit)) {
    TRACING_SCOPE("Exec Ir Unit");
    thead_doing_work(comp, comp_thread);

    comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, Span{}, "Currently don't support executing code at compile time");
    return;
#if 0

    ASSERT(!comp_thread->is_depends() && !comp_thread->is_panic());

    ASSERT(unit->waiting_on_count == 0);

    IR::Builder* builder = comp->new_ir();

    Context context = {};
    context.current_unit = unit;
    context.comptime_compilation = true;
    context.dependency_load_pipe = &comp->pipelines.exec_code;

    ExecCodeExtra* exec_code_extra = (ExecCodeExtra*)unit->extra;

    {
      AST_LOCAL ast = unit->ast;
      const Type& cast_to = exec_code_extra->expected_type;

      if (cast_to.is_valid()) {
        ASSERT(ast->node_type == cast_to);
      }

      //Have to compile to vm
      BuildOptions options;
      options.default_calling_convention = &convention_vm;
      options.endpoint_system = &system_vm;
      options.entry_point = comp_thread->build_options.entry_point;
      options.output_file = comp_thread->build_options.output_file;
      options.file_name = comp_thread->build_options.file_name;

      //Swap forward
      std::swap(options, comp_thread->build_options);

      const CallingConvention* convention = comp_thread->build_options.default_calling_convention;

      compile_expression_as_func_ir(comp, comp_thread, &context, builder, ast);
      if (comp_thread->is_panic()) {
        return;
      }

      vm_run(&comp_thread->errors, builder);
      if (comp_thread->is_panic()) {
        return;
      }

      ASSERT(cast_to.is_valid());
      if (cast_to == ast->node_type) {
        auto size = cast_to.size();
        memcpy_s(exec_code_extra->destination, size, )
      }
      else {

        do_literal_cast(comp, comp_thread, ast, cast_to, const_val, res);
        if (comp_thread->is_panic()) {
          return;
        }


        ast->node_type = cast_to;
      }

      //Swap back
      std::swap(options, comp_thread->build_options);
    }

    ASSERT(!comp_thread->is_panic() && !comp_thread->is_depends());

    //Finished
    auto compilation = comp->services.compilation.get();

    compilation->exec_code_extras.free(exec_code_extra);
    close_compilation_unit(comp_thread, compilation._ptr, unit);
#endif
    return;
  }

  if (comp->pipelines.type_check.try_pop_front(&unit)) {
    TRACING_SCOPE("Type check");
    thead_doing_work(comp, comp_thread);

    ASSERT(!comp_thread->is_depends() && !comp_thread->is_panic());


    ASSERT(unit->waiting_on_count == 0);

    Context context = {};
    context.dependency_load_pipe = &comp->pipelines.type_check;
    context.current_unit = unit;

    ASSERT(unit->ast != nullptr);

    //HACK: deal with these dependencies better - perhaps transfer them?
    if (!unit->ast->node_type.is_valid()) {
      type_check_ast(comp, comp_thread, &context, unit->ast);
      if (comp_thread->is_panic()) {
        return;
      }

      if (comp_thread->is_depends()) {
        if (try_dispatch_dependencies(comp, comp_thread, &context)) {
          return;
        }
      }
    }

    switch (unit->emit) {
      case COMPILATION_EMIT_TYPE::STRUCTURE: {
          auto compilation = comp->services.compilation.get();

          ASSERT(unit->extra == nullptr);

          close_compilation_unit(comp_thread, compilation._ptr, unit);
          break;
        }
      case COMPILATION_EMIT_TYPE::EXEC_CODE: {
          comp->pipelines.exec_ir.push_back(unit);
          break;
        }
      case COMPILATION_EMIT_TYPE::FUNC_SIG: {
          ASSERT(unit->extra == nullptr);
          //State is shared so don't free it
          //comp->states.free(state);
          auto compilation = comp->services.compilation.get();

          close_compilation_unit(comp_thread, compilation._ptr, unit);
          break;
        }
      case COMPILATION_EMIT_TYPE::FUNC_BODY: {
          comp->pipelines.emit_function.push_back(unit);
          break;
        }
      case COMPILATION_EMIT_TYPE::GLOBAL: {
          comp->pipelines.emit_global.push_back(unit);
          break;
        }
      case COMPILATION_EMIT_TYPE::IMPORT: {
          comp->pipelines.emit_import.push_back(unit);
          break;
        }
    }


    return;
  }

  if (comp->pipelines.depend_check.try_pop_front(&unit)) {
    TRACING_SCOPE("Depend check");
    thead_doing_work(comp, comp_thread);
    ASSERT(!comp_thread->is_depends() && !comp_thread->is_panic());

    ASSERT(unit->waiting_on_count == 0);

    DependencyChecker st = {};

    //RELOAD TO DEPEND CHECK IF THIS FAILS!!!!!
    //This is because unfound names trigger dependencies and the global may not be type checked yet
    //We wont know this unless we do a dependency check again
    st.dependency_load_pipe = &comp->pipelines.depend_check;
    st.comptime_compilation = false;//meaningless here
    st.current_unit = unit;

    ASSERT(unit->ast != nullptr);

    dependency_check_ast_node(comp, comp_thread, &st, unit->ast);

    if (comp_thread->is_depends()) {
      if (try_dispatch_dependencies(comp, comp_thread, &st)) {
        return;
      }
    }

    comp->pipelines.type_check.push_back(unit);
    return;
  }

  ASSERT(unit == nullptr);//shouldn't have been modified in this time

  {
    auto compilation = comp->services.compilation.get();
    //Wait for there to be no compiling to check unfound deps - best chance they exist
    if (compilation->unfound_names.names.size > 0) {
      TRACING_SCOPE("Check Unfound Names");
      thead_doing_work(comp, comp_thread);

      usize num_unfound = compilation->unfound_names.names.size;

      auto names = comp->services.names.get();


      const auto found_dep_l = [names = names._ptr, dep_ptr = &compilation->dependencies](const UnfoundNameHolder& dep) -> bool {
        const GlobalName* gn = names->find_global_name(dep.name.ns, dep.name.ident);

        if (gn != nullptr) {
          dep_ptr->remove_dependency_from(dep.dependency);
          return true;
        }
        else {
          return false;
        }
      };


      //Remove units if dependency has been found
      compilation->unfound_names.names.remove_if(found_dep_l);

      if (num_unfound == compilation->unfound_names.names.size) {
        //All names are still unfound
        auto i = compilation->unfound_names.names.mut_begin();
        auto end = compilation->unfound_names.names.mut_end();

        comp_thread->errors.error_messages.reserve_extra(compilation->unfound_names.names.size);

        for (; i < end; i++) {
          comp_thread->errors.error_messages.insert(std::move(i->as_error));
        }

        comp_thread->errors.panic = true;
        return;
      }
    }

    if (compilation->dependencies.free_dependencies.size > 0) {
      launch_free_dependencies(comp_thread, compilation._ptr);
      return;
    }
  }

  //format_print("---- DEBUG: Did nothing in pass\n");

  if (comp_thread->doing_work) {
    comp_thread->doing_work = false;
    comp->work_counter -= 1;

    if (comp->work_counter == 0) {
      auto compilation = comp->services.compilation.get();

      auto i = compilation->store.active_units.begin();
      const auto end = compilation->store.active_units.end();

      Array<char> error = {};
      format_to_array(error, "Work still exists but is not accessable\nThe following compilation units are inaccessable:\n");

      for (; i < end; ++i) {
        CompilationUnit* unit = *i;
        ASSERT(unit != nullptr);
        const char* debug_name = "Type unsupported in this mode";
        if (unit->insert_to != nullptr) {
          if (unit->insert_to->_debug_name != nullptr) {
            debug_name = unit->insert_to->_debug_name;
          }
        }
        else {
          debug_name = "Should be active";
        }


        format_to_array(error, "- Id: {} | Next Type: {} | Waiting on count: {}\n", unit->id, debug_name, unit->waiting_on_count);
      }

      format_to_array(error, "Pipeline states:\n");
      comp->pipelines.depend_check.mutex.acquire();
      format_to_array(error, "- Depend Check Size: {}\n", comp->pipelines.depend_check.size);
      comp->pipelines.depend_check.mutex.release();

      comp->pipelines.type_check.mutex.acquire();
      format_to_array(error, "- Type Check Size: {}\n", comp->pipelines.type_check.size);
      comp->pipelines.type_check.mutex.release();

      comp->pipelines.exec_ir.mutex.acquire();
      format_to_array(error, "- Exec Code Size: {}\n", comp->pipelines.exec_ir.size);
      comp->pipelines.exec_ir.mutex.release();

      comp->pipelines.emit_function.mutex.acquire();
      format_to_array(error, "- Emit Function Size: {}\n", comp->pipelines.emit_function.size);
      comp->pipelines.emit_function.mutex.release();

      comp->pipelines.emit_global.mutex.acquire();
      format_to_array(error, "- Emit Global Size: {}\n", comp->pipelines.emit_global.size);
      comp->pipelines.emit_global.mutex.release();

      comp->pipelines.emit_import.mutex.acquire();
      format_to_array(error, "- Emit Import Size: {}\n", comp->pipelines.emit_import.size);
      comp->pipelines.emit_import.mutex.release();

      comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, Span{}, error.data);
      return;
    }
  }
}

void compiler_loop(CompilerGlobals* const comp, CompilerThread* const comp_thread) {
  comp_thread->doing_work = true;

  while (comp->is_compiling()) {
    run_compiler_pipes(comp, comp_thread);
    if (comp_thread->is_panic()) {
      comp->global_panic.set();
    }
  }

  if (comp_thread->is_panic()) {
    ASSERT(comp->is_global_panic());

    comp->global_errors_mutex.acquire();
    comp->global_errors.concat(std::move(comp_thread->errors.error_messages));
    comp->global_errors_mutex.release();
  }
}

struct ThreadData {
  CompilerGlobals* comp;
  CompilerThread* comp_thread;
};

void compiler_loop_thread_proc(const ThreadHandle* handle, void* data) {
  ASSERT(data != nullptr);
  ThreadData* t_data = (ThreadData*)data;
  CompilerGlobals* comp = t_data->comp;
  CompilerThread* comp_thread = t_data->comp_thread;
  ASSERT(comp != nullptr);

  compiler_loop(comp, comp_thread);
}

void compiler_loop_threaded(CompilerGlobals* const comp, CompilerThread* const comp_thread) noexcept {
  comp->work_counter = comp->active_threads;
  const usize EXTRA_THREADS = comp->active_threads - 1;

  if (EXTRA_THREADS > 0) {
    ThreadData* datas = allocate_default<ThreadData>(EXTRA_THREADS);
    CompilerThread* comp_threads = allocate_default<CompilerThread>(EXTRA_THREADS);
    const ThreadHandle** handles = allocate_default<const ThreadHandle*>(EXTRA_THREADS);

    for (usize i = 0; i < EXTRA_THREADS; i++) {
      datas[i].comp = comp;
      datas[i].comp_thread = comp_threads + i;
      copy_compiler_constants(comp, comp_threads + i);
    }

    copy_compiler_constants(comp, comp_thread);//Just in case

    //Start the threads
    for (usize i = 0; i < EXTRA_THREADS; i++) {
      handles[i] = start_thread(compiler_loop_thread_proc, datas + i);
    }

    compiler_loop(comp, comp_thread);

    {
      TRACING_SCOPE("Close Threads");
      for (usize i = 0; i < EXTRA_THREADS; i++) {
        wait_for_thread_end(handles[i]);
      }

      free_destruct_n(handles, EXTRA_THREADS);
      free_destruct_n(datas, EXTRA_THREADS);
      free_destruct_n(comp_threads, EXTRA_THREADS);
    }
  }
  else {
    compiler_loop(comp, comp_thread);
  }
}

void compile_all(CompilerGlobals* const comp, CompilerThread* const comp_thread) {
  TRACING_FUNCTION();

  ASSERT(comp->active_threads >= 1);

  compiler_loop_threaded(comp, comp_thread);
  if (comp->is_global_panic()) {
    return;
  }

  ASSERT(comp->services.compilation.get()->in_flight_units == 0);

  {
    TRACING_SCOPE("Load Imports");
    auto p = comp->services.out_program.get();

    FOR(comp->dyn_lib_imports, it) {
      comp->platform_interface.emit_dyn_library_function(comp_thread, it, comp->build_options.default_calling_convention,
                                                         p._ptr);

      if (comp_thread->is_panic()) {
        return;
      }
    }
  }

  {
    TRACING_SCOPE("Create Entry Point");
    if (comp->entry_point_label.label == 0) {
      comp_thread->report_error(ERROR_CODE::LINK_ERROR, Span{}, "Did not find entry point (expected name = \"{}\")",
                                comp->build_options.entry_point);
      return;
    }

    comp->platform_interface.emit_start(comp, comp->entry_point_label, comp->services.out_program._ptr);
  }
}

#if 0
void print_compiled_functions(CompilerGlobals* const comp) {
  comp->functions_mutex.acquire();

  auto i = comp->functions_single_threaded.begin_const_iter();
  const auto end = comp->functions_single_threaded.end_const_iter();

  for (; i != end; i.next()) {
    const IR::Function* func = i.get();

    printf("FUNCTION %s:\n", func->signature.name->string);
    IR::print_ir(func->ir);
    IO::print('\n');
  }

  comp->functions_mutex.release();
}
#endif

Type create_named_type(CompilerGlobals* comp, CompilerThread* comp_thread, NameManager* names, const Span& span, Namespace* ns,
                       const InternString* name, const Structure* s) {
  Global* g = comp->new_global();

  //Make sure that t_type is already created before this since we need it
  ASSERT(comp->builtin_types->t_type.is_valid());

  Type type = { name, s };

  g->decl.meta_flags = (u8)META_FLAG::COMPTIME;
  g->decl.name = name;
  g->decl.type = comp->builtin_types->t_type;
  g->decl.span = span;

  g->is_constant = true;
  g->constant_value = comp->new_constant<Type>();

  memcpy_ts((Type*)g->constant_value, 1, &type, 1);

  names->add_global_name(comp_thread, ns, name, NULL_ID, g);

  return type;
}

void create_named_enum_value(CompilerGlobals* comp, CompilerThread* comp_thread, NameManager* names, const Span& span, Namespace* ns, const EnumValue* v) {
  Global* g = comp->new_global();

  ASSERT(v->type.is_valid());

  g->decl.meta_flags = (u8)META_FLAG::COMPTIME;
  g->decl.name = v->name;
  g->decl.type = v->type;
  g->decl.span = span;

  g->is_constant = true;
  g->constant_value = comp->new_constant<const EnumValue*>();

  memcpy_ts((const EnumValue**)g->constant_value, 1, &v, 1);

  names->add_global_name(comp_thread, ns, v->name, NULL_ID, g);
}

void init_compiler(const APIOptions& options, CompilerGlobals* comp, CompilerThread* comp_thread) {
  TRACING_FUNCTION();

  //Setup the built in namespace
  Namespace* builtin_namespace = comp->new_namespace();
  comp->builtin_namespace = builtin_namespace;
  comp->platform_interface = *options.platform_interface;

  //Init the types
  auto file_loader = comp->services.file_loader.get();
  auto names = comp->services.names.get();
  auto structures = comp->services.structures.get();
  auto strings = comp->services.strings.get();
  auto* builtin_types = comp->builtin_types;

  const auto register_builtin_type = [names = names._ptr, comp_thread, comp, builtin_namespace](const Structure* s)->Type {
    Type t = create_named_type(comp, comp_thread, names, Span{}, builtin_namespace, s->struct_name, s);
    ASSERT(!comp_thread->is_panic());
    return t;
  };

  const auto register_builtin_enum_value = [names = names._ptr, comp_thread, comp, builtin_namespace](const EnumValue* v) {
    create_named_enum_value(comp, comp_thread, names, Span{}, builtin_namespace, v);
    ASSERT(!comp_thread->is_panic());
  };

  {
    Structure* const s_type = STRUCTS::new_base_structure(structures._ptr,
                                                          strings->intern("type"));
    s_type->type = STRUCTURE_TYPE::TYPE;
    s_type->ir_format = IR::Format::opaque;
    s_type->size = sizeof(Type);
    s_type->alignment = alignof(Type);
    builtin_types->t_type = to_type(s_type);

    /*_ = */ register_builtin_type(s_type);
  }


  {
    const auto base_type = [&](const auto& name, STRUCTURE_TYPE ty, u32 size, IR::Format format, Type* t) {
      Structure* s = STRUCTS::new_base_structure(structures._ptr, strings->intern(name));
      s->type = ty;
      s->size = size;
      s->alignment = size;

      *t = register_builtin_type(s);
    };

    base_type("void", STRUCTURE_TYPE::VOID, 0, IR::Format::opaque, &builtin_types->t_void);
    base_type("ascii", STRUCTURE_TYPE::ASCII_CHAR, 1, IR::Format::uint8, &builtin_types->t_ascii);
  }

  {
    const auto int_type = [&](const auto& name, bool is_signed, u32 size, IR::Format ir_format, Type* t) {
      IntegerStructure* s = STRUCTS::new_int_structure(structures._ptr, strings->intern(name));
      s->is_signed = is_signed;
      s->size = size;
      s->alignment = size;
      s->ir_format = ir_format;

      *t = register_builtin_type(s);
    };

    int_type("u8", false, 1, IR::Format::uint8, &builtin_types->t_u8);
    int_type("i8", true, 1, IR::Format::sint8, &builtin_types->t_i8);
    int_type("u16", false, 2, IR::Format::uint8, &builtin_types->t_u16);
    int_type("i16", true, 2, IR::Format::sint8, &builtin_types->t_i16);
    int_type("u32", false, 4, IR::Format::uint32, &builtin_types->t_u32);
    int_type("i32", true, 4, IR::Format::sint32, &builtin_types->t_i32);
    int_type("u64", false, 8, IR::Format::uint64, &builtin_types->t_u64);
    int_type("i64", true, 8, IR::Format::sint64, &builtin_types->t_i64);
  }

  {
    Structure* const s_void_ptr = STRUCTS::new_pointer_structure(structures._ptr, strings._ptr, comp->platform_interface.ptr_size, builtin_types->t_void);
    builtin_types->t_void_ptr = register_builtin_type(s_void_ptr);
  }

  {
    Array<Type> params = {};

    Structure* const s_void_call = STRUCTS::new_lambda_structure(structures._ptr, strings._ptr, comp->platform_interface.ptr_size,
                                                                 nullptr, std::move(params), builtin_types->t_void);
    builtin_types->t_void_call = register_builtin_type(s_void_call);
  }


  {
    EnumStructure* const s_bool = STRUCTS::new_enum_structure(structures._ptr, strings._ptr, builtin_types->t_u8);
    const InternString* bool_name = strings->intern("bool");

    builtin_types->t_bool = create_named_type(comp, comp_thread, names._ptr, Span{}, builtin_namespace, bool_name, s_bool);
    ASSERT(!comp_thread->is_panic());

    s_bool->enum_values.reserve_extra(2);
    {
      EnumValue* const e_true = STRUCTS::new_enum_value(structures._ptr, s_bool, bool_name,
                                                        strings->intern("true"));
      e_true->representation = 1;
      builtin_types->e_true = e_true;
      register_builtin_enum_value(e_true);


      EnumValue* const e_false = STRUCTS::new_enum_value(structures._ptr, s_bool, bool_name,
                                                         strings->intern("false"));

      e_true->representation = 0;
      builtin_types->e_false = e_false;
      register_builtin_enum_value(e_false);

    }
    s_bool->enum_values.shrink();
  }

  {
    //Nullptr
    Global* g = comp->new_global();

    g->decl.name = strings->intern("nullptr");
    g->decl.span = Span{};
    g->decl.type = builtin_types->t_void_ptr;
    g->decl.meta_flags |= META_FLAG::COMPTIME;
    g->decl.meta_flags |= META_FLAG::CONST;

    g->is_constant = true;
    g->constant_value = comp->new_constant<const void*>();
    *(const void**)g->constant_value = 0;

    names->add_global_name(comp_thread, builtin_namespace, g->decl.name, NULL_ID, g);
  }

  //Intrinsics
#define MOD(n) comp->intrinsics . n = strings->intern(#n);
  INTRINSIC_MODS;
#undef MOD

  //Other important names
#define MOD(n) comp->important_names . n = strings->intern(#n);
  IMPORTANT_NAMES_INC;
#undef MOD

  comp->print_options = options.print;
  comp->optimization_options = options.optimize;

  //File stuff
  if (options.build.current_directory == nullptr) {
    comp_thread->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                              "Current directory not specified");
    return;
  }

  FileLocation cwd = parse_file_location(options.build.current_directory, nullptr, strings._ptr);
  file_loader->cwd = cwd;

  if (options.build.file_name == nullptr) {
    comp_thread->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                              "Expected input file name");
    return;
  }

  comp->build_options.file_name = strings->intern(options.build.file_name);

  if (options.build.entry_point == nullptr) {
    comp_thread->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                              "Expected entry point");
    return;
  }

  comp->build_options.entry_point = strings->intern(options.build.entry_point);

  if (options.build.output_file == nullptr) {
    comp_thread->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                              "Expected output file");
    return;

  }

  comp->build_options.output_file = strings->intern(options.build.output_file);


  if (options.build.std_lib_folder == nullptr) {
    comp_thread->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                              "Expected std lib folder");
    return;
  }
  comp->build_options.std_lib_folder = parse_file_location(cwd.full_name->string, options.build.std_lib_folder, strings._ptr).full_name;

  if (options.build.lib_folder == nullptr) {
    comp_thread->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                              "Expected lib folder");
    return;
  }
  comp->build_options.lib_folder = parse_file_location(cwd.full_name->string, options.build.lib_folder, strings._ptr).full_name;

  {
    const auto list = comp->platform_interface.valid_calling_conventions;
    const auto num = comp->platform_interface.num_calling_conventions;

    if (options.build.default_calling_convention >= num) {
      Array<char> error_message = {};
      format_to_array(error_message, "\"[{}]\" was not a valid calling convention for system \"{}\"\n",
                      options.build.default_calling_convention,
                      comp->platform_interface.system_name);

      if (num > 0) {
        format_to_array(error_message, "{} options are available:", num);

        for (usize i = 0; i < num; ++i) {
          const CallingConvention* cc = list[i];
          ASSERT(cc != nullptr);
          format_to_array(error_message, "\n[{}] = \"{}\"", i, cc->name);
        }
      }
      else {
        format_to_array(error_message, "No calling conventions available");
      }

      comp_thread->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{}, error_message.data);
      return;
    }
    else {
      comp->build_options.default_calling_convention = list[options.build.default_calling_convention];
    }


  }
}