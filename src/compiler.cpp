#include "compiler.h"
#include "type.h"
#include "ast.h"
#include "bytecode.h"
#include "parser.h"
#include "vm.h"
#include "backends.h"
#include "format.h"
#include "operators.h"
#include "files.h"
#include "PE_file_format.h"
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

Function* CompilerGlobals::new_function() {
  functions_mutex.acquire();
  Function* func = functions_single_threaded.insert();
  //func->func_type = FUNCTION_TYPE::DEFAULT;

  func->code_block.label = labels++;
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
                                      State* state,
                                      void* extra,
                                      bool print) {
  CompilationUnit* unit = comp->store.allocate_unit();
  unit->waiting_on_count = 0;
  unit->dependency_list = nullptr;
  unit->insert_to = nullptr;

  unit->emit = emit_type;
  unit->ast = ast;
  unit->available_names = names;
  unit->state = state;
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

Value* State::get_val(const ValueIndex& i) {
  return value_tree.values.data + i.val;
}

void State::set_value(ValueIndex index) {
  auto* val = get_val(index);
  if (!val->has_value) {
    val->has_value = true;

    val->creation.time = control_flow.now();
    val->creation.related_index = index;
  }
  else {
    val->is_modified = true;
  }

  use_value(index, index);
}

void State::use_mem(const MemIndex index) {
  const MemValue* val = get_mem(index);
  ValueIndex base = ValueIndex{ val->mem.base };
  use_value(base);

  if (val->mem.scale != 0) {
    ValueIndex index = ValueIndex{ val->mem.index };
    use_value(index);
  }
}

void State::use_value(const ValueIndex index) {
  use_value(index, index);
}

static void use_value_impl(Value& val, ValueIndex related, const ControlFlow& flow) {
  struct Lambda {
    const ControlFlow* flow;

    bool operator()(const ValueUse& p) const noexcept {
      return flow->test_a_flows_to_b(p.time.flow, flow->current_flow)
        || p.time.flow == flow->current_flow;
    }
  };

  val.last_uses.remove_if(Lambda{ &flow });
  val.last_uses.insert(
    ValueUse{
      related,
      flow.now(),
    }
  );
}

void State::use_value(const ValueIndex index, const ValueIndex related) {
  auto& val = *get_val(index);
  ASSERT(val.has_value);

  if (val.creation.time.flow != control_flow.current_flow
      && !captured_values.contains(index)) {
    captured_values.insert(index);
  }

  use_value_impl(val, related, control_flow);
}

void State::use_value_captured(const ValueIndex index, const ValueIndex related) {
  auto& val = *get_val(index);
  ASSERT(val.has_value);

  use_value_impl(val, related, control_flow);
}

MemIndex State::new_mem() {
  MemIndex i = {};
  i.index = mem_values.size;

  mem_values.insert_uninit(1);
  return i;
}

MemValue* State::get_mem(const MemIndex& mem) {
  return mem_values.data + mem.index;
}

ValueIndex State::new_value() {
  value_tree.values.insert_uninit(1);
  value_tree.adjacency_list.insert_uninit(1);

  auto val_index = ValueIndex{ value_tree.intersection_check.new_value() };

  ASSERT(val_index.val < 256);//TODO: support more values

  auto* val = value_tree.values.data + val_index.val;
  val->creation = ValueUse{ val_index, control_flow.now() };

  return val_index;
}

void State::value_copy(ValueIndex a, ValueIndex b) {
  auto* b_val = value_tree.values.data + b.val;

  if (!b_val->has_value) {
    b_val->has_value = true;
    b_val->creation.related_index = a;
    b_val->creation.time = control_flow.now();
  }
  else {
    b_val->is_modified = true;
  }

  use_value(a, b);
  use_value(b);
}

Local* DependencyCheckStateAndContext::get_local(const InternString* name) {
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

void StackState::require_call_alignment(u64 req_align) {
  call_alignment = lowest_common_multiple(req_align, call_alignment);
}

i32 StackState::pass_stack_local(u64 size, u64 alignment) {
  const uint64_t mod_align = current_passed % alignment;

  if (mod_align > 0) {
    current_passed += alignment - mod_align;
  }
  current_passed += size;

  if (current_passed > max_passed) {
    max_passed = current_passed;
  }

  return -(int32_t)current_passed;
}

i32 StackState::next_stack_local(u64 size, u64 alignment) {
  const uint64_t mod_align = current % alignment;

  if (mod_align > 0) {
    current += alignment - mod_align;
  }
  current += size;

  if (current > max) {
    max = current;
  }

  return -(int32_t)current;
}

void ValueTree::combine_intersection(const ValueIndex from, const ValueIndex to) {
  TRACING_FUNCTION();

  //Only need to insert if not already inserted
  if (intersection_check.test_a_intersects_b(from.val, to.val)) {

    struct SameVI {
      ValueIndex test;
      bool operator()(const ValueIndex v) const { return v == test; }
    };

    //Remove interestion with from
    intersection_check.remove_a_intersects_b(to.val, from.val);
    adjacency_list.data[to.val].remove_if(SameVI{ from });

    const auto& from_list = adjacency_list.data[from.val];

    //Replace all froms with tos
    for (const auto& v : from_list) {
      if (v == to) {
        continue;
      }

      //Replace intersections
      intersection_check.remove_a_intersects_b(v.val, from.val);
      intersection_check.set_a_intersects_b(v.val, to.val);
      intersection_check.set_a_intersects_b(to.val, v.val);

      auto& list = adjacency_list.data[v.val];
      bool has_to = false;
      bool found_from = false;

      for (size_t i = 0; i < list.size; i++) {
        if (list.data[i] == to) {
          has_to = true;
        }
        else if (list.data[i] == from) {
          list.data[i] = to;
          found_from = true;
        }
        else if (has_to && found_from) {
          //Skip the second version of to by shinking the array
          list.data[i - 1] = list.data[i];
        }
      }

      //Skip the second version of to by shinking the array
      if (has_to && found_from) {
        list.size--;
      }
    }

    //Combine the adjacency list
    combine_unique(from_list, adjacency_list.data[to.val]);
  }
}

const Structure* find_or_make_array_structure(Structures* const structures,
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
          return s;
        }
      }
    }
  }

  //Doesnt exist - need to make new type
  return STRUCTS::new_array_structure(structures, strings, base, length);
}

static ConstantVal copy_constant_value(CompilerGlobals* comp, ConstantVal val) {
  u8* data = comp->new_constant(val.size);

  memcpy_ts<u8>(data, val.size, (u8*)val.ptr, val.size);
  return { data, val.size };
}

static size_t new_data_object(CompilerGlobals* const comp, const InternString* name, u32 size, u32 alignment) {
  DataHolder holder = {};
  holder.name = name;
  holder.size = size;
  holder.alignment = alignment;

  comp->data_holders.insert(holder);
  return comp->data_holders.size;
}

const Structure* find_or_make_pointer_structure(Structures* const structures, StringInterner* strings,
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
          return s;
        }
      }
    }
  }



  //Doesnt exist - need to make new type
  return STRUCTS::new_pointer_structure(structures, strings, ptr_size, base);
}

const Structure* find_or_make_tuple_structure(Structures* const structures, StringInterner* strings, Array<Type>&& els) {
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
        if (els.size == 0) { return s; }

        auto el_i = els.begin();
        auto tl_i = tls->elements.begin();

        const auto el_end = els.end();

        for (; el_i < el_end; tl_i++, el_i++) {
          if (*el_i != tl_i->type) {
            goto NOT_SAME;
          }
        }

        return s;
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

  SignatureStructure* sig_struct = STRUCTS::new_lambda_structure(structures, strings, ptr_size, conv, std::move(params), ret_type);

  sig_struct->return_via_addres = register_passed_as_pointer(sig_struct->return_type.structure);
  if (sig_struct->return_via_addres) {
    sig_struct->actual_parameter_types.insert(
      to_type(find_or_make_pointer_structure(structures, strings, ptr_size, sig_struct->return_type))
    );
  }

  {
    auto i = sig_struct->parameter_types.begin();
    const auto end = sig_struct->parameter_types.end();

    size_t num_params = sig_struct->actual_parameter_types.size;
    const CallingConvention* convention = sig_struct->calling_convention;

    for (; i < end; i++) {
      const bool too_big = register_passed_as_pointer(i->structure);
      const bool as_ptr = (num_params < convention->num_parameter_registers
                           || convention->stack_pass_type == STACK_PASS_TYPE::POINTER)
        && too_big;

      if (as_ptr) {
        //Load as pointer
        sig_struct->actual_parameter_types.insert(
          to_type(find_or_make_pointer_structure(structures, strings, ptr_size, *i))
        );
      }
      else {
        sig_struct->actual_parameter_types.insert(*i);
      }

      num_params++;
    }
  }

  return sig_struct;
}

static void expect_type(CompilerThread* const comp_thread, AST_LOCAL a) {
  if (a->node_type != comp_thread->builtin_types->t_type) {
    comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, a->node_span,
                              "Expected a type");
    return;
  }
}


inline constexpr bool is_global_depend(const GlobalName* g) {
  return (g->global == nullptr || (g->global->constant_value.ptr == nullptr && g->global->data_holder_index == 0));
}

void test_global_dependency(CompilerGlobals* const comp, CompilerThread* const comp_thread, DependencyCheckStateAndContext* const state, const Span& span, const InternString* ident) {
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
                               DependencyCheckStateAndContext* const state,
                               AST_LOCAL a) {
  TRACING_FUNCTION();

  ASSERT(a != nullptr);

  switch (a->ast_type) {
    case AST_TYPE::INVALID: INVALID_CODE_PATH(); break;
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


        if (at->expr->value == nullptr) {
          UnitID id = compile_and_execute(comp, state->current_unit->available_names, at->expr,
                                          comp_thread->builtin_types->t_u64);

          set_dependency(comp_thread, state, id);
        }

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

        if (struct_body->value == nullptr || !((const Type*)struct_body->value)->is_valid()) {
          //Doesnt exist or is not valid so need to wait for that
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
    case AST_TYPE::STATIC_LINK: {
        ASTStaticLink* imp = (ASTStaticLink*)a;

        imp->meta_flags |= META_FLAG::CONST;

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

void set_runtime_flags(AST_LOCAL ast, State* const state, bool modified, uint8_t valid_rvts) {
  if (modified) {
    ast->meta_flags &= ~META_FLAG::COMPTIME;
  }
  ast->valid_rvts &= valid_rvts;

  switch (ast->ast_type) {
    case AST_TYPE::IDENTIFIER_EXPR: {
        ASTIdentifier* id = (ASTIdentifier*)ast;

        if (id->id_type == ASTIdentifier::LOCAL) {
          id->local->valid_rvts &= ast->valid_rvts;
        }
        break;
      }
    case AST_TYPE::MEMBER_ACCESS: {
        set_runtime_flags(((ASTMemberAccessExpr*)ast)->expr, state, modified, valid_rvts);
        break;
      }
    case AST_TYPE::INDEX_EXPR: {
        set_runtime_flags(((ASTIndexExpr*)ast)->expr, state, modified, valid_rvts);
        break;
      }
    default: break;
  }
}


struct UntypedNode {
  AST_LOCAL node;
  Type infer;
};

struct Typer {
  Array<UntypedNode> new_untyped_stack = {};
  Array<UntypedNode> untyped_stack = {};

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

static bool type_check_unary_operator(CompilerGlobals* const comp,
                                      CompilerThread* const comp_thread,
                                      State* const state,
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

        switch (is->size) {
          case 8: {
              expr->emit = &UnOpArgs::emit_neg_i64;
              break;
            }
          case 1:
          case 2:
          case 4:
          default: {
              comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, expr->node_span,
                                        "TODO: Implement type checking for negation of integer: {}",
                                        is->struct_name);
              return false;
            }
        }

        expr->node_type = ty;
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
      compile_take_address(comp, comp_thread, state, expr);
      return true;
    case UNARY_OPERATOR::DEREF:
      //TODO: can we infer anything here
      if (!prim->node_type.is_valid()) {
        pass_meta_flags_up(&expr->meta_flags, &prim->meta_flags);
        typer->push_node(prim, {});
        return false;
      }

      pass_meta_flags_down(&expr->meta_flags, &prim->meta_flags);
      compile_deref(comp, comp_thread, expr);
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
                                State* state,
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
              const auto* int_l = left.unchecked_base<IntegerStructure>();

              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    if (left == right) {

                      switch (int_l->size) {
                        case 1: {
                            expr->node_type = left;
                            expr->emit_info.main_op = MainOp::LEFT;
                            expr->emit_info.main_type = left;
                            expr->emit_info.func = &BinOpArgs::emit_add_8s;
                            return;
                          }
                        case 8: {
                            expr->node_type = left;
                            expr->emit_info.main_op = MainOp::LEFT;
                            expr->emit_info.main_type = left;
                            expr->emit_info.func = &BinOpArgs::emit_add_64s;
                            return;
                          }
                      }
                    }
                    break;
                  }

                case STRUCTURE_TYPE::POINTER: {
                    if (int_l->size == 8 && !int_l->is_signed) {
                      expr->node_type = right;
                      expr->emit_info.main_op = MainOp::RIGHT;
                      expr->emit_info.main_type = right;
                      expr->emit_info.func = &BinOpArgs::emit_add_64_to_ptr;
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
                if (int_r->size == 8 && !int_r->is_signed) {
                  expr->node_type = left;
                  expr->emit_info.main_op = MainOp::LEFT;
                  expr->emit_info.main_type = left;
                  expr->emit_info.func = &BinOpArgs::emit_add_64_to_ptr;
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
              const auto* int_l = left.unchecked_base<IntegerStructure>();

              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    if (left == right) {
                      switch (int_l->size) {
                        case 8: {
                            expr->node_type = left;
                            expr->emit_info.main_op = MainOp::LEFT;
                            expr->emit_info.main_type = left;
                            expr->emit_info.func = &BinOpArgs::emit_sub_64s;
                            return;
                          }
                      }
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
                      expr->node_type = left;
                      expr->emit_info.main_op = MainOp::LEFT;
                      expr->emit_info.main_type = left;
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
              const auto* int_l = left.unchecked_base<IntegerStructure>();

              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    if (left == right) {
                      switch (int_l->size) {
                        case 8: {
                            expr->node_type = left;
                            expr->emit_info.main_op = MainOp::LEFT;
                            expr->emit_info.main_type = left;
                            expr->emit_info.func = &BinOpArgs::emit_mul_64s;
                            return;
                          }

                        default: break;
                      }
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
              const auto* int_l = left.unchecked_base<IntegerStructure>();

              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    if (left == right) {
                      switch (int_l->size) {
                        case 8: {
                            expr->node_type = left;
                            expr->emit_info.main_op = MainOp::LEFT;
                            expr->emit_info.main_type = left;
                            if (int_l->is_signed) {
                              expr->emit_info.func = &BinOpArgs::emit_div_i64s;
                            }
                            else {
                              expr->emit_info.func = &BinOpArgs::emit_div_u64s;
                            }
                            return;
                          }
                        default: break;
                      }
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

              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    if (left == right) {
                      if (int_l->size == 8
                          && !int_l->is_signed) {
                        expr->node_type = left;
                        expr->emit_info.main_op = MainOp::LEFT;
                        expr->emit_info.main_type = left;
                        expr->emit_info.func = &BinOpArgs::emit_mod_u64s;
                        return;
                      }
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
                expr->emit_info.main_op = MainOp::LEFT;
                expr->emit_info.main_type = left;
                expr->emit_info.func = &BinOpArgs::emit_eq_8s;
                return;
              }

              break;
            }

          case STRUCTURE_TYPE::ENUM: {
              if (left == right) {
                const auto* en = left.extract_base<EnumStructure>();
                ASSERT(en->base.struct_type() == STRUCTURE_TYPE::INTEGER);

                switch (en->base.structure->size) {
                  case 1: {
                      expr->node_type = comp_thread->builtin_types->t_bool;
                      expr->emit_info.main_op = MainOp::LEFT;
                      expr->emit_info.main_type = left;
                      expr->emit_info.func = &BinOpArgs::emit_eq_8s;
                      return;
                    }

                  default: break;
                }
              }

              break;
            }

          case STRUCTURE_TYPE::INTEGER: {
              const auto* int_l = left.unchecked_base<IntegerStructure>();

              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    if (left == right) {
                      switch (int_l->size) {
                        case 1: {
                            expr->node_type = comp_thread->builtin_types->t_bool;
                            expr->emit_info.main_op = MainOp::LEFT;
                            expr->emit_info.main_type = left;
                            expr->emit_info.func = &BinOpArgs::emit_eq_8s;
                            return;
                          }

                        case 8: {
                            expr->node_type = comp_thread->builtin_types->t_bool;
                            expr->emit_info.main_op = MainOp::LEFT;
                            expr->emit_info.main_type = left;
                            expr->emit_info.func = &BinOpArgs::emit_eq_64s;
                            return;
                          }

                        default: break;
                      }
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
                      expr->emit_info.main_op = MainOp::LEFT;
                      expr->emit_info.main_type = left;
                      expr->emit_info.func = &BinOpArgs::emit_eq_64s;
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
              if (right.struct_type() == STRUCTURE_TYPE::ASCII_CHAR) {
                expr->node_type = comp_thread->builtin_types->t_bool;
                expr->emit_info.main_op = MainOp::LEFT;
                expr->emit_info.main_type = left;
                expr->emit_info.func = &BinOpArgs::emit_neq_8s;
                return;
              }

              break;
            }

          case STRUCTURE_TYPE::ENUM: {
              if (left == right) {
                const auto* en = left.extract_base<EnumStructure>();
                ASSERT(en->base.struct_type() == STRUCTURE_TYPE::INTEGER);

                switch (en->base.structure->size) {
                  case 1: {
                      expr->node_type = comp_thread->builtin_types->t_bool;
                      expr->emit_info.main_op = MainOp::LEFT;
                      expr->emit_info.main_type = left;
                      expr->emit_info.func = &BinOpArgs::emit_neq_8s;
                      return;
                    }

                  default: break;
                }
              }

              break;
            }

          case STRUCTURE_TYPE::INTEGER: {
              const auto* int_l = left.unchecked_base<IntegerStructure>();

              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    if (left == right) {
                      switch (int_l->size) {
                        case 1: {
                            expr->node_type = comp_thread->builtin_types->t_bool;
                            expr->emit_info.main_op = MainOp::LEFT;
                            expr->emit_info.main_type = left;
                            expr->emit_info.func = &BinOpArgs::emit_neq_8s;
                            return;
                          }

                        case 8: {
                            expr->node_type = comp_thread->builtin_types->t_bool;
                            expr->emit_info.main_op = MainOp::LEFT;
                            expr->emit_info.main_type = left;
                            expr->emit_info.func = &BinOpArgs::emit_neq_64s;
                            return;
                          }

                        default: break;
                      }
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
                      expr->emit_info.main_op = MainOp::LEFT;
                      expr->emit_info.main_type = left;
                      expr->emit_info.func = &BinOpArgs::emit_neq_64s;
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
              const auto* int_l = left.unchecked_base<IntegerStructure>();

              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    if (left == right) {
                      switch (int_l->size) {
                        case 8: {
                            expr->node_type = comp_thread->builtin_types->t_bool;
                            expr->emit_info.main_op = MainOp::LEFT;
                            expr->emit_info.main_type = left;
                            if (int_l->is_signed) {
                              expr->emit_info.func = &BinOpArgs::emit_lesser_i64s;
                            }
                            else {
                              expr->emit_info.func = &BinOpArgs::emit_lesser_u64s;
                            }
                            return;
                          }

                        default: break;
                      }
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

    case BINARY_OPERATOR::GREATER: {
        switch (left.struct_type()) {
          case STRUCTURE_TYPE::INTEGER: {
              const auto* int_l = left.unchecked_base<IntegerStructure>();

              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    if (left == right) {
                      switch (int_l->size) {
                        case 8: {
                            expr->node_type = comp_thread->builtin_types->t_bool;
                            expr->emit_info.main_op = MainOp::LEFT;
                            expr->emit_info.main_type = left;
                            if (int_l->is_signed) {
                              expr->emit_info.func = &BinOpArgs::emit_greater_i64s;
                            }
                            else {
                              expr->emit_info.func = &BinOpArgs::emit_greater_u64s;
                            }
                            return;
                          }

                        default: break;
                      }
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

    case BINARY_OPERATOR::OR: {
        switch (left.struct_type()) {
          case STRUCTURE_TYPE::ENUM: {
              if (left == right) {
                const auto* en = left.extract_base<EnumStructure>();
                ASSERT(en->base.struct_type() == STRUCTURE_TYPE::INTEGER);

                switch (en->base.structure->size) {
                  case 1: {
                      expr->node_type = left;
                      expr->emit_info.main_op = MainOp::LEFT;
                      expr->emit_info.main_type = left;
                      expr->emit_info.func = &BinOpArgs::emit_or_8s;
                      return;
                    }

                  default: break;
                }
              }

              break;
            }

          case STRUCTURE_TYPE::INTEGER: {
              const auto* int_l = left.unchecked_base<IntegerStructure>();

              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    if (left == right) {
                      switch (int_l->size) {
                        case 1: {
                            expr->node_type = left;
                            expr->emit_info.main_op = MainOp::LEFT;
                            expr->emit_info.main_type = left;
                            expr->emit_info.func = &BinOpArgs::emit_or_8s;
                            return;
                          }
                        case 8: {
                            expr->node_type = left;
                            expr->emit_info.main_op = MainOp::LEFT;
                            expr->emit_info.main_type = left;
                            expr->emit_info.func = &BinOpArgs::emit_or_64s;
                            return;
                          }

                        default: break;
                      }
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

    case BINARY_OPERATOR::XOR: {
        switch (left.struct_type()) {
          case STRUCTURE_TYPE::INTEGER: {
              const auto* int_l = left.unchecked_base<IntegerStructure>();

              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    if (left == right) {
                      switch (int_l->size) {
                        case 1: {
                            expr->node_type = left;
                            expr->emit_info.main_op = MainOp::LEFT;
                            expr->emit_info.main_type = left;
                            expr->emit_info.func = &BinOpArgs::emit_xor_8s;
                            return;
                          }
                        case 8: {
                            expr->node_type = left;
                            expr->emit_info.main_op = MainOp::LEFT;
                            expr->emit_info.main_type = left;
                            expr->emit_info.func = &BinOpArgs::emit_or_64s;
                            return;
                          }

                        default: break;
                      }
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

    case BINARY_OPERATOR::AND: {
        switch (left.struct_type()) {
          case STRUCTURE_TYPE::ENUM: {
              if (left == right) {
                const auto* en = left.extract_base<EnumStructure>();
                ASSERT(en->base.struct_type() == STRUCTURE_TYPE::INTEGER);

                switch (en->base.structure->size) {
                  case 1: {
                      expr->node_type = left;
                      expr->emit_info.main_op = MainOp::LEFT;
                      expr->emit_info.main_type = left;
                      expr->emit_info.func = &BinOpArgs::emit_and_8s;
                      return;
                    }

                  default: break;
                }
              }

              break;
            }

          case STRUCTURE_TYPE::INTEGER: {
              const auto* int_l = left.unchecked_base<IntegerStructure>();

              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    if (left == right) {
                      switch (int_l->size) {
                        case 1: {
                            expr->node_type = left;
                            expr->emit_info.main_op = MainOp::LEFT;
                            expr->emit_info.main_type = left;
                            expr->emit_info.func = &BinOpArgs::emit_and_8s;
                            return;
                          }
                        case 8: {
                            expr->node_type = left;
                            expr->emit_info.main_op = MainOp::LEFT;
                            expr->emit_info.main_type = left;
                            expr->emit_info.func = &BinOpArgs::emit_and_64s;
                            return;
                          }

                        default: break;
                      }
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
  }

  const char* const op_string = BINARY_OP_STRING::get(expr->op);

  comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->node_span,
                            "No binary operator '{}' exists for left type: '{}', and right type: '{}'",
                            op_string, left.name, right.name);
  return;
}

constexpr static bool already_const_type(AST_TYPE at) {
  return at == AST_TYPE::NUMBER
    || at == AST_TYPE::ASCII_STRING
    || at == AST_TYPE::ASCII_CHAR
    || at == AST_TYPE::STRUCT;
}

constexpr static bool can_compile_const_value(AST_LOCAL expr) {
  return expr->value == nullptr
    && TEST_MASK(expr->meta_flags, META_FLAG::COMPTIME)
    && !already_const_type(expr->ast_type);
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
                                       State* const state,
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
      ASSERT(global->constant_value.ptr != nullptr);
      ASSERT(global->constant_value.size == sizeof(usize));

      call->sig = sig_struct;
      call->label = *(usize*)global->constant_value.ptr;
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

  //if(comp->is_panic()) //Currently not needed because this is the last thing
}

static RuntimeValue take_address(CompilerGlobals* comp, State* state, CodeBlock* code, const RuntimeValue* val) {
  UnOpArgs args = {};
  args.comp = comp;
  args.state = state;
  args.code = code;
  args.prim = val;

  return args.emit_address();
}


static void cast_operator_type(CompilerGlobals* const comp,
                               CompilerThread* const comp_thread,
                               State* const state,
                               ASTCastExpr* const cast) {
  TRACING_FUNCTION();

  const Type& cast_to = *(const Type*)(cast->type->value);
  META_FLAGS from_flags = cast->expr->meta_flags;
  const Type& cast_from = cast->expr->node_type;

  DEFER(&) { if (!comp_thread->is_panic()) ASSERT(cast->emit != nullptr); };

  const auto emit_cast_func = [from_flags, &cast_to](ASTCastExpr* cast, CAST_FUNCTION cast_fn) {
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
            //Must be in memory to cast like this
            set_runtime_flags(cast->expr, state, false, (uint8_t)RVT::MEMORY);
            emit_cast_func(cast, take_address);
            return;
          }
        }

        break;
      }
    case STRUCTURE_TYPE::INTEGER: {
        const auto* from_int = cast_from.unchecked_base<IntegerStructure>();

        if (cast_to.struct_type() == STRUCTURE_TYPE::INTEGER) {
          const auto* to_int = cast_to.unchecked_base<IntegerStructure>();

          if (from_int->size >= to_int->size) {
            //Can cast down ints easily
            emit_cast_func(cast, CASTS::no_op);
            return;
          }
          else {
            //Cast up in size
            //Need specific instructions for different sizes
            //Can cast up to r64 and then "cast down" which is free

            if (from_int->size == 1) {
              if (from_int->is_signed) {
                emit_cast_func(cast, CASTS::i8_to_r64);
                return;
              }
              else {
                emit_cast_func(cast, CASTS::u8_to_r64);
                return;
              }
            }
            else if (from_int->size == 4) {
              if (from_int->is_signed) {
                emit_cast_func(cast, CASTS::i32_to_r64);
                return;
              }
              else {
                emit_cast_func(cast, CASTS::u32_to_r64);
                return;
              }
            }
          }

          comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, cast->node_span,
                                    "Cannot cast type '{}' to type '{}'\n"
                                    "They are both integers and this should be implemented",
                                    cast_from.name, cast_to.name);
          return;
        }
        else if (cast_to.struct_type() == STRUCTURE_TYPE::ASCII_CHAR) {
          if (from_int->size == 1 && !from_int->is_signed) {
            //Can case u8 to ascii
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

static void do_literal_cast(CompilerGlobals* const comp,
                            CompilerThread* const comp_thread,
                            const AST_LOCAL expr, const Type& to_type,
                            const uint8_t* from, uint8_t* to) {
  TRACING_FUNCTION();
  //This should not be reached if the cast doesnt work

  const Type& from_type = expr->node_type;

  if (from_type == to_type) {
    const size_t size = from_type.structure->size;
    memcpy_ts(to, size, from, size);
    return;
  }

  switch (from_type.structure->type) {
    case STRUCTURE_TYPE::FIXED_ARRAY: {
        ASSERT(expr->ast_type == AST_TYPE::ARRAY_EXPR);
        ASTArrayExpr* arr_expr = (ASTArrayExpr*)expr;

        const auto* from_arr = from_type.unchecked_base<ArrayStructure>();
        const auto* to_arr = to_type.unchecked_base<ArrayStructure>();

        ASSERT(to_arr->type == STRUCTURE_TYPE::FIXED_ARRAY);
        ASSERT(from_arr->length == to_arr->length);

        const size_t from_base_size = from_arr->base.structure->size;
        const size_t to_base_size = to_arr->base.structure->size;

        usize count = 0;
        FOR_AST(arr_expr->elements, it) {
          do_literal_cast(comp, comp_thread, it, to_arr->base, from + (count * from_base_size), to + (count * to_base_size));
          if (comp_thread->is_panic()) {
            return;
          }

          count += 1;
        }

        break;
      }
    default: {
        comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, expr->node_span,
                                  "Attempted to cast '{}' to '{}' as if they were literals\n"
                                  "This is apparently invalid BUT the compiler thought it was valid ...\n"
                                  "There are 2 options:\n"
                                  "\tThe function which actually decides how to do the cast has a bug\n"
                                  "\tThe function which checks if the cast is valid has a bug\n\n",
                                  "Sorry :( This text exists because they are in 2 different parts of the codebase and it doesnt seem that farfetched that I would remove/add a case in one function and forget about the other\n\n"
                                  "At the time of writing im doing a big refactor in type system (meta type flags and distinct types are fun) and I basically have to do what feels like 100 million minor changes and writing this is my escapism, thank you for reading it and knowing my pain\n\n"
                                  "Again apology for the bug");
        return;
      }
  }
}

//Return true on success, false on fail/wait
bool type_check_single_node(CompilerGlobals* const comp,
                            CompilerThread* const comp_thread,
                            Context* const context,
                            State* const state,
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
    case AST_TYPE::INVALID: INVALID_CODE_PATH(); break;

    case AST_TYPE::NAMED_TYPE: {
        ASTNamedType* nt = (ASTNamedType*)a;

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

        ASSERT(global->constant_value.ptr != nullptr);
        /*if (global->constant_value.ptr == nullptr) {
          ASSERT(g->unit != nullptr);
          comp->set_dep(context, g->unit);
          return;
        }*/

        nt->value = global->constant_value.ptr;
        nt->node_type = comp_thread->builtin_types->t_type;

        return true;
      }
    case AST_TYPE::ARRAY_TYPE: {
        ASTArrayType* at = (ASTArrayType*)a;

        if (!at->base->node_type.is_valid()) {
          typer->push_node(at->base, comp_thread->builtin_types->t_type);
          return false;
        }

        expect_type(comp_thread, at->base);
        if (comp_thread->is_panic()) {
          return  false;
        }

        ASSERT(at->expr != nullptr);
        ASSERT(at->expr->value != nullptr);

        if (!TYPE_TESTS::is_int(at->expr->node_type)) {
          comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, at->expr->node_span,
                                    "Expected an integer type value for array length\n"
                                    "Instead found: {}", at->expr->node_type.name);
          return false;
        }

        //TODO: Check this stuff

        uint64_t length;
        if (TYPE_TESTS::is_signed_int(at->expr->node_type)) {
          int64_t i_length = *(const int64_t*)at->expr->value;
          if (i_length < 0) {
            comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, at->expr->node_span,
                                      "Length of array must positive\n"
                                      "Instead found: {}", i_length);
            return false;
          }
          else {
            length = (uint64_t)i_length;
          }
        }
        else {
          length = *(const uint64_t*)at->expr->value;
        }

        const Structure* s;
        {
          AtomicLock<Structures> structures = {};
          AtomicLock<StringInterner> strings = {};
          comp->services.get_multiple(&structures, &strings);

          s = find_or_make_array_structure(structures._ptr,
                                           strings._ptr,
                                           *(const Type*)at->base->value, length);
        }

        Type* type = comp->new_constant<Type>();
        type->name = s->struct_name;
        type->structure = s;

        at->value = type;
        at->node_type = comp_thread->builtin_types->t_type;

        return true;
      }
    case AST_TYPE::PTR_TYPE: {
        ASTPtrType* ptr = (ASTPtrType*)a;

        if (!ptr->base->node_type.is_valid()) {
          typer->push_node(ptr->base, comp_thread->builtin_types->t_type);
          return false;
        }

        expect_type(comp_thread, ptr->base);
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
                                             comp_thread->build_options.ptr_size, *(const Type*)(ptr->base->value));
        }

        Type* type = comp->new_constant<Type>();
        type->name = s->struct_name;
        type->structure = s;

        ptr->value = type;
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
          expect_type(comp_thread, i);
          if (comp_thread->is_panic()) {
            return false;
          }

          ASSERT(i != nullptr);
          ASSERT(i->value != nullptr);
          args.insert(*(const Type*)(i->value));
        }

        expect_type(comp_thread, lt->ret);
        if (comp_thread->is_panic()) {
          return false;
        }

        ASSERT(lt->ret != nullptr);
        ASSERT(lt->ret->value != nullptr);

        Type ret = *(const Type*)lt->ret->value;

        const Structure* s;
        {
          AtomicLock<Structures> structures = {};
          AtomicLock<StringInterner> strings = {};
          comp->services.get_multiple(&structures, &strings);
          s = find_or_make_lamdba_structure(structures._ptr,
                                            strings._ptr,
                                            comp_thread->build_options.ptr_size,
                                            comp_thread->build_options.default_calling_convention,
                                            std::move(args), ret);
        }

        Type* type = comp->new_constant<Type>();
        type->name = s->struct_name;
        type->structure = s;

        lt->value = type;

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
          expect_type(comp_thread, i);
          if (comp_thread->is_panic()) {
            return false;
          }

          args.insert(*(const Type*)(i->value));
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

        Type* type = comp->new_constant<Type>();
        type->name = s->struct_name;
        type->structure = s;

        tt->value = type;
        tt->node_type = comp_thread->builtin_types->t_type;
        return true;
      }
    case AST_TYPE::STRUCT_EXPR: {
        ASTStructExpr* se = (ASTStructExpr*)a;
        ASTStructBody* struct_body = (ASTStructBody*)se->struct_body;

        ASSERT(struct_body->value != nullptr && ((const Type*)struct_body->value)->is_valid());

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

        const SignatureStructure* sig_struct;
        {
          AtomicLock<Structures> structures = {};
          AtomicLock<StringInterner> strings = {};
          comp->services.get_multiple(&structures, &strings);

          sig_struct = find_or_make_lamdba_structure(structures._ptr,
                                                     strings._ptr,
                                                     comp_thread->build_options.ptr_size,
                                                     ast_sig->convention,
                                                     std::move(params),
                                                     *(const Type*)(ast_sig->return_type->value));
        }

        ast_sig->node_type = comp_thread->builtin_types->t_void;
        ast_sig->sig->sig_struct = sig_struct;

        return true;
      }

    case AST_TYPE::LAMBDA: {
        ASTLambda* lambda = (ASTLambda*)a;
        ASTFuncSig* ast_sig = (ASTFuncSig*)lambda->sig;

        ASSERT(ast_sig->node_type.is_valid());//should be done in the signature unit

        if (!lambda->body->node_type.is_valid()) {
          state->return_type = *(const Type*)ast_sig->return_type->value;//only do once

          typer->push_node(lambda->body, {});
          return false;
        }

        lambda->node_type = to_type(lambda->function->signature.sig_struct);
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
                                                                    comp_thread->build_options.ptr_size, as->base));
            }

            //TODO: do this outside of memory
            set_runtime_flags(base, state, false, (u8)RVT::MEMORY);
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

        //TODO: Make this work for other types
        set_runtime_flags(base, state, false, (uint8_t)RVT::MEMORY);
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
          //If the index is not known at compile time then the array must be in memory or in a register
          uint8_t valids = (uint8_t)RVT::MEMORY;

          if (base->node_type.structure->size <= 8) {
            //Can be in a register if it will fit
            valids |= RVT::REGISTER;
          }

          set_runtime_flags(base, state, false, valids);
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

          ASSERT(ty_g->constant_value.ptr != nullptr && ty_g->constant_value.size == sizeof(Type));
          tup->named_type = *(const Type*)ty_g->constant_value.ptr;
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
          if (num->suffix == comp_thread->builtin_types->t_i64.name) {
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

    case AST_TYPE::STATIC_LINK: {
        ASTStaticLink* imp = (ASTStaticLink*)a;

        if (!imp->import_type->node_type.is_valid()) {
          typer->push_node(imp->import_type, comp_thread->builtin_types->t_type);
          return false;
        }

        imp->node_type = *(const Type*)imp->import_type->value;

        FileLocation loc;
        {
          auto strings = comp->services.strings.get();

          loc = parse_file_location(comp_thread->build_options.lib_folder->string, imp->lib_file->string,
                                    strings._ptr);
        }

        FOR(comp->lib_import, it) {
          if (it->path == loc.full_name
              && imp->name == it->name) {
            //Already imported
            goto ALREADY_IMPORTED;
          }
        }

        {
          LibraryImport import_lib = {};
          import_lib.name = imp->name;
          import_lib.path = loc.full_name;
          import_lib.label = comp->labels++;

          comp->lib_import.insert(std::move(import_lib));
          imp->import_index = comp->lib_import.size;
        }

      ALREADY_IMPORTED:
        ASSERT(a->node_type.is_valid());
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

        //may set comtime to false
        cast_operator_type(comp, comp_thread, state, cast_expr);
        if (comp_thread->is_panic()) {
          return false;
        }

        pass_meta_flags_down(&a->meta_flags, &cast->meta_flags);

        a->node_type = *(const Type*)ty->value;
        return true;
      }
    case AST_TYPE::UNARY_OPERATOR: {
        return type_check_unary_operator(comp, comp_thread, state, typer, this_untyped);
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

        if (!TEST_MASK(a->meta_flags, META_FLAG::COMPTIME)) {
          if (can_compile_const_value(left)) {
            UnitID id = compile_and_execute(comp, context->current_unit->available_names, left, {});

            set_dependency(comp_thread, context, id);
          }
          else if (can_compile_const_value(right)) {
            UnitID id = compile_and_execute(comp, context->current_unit->available_names, right, {});

            set_dependency(comp_thread, context, id);
          }
        }

        //TODO: inference
        type_check_binary_operator(comp, comp_thread, state, typer, this_untyped);
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

        compile_find_function_call(comp, comp_thread, context, state, call);
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

          Type type = *(const Type*)(decl->type_ast->value);

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

        if (can_compile_const_value(decl_expr)) {
          UnitID id = compile_and_execute(comp, context->current_unit->available_names, decl_expr, {});

          set_dependency(comp_thread, context, id);
        }

        //TODO: Do globals need to check for shadowing?

        ASSERT(decl->global_ptr != nullptr);
        decl->global_ptr->decl.type = decl->type;

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

          Type type = *(const Type*)(decl->type_ast->value);

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

        if (can_compile_const_value(decl_expr)) {
          UnitID id = compile_and_execute(comp, context->current_unit->available_names, decl_expr, {});

          set_dependency(comp_thread, context, id);
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

        a->node_type = comp_thread->builtin_types->t_void;
        return true;
      }
    case AST_TYPE::RETURN: {
        ASTReturn* ret = (ASTReturn*)a;
        ASSERT(state->return_type.is_valid());

        if (ret->expr != nullptr) {
          if (!ret->expr->node_type.is_valid()) {
            typer->push_node(ret->expr, state->return_type);
            return false;
          }

          if (can_compile_const_value(ret->expr)) {
            UnitID id = compile_and_execute(comp, context->current_unit->available_names, ret->expr, {});

            set_dependency(comp_thread, context, id);
          }
        }
        else {
          if (state->return_type != comp_thread->builtin_types->t_void) {
            comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, ret->node_span,
                                      "Return type was {}, yet the expression was empty\n",
                                      state->return_type.name);
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

        ASSERT(body->value == nullptr);

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
            AST_LOCAL ty = tn->type;

            cmp_s->elements.insert_uninit(1);
            auto* b = cmp_s->elements.back();

            b->type = *(const Type*)ty->value;
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

          Type* ty = comp->new_constant<Type>();
          *ty = to_type(cmp_s);

          body->value = ty;
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

        //Extract the value of the type
        ASSERT(name->type->value != nullptr);
        name->node_type = *(const Type*)name->type->value;

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
                           State* const state,
                           AST_LOCAL ast) {
  Typer typer = {};

  typer.push_node(ast, {});
  typer.load_new_nodes();

  while (typer.untyped_stack.size > 0) {
    UntypedNode* n = typer.untyped_stack.back();

    bool finished = type_check_single_node(comp, comp_thread, context, state, &typer, n);
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


static RuntimeValue advance_runtime_arg(State* state,
                                        CallingConvArgIterator* itr,
                                        const Structure* s) {
  RuntimeValue val = {};
  if (itr->regs_used < itr->conv->num_parameter_registers) {
    //Load into register
    val.type = RVT::REGISTER;
    val.reg = state->new_value();

    auto* reg_val = state->get_val(val.reg);
    reg_val->value_type = ValueType::FIXED;
    reg_val->reg = itr->conv->parameter_registers[itr->regs_used];
    itr->regs_used++;
  }
  else {
    //Passed memory
    ASSERT(itr->conv->stack_direction == STACK_DIRECTION::RIGHT_TO_LEFT);

    const size_t size = itr->conv->stack_pass_type == STACK_PASS_TYPE::POINTER ? 8 : s->size;
    const size_t align = itr->conv->stack_pass_type == STACK_PASS_TYPE::POINTER ? 8 : s->alignment;

    const int32_t offset = state->stack.pass_stack_local(size, align);


    val.type = RVT::MEMORY;
    val.mem = state->new_mem();

    auto* val_mem = state->get_mem(val.mem);
    val_mem->mem.base = (uint8_t)state->rsp.val;
    val_mem->mem.disp = offset;
    val_mem->mem.scale = 0;
    //No index: val_mem->mem.index

    val_mem->size = size;
  }

  return val;
}

static RuntimeValue advance_runtime_param(State* state,
                                          CallingConvParamIterator* itr,
                                          const Structure* s) {
  RuntimeValue val = {};
  if (itr->regs_used < itr->conv->num_parameter_registers) {
    //Load into register
    val.type = RVT::REGISTER;
    val.reg = state->new_value();

    auto* reg_val = state->get_val(val.reg);
    reg_val->value_type = ValueType::FIXED;
    reg_val->reg = itr->conv->parameter_registers[itr->regs_used];

    itr->regs_used++;
    state->set_value(val.reg);
  }
  else {
    //Passed memory
    ASSERT(itr->conv->stack_direction == STACK_DIRECTION::RIGHT_TO_LEFT);

    const int32_t size = itr->conv->stack_pass_type == STACK_PASS_TYPE::POINTER ? 8 : s->size;
    const int32_t align = itr->conv->stack_pass_type == STACK_PASS_TYPE::POINTER ? 8 : s->alignment;

    //Determine the stack location
    const int32_t mod_align = itr->stack_passed % align;

    if (mod_align > 0) {
      itr->stack_passed += align - mod_align;
    }

    itr->stack_passed += size;

    val.type = RVT::MEMORY;
    val.mem = state->new_mem();

    auto* val_mem = state->get_mem(val.mem);
    val_mem->mem.base = (uint8_t)state->rbp.val;
    val_mem->mem.disp = itr->stack_passed;
    val_mem->mem.scale = 0;
    //No index: val_mem->mem.index

    val_mem->size = size;
  }

  return val;
}

static RuntimeValue load_to_argument_itr(CompilerGlobals* comp,
                                         State* state,
                                         CodeBlock* const code,
                                         const Structure* s,
                                         RuntimeValue* val,
                                         CallingConvArgIterator* itr) {
  RuntimeValue param = advance_runtime_arg(state, itr, s);

  const bool reg_passed_as_ptr = register_passed_as_pointer(s);

  if (param.type == RVT::REGISTER && reg_passed_as_ptr) {
    //LEA
    UnOpArgs args = {};
    args.comp = comp;
    args.state = state;
    args.code = code;
    args.prim = val;

    //Load as pointer
    RuntimeValue address = args.emit_address();
    const Structure* ptr_type;
    {
      AtomicLock<Structures> structures = {};
      AtomicLock<StringInterner> strings = {};
      comp->services.get_multiple(&structures, &strings);
      ptr_type = find_or_make_pointer_structure(structures._ptr,
                                                strings._ptr,
                                                comp->build_options.ptr_size, to_type(s));
    }

    //Copy
    copy_runtime_to_runtime(comp, state, code, ptr_type, &address, &param);
  }
  else if (param.type == RVT::REGISTER && !reg_passed_as_ptr) {
    copy_runtime_to_runtime(comp, state, code, s, val, &param);
  }
  else if (param.type == RVT::MEMORY
           && (itr->conv->stack_pass_type == STACK_PASS_TYPE::VALUE || !reg_passed_as_ptr)) {
    copy_runtime_to_runtime(comp, state, code, s, val, &param);
  }
  else if (param.type == RVT::MEMORY && itr->conv->stack_pass_type == STACK_PASS_TYPE::POINTER
           && reg_passed_as_ptr) {
    //LEA
    UnOpArgs args = {};
    args.comp = comp;
    args.state = state;
    args.code = code;
    args.prim = val;

    RuntimeValue address = args.emit_address();

    //Copy
    copy_runtime_to_runtime(comp, state, code, s, &address, &param);
  }
  else {
    INVALID_CODE_PATH("Runtime Value union option not covered");
  }

  return param;
}

static void load_const_to_mem(CompilerGlobals* const comp,
                              State* const state,
                              CodeBlock* const code,
                              const Structure* s,
                              ConstantVal constant,
                              MemIndex mem) {

  const uint32_t size = s->size;
  
  //TODO: alignment
  //const uint32_t align = s->alignment;

  const size_t s_div_8 = size / 8;
  const size_t s_mod_8 = size % 8;

  ASSERT(size == constant.size);

  //Needs to go on stack
  auto* const mem_val = state->mem_values.data + mem.index;
  if (mem_val->size > 0) {
    ASSERT(mem_val->size >= size);
  }

  int64_t* iptr = (int64_t*)constant.ptr;

  MemComplex indexed_mem = mem_val->mem;

  if (comp->build_options.endpoint_system == &system_x86_64) {
    auto mov_val = state->new_value();

    for (size_t itr = 0; itr < s_div_8; itr++) {
      const int64_t val = *iptr;

      if (can_be_from_sign_extension(val)) {
        //Can just load the value as 32 bits and it will be sign extended
        state->use_mem(mem);
        ByteCode::EMIT::COPY_64_TO_MEM(code->code, val, indexed_mem);
        state->control_flow.expression_num++;
      }
      else {

        ByteCode::EMIT::SET_R64_TO_64(code->code, (uint8_t)mov_val.val, *iptr);
        state->use_mem(mem);
        state->set_value(mov_val);
        state->control_flow.expression_num++;

        ByteCode::EMIT::COPY_R64_TO_MEM(code->code, (uint8_t)mov_val.val, indexed_mem);
        state->use_mem(mem);
        state->use_value(mov_val);
        state->control_flow.expression_num++;
      }

      iptr++;
      indexed_mem.disp += 8;
    }
  }
  else {
    for (size_t itr = 0; itr < s_div_8; itr++) {
      ByteCode::EMIT::COPY_64_TO_MEM(code->code, *iptr, indexed_mem);
      state->control_flow.expression_num++;

      iptr++;
      indexed_mem.disp += 8;
    }
  }

  const uint8_t* ptr = (const uint8_t*)iptr;

  switch (s_mod_8) {
    case 0: break;//Already loaded
    case 1:
      ByteCode::EMIT::COPY_8_TO_MEM(code->code, ptr[0], indexed_mem);
      break;
    case 2:
      ByteCode::EMIT::COPY_16_TO_MEM(code->code, x16_from_bytes(ptr), indexed_mem);
      break;
    case 3:
      ByteCode::EMIT::COPY_16_TO_MEM(code->code, x16_from_bytes(ptr), indexed_mem);
      indexed_mem.disp += 2;
      ptr += 2;
      ByteCode::EMIT::COPY_8_TO_MEM(code->code, ptr[0], indexed_mem);
      break;
    case 4:
      ByteCode::EMIT::COPY_32_TO_MEM(code->code, x32_from_bytes(ptr), indexed_mem);
      break;
    case 5:
      ByteCode::EMIT::COPY_32_TO_MEM(code->code, x32_from_bytes(ptr), indexed_mem);
      indexed_mem.disp += 4;
      ptr += 4;
      ByteCode::EMIT::COPY_8_TO_MEM(code->code, ptr[0], indexed_mem);
      break;
    case 6:
      ByteCode::EMIT::COPY_32_TO_MEM(code->code, x32_from_bytes(ptr), indexed_mem);
      indexed_mem.disp += 4;
      ptr += 4;
      ByteCode::EMIT::COPY_16_TO_MEM(code->code, x16_from_bytes(ptr), indexed_mem);
      break;
    case 7:
      ByteCode::EMIT::COPY_32_TO_MEM(code->code, x32_from_bytes(ptr), indexed_mem);
      indexed_mem.disp += 4;
      ptr += 4;
      ByteCode::EMIT::COPY_16_TO_MEM(code->code, x16_from_bytes(ptr), indexed_mem);
      indexed_mem.disp += 2;
      ptr += 2;
      ByteCode::EMIT::COPY_8_TO_MEM(code->code, ptr[0], indexed_mem);
      break;
    default:
      INVALID_CODE_PATH("Somehow n mod 8 was bigger than 8 ... We broke maths");
  }
  state->use_mem(mem);
  comp->free_constant((void*)constant.ptr);
  state->control_flow.expression_num++;
}

static void copy_mem_to_mem(CompilerGlobals* const comp,
                            State* const state,
                            CodeBlock* const code,
                            const Structure* s,
                            MemIndex from,
                            MemIndex to) {

  const uint32_t size = s->size;
  
  //TODO: alignment
  //const uint32_t align = s->alignment;

  const size_t s_div_8 = size / 8;
  size_t s_mod_8 = size % 8;

  //Needs to go on stack
  auto* const to_val = state->mem_values.data + to.index;
  if (to_val->size > 0) {
    ASSERT(to_val->size >= size);
  }

  MemComplex to_mem = to_val->mem;

  auto* const from_val = state->mem_values.data + from.index;
  if (from_val->size > 0) {
    ASSERT(from_val->size >= size);
  }

  MemComplex from_mem = from_val->mem;



  auto mov_val = state->new_value();

  for (size_t itr = 0; itr < s_div_8; itr++) {
    ByteCode::EMIT::COPY_R64_FROM_MEM(code->code, (uint8_t)mov_val.val, from_mem);
    state->use_mem(from);
    state->set_value(mov_val);
    state->control_flow.expression_num++;

    ByteCode::EMIT::COPY_R64_TO_MEM(code->code, (uint8_t)mov_val.val, to_mem);
    state->use_mem(to);
    state->use_value(mov_val);
    state->control_flow.expression_num++;

    from_mem.disp += 8;
    to_mem.disp += 8;
  }

  while (s_mod_8 > 0) {
    switch (s_mod_8) {
      case 1: {
          ByteCode::EMIT::COPY_R8_FROM_MEM(code->code, (uint8_t)mov_val.val, from_mem);
          state->use_mem(from);
          state->set_value(mov_val);
          state->control_flow.expression_num++;

          ByteCode::EMIT::COPY_R8_TO_MEM(code->code, (uint8_t)mov_val.val, to_mem);
          state->use_mem(to);
          state->use_value(mov_val);
          state->control_flow.expression_num++;

          from_mem.disp += 1;
          to_mem.disp += 1;
          s_mod_8 -= 1;
          break;
        }
      case 2: {
          ByteCode::EMIT::COPY_R16_FROM_MEM(code->code, (uint8_t)mov_val.val, from_mem);
          state->use_mem(from);
          state->set_value(mov_val);
          state->control_flow.expression_num++;

          ByteCode::EMIT::COPY_R16_TO_MEM(code->code, (uint8_t)mov_val.val, to_mem);
          state->use_mem(to);
          state->use_value(mov_val);
          state->control_flow.expression_num++;

          from_mem.disp += 2;
          to_mem.disp += 2;
          s_mod_8 -= 2;
          break;
        }
      case 4: {
          ByteCode::EMIT::COPY_R32_FROM_MEM(code->code, (uint8_t)mov_val.val, from_mem);
          state->use_mem(from);
          state->set_value(mov_val);
          state->control_flow.expression_num++;

          ByteCode::EMIT::COPY_R32_TO_MEM(code->code, (uint8_t)mov_val.val, to_mem);
          state->use_mem(to);
          state->use_value(mov_val);
          state->control_flow.expression_num++;

          from_mem.disp += 4;
          to_mem.disp += 4;
          s_mod_8 -= 4;
          break;
        }
    }
  }
}

static void emit_copy_r_to_mem(const Structure* s, Array<uint8_t>& arr, uint8_t r, const MemComplex& mem) {
  switch (s->size) {
    case 1:
      ByteCode::EMIT::COPY_R8_TO_MEM(arr, r, mem);
      break;
    case 2:
      ByteCode::EMIT::COPY_R16_TO_MEM(arr, r, mem);
      break;
    case 4:
      ByteCode::EMIT::COPY_R32_TO_MEM(arr, r, mem);
      break;
    case 8:
      ByteCode::EMIT::COPY_R64_TO_MEM(arr, r, mem);
      break;
    default:
      INVALID_CODE_PATH("Cant handle register sizes that arent a power of 2");
  }
}

static void emit_copy_r_from_mem(const Structure* s, Array<uint8_t>& arr, uint8_t r, const MemComplex& mem) {
  switch (s->size) {
    case 1:
      ByteCode::EMIT::COPY_R8_FROM_MEM(arr, r, mem);
      break;
    case 2:
      ByteCode::EMIT::COPY_R16_FROM_MEM(arr, r, mem);
      break;
    case 4:
      ByteCode::EMIT::COPY_R32_FROM_MEM(arr, r, mem);
      break;
    case 8:
      ByteCode::EMIT::COPY_R64_FROM_MEM(arr, r, mem);
      break;
    default:
      INVALID_CODE_PATH("Cant handle register sizes that arent a power of 2");
  }
}

static void emit_copy_r_to_r(const Structure* s, Array<uint8_t>& arr, uint8_t r1, uint8_t r2) {
  switch (s->size) {
    case 1:
      ByteCode::EMIT::COPY_R8_TO_R8(arr, r1, r2);
      break;
    case 2:
      ByteCode::EMIT::COPY_R16_TO_R16(arr, r1, r2);
      break;
    case 4:
      ByteCode::EMIT::COPY_R32_TO_R32(arr, r1, r2);
      break;
    case 8:
      ByteCode::EMIT::COPY_R64_TO_R64(arr, r1, r2);
      break;
    default:
      INVALID_CODE_PATH("Cant handle register sizes that arent a power of 2");
  }
}

static void load_const_to_reg(CompilerGlobals* const comp,
                              State* const state,
                              CodeBlock* const code,
                              ConstantVal constant,
                              ValueIndex reg) {
  ASSERT(constant.size <= 8);

  switch (constant.size) {
    case 1: {
        uint8_t u8 = *(const uint8_t*)constant.ptr;
        ByteCode::EMIT::SET_R8_TO_8(code->code, (uint8_t)reg.val, u8);
        break;
      }
    case 2: {
        uint16_t u16 = x16_from_bytes((const uint8_t*)constant.ptr);
        ByteCode::EMIT::SET_R16_TO_16(code->code, (uint8_t)reg.val, u16);
        break;
      }
    case 4: {
        uint32_t u32 = x32_from_bytes((const uint8_t*)constant.ptr);
        ByteCode::EMIT::SET_R32_TO_32(code->code, (uint8_t)reg.val, u32);
        break;
      }
    case 8: {
        uint64_t u64 = x64_from_bytes((const uint8_t*)constant.ptr);
        ByteCode::EMIT::SET_R64_TO_64(code->code, (uint8_t)reg.val, u64);
        break;
      }
    default:
      printf("ERROR: Unsupported constant size: %zu\n", constant.size);
      INVALID_CODE_PATH("Cant handle register sizes that arent a power of 2");
  }

  comp->free_constant((void*)constant.ptr);

  state->set_value(reg);
  state->control_flow.expression_num++;
}

RuntimeValue new_const_in_reg(CompilerGlobals* const comp, State* const state, CodeBlock* const code, const uint8_t* data, size_t size) {
  ValueIndex val = state->new_value();

  RuntimeValue r = {};
  r.type = RVT::REGISTER;
  r.reg = val;

  state->set_value(val);

  switch (size) {
    case 1: {
        uint8_t u8 = *data;
        ByteCode::EMIT::SET_R8_TO_8(code->code, (uint8_t)val.val, u8);
        break;
      }
    case 2: {
        uint16_t u16 = x16_from_bytes(data);
        ByteCode::EMIT::SET_R16_TO_16(code->code, (uint8_t)val.val, u16);
        break;
      }
    case 4: {
        uint32_t u32 = x32_from_bytes(data);
        ByteCode::EMIT::SET_R32_TO_32(code->code, (uint8_t)val.val, u32);
        break;
      }
    case 8: {
        uint64_t u64 = x64_from_bytes(data);
        ByteCode::EMIT::SET_R64_TO_64(code->code, (uint8_t)val.val, u64);
        break;
      }
    default:
      printf("ERROR: Unsupported constant size: %zu\n", size);
      INVALID_CODE_PATH("Cant handle register sizes that arent a power of 2");
  }

  state->control_flow.expression_num++;

  return r;
}

void load_const_to_runtime_val(CompilerGlobals* const comp,
                               State* const state,
                               CodeBlock* const code,
                               const Structure* type,
                               ConstantVal constant,
                               RuntimeValue* const to) {
  switch (to->type) {
    case RVT::CONST:
    case RVT::UNKNOWN: {
        to->type = RVT::CONST;
        to->constant = constant;
        break;
      }
    case RVT::REGISTER: {
        load_const_to_reg(comp, state, code, constant, to->reg);
        break;
      }
    case RVT::MEMORY: {
        load_const_to_mem(comp, state, code, type, constant, to->mem);
        break;
      }
  }
}

static void copy_mem_to_runtime(CompilerGlobals* const comp,
                                State* const state,
                                CodeBlock* const code,
                                const Structure* type,
                                MemIndex mem,
                                RuntimeValue* to) {
  ASSERT(to->type != RVT::CONST);
  DEFER(&) { state->control_flow.expression_num++; };

  switch (to->type) {
    case RVT::CONST: INVALID_CODE_PATH(); break;
    case RVT::UNKNOWN: {
        to->type = RVT::MEMORY;
        to->mem = mem;
        break;
      }
    case RVT::MEMORY: {
        if (to->mem == mem) {
          return;//Are the same so no need to copy
        }

        copy_mem_to_mem(comp, state, code, type, mem, to->mem);
        break;
      }
    case RVT::REGISTER: {

        emit_copy_r_from_mem(type, code->code, (uint8_t)to->reg.val, state->get_mem(mem)->mem);
        state->set_value(to->reg);
        state->use_mem(mem);
        break;
      }
  }
}

static void copy_reg_to_runtime(CompilerGlobals* const comp,
                                State* const state,
                                CodeBlock* const code,
                                const Structure* structure,
                                ValueIndex reg,
                                RuntimeValue* to) {
  ASSERT(to->type != RVT::CONST);
  DEFER(&) { state->control_flow.expression_num++; };

  size_t size = structure->size;
  ASSERT(size <= 8);

  switch (to->type) {
    case RVT::MEMORY: {
        emit_copy_r_to_mem(structure, code->code, (uint8_t)reg.val, state->get_mem(to->mem)->mem);
        state->use_mem(to->mem);
        state->use_value(reg);
        break;
      }
    case RVT::REGISTER: {
        if (reg == to->reg) {
          return;//are the same so no need to copy
        }

        emit_copy_r_to_r(structure, code->code, (uint8_t)reg.val, (uint8_t)to->reg.val);
        state->value_copy(reg, to->reg);
        break;
      }
    default: {
        to->type = RVT::REGISTER;
        to->reg = state->new_value();

        emit_copy_r_to_r(structure, code->code, (uint8_t)reg.val, (uint8_t)to->reg.val);
        state->value_copy(reg, to->reg);
        break;
      }
  }
}

void copy_runtime_to_runtime(CompilerGlobals* const comp,
                             State* const state,
                             CodeBlock* const code,
                             const Structure* structure,
                             const RuntimeValue* from,
                             RuntimeValue* to) {
  if (*from == *to) {
    return;
  }

  DEFER(&) { state->control_flow.expression_num++; };

  ASSERT(from->type != RVT::UNKNOWN);

  switch (from->type) {
    case RVT::UNKNOWN: INVALID_CODE_PATH(); break;
    case RVT::CONST: {
        load_const_to_runtime_val(comp, state, code, structure, from->constant, to);
        break;
      }
    case RVT::REGISTER: {
        copy_reg_to_runtime(comp, state, code, structure, from->reg, to);
        break;
      }
    case RVT::MEMORY: {
        copy_mem_to_runtime(comp, state, code, structure, from->mem, to);
      }
  }
}

static void load_runtime_hint(CompilerGlobals* const comp,
                              State* const state,
                              const Structure* s,
                              RuntimeHint* hint,
                              uint8_t possible) {
  ASSERT(hint->is_hint);
  hint->is_hint = false;

  ASSERT(possible != 0);//Cant have no options ever
  possible &= hint->hint_types;
  ASSERT(possible != 0);//Cant have overlapping options

  size_t size = s->size;

  if ((possible & (uint8_t)RVT::CONST) > 0) {
    hint->val.type = RVT::CONST;
  }
  else if (size <= 8 && (possible & (uint8_t)RVT::REGISTER) > 0) {
    hint->val.type = RVT::REGISTER;
    hint->val.reg = state->new_value();
  }
  else if ((possible & (uint8_t)RVT::MEMORY) > 0) {
    hint->val.type = RVT::MEMORY;
    hint->val.mem.index = state->mem_values.size;

    state->mem_values.insert_uninit(1);

    auto* stack_val = state->mem_values.back();
    stack_val->mem.base = (uint8_t)state->rbp.val;
    stack_val->mem.disp = state->stack.next_stack_local(size, s->alignment);
    stack_val->mem.scale = 0;
    stack_val->size = size;
  }
  else {
    hint->val.type = RVT::UNKNOWN;
  }
}

void copy_runtime_to_runtime_hint(CompilerGlobals* const comp,
                                  State* const state,
                                  CodeBlock* const code,
                                  const Structure* type,
                                  const RuntimeValue* from,
                                  RuntimeHint* to,
                                  const uint8_t possible_types) {

  if (!to->is_hint) {
    ASSERT(((uint8_t)to->val.type & possible_types) > 0);
    copy_runtime_to_runtime(comp, state, code, type, from, &to->val);
  }
  else {
    if ((possible_types & (uint8_t)from->type) > 0 && ((uint8_t)from->type & to->hint_types) > 0) {
      //Copying type is valid in both
      //For now just copy that runtime value isntead of doing a runtime copy
      to->is_hint = false;
      to->val = *from;
    }
    else {
      load_runtime_hint(comp, state, type, to, possible_types);
      copy_runtime_to_runtime(comp, state, code, type, from, &to->val);
    }
  }
}

static RuntimeValue load_data_memory(State* state, size_t size, size_t index, CodeBlock* code) {
  const auto reg_mem = state->new_value();


  ByteCode::EMIT::LOAD_DATA_MEM(code->code, (uint8_t)reg_mem.val, index);//changed later to be the actual location
  state->set_value(reg_mem);

  state->control_flow.expression_num++;

  RuntimeValue global_mem = {};
  global_mem.type = RVT::MEMORY;
  global_mem.mem = state->new_mem();

  MemValue* mem_val = state->get_mem(global_mem.mem);
  mem_val->size = size;
  mem_val->mem.base = (uint8_t)reg_mem.val;

  return global_mem;
}

static void compile_bytecode_of_expression(CompilerGlobals* const comp,
                                           Context* const context,
                                           State* const state,
                                           CodeBlock* const code,
                                           AST_LOCAL expr,
                                           RuntimeHint* hint);

static RuntimeValue compile_bytecode_of_expression_new(CompilerGlobals* const comp,
                                                       Context* const context,
                                                       State* const state,
                                                       CodeBlock* const code,
                                                       AST_LOCAL expr,
                                                       uint8_t hint) {
  RuntimeHint rt_hint = {};
  rt_hint.is_hint = true;
  rt_hint.hint_types = hint;
  compile_bytecode_of_expression(comp, context, state, code, expr, &rt_hint);

  ASSERT(!rt_hint.is_hint);
  ASSERT(((uint8_t)rt_hint.val.type & hint) > 0);
  return rt_hint.val;
}

static void compile_bytecode_of_expression_existing(CompilerGlobals* const comp,
                                                    Context* const context,
                                                    State* const state,
                                                    CodeBlock* const code,
                                                    AST_LOCAL expr,
                                                    RuntimeValue* hint) {
  ASSERT(hint->type != RVT::CONST);//Cant load to an existing constant

  RuntimeHint rt_hint = {};
  rt_hint.is_hint = false;
  rt_hint.val = *hint;

  compile_bytecode_of_expression(comp, context, state, code, expr, &rt_hint);

  ASSERT(rt_hint.val == *hint);
}

static void compile_function_call(CompilerGlobals* const comp,
                                  Context* const context,
                                  State* const state,
                                  CodeBlock* const code,
                                  const ASTFunctionCallExpr* const call,
                                  RuntimeHint* hint) {
  TRACING_FUNCTION();

  const auto* sig_struct = call->sig;

  auto save_stack_params = state->stack.current_passed;
  DEFER(&) { state->stack.current_passed = save_stack_params; };

  state->made_call = true;
  state->stack.require_call_alignment(16);//TODO: Actually work out the alignment

  const CallingConvention* convention = sig_struct->calling_convention;

  bool has_return = sig_struct->return_type != comp->builtin_types->t_void;
  bool return_via_pointer = has_return && register_passed_as_pointer(sig_struct->return_type);

  struct StructuredVal {
    RuntimeValue rv = {};
    const Structure* str = nullptr;
  };

  Array<StructuredVal> parameter_vals;
  parameter_vals.reserve_total(call->arguments.count + return_via_pointer);

  if (return_via_pointer) {
    parameter_vals.insert_uninit(1);
  }


  //Compile expression for arguments
  {
    usize i = 0;
    FOR_AST(call->arguments, it) {
      const Type call_type = sig_struct->parameter_types.data[i];

      const RuntimeValue val = compile_bytecode_of_expression_new(comp, context, state, code, it, ALL_RVTS);

      parameter_vals.insert({ val, call_type.structure });
      i++;
    }
  }

  state->control_flow.expression_num++;

  //Set argument registers and stack


  if (return_via_pointer) {
    ASSERT(hint != nullptr);
    //Load the return on the stack and then pass a pointer
    load_runtime_hint(comp, state, sig_struct->return_type.structure, hint, (uint8_t)RVT::MEMORY);

    UnOpArgs args = {};
    args.comp = comp;
    args.state = state;
    args.code = code;
    args.prim = &hint->val;

    //First element is reserved ahead of time
    parameter_vals.data[0].rv = args.emit_address();
    parameter_vals.data[0].str = comp->builtin_types->t_void_ptr.structure;//just any pointer type

    state->control_flow.expression_num++;
  }

  CallingConvArgIterator conv_iter = {
    convention,
    0
  };

  auto i = parameter_vals.mut_begin();
  const auto end = parameter_vals.end();

  //Load the actual arguments
  for (; i < end; i++) {
    RuntimeValue param_val = load_to_argument_itr(comp, state, code, i->str, &i->rv, &conv_iter);

    i->rv = std::move(param_val);
  }

  state->control_flow.expression_num++;

  //use all the registers
  {
    auto i = parameter_vals.begin();
    const auto end = parameter_vals.end();

    for (; i < end; i++) {
      //Cant exit early as the last value might be a register
      if (i->rv.type == RVT::REGISTER) {
        state->use_value(i->rv.reg);
      }
    }
  }

  //size_t stack_params = state->stack.current_passed;

  ByteCode::EMIT::CALL_LABEL(code->code, call->label);
  state->control_flow.calls.insert(state->control_flow.now());

  state->control_flow.expression_num++;

  if (has_return && !return_via_pointer) {
    ASSERT(hint != nullptr);

    //Need to reserve RAX if we didnt already pass a pointer in
    const ValueIndex rax = state->new_value();
    state->set_value(rax);//set by the called function

    {
      auto* rax_val = state->value_tree.values.data + rax.val;

      rax_val->value_type = ValueType::FIXED;
      rax_val->reg = convention->return_register;
    }

    //Fake copy so dont need to insert copy later if one is needed
    state->control_flow.expression_num++;

    if (hint->is_hint) {
      load_runtime_hint(comp, state, call->node_type.structure, hint, call->valid_rvts & NON_CONST_RVTS);
    }

    copy_reg_to_runtime(comp, state, code, call->node_type.structure, rax, &hint->val);
  }
  else {
    //Fixes void functions
    hint->is_hint = false;
  }
}

//Note: Recursive 
static void compile_bytecode_of_expression(CompilerGlobals* const comp,
                                           Context* const context,
                                           State* const state,
                                           CodeBlock* const code,
                                           AST_LOCAL expr,
                                           RuntimeHint* hint) {
  TRACING_FUNCTION();

  ASSERT(hint != nullptr);

  DEFER(&) {
    ASSERT(hint != nullptr);
    ASSERT(!hint->is_hint);

    state->control_flow.expression_num++;
  };

  if (expr->value != nullptr) {
    //Compile time expression

    //Copy the value to a new constant
    const size_t size = expr->node_type.structure->size;

    uint8_t* bytes = (uint8_t*)comp->new_constant(size);
    const uint8_t* c_bytes = (const uint8_t*)expr->value;

    memcpy_ts(bytes, size, c_bytes, size);

    const ConstantVal constant = { bytes, size };

    if (hint->is_hint) {
      load_runtime_hint(comp, state, expr->node_type.structure, hint, expr->valid_rvts);
    }

    load_const_to_runtime_val(comp, state, code, expr->node_type.structure, constant, &hint->val);
    return;
  }

  switch (expr->ast_type) {
    case AST_TYPE::STRUCT_EXPR: {
        ASTStructExpr* se = (ASTStructExpr*)expr;
        ASTStructBody* s = (ASTStructBody*)se->struct_body;

        ASSERT(context->comptime_compilation);

        if (hint->is_hint) {
          load_runtime_hint(comp, state, expr->node_type.structure, hint, expr->valid_rvts);
        }

        Type* struct_c = comp->new_constant<Type>();
        memcpy_ts(struct_c, 1, (const Type*)s->value, 1);

        load_const_to_runtime_val(comp, state, code, expr->node_type.structure,
                                  ConstantVal{ (uint8_t*)struct_c, sizeof(Type) },
                                  &hint->val);
        break;
      }
    case AST_TYPE::LAMBDA_EXPR: {
        ASTLambdaExpr* le = (ASTLambdaExpr*)expr;
        ASTLambda* l = (ASTLambda*)le->lambda;

        ASSERT(context->comptime_compilation);

        if (hint->is_hint) {
          load_runtime_hint(comp, state, expr->node_type.structure, hint, expr->valid_rvts);
        }

        usize* label = comp->new_constant<usize>();
        *label = l->function->code_block.label;

        load_const_to_runtime_val(comp, state, code, expr->node_type.structure,
                                  ConstantVal{ (uint8_t*)label, sizeof(usize) },
                                  &hint->val);
        break;
      }
    case AST_TYPE::MEMBER_ACCESS: {
        ASSERT(hint != nullptr);

        ASTMemberAccessExpr* member_e = (ASTMemberAccessExpr*)expr;
        AST_LOCAL m_base = member_e->expr;

        STRUCTURE_TYPE st = m_base->node_type.struct_type();
        if (st == STRUCTURE_TYPE::COMPOSITE) {
          RuntimeValue obj = compile_bytecode_of_expression_new(comp,
                                                                context,
                                                                state,
                                                                code,
                                                                m_base,
                                                                (uint8_t)RVT::MEMORY);

          ASSERT(obj.type == RVT::MEMORY);

          RuntimeValue member = {};
          member.type = RVT::MEMORY;
          member.mem = state->new_mem();

          //Set up memory as offset from the original object memory
          {
            MemValue* member_mem = state->get_mem(member.mem);
            const MemValue* obj_mem = state->get_mem(obj.mem);

            member_mem->mem = obj_mem->mem;

            member_mem->mem.disp += member_e->offset;
            member_mem->size = expr->node_type.structure->size;
          }

          copy_runtime_to_runtime_hint(comp, state, code, expr->node_type.structure, &member, hint, expr->valid_rvts & NON_CONST_RVTS);
        }
        else if (st == STRUCTURE_TYPE::FIXED_ARRAY) {
          if (member_e->name == comp->important_names.ptr) {
            RuntimeValue obj = compile_bytecode_of_expression_new(comp,
                                                                  context,
                                                                  state,
                                                                  code,
                                                                  m_base,
                                                                  (uint8_t)RVT::MEMORY);

            ASSERT(obj.type == RVT::MEMORY);

            RuntimeValue val = take_address(comp, state, code, &obj);
            copy_runtime_to_runtime_hint(comp, state, code, expr->node_type.structure, &val, hint, expr->valid_rvts & NON_CONST_RVTS);
          }
          else if (member_e->name == comp->important_names.len) {
            const ArrayStructure* as = m_base->node_type.unchecked_base<ArrayStructure>();

            u64 val = as->length;

            const size_t size = sizeof(val);

            uint8_t* val_c = comp->new_constant(size);
            memcpy_ts(val_c, size, (uint8_t*)&val, size);

            const ConstantVal constant = { val_c, size };

            if (hint->is_hint) {
              load_runtime_hint(comp, state, expr->node_type.structure, hint, expr->valid_rvts);
            }

            load_const_to_runtime_val(comp, state, code, expr->node_type.structure, constant, &hint->val);
          }
          else {
            INVALID_CODE_PATH("Not a valid name");
          }
        }
        else {
          INVALID_CODE_PATH("Type does not have members");
        }

        break;
      }
    case AST_TYPE::INDEX_EXPR: {
        ASSERT(hint != nullptr);
        const size_t base_size = expr->node_type.structure->size;

        ASTIndexExpr* index = (ASTIndexExpr*)expr;
        AST_LOCAL index_expr = index->expr;
        AST_LOCAL index_index = index->index;

        ASSERT(TYPE_TESTS::can_index(index_expr->node_type));

        RuntimeValue arr = compile_bytecode_of_expression_new(comp,
                                                              context,
                                                              state,
                                                              code,
                                                              index_expr,
                                                              RVT::REGISTER | RVT::MEMORY);

        RuntimeValue index_val = compile_bytecode_of_expression_new(comp,
                                                                    context,
                                                                    state,
                                                                    code,
                                                                    index_index,
                                                                    RVT::REGISTER | RVT::CONST);

        if (TYPE_TESTS::is_pointer(index_expr->node_type)) {
          //Dereference the pointer to an array

          UnOpArgs args = {};
          args.comp = comp;
          args.state = state;
          args.code = code;
          args.prim = &arr;

          arr = args.emit_deref();
        }
        else {
          ASSERT(TYPE_TESTS::is_array(index_expr->node_type));
        }

        if (arr.type == RVT::REGISTER) {
          //If its in a register then a shift is equivalent to the 
          RuntimeValue use_val = {};
          use_val.type = RVT::REGISTER;
          use_val.reg = state->new_value();

          BinOpArgs args = {};
          args.comp = comp;
          args.state = state;
          args.code = code;

          args.info = nullptr;

          RuntimeValue res;
          if (index_val.type == RVT::CONST) {
            auto index = *(uint64_t*)index_val.constant.ptr;

            const uint8_t shift = (uint8_t)(base_size * index * 8);

            ByteCode::EMIT::SET_R8_TO_8(code->code, (uint8_t)use_val.reg.val, shift);
            state->set_value(use_val.reg);
            state->control_flow.expression_num++;

            args.left = &arr;
            args.right = &use_val;

            res = args.emit_shift_r_u64_by_8();
          }
          else {
            ASSERT(index_val.type == RVT::REGISTER);

            ByteCode::EMIT::SET_R8_TO_8(code->code, (uint8_t)use_val.reg.val, (uint8_t)(base_size * 8));
            state->set_value(use_val.reg);
            state->control_flow.expression_num++;

            args.left = &use_val;
            args.right = &index_val;

            RuntimeValue fixed_index = args.emit_mul_64s();

            args.left = &arr;
            args.right = &fixed_index;

            res = args.emit_shift_r_u64_by_8();
          }

          copy_runtime_to_runtime_hint(comp, state, code, expr->node_type.structure, &res, hint, expr->valid_rvts & NON_CONST_RVTS);
        }
        else {
          RuntimeValue index_mem = {};

          index_mem.type = RVT::MEMORY;

          const MemIndex arr_index = state->new_mem();
          index_mem.mem = arr_index;


          ASSERT(arr.type == RVT::MEMORY);
          //In memory - do a memory index

          if (index_val.type == RVT::CONST) {
            auto index = *(uint64_t*)index_val.constant.ptr;


            const MemValue* arr_mem = state->get_mem(arr.mem);
            MemValue* index_mem = state->get_mem(arr_index);

            const size_t disp_index = (base_size * index);

            if (arr_mem->size > 0) {
              ASSERT(arr_mem->size >= disp_index);//Semi Bounds checking
            }

            index_mem->mem = arr_mem->mem;
            index_mem->size = base_size;
            index_mem->mem.disp += (int32_t)disp_index;

          }
          else {
            ASSERT(index_val.type == RVT::REGISTER);
            ValueIndex index = index_val.reg;


            const MemValue* arr_mem = state->get_mem(arr.mem);
            MemValue* index_mem = state->get_mem(arr_index);

            ASSERT(arr_mem->mem.scale == 0);

            index_mem->mem = arr_mem->mem;
            index_mem->size = base_size;
            index_mem->mem.index = (uint8_t)index.val;
            index_mem->mem.scale = (uint8_t)base_size;

          }

          copy_runtime_to_runtime_hint(comp, state, code, expr->node_type.structure, &index_mem, hint, expr->valid_rvts & NON_CONST_RVTS);
        }

        break;
      }
    case AST_TYPE::TUPLE_LIT: {
        ASSERT(hint != nullptr);
        ASSERT(expr->node_type.struct_type() == STRUCTURE_TYPE::COMPOSITE);
        const auto* cpst = expr->node_type.unchecked_base<CompositeStructure>();

        ASTTupleLitExpr* lit = (ASTTupleLitExpr*)expr;


        //Load the hint to mem
        if (hint->is_hint) {
          load_runtime_hint(comp, state, expr->node_type.structure, hint, expr->valid_rvts);
        }

        ASSERT(hint->val.type == RVT::MEMORY);

        RuntimeValue tup_single = {};
        tup_single.type = RVT::MEMORY;
        tup_single.mem = state->new_mem();
        uint32_t base_disp = 0;

        {
          const MemValue* arr_mem = state->get_mem(hint->val.mem);
          MemValue* el_mem = state->get_mem(tup_single.mem);

          el_mem->mem = arr_mem->mem;
          base_disp = el_mem->mem.disp;
        }

        auto i_t = cpst->elements.begin();

        //save stack as expression stack cant be used by anything - its copied anyway
        auto save_stack = state->stack.current;

        FOR_AST(lit->elements, it) {
          //Set the correct offset
          MemValue* el_mem = state->get_mem(tup_single.mem);

          el_mem->mem.disp = base_disp + i_t->offset;
          el_mem->size = i_t->type.structure->size;

          //Load to that location
          compile_bytecode_of_expression_existing(comp, context, state, code, it, &tup_single);

          state->stack.current = save_stack;//reset stack
          i_t++;
        }
        break;
      }
    case AST_TYPE::ARRAY_EXPR: {
        ASSERT(hint != nullptr);
        const auto* const arr_type = expr->node_type.unchecked_base<ArrayStructure>();

        const size_t base_size = arr_type->base.structure->size;

        //const size_t full_align = arr_type->alignment;
        const size_t full_size = arr_type->size;

        ASTArrayExpr* arr_expr = (ASTArrayExpr*)expr;

        if (hint->is_hint) {
          load_runtime_hint(comp, state, expr->node_type.structure, hint, expr->valid_rvts);
        }

        //TODO: Constants

        if (full_size <= 8) {
          ASSERT(hint->val.type != RVT::CONST);

          //Its shifting time

          AST_LINKED* l = arr_expr->elements.start;

          uint8_t shift_dst = 0;

          ASSERT(l != nullptr);

          //Set up mask
          RuntimeValue mask = {};

          if (full_size > base_size) {
            mask.type = RVT::REGISTER;
            mask.reg = state->new_value();
            state->set_value(mask.reg);

            //Fill the bottom bits as a mask
            ByteCode::EMIT::SET_R64_TO_64(code->code,
                                          (uint8_t)mask.reg.val, bit_fill_lower<uint64_t>((uint8_t)base_size));

            state->control_flow.expression_num++;
          }

          BinOpArgs args = {};
          args.comp = comp;
          args.state = state;
          args.code = code;

          args.info = nullptr;

          //First element doesnt need shifting and works as the base value to shift into
          RuntimeValue res = compile_bytecode_of_expression_new(comp,
                                                                context,
                                                                state,
                                                                code,
                                                                l->curr,
                                                                (uint8_t)RVT::REGISTER);

          args.left = &res;
          args.right = &mask;

          if (full_size > base_size) {
            res = args.emit_add_64s();
          }

          l = l->next;

          while (l) {
            AST_LOCAL i = l->curr;
            l = l->next;

            ASSERT(full_size > base_size);

            RuntimeValue el = compile_bytecode_of_expression_new(comp,
                                                                 context,
                                                                 state,
                                                                 code,
                                                                 i,
                                                                 (uint8_t)RVT::REGISTER);
            args.left = &el;
            args.right = &mask;

            //Ask out upper bits
            el = args.emit_add_64s();

            RuntimeValue shift_val = {};
            shift_val.type = RVT::REGISTER;
            shift_val.reg = state->new_value();

            state->set_value(shift_val.reg);
            ByteCode::EMIT::SET_R8_TO_8(code->code, (uint8_t)shift_val.reg.val, shift_dst);

            shift_dst += (uint8_t)base_size;
            state->control_flow.expression_num++;

            args.left = &el;
            args.right = &shift_val;

            //Shift the value
            RuntimeValue shifted = args.emit_shift_l_64_by_8();

            args.left = &res;
            args.right = &shifted;

            //Or the value
            res = args.emit_or_64s();
          }

          copy_runtime_to_runtime(comp, state, code, expr->node_type.structure, &res, &hint->val);
        }
        else {
          //Can only load large arrays into memory

          ASSERT(hint->val.type == RVT::MEMORY);

          RuntimeValue arr_single = {};
          arr_single.type = RVT::MEMORY;
          arr_single.mem = state->new_mem();

          {
            const MemValue* arr_mem = state->get_mem(hint->val.mem);
            MemValue* index_mem = state->get_mem(arr_single.mem);

            index_mem->mem = arr_mem->mem;
            index_mem->size = base_size;
          }

          //save stack as expression stack cant be used by anything - its copied anyway
          auto save_stack = state->stack.current;

          FOR_AST(arr_expr->elements, it) {
            compile_bytecode_of_expression_existing(comp, context, state, code, it, &arr_single);

            state->stack.current = save_stack;//reset stack
            state->get_mem(arr_single.mem)->mem.disp += (int32_t)base_size;
          }
        }
        break;
      }
    case AST_TYPE::ASCII_CHAR: {
        ASTAsciiChar* ch = (ASTAsciiChar*)expr;

        ASSERT(hint != nullptr);
        if (hint->is_hint) {
          load_runtime_hint(comp, state, expr->node_type.structure, hint, expr->valid_rvts);
        }

        char* char_c = comp->new_constant<char>();
        *char_c = ch->character;

        load_const_to_runtime_val(comp, state, code, expr->node_type.structure,
                                  ConstantVal{ (uint8_t*)char_c, 1 },
                                  &hint->val);
        break;
      }
    case AST_TYPE::ASCII_STRING: {
        ASTAsciiString* st = (ASTAsciiString*)expr;

        ASSERT(hint != nullptr);
        const auto* const arr_type = expr->node_type.unchecked_base<ArrayStructure>();

        const size_t size = arr_type->size;
        char* string_c = (char*)comp->new_constant(size);

        const ConstantVal constant = { (uint8_t*)string_c, size };

        memcpy_ts(string_c, size, st->string->string, size);

        if (hint->is_hint) {
          load_runtime_hint(comp, state, expr->node_type.structure, hint, expr->valid_rvts);
        }

        load_const_to_runtime_val(comp, state, code, expr->node_type.structure, constant, &hint->val);
        break;
      }
    case AST_TYPE::NUMBER: {
        ASSERT(hint != nullptr);
        ASTNumber* num = (ASTNumber*)expr;

        const size_t size = expr->node_type.structure->size;

        uint8_t* val_c = comp->new_constant(size);
        memcpy_ts(val_c, size, (uint8_t*)&num->num_value, size);

        const ConstantVal constant = { val_c, size };

        if (hint->is_hint) {
          load_runtime_hint(comp, state, expr->node_type.structure, hint, expr->valid_rvts);
        }

        load_const_to_runtime_val(comp, state, code, expr->node_type.structure, constant, &hint->val);

        break;
      }
    case AST_TYPE::IDENTIFIER_EXPR: {
        ASSERT(hint != nullptr);
        ASTIdentifier* ident = (ASTIdentifier*)expr;

        if (ident->id_type == ASTIdentifier::LOCAL) {
          Local* local = ident->local;

          RuntimeValue local_val = local->val;
          if (local_val.type == RVT::CONST) {
            //Need to actively copy the constant otherwise it will be freed twice
            ConstantVal copy_to = {};
            const ConstantVal to_copy = local_val.constant;

            copy_to.ptr = comp->new_constant(to_copy.size);
            copy_to.size = to_copy.size;

            memcpy_ts((u8*)copy_to.ptr, copy_to.size, (const u8*)to_copy.ptr, copy_to.size);

            local_val.constant = copy_to;
          }

          copy_runtime_to_runtime_hint(comp, state, code, expr->node_type.structure, &local->val, hint, expr->valid_rvts & NON_CONST_RVTS);

          break;
        }
        else if (ident->id_type == ASTIdentifier::GLOBAL) {
          const Global* glob = ident->global;
          ASSERT(glob != nullptr);

          if (glob->constant_value.ptr != nullptr) {
            if (hint->is_hint) {
              load_runtime_hint(comp, state, expr->node_type.structure, hint, expr->valid_rvts);
            }

            ConstantVal copied_val = copy_constant_value(comp, glob->constant_value);

            load_const_to_runtime_val(comp,
                                      state,
                                      code,
                                      glob->decl.type.structure, copied_val,
                                      &hint->val);
          }
          else {
            RuntimeValue global_mem = load_data_memory(state, glob->decl.type.structure->size, glob->data_holder_index - 1, code);

            copy_runtime_to_runtime_hint(comp, state, code, expr->node_type.structure, &global_mem, hint, expr->valid_rvts & NON_CONST_RVTS);
          }
          break;
        }

        INVALID_CODE_PATH("Missing identifier type");
        break;
      }
    case AST_TYPE::STATIC_LINK: {
        ASTStaticLink* li = (ASTStaticLink*)expr;

        LibraryImport* imp = comp->lib_import.data + (li->import_index - 1);

        usize* label_holder = comp->new_constant<usize>();
        *label_holder = imp->label;

        ConstantVal val = {};
        val.ptr = label_holder;
        val.size = sizeof(usize);

        if (hint->is_hint) {
          load_runtime_hint(comp, state, expr->node_type.structure, hint, expr->valid_rvts);
        }

        load_const_to_runtime_val(comp,
                                  state,
                                  code,
                                  ((const Type*)li->import_type->value)->structure, val,
                                  &hint->val);

        break;
      }
    case AST_TYPE::CAST: {
        ASSERT(hint != nullptr);
        const ASTCastExpr* const cast = (ASTCastExpr*)expr;
        RuntimeValue val = compile_bytecode_of_expression_new(comp,
                                                              context,
                                                              state,
                                                              code,
                                                              cast->expr, ALL_RVTS);

        val = cast->emit(comp, state, code, &val);

        copy_runtime_to_runtime_hint(comp, state, code, expr->node_type.structure, &val, hint, expr->valid_rvts);
        return;
      }
    case AST_TYPE::UNARY_OPERATOR: {
        ASSERT(hint != nullptr);
        const ASTUnaryOperatorExpr* const un_op = (ASTUnaryOperatorExpr*)expr;

        RuntimeValue temp = compile_bytecode_of_expression_new(comp,
                                                               context,
                                                               state,
                                                               code,
                                                               un_op->expr,
                                                               ALL_RVTS);

        UnOpArgs args = {};
        args.comp = comp;
        //args.comp_thread = comp_thread;
        args.state = state;
        args.code = code;
        args.prim = &temp;

        RuntimeValue rt = (args.*un_op->emit)();

        copy_runtime_to_runtime_hint(comp, state, code, expr->node_type.structure, &rt, hint, expr->valid_rvts & NON_CONST_RVTS);
        break;
      }
    case AST_TYPE::BINARY_OPERATOR: {
        ASSERT(hint != nullptr);
        const ASTBinaryOperatorExpr* const bin_op = (ASTBinaryOperatorExpr*)expr;
        AST_LOCAL left = bin_op->left;
        AST_LOCAL right = bin_op->right;

        RuntimeValue temp_left = compile_bytecode_of_expression_new(comp,
                                                                    context,
                                                                    state,
                                                                    code,
                                                                    left,
                                                                    ALL_RVTS);

        RuntimeValue temp_right = compile_bytecode_of_expression_new(comp,
                                                                     context,
                                                                     state,
                                                                     code,
                                                                     right,
                                                                     ALL_RVTS);

        BinOpArgs args = {};
        args.comp = comp;
        args.state = state;
        args.code = code;
        args.left = &temp_left;
        args.right = &temp_right;

        args.info = &bin_op->emit_info;

        RuntimeValue res = args.emit();

        copy_runtime_to_runtime_hint(comp, state, code, expr->node_type.structure, &res, hint, expr->valid_rvts & NON_CONST_RVTS);
        break;
      }
    case AST_TYPE::FUNCTION_CALL:
      compile_function_call(comp, context, state, code, (ASTFunctionCallExpr*)expr, hint);
      break;
    default: {
        //Invalid enum type
        //probably just didnt get around to supporting it
        INVALID_CODE_PATH("Invalid EXPRESSION_TYPE enum");
      }
  }
}

void compile_bytecode_of_statement(CompilerGlobals* const comp,
                                   Context* const context,
                                   State* const state,
                                   AST_LOCAL const statement,
                                   CodeBlock* const code) {
  switch (statement->ast_type) {
    case AST_TYPE::ASSIGN: {
        ASTAssign* assign = (ASTAssign*)statement;

        RuntimeValue assign_to = compile_bytecode_of_expression_new(comp,
                                                                    context,
                                                                    state,
                                                                    code,
                                                                    assign->assign_to,
                                                                    (uint8_t)RVT::MEMORY);

        //Load into 'assign_to'
        compile_bytecode_of_expression_existing(comp,
                                                context,
                                                state,
                                                code,
                                                assign->value,
                                                &assign_to);
        return;
      }
    case AST_TYPE::BLOCK: {
        ASTBlock* block = (ASTBlock*)statement;

        FOR_AST(block->block, it) {
          compile_bytecode_of_statement(comp, context, state, it, code);
        }

        return;
      }
    case AST_TYPE::RETURN: {
        ASTReturn* ret = (ASTReturn*)statement;

        if (ret->expr != nullptr) {
          compile_bytecode_of_expression_existing(comp,
                                                  context,
                                                  state,
                                                  code,
                                                  ret->expr,
                                                  &state->return_val);

          if (state->return_val.type == RVT::REGISTER) {
            state->use_value(state->return_val.reg);
          }
        }
        else {
          ASSERT(state->return_type.struct_type() == STRUCTURE_TYPE::VOID);
        }

        ByteCode::EMIT::JUMP_TO_FIXED(code->code, state->return_label);
        return;
      }
    case AST_TYPE::WHILE: {
        ASTWhile* const while_loop = (ASTWhile*)statement;

        const uint64_t base_label = comp->labels++;
        ByteCode::EMIT::LABEL(code->code, base_label);//to jump back to in the loop

        const size_t pre_loop_flow = state->control_flow.current_flow;
        state->control_flow.new_flow();
        const size_t cond_flow = state->control_flow.current_flow;
        state->control_flow.set_a_flows_to_b(pre_loop_flow, cond_flow);

        Array<ValueIndex> outer_captures = std::move(state->captured_values);

        RuntimeValue cond = compile_bytecode_of_expression_new(comp,
                                                               context,
                                                               state,
                                                               code,
                                                               while_loop->condition,
                                                               RVT::CONST | RVT::REGISTER);

        //Should be the same as cond_flow but best to do this
        const size_t post_cond_flow = state->control_flow.current_flow;

        constexpr auto save_captures = [](Array<ValueIndex>& outer_captures,
                                          State* const state,
                                          CodeBlock* code) {
          usize num_outer_captures = outer_captures.size;

          for (usize i = 0; i < state->captured_values.size; i++) {
            ValueIndex index = state->captured_values.data[i];

            ByteCode::EMIT::RESERVE(code->code, (uint8_t)index.val);
            state->use_value_captured(index, index);

            for (usize j = 0; j < num_outer_captures; j++) {
              if (outer_captures.data[j] == index) goto NEXT;
            }

            outer_captures.insert(index);

          NEXT:
            continue;
          }
        };

        if (cond.type == RVT::CONST) {
          //Only compile loop if its true

          if (*(uint64_t*)cond.constant.ptr != 0) {
            state->control_flow.new_flow();
            state->control_flow.set_a_flows_to_b(post_cond_flow, state->control_flow.current_flow);

            compile_bytecode_of_statement(comp, context, state, while_loop->statement, code);

            save_captures(outer_captures, state, code);
            state->control_flow.set_a_flows_to_b(state->control_flow.current_flow, cond_flow);
            ByteCode::EMIT::JUMP_TO_FIXED(code->code, base_label);

            state->control_flow.new_flow();//after loop - doesn't actually flow to this next one
          }
        }
        else {
          //Conditional jump out of the loop
          const uint64_t exit_label = comp->labels++;
          ByteCode::EMIT::JUMP_TO_FIXED_IF_VAL_ZERO(code->code, (uint8_t)cond.reg.val, exit_label);
          state->use_value(cond.reg);

          //loop branch
          state->control_flow.new_flow();
          state->control_flow.set_a_flows_to_b(post_cond_flow, state->control_flow.current_flow);
          compile_bytecode_of_statement(comp, context, state, while_loop->statement, code);


          save_captures(outer_captures, state, code);
          ByteCode::EMIT::JUMP_TO_FIXED(code->code, base_label);

          const size_t end_loop_flow = state->control_flow.current_flow;
          state->control_flow.set_a_flows_to_b(end_loop_flow, cond_flow);


          //After the loop
          state->control_flow.new_flow();
          state->control_flow.set_a_flows_to_b(post_cond_flow, state->control_flow.current_flow);

          ByteCode::EMIT::LABEL(code->code, exit_label);
        }
        return;
      }
    case AST_TYPE::IF_ELSE: {
        ASTIfElse* const if_else = (ASTIfElse*)statement;

        RuntimeValue cond = compile_bytecode_of_expression_new(comp,
                                                               context,
                                                               state,
                                                               code,
                                                               if_else->condition,
                                                               RVT::CONST | RVT::REGISTER);

        if (cond.type == RVT::CONST) {
          //Just becomes a block - no need for flows and stuff

          if (*(uint64_t*)cond.constant.ptr != 0) {
            //Compile if branch
            compile_bytecode_of_statement(comp, context, state, if_else->if_statement, code);
          }
          else {
            //Compile else branch
            compile_bytecode_of_statement(comp, context, state, if_else->else_statement, code);
          }
        }
        else {
          //Condition jump
          const uint64_t else_label = comp->labels++;

          ByteCode::EMIT::JUMP_TO_FIXED_IF_VAL_ZERO(code->code, (uint8_t)cond.reg.val, else_label);
          state->use_value(cond.reg);

          const size_t start_flow = state->control_flow.current_flow;

          //If branch
          state->control_flow.new_flow();
          state->control_flow.set_a_flows_to_b(start_flow, state->control_flow.current_flow);
          compile_bytecode_of_statement(comp, context, state, if_else->if_statement, code);

          const size_t end_if_flow = state->control_flow.current_flow;

          //Jump from if branch to after the else branch
          const uint64_t escape_label = comp->labels++;
          ByteCode::EMIT::JUMP_TO_FIXED(code->code, escape_label);

          //Else branch
          ByteCode::EMIT::LABEL(code->code, else_label);
          state->control_flow.new_flow();
          state->control_flow.set_a_flows_to_b(start_flow, state->control_flow.current_flow);

          if (if_else->else_statement != 0) {
            compile_bytecode_of_statement(comp, context, state, if_else->else_statement, code);
          }

          ByteCode::EMIT::JUMP_TO_FIXED(code->code, escape_label);

          const size_t end_else_flow = state->control_flow.current_flow;

          //Leave the if
          state->control_flow.new_flow();
          state->control_flow.set_a_flows_to_b(end_if_flow, state->control_flow.current_flow);
          state->control_flow.set_a_flows_to_b(end_else_flow, state->control_flow.current_flow);

          ByteCode::EMIT::LABEL(code->code, escape_label);
        }
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
          RuntimeHint hint = {};
          hint.is_hint = true;
          hint.hint_types = (comp->optimization_options.non_stack_locals ?
                             NON_CONST_RVTS
                             : (u8)RVT::MEMORY);

          load_runtime_hint(comp, state, local->decl.type.structure, &hint, local->valid_rvts);

          local->val = std::move(hint.val);

          compile_bytecode_of_expression_existing(comp,
                                                  context,
                                                  state,
                                                  code,
                                                  decl->expr,
                                                  &local->val);
        }
        else {
          local->val = compile_bytecode_of_expression_new(comp,
                                                          context,
                                                          state,
                                                          code,
                                                          decl->expr,
                                                          (u8)RVT::CONST);
        }

        ASSERT(local->val.type != RVT::UNKNOWN);
        return;
      }
    default: {
        AST_LOCAL expr = statement;

        auto _unused = compile_bytecode_of_expression_new(comp, context, state, code, expr, ALL_RVTS);
        (void)_unused;
        return;
      }
  }
}

static void map_values(const System* sys,
                       const CallingConvention* const conv,
                       CodeBlock* const code,
                       const State* const state,
                       uint64_t regs) {
  TRACING_FUNCTION();

  //Only non volatiles
  regs &= conv->non_volatiles_bit_mask;

  Array<uint8_t> temp = {};

  //Prolog

  //Call label
  ByteCode::EMIT::LABEL(temp, code->label);

  int64_t base_pointer_offset = 0;
  int64_t offset_to_prev_frame = 8;//return pointer

  if (state->needs_new_frame()) {
    ByteCode::EMIT::PUSH_FRAME(temp);

    offset_to_prev_frame += 8;//pushed rbp

    const uint8_t num_regs = sys->num_registers;
    uint8_t non_v_regs = 0;
    for (; non_v_regs < num_regs; non_v_regs++) {
      if (regs & ((uint64_t)1 << non_v_regs)) {
        base_pointer_offset -= 8;
        ByteCode::EMIT::PUSH_R64(temp, sys->all_registers[non_v_regs].REG);
      }
    }
  }

  //Finally allocate the stack
  uint64_t stack_needed = state->stack.max
    + ((uint64_t)state->made_call * conv->shadow_space_size)
    + state->stack.max_passed
    + (-base_pointer_offset);

  //Branch modifies "stack_needed"
  if (state->made_call) {
    //Need to align to the call border
    u64 aligned_stack = ceil_to_n<u64>(stack_needed, state->stack.call_alignment);

    //The call instruction will push 8 bytes onto the stack anyway so need to fix that
    if ((aligned_stack - 8) > stack_needed) {
      stack_needed = (aligned_stack - 8);
    }
    else {
      stack_needed = ceil_to_n<u64>(stack_needed + 8, state->stack.call_alignment) - 8;
    }
  }


  if (stack_needed > 0) {
    ByteCode::EMIT::ALLOCATE_STACK(temp, stack_needed);
  }

  //Function body

  const auto UNROLL_COALESCE = [values = &state->value_tree.values](uint8_t index)->const Value* {
    const Value* val = values->data + index;

    while (val->is_coalesced()) {
      val = values->data + val->index.val;
    }

    return val;
  };

  const auto check_mem = [&](const MemComplex& mem) -> MemComplex {
    const Value* const b = UNROLL_COALESCE(mem.base);

    MemComplex out_mem = {};
    out_mem.base = b->reg;
    out_mem.disp = mem.disp;

    if (b->reg == conv->base_pointer_reg) {
      if (!state->needs_new_frame()) {
        //No new frame, use stack pointer instead
        out_mem.base = conv->stack_pointer_reg;
      }

      if (out_mem.disp >= 0) {
        out_mem.disp += (int32_t)offset_to_prev_frame;
      }
      else {
        out_mem.disp += (int32_t)base_pointer_offset;
      }
    }

    out_mem.scale = mem.scale;
    out_mem.index = 0;

    {
      auto s = mem.scale;
      ASSERT(s == 0 || s == 1 || s == 2 || s == 4 || s == 8);

      if (s != 0) {
        const Value* const i = UNROLL_COALESCE(mem.index);
        out_mem.index = i->reg;
      }
    }

    return out_mem;
  };

  const auto OP_R_64 = [&](ByteCode::OP_R_64&& p) {
    auto* v = UNROLL_COALESCE(p.val);
    ByteCode::OP_R_64::emit(temp, p.op, v->reg, p.u64);
  };

  const auto OP_R_32 = [&](ByteCode::OP_R_32&& p) {
    auto* v = UNROLL_COALESCE(p.val);
    ByteCode::OP_R_32::emit(temp, p.op, v->reg, p.u32);
  };

  const auto OP_R_16 = [&](ByteCode::OP_R_16&& p) {
    auto* v = UNROLL_COALESCE(p.val);
    ByteCode::OP_R_16::emit(temp, p.op, v->reg, p.u16);
  };

  const auto OP_R_8 = [&](ByteCode::OP_R_8&& p) {
    auto* v = UNROLL_COALESCE(p.val);
    ByteCode::OP_R_8::emit(temp, p.op, v->reg, p.u8);
  };

  const auto OP_R_MEM = [&](ByteCode::OP_R_MEM&& p) {
    auto* v = UNROLL_COALESCE(p.val);
    ByteCode::OP_R_MEM::emit(temp, p.op, v->reg, check_mem(p.mem));
  };

  const auto OP_8_MEM = [&](ByteCode::OP_8_MEM&& p) {
    ByteCode::OP_8_MEM::emit(temp, p.op, p.u8, check_mem(p.mem));
  };

  const auto OP_16_MEM = [&](ByteCode::OP_16_MEM&& p) {
    ByteCode::OP_16_MEM::emit(temp, p.op, p.u16, check_mem(p.mem));
  };

  const auto OP_32_MEM = [&](ByteCode::OP_32_MEM&& p) {
    ByteCode::OP_32_MEM::emit(temp, p.op, p.u32, check_mem(p.mem));
  };

  const auto OP_64_MEM = [&](ByteCode::OP_64_MEM&& p) {
    ByteCode::OP_64_MEM::emit(temp, p.op, p.u64, check_mem(p.mem));
  };

  /*const auto OP_MEM = [&](ByteCode::OP_MEM&& p) {
    ByteCode::OP_MEM::emit(temp, p.op, check_mem(p.mem));
  };*/

  const auto OP_64 = [&](ByteCode::OP_64&& p) {
    ByteCode::OP_64::emit(temp, p.op, p.u64);
  };

  const auto OP_64_64 = [&](ByteCode::OP_64_64&& p) {
    ByteCode::OP_64_64::emit(temp, p.op, std::move(p.u64_1), std::move(p.u64_2));
  };

  const auto OP = [&](ByteCode::OP&& p) {
    ByteCode::OP::emit(temp, p.op);
  };

  const auto OP_R = [&](ByteCode::OP_R&& p) {
    auto* v = UNROLL_COALESCE(p.val);
    ByteCode::OP_R::emit(temp, p.op, v->reg);
  };

  const auto OP_R_R = [&](ByteCode::OP_R_R&& p) {
    auto* v1 = UNROLL_COALESCE(p.val1);
    auto* v2 = UNROLL_COALESCE(p.val2);
    ByteCode::OP_R_R::emit(temp, p.op, v1->reg, v2->reg);
  };

#define X(name, structure) case ByteCode :: name: {\
      structure(ByteCode::PARSE :: name (bytecode));\
      bytecode += ByteCode::SIZE_OF :: name;\
      break;\
    }

  const uint8_t* bytecode = code->code.begin();
  const uint8_t* const end = code->code.end();

  while (bytecode < end) {
    switch (*bytecode) {
      case ByteCode::COPY_R8_TO_R8:
      case ByteCode::COPY_R16_TO_R16:
      case ByteCode::COPY_R32_TO_R32:
      case ByteCode::COPY_R64_TO_R64: {
          const auto p = ByteCode::OP_R_R::parse(bytecode);
          auto* v1 = UNROLL_COALESCE(p.val1);
          auto* v2 = UNROLL_COALESCE(p.val2);
          if (v1 != v2) {
            ByteCode::OP_R_R::emit(temp, p.op, v1->reg, v2->reg);
          }
          bytecode += ByteCode::OP_R_R::INSTRUCTION_SIZE;
          break;
        }
      default:
        switch (*bytecode) {
          BYTECODES_X
        }
    }
  }

#undef OP_R_64
#undef OP_R_MEM
#undef OP_8_MEM
#undef OP_16_MEM
#undef OP_32_MEM
#undef OP_64_MEM
#undef OP_64
#undef OP
#undef OP_R
#undef OP_R_R

  //Epilog
  {
    ByteCode::EMIT::LABEL(temp, state->return_label);

    //Load the non volatile regs
    uint8_t non_v_regs = sys->num_registers + 1;
    const uint8_t num_regs = 0;
    for (; non_v_regs > num_regs; non_v_regs--) {
      if (regs & ((uint64_t)1 << (non_v_regs - 1))) {
        MemComplex mem = {};
        mem.base = conv->base_pointer_reg;
        mem.disp = (int32_t)base_pointer_offset;
        mem.scale = 0;

        ByteCode::EMIT::COPY_R64_FROM_MEM(temp,
                                          sys->all_registers[(non_v_regs - 1)].REG,
                                          mem);

        base_pointer_offset += 8;
      }
    }

    if (state->needs_new_frame()) {
      ByteCode::EMIT::POP_FRAME(temp);
    }

    ByteCode::EMIT::RETURN(temp);
  }

  code->code = std::move(temp);
}


constexpr static ValueIndex resolve_coalesced(ValueIndex i, const ValueTree& tree) noexcept {
  while (tree.values.data[i.val].is_coalesced()) {
    i.val = tree.values.data[i.val].index.val;
  }

  return i;
}

static void compute_value_intersections(const CompilerConstants* comp, ValueTree& tree, const ControlFlow& flow) {
  TRACING_FUNCTION();

  if (comp->print_options.intersections) {
    IO_Single::lock();
    IO_Single::print("--- Intersections ---\n");
  }

  DEFER(&) {
    if (comp->print_options.intersections) {
      IO_Single::print("---------------------\n");
      IO_Single::unlock();
    }
  };

  for (size_t i = 0; i < tree.values.size - 1; i++) {
    auto i_val = tree.values.data + i;

    for (size_t j = i + 1; j < tree.values.size; j++) {
      auto j_val = tree.values.data + j;

      if (i_val->creation.time.flow != j_val->creation.time.flow
          && !flow.test_a_flows_to_b(i_val->creation.time.flow, j_val->creation.time.flow)
          && !flow.test_a_flows_to_b(j_val->creation.time.flow, i_val->creation.time.flow)) {
        //cant intersect as control flow not linked
        continue;
      }

      if (i_val->creation.time.flow == j_val->creation.time.flow
          && i_val->creation.time.time == j_val->creation.time.time) {
        if (comp->print_options.intersections) {
          format_print_ST("{} - {}\n", i, j);
        }
        tree.set_intersection(ValueIndex{ i }, ValueIndex{ j });
        continue;
      }

      if ((i_val->creation.time.flow == j_val->creation.time.flow
           && i_val->creation.time.time < j_val->creation.time.time)
          || flow.test_a_flows_to_b(i_val->creation.time.flow, j_val->creation.time.flow)) {
        //i is before j

        auto i_last = i_val->last_uses.begin();
        const auto i_end = i_val->last_uses.end();

        for (; i_last < i_end; i_last++) {
          if ((j_val->creation.time.flow == i_last->time.flow
               && i_last->time.time < j_val->creation.time.time)
              || flow.test_a_flows_to_b(j_val->creation.time.flow, i_last->time.flow)) {
            continue;
          }

          //j is created between i start and end -> intersection

          if (comp->print_options.intersections) {
            format_print_ST("{} - {}\n", i, j);
          }
          tree.set_intersection(ValueIndex{ i }, ValueIndex{ j });
          break;
        }

      }
      else {
        //j is before i

        auto j_last = j_val->last_uses.begin();
        const auto j_end = j_val->last_uses.end();

        for (; j_last < j_end; j_last++) {
          if ((i_val->creation.time.flow == j_last->time.flow
               && j_last->time.time < i_val->creation.time.time)
              || flow.test_a_flows_to_b(i_val->creation.time.flow, j_last->time.flow)) {
            continue;
          }

          //i is created between j start and end -> intersection

          if (comp->print_options.intersections) {
            format_print_ST("{} - {}\n", i, j);
          }
          tree.set_intersection(ValueIndex{ i }, ValueIndex{ j });
          break;
        }
      }
    }
  }
}

static bool is_inside_range(const ControlFlow& flow,
                            const TimePoint& start, const TimePoint& end,
                            const TimePoint& val) {
  return (flow.test_a_flows_to_b(start.flow, val.flow)
          || (start.flow == val.flow && start.time < end.time))
    && (flow.test_a_flows_to_b(val.flow, end.flow)
        || (val.flow == end.flow && val.time < end.time));
}

static void set_calls(State* const state) {
  const size_t num_values = state->value_tree.values.size;
  const size_t num_calls = state->control_flow.calls.size;

  for (size_t i = 0; i < num_values; i++) {
    Value* const val = state->value_tree.values.data + i;

    const size_t num_last_uses = val->last_uses.size;
    for (size_t n = 0; n < num_last_uses; n++) {
      for (size_t c = 0; c < num_calls; c++) {
        if (is_inside_range(state->control_flow,
                            val->creation.time, val->last_uses.data[n].time,
                            state->control_flow.calls.data[c])) {
          val->crosses_call = true;
          goto NEXT_VAL;
        }
      }
    }

  NEXT_VAL:
    continue;
  }
}

static uint64_t select(const CompilerConstants* comp, const CallingConvention* conv, State* const state) {
  TRACING_FUNCTION();

  const ValueTree& tree = state->value_tree;

  struct UnindexedAdjacency {
    ValueIndex current = {};
    Array<ValueIndex> adjacent = {};
  };

  Array<UnindexedAdjacency> a_l = {};

  {
    const size_t size = state->value_tree.values.size;

    for (size_t i = 0; i < size; i++) {
      auto* i_val = state->value_tree.values.data + i;

      if (i_val->value_type != ValueType::FREE) {
        continue;
      }

      a_l.insert_uninit(1);
      auto* ui_adjacency = a_l.back();
      ui_adjacency->current = ValueIndex{ i };

      auto& to = ui_adjacency->adjacent;
      auto& from = state->value_tree.adjacency_list.data[i];

      auto from_i = from.begin();
      const auto from_end = from.end();

      for (; from_i < from_end; from_i++) {
        auto* from_val = tree.values.data + from_i->val;
        if (from_val->value_type == ValueType::FREE) {
          to.insert(*from_i);
        }
      }
    }
  }

  const size_t stack_max = a_l.size;
  size_t* const stack = stack_max > 0 ? allocate_default<size_t>(stack_max) : nullptr;
  DEFER(stack) { free_no_destruct(stack); };

  size_t index = 0;
  size_t num_intersects = 1;

  //Load stack - runs while there are active values not in the stack
  while (a_l.size > 0) {

    //Removed will be less than index if new stack values are added
    size_t removed = index;

    auto i = a_l.mut_begin();
    auto move_to = i;
    auto end = a_l.end();

    for (; i < end; i++) {
      if (i->adjacent.size < num_intersects) {
        //Valid to put in the stack
        stack[index++] = i->current.val;
        continue;
      }

      //Propogate the values back
      if (i != move_to) {
        *move_to = std::move(*i);
      }
      move_to++;
    }

    //Reduce size of a_l to reflect removed values
    a_l.size -= (i - move_to);

    const size_t num_removed = index - removed;

    if (num_removed > 0) {
      if (num_removed > (num_intersects - 1)) {
        num_intersects = 1;
      }
      else {
        num_intersects -= num_removed;
      }

      i = a_l.mut_begin();
      end = a_l.mut_end();

      for (; i < end; i++) {

        struct Lambda {
          const size_t* removed;
          const size_t* removed_end;

          //Remove if the value is newly in the stack - i.e. already removed from the graph
          bool operator()(const ValueIndex v) const {
            auto rem_i = removed;

            for (; rem_i < removed_end; rem_i++) {
              if (v.val == *rem_i) {
                return true;
              }
            }

            return false;
          }
        };

        i->adjacent.remove_if(Lambda{ stack + removed, stack + index });
      }
    }
    else {
      num_intersects++;
    }
  }


  size_t num_registers = conv->num_non_volatile_registers + conv->num_volatile_registers;
  ASSERT(num_registers < 64);


  //Load colours
  for (size_t iter = stack_max; iter > 0; iter--) {
    const size_t i = stack[iter - 1];
    auto* i_val = tree.values.data + i;

    uint64_t regs = 0;//not going to have more than 64 registers ... hopefully ^ checked in assert

    //For each adjacent check for reserved colours
    {
      auto* intersects_i = tree.adjacency_list.data[i].begin();
      const auto* intersects_end = tree.adjacency_list.data[i].end();

      for (; intersects_i < intersects_end; intersects_i++) {
        ValueIndex other_index = resolve_coalesced(*intersects_i, tree);
        auto* other_val = tree.values.data + other_index.val;
        if (other_val->fixed()) {
          regs |= ((uint64_t)1 << (other_val->reg));
        }
      }
    }

    if (i_val->fixed()) {
      //Check there is not a conflict
      ASSERT((regs & ((uint64_t)1 << (i_val->reg))) > 0);

      continue;
    }

    uint8_t colour = 0;
    if (i_val->crosses_call) {
      //requires non volatile reg
      colour += conv->num_volatile_registers;
    }

    //Find first index that is 0 (i.e. a free colour/reg)
    //Search in order of options->calling_convention->all_regs_unordered because this will do
    //volatile registers first and then non volatile registers if required
    while (colour < num_registers && (regs & ((uint64_t)1 << conv->all_regs_unordered[colour])) != 0) {
      colour++;
    }

    ASSERT(colour < num_registers);

    i_val->value_type = ValueType::FIXED;
    i_val->reg = conv->all_regs_unordered[colour];
  }



  if (comp->print_options.reg_mapping) {
    IO_Single::lock();
    IO_Single::print("--- Reg Mapping ---\n");
  }

  uint64_t used_regs = 0;
  {
    size_t index = 0;
    auto i = tree.values.begin();
    const auto end = tree.values.end();

    for (; i < end; i++) {
      if (i->fixed()) {

        if (comp->print_options.reg_mapping) {
          printf("%llu -> %s\n", index, comp->build_options.endpoint_system->reg_name_from_num(i->reg));
        }

        used_regs |= ((uint64_t)1 << (i->reg));
      }
      else {
        ASSERT(i->value_type == ValueType::COALESCED);
        if (comp->print_options.reg_mapping) {
          printf("%llu -> %llu\n", index, i->index.val);
        }
      }

      index += 1;
    }

    //Managed separately
    auto bp = conv->base_pointer_reg;
    auto sp = conv->stack_pointer_reg;

    used_regs &= ~(bp | sp);
  }

  if (comp->print_options.reg_mapping) {
    IO_Single::print("-----\n");
    IO_Single::unlock();
  }

  return used_regs;
}

static void combine_last_uses(Array<ValueUse>& arr1, const Array<ValueUse>& arr2, const ControlFlow* const c_flow) {
  Array<ValueUse> temp = {};

  copy_array(arr2, temp);

  struct ContainsLaterUse {
    const ControlFlow* flow;
    const Array<ValueUse>& arr_test;

    bool operator()(const ValueUse& v) const {
      auto i = arr_test.begin();
      const auto end = arr_test.end();

      for (; i < end; i++) {
        if (v.time.flow == i->time.flow && v.time.time < i->time.time) {
          return true;
        }
        else if (flow->test_a_flows_to_b(v.time.flow, i->time.flow)) {
          return true;
        }
      }

      return false;
    }
  };

  arr1.remove_if(ContainsLaterUse{ c_flow, temp });
  temp.remove_if(ContainsLaterUse{ c_flow, arr1 });

  arr1.concat(std::move(temp));
}

bool test_is_child(const CallingConvention* convention, const ValueTree& tree,
                   size_t pos_child_index, const Value& possible_child,
                   size_t pos_parent_index, const Value& possible_parent,
                   Array<size_t>& ignore_vals) {
  if (pos_child_index == pos_parent_index) {
    return false;//just in case
  }

  //cant merge 2 values that are fixed at different values
  if (possible_child.value_type == ValueType::FIXED
      && possible_parent.value_type == ValueType::FIXED
      && possible_child.reg != possible_parent.reg) {
    return false;
  }

  //cant merge values that cross function calls with a volatile register

  if ((possible_child.crosses_call
       && possible_parent.value_type == ValueType::FIXED
       && convention->is_volatile(possible_parent.reg))
      ||
      (possible_parent.crosses_call
       && possible_child.value_type == ValueType::FIXED
       && convention->is_volatile(possible_child.reg))) {
    return false;
  }

  //Could be child
  if (possible_child.value_type == ValueType::FIXED) {
    //Test if there is a fixed value that stops us from merging
    uint8_t fixed_reg = possible_child.reg;

    auto* vi = tree.adjacency_list.data[pos_parent_index].begin();
    const auto* end = tree.adjacency_list.data[pos_parent_index].end();

    const ValueIndex parent_created_by = resolve_coalesced(possible_parent.creation.related_index, tree);

    for (; vi < end; vi++) {
      const ValueIndex other = resolve_coalesced(*vi, tree);

      if (other.val == pos_parent_index || other.val == pos_child_index || ignore_vals.contains(other.val)) continue;

      const auto& other_val = tree.values.data[other.val];

      if (other_val.fixed() && other_val.reg == fixed_reg) {
        //could be special case where the last use is fixed to the same as the creating type
        if (parent_created_by == other) {

          ignore_vals.insert(pos_child_index);
          bool res = test_is_child(convention, tree, pos_parent_index, possible_parent, other.val, other_val, ignore_vals);
          ignore_vals.pop();

          if (res) {
            //Will actually be resolved later
            continue;
          }
          else {
            return false;
          }
        }
        else {
          //Cant be child :(
          return false;
        }
      }
    }
  }

  if (possible_parent.value_type == ValueType::FIXED) {
    //Test if there is a fixed value that stops us from merging
    uint8_t fixed_reg = possible_parent.reg;

    auto* vi = tree.adjacency_list.data[pos_child_index].begin();
    const auto* end = tree.adjacency_list.data[pos_child_index].end();

    for (; vi < end; vi++) {
      const ValueIndex other = resolve_coalesced(*vi, tree);

      if (other.val == pos_parent_index || other.val == pos_child_index || ignore_vals.contains(other.val)) continue;

      const auto& other_val = tree.values.data[other.val];

      if (other_val.fixed() && other_val.reg == fixed_reg) {

        const ValueIndex other_created_by = resolve_coalesced(other_val.creation.related_index, tree);

        //Could be special case where other is actually a child of the possible child
        if (other_created_by.val == pos_child_index) {

          ignore_vals.insert(pos_parent_index);
          bool res = test_is_child(convention, tree, other.val, other_val, pos_child_index, possible_child, ignore_vals);
          ignore_vals.pop();

          if (res) {
            //Will actually be resolved later
            continue;
          }
          else {
            return false;
          }
        }
        else {
          //Cant be parent :(
          return false;
        }
      }
    }
  }

  if (!possible_child.is_modified && !possible_parent.is_modified) {
    //l1 created l2 and l1 or l2 are ever modified so can merge
    return true;
  }

  //l1 created by l2
  //was it the last thing l2 did?
  for (const ValueUse& use : possible_parent.last_uses) {
    const size_t use_created = resolve_coalesced(use.related_index, tree).val;

    if (use_created == pos_child_index //last thing l2 did was related to l1
        && use.time.flow == possible_child.creation.time.flow //same block
        && use.time.time == possible_child.creation.time.time /*same time*/) {
      //Last thing l2 did is create l1

      //Coalesce l1 into l2
      return true;
    }
  }

  return false;
}


void coalesce(const CompilerConstants* const comp, const CallingConvention* conv, State* const state) {
  TRACING_FUNCTION();

  ValueTree& tree = state->value_tree;
  const ControlFlow& c_flow = state->control_flow;

  const size_t size = tree.values.size;
  for (size_t l1 = 0; l1 < size; l1++) {
    auto& l1_val = tree.values.data[l1];

    ValueIndex created_by = resolve_coalesced(l1_val.creation.related_index, tree);
    auto& possible_parent = tree.values.data[created_by.val];

    if (possible_parent.fixed()) continue;//Don't coalesce if the value is fixed by default?

    Array<size_t> ignore_vals = {};

    const bool is_child = test_is_child(conv, tree,
                                        l1, l1_val,
                                        created_by.val, possible_parent,
                                        ignore_vals);

    if (is_child) {
      auto& parent_val = tree.values.data[created_by.val];

      if (comp->print_options.coalesce_values) {
        printf("%llu is child of %llu\n", l1, created_by.val);
      }

      if (l1_val.fixed()) {
        parent_val.value_type = ValueType::FIXED;
        parent_val.reg = l1_val.reg;
      }

      l1_val.value_type = ValueType::COALESCED;
      l1_val.index.val = created_by.val;

      parent_val.crosses_call |= l1_val.crosses_call;
      parent_val.is_modified |= l1_val.is_modified;



      combine_last_uses(parent_val.last_uses, l1_val.last_uses, &c_flow);
      tree.combine_intersection(ValueIndex{ l1 }, ValueIndex{ created_by.val });

    }
  }
}

// Simplified Graph colouring Algorithm
//
// Build -> Coalesce -> Select -> Map
//
// Also emits a function prolog and function epilog
void graph_colour_algo(CompilerConstants* const comp_const,
                       const CallingConvention* const conv,
                       CodeBlock* const code,
                       State* const state) noexcept {
  TRACING_FUNCTION();

  if (comp_const->print_options.pre_reg_alloc) {
    IO_Single::lock();
    IO_Single::print("\n=== Pre Register Allocation Bytecode ===\n\n");
    ByteCode::print_bytecode(&reg_num_as_string, stdout, code->code.data, code->code.size);
    IO_Single::print("\n=============================\n\n");
    IO_Single::unlock();
  }

  //Computers all the intersections in the value tree based on control flow
  compute_value_intersections(comp_const, state->value_tree, state->control_flow);

  //Combine values together that can be combined
  coalesce(comp_const, conv, state);

  //Check if each value is used across a call
  set_calls(state);

  //Actually select which register is used for each value
  const uint64_t regs = select(comp_const, conv, state);

  //map values to their registers and emit function prolog and epilog
  map_values(comp_const->build_options.endpoint_system, conv, code, state, regs);

  code->code.shrink();

  if (comp_const->print_options.normal_bytecode) {
    IO_Single::lock();
    IO_Single::print("\n=== Normal Bytecode ===\n\n");
    ByteCode::print_bytecode(comp_const->build_options.endpoint_system->reg_name_from_num, stdout, code->code.data, code->code.size);
    IO_Single::print("\n=============================\n\n");
    IO_Single::unlock();
  }
}

void init_state_regs(const CallingConvention* convention, State* state) {
  //Set up rbp that can be used at any time
  state->rbp = state->new_value();
  state->set_value(state->rbp);

  auto& rbp_val = state->value_tree.values.data[state->rbp.val];

  rbp_val.value_type = ValueType::FIXED;
  rbp_val.reg = convention->base_pointer_reg;

  //Set up rsp that can be used at any time
  state->rsp = state->new_value();
  state->set_value(state->rsp);

  auto& rsp_val = state->value_tree.values.data[state->rsp.val];

  rsp_val.value_type = ValueType::FIXED;
  rsp_val.reg = convention->stack_pointer_reg;
}

//Cant error???
void compile_function_body_code(CompilerGlobals* const comp,
                                CompilerThread* const comp_thread,
                                Context* const context,
                                State* const state,
                                ASTLambda* const ast_lambda,
                                Function* const func) {
  TRACING_FUNCTION();

  ASSERT(ast_lambda->sig->ast_type == AST_TYPE::FUNCTION_SIGNATURE);
  ASTFuncSig* sig = (ASTFuncSig*)ast_lambda->sig;

  //Should never be called twice on the same function
  ASSERT(func->code_block.code.size == 0);

  //Enter the body - setup
  //func->code_block.label = comp->labels++;//already set
  state->return_label = comp->labels++;

  state->control_flow.new_flow();
  ASSERT(state->control_flow.current_flow == 0);

  //Useful values
  init_state_regs(func->signature.sig_struct->calling_convention, state);

  bool return_as_ptr = func->signature.sig_struct->return_via_addres;

  const Type actual_return_type = return_as_ptr
    ? func->signature.sig_struct->actual_parameter_types.data[0]
    : func->signature.sig_struct->return_type;

  const CallingConvention* convention = func->signature.sig_struct->calling_convention;

  //Parameter set values
  {
    CallingConvParamIterator param_itr = {
      convention,
      0,
      convention->shadow_space_size
    };

    if (return_as_ptr) {
      state->return_val = advance_runtime_param(state, &param_itr, actual_return_type.structure);
    }
    else {
      state->return_val.type = RVT::REGISTER;
      state->return_val.reg = state->new_value();

      auto* ret_val = state->get_val(state->return_val.reg);
      ret_val->value_type = ValueType::FIXED;
      ret_val->reg = convention->return_register;
    }

    //The actual types
    auto i_param = func->signature.sig_struct->actual_parameter_types.begin();
    auto end_param = func->signature.sig_struct->actual_parameter_types.end();

    if (return_as_ptr) { i_param++; }

    FOR_AST(sig->parameters, it) {
      ASSERT(i_param < end_param);

      ASTTypedName* tn = (ASTTypedName*)it;

      Local* loc = tn->local_ptr;
      loc->val = advance_runtime_param(state, &param_itr, i_param->structure);
      loc->valid_rvts &= NON_CONST_RVTS;

      i_param += 1;
    }

    ASSERT(i_param == end_param);
  }

  state->control_flow.expression_num++;

  //Actual values for the parameters - allows them to be different from parameter registers
  {
    int32_t used_shadow_space = 0;

    if (return_as_ptr) {
      //Should fix any issues later on
      UnOpArgs args = {};
      args.comp = comp;
      args.state = state;
      args.code = &func->code_block;
      args.prim = &state->return_val;

      state->return_val = args.emit_deref();
    }

    if (return_as_ptr && state->return_val.type == RVT::REGISTER) {
      if (comp_thread->optimization_options.non_stack_locals) {
        RuntimeHint load_hint = {};
        load_hint.hint_types = NON_CONST_RVTS;

        load_runtime_hint(comp, state, actual_return_type.structure, &load_hint, NON_CONST_RVTS);

        copy_runtime_to_runtime(comp, state, &func->code_block, actual_return_type.structure, &state->return_val, &load_hint.val);

        state->return_val = std::move(load_hint.val);
      }
      else {
        RuntimeValue rt_v = {};

        rt_v.type = RVT::MEMORY;
        rt_v.mem = state->new_mem();

        auto* stack_v = state->get_mem(rt_v.mem);
        stack_v->mem.base = (uint8_t)state->rbp.val;

        if (convention->shadow_space_size >= used_shadow_space + 8) {
          stack_v->mem.disp = used_shadow_space;
          used_shadow_space += 8;
        }
        else {
          stack_v->mem.disp = state->stack.next_stack_local(8, 8);
        }

        stack_v->mem.scale = 0;
        stack_v->size = 8;

        copy_reg_to_runtime(comp, state, &func->code_block, actual_return_type.structure, state->return_val.reg, &rt_v);

        state->return_val = std::move(rt_v);
      }
    }

    auto i_act_param = func->signature.sig_struct->actual_parameter_types.begin();
    const auto end_act_param = func->signature.sig_struct->actual_parameter_types.end();

    if (return_as_ptr) { i_act_param++; }

    auto i_param = func->signature.sig_struct->parameter_types.begin();
    const auto end_param = func->signature.sig_struct->parameter_types.end();

    FOR_AST(sig->parameters, it) {
      ASSERT(it->ast_type == AST_TYPE::TYPED_NAME);
      ASTTypedName* tn = (ASTTypedName*)it;

      Local* loc = tn->local_ptr;

      ASSERT(i_param < end_param);
      ASSERT(i_act_param < end_act_param);

      const Type& param_t = *i_param;
      const Type& act_param_t = *i_act_param;

      ASSERT(loc->decl.type == param_t);

      RuntimeValue rt_p = {};

      if (param_t == act_param_t) {
        if (loc->val.type != RVT::REGISTER) {
          //Dont need to save somehere else
          continue;
        }


        //Not passed by pointer
        if (comp_thread->optimization_options.non_stack_locals) {
          RuntimeHint load_hint = {};
          load_hint.hint_types = loc->valid_rvts;

          load_runtime_hint(comp, state, param_t.structure, &load_hint, NON_CONST_RVTS);

          copy_runtime_to_runtime(comp, state, &func->code_block, param_t.structure, &loc->val, &load_hint.val);

          rt_p = std::move(load_hint.val);
        }
        else {
          ASSERT((loc->valid_rvts & RVT::MEMORY) > 0);
          rt_p.type = RVT::MEMORY;
          rt_p.mem = state->new_mem();

          auto* stack_v = state->get_mem(rt_p.mem);
          stack_v->mem.base = (uint8_t)state->rbp.val;

          if (convention->shadow_space_size >= used_shadow_space + 8) {
            stack_v->mem.disp = (int32_t)used_shadow_space;
            used_shadow_space += 8;
          }
          else {
            stack_v->mem.disp = state->stack.next_stack_local(8, 8);
          }

          stack_v->mem.scale = 0;
          stack_v->size = param_t.structure->size;

          copy_reg_to_runtime(comp, state, &func->code_block, param_t.structure, loc->val.reg, &rt_p);
        }
      }
      else if (act_param_t.struct_type() == STRUCTURE_TYPE::POINTER
               && act_param_t.unchecked_base<PointerStructure>()->base == param_t) {
        //passed as pointer
        //Just do a simple deref
        //This should actually solves all issues with lifetimes later in the system
        UnOpArgs args = {};
        args.comp = comp;
        args.state = state;
        args.code = &func->code_block;
        args.prim = &loc->val;

        rt_p = args.emit_deref();
      }
      else {
        //Should never be here
        INVALID_CODE_PATH("Signature mismatch in code generation");
      }

      loc->val = std::move(rt_p);

      i_param += 1;
      i_act_param += 1;
    }
  }

  state->control_flow.expression_num++;

  //Calculate all the values
  //and compile the bytecode
  {
    AST_ARR arr = ((ASTBlock*)ast_lambda->body)->block;
    FOR_AST(arr, i) {
      compile_bytecode_of_statement(comp, context, state, i, &func->code_block);
    }
  }

  graph_colour_algo(comp_thread, convention, &func->code_block, state);
}

static void compile_import_file(CompilerGlobals* comp, CompilerThread* comp_thread, const FileLocation* src_loc, ASTImport* imp, Namespace* import_to) {
  TRACING_FUNCTION();

  const char* path = nullptr;

  AST_LOCAL expr = imp->expr_location;

  if (already_const_type(imp->expr_location->ast_type)) {
    //Temp
    ASSERT(expr->ast_type == AST_TYPE::ASCII_STRING);

    ASTAsciiString* str = (ASTAsciiString*)expr;
    path = str->string->string;
  }
  else {
    ASSERT(expr->value != nullptr);

    path = (const char*)expr->value;
  }

  ASSERT(path != nullptr);

  FileLocation loc;
  {
    auto strings = comp->services.strings.get();
    loc = parse_file_location(src_loc->directory->string, path,
                              strings._ptr);
  }

  if (expr->value != nullptr) {
    comp->free_constant(expr->value);
    expr->value = nullptr;
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

void compile_init_expr_of_global(CompilerGlobals* comp, CompilerThread* comp_thread, Context* context, State* state, ASTDecl* decl, Global* global) {
  TRACING_FUNCTION();
  AST_LOCAL decl_expr = decl->expr;

  if (TEST_MASK(global->decl.meta_flags, META_FLAG::COMPTIME)) {
    if (decl_expr->value == nullptr) {
      UnitID id = compile_and_execute(comp, context->current_unit->available_names, decl->expr, {});

      set_dependency(comp_thread, context, id);
      return;
    }

    global->constant_value.ptr = decl_expr->value;
    global->constant_value.size = global->decl.type.structure->size;
    decl_expr->value = nullptr;
  }
  else {
    ASSERT(global->decl.type.is_valid());

    state->control_flow.new_flow();

    global->init.label = comp->labels++;
    state->return_label = comp->labels++;

    global->data_holder_index = new_data_object(comp, global->decl.name, global->decl.type.structure->size, global->decl.type.structure->alignment);

    ValueIndex reg_mem = state->new_value();

    ByteCode::EMIT::LOAD_DATA_MEM(global->init.code, (uint8_t)reg_mem.val, global->data_holder_index - 1);//changed later to be the actual location
    state->set_value(reg_mem);

    state->control_flow.expression_num++;

    RuntimeValue global_mem = {};
    global_mem.type = RVT::MEMORY;
    global_mem.mem = state->new_mem();

    MemValue* mem_val = state->get_mem(global_mem.mem);
    mem_val->size = global->decl.type.structure->size;
    mem_val->mem.base = (uint8_t)reg_mem.val;

    compile_bytecode_of_expression_existing(comp, context, state, &global->init, decl_expr, &global_mem);

    graph_colour_algo(comp, comp->build_options.default_calling_convention, &global->init, state);
  }
}

UnitID compile_and_execute(CompilerGlobals* const comp, Namespace* const available_names, AST_LOCAL ast, Type cast_to) {
  CompilationUnit* unit;
  {
    auto compilation = comp->services.compilation.get();

    ExecCodeExtra* extra = compilation->exec_code_extras.allocate();
    extra->cast_to = cast_to;

    State* state = compilation->states.allocate();

    unit = new_compilation_unit(compilation._ptr,
                                COMPILATION_EMIT_TYPE::EXEC_CODE, ast, available_names,
                                state, extra, comp->print_options.comp_units);
  }

  ASSERT(unit != nullptr);

  if (ast->node_type.is_valid()) {
    comp->pipelines.exec_code.push_back(unit);
  }
  else {
    comp->pipelines.type_check.push_back(unit);
  }

  return unit->id;
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

    State* state = compilation->states.allocate();

    imp_unit = new_compilation_unit(compilation._ptr,
                                    COMPILATION_EMIT_TYPE::IMPORT, imp, ns,
                                    state, (void*)extra,
                                    comp->print_options.comp_units);
  }

  ASSERT(imp_unit != nullptr);
  comp->pipelines.depend_check.push_back(imp_unit);
}

void add_comp_unit_for_global(CompilerGlobals* const comp, CompilerThread* const comp_thread, Namespace* ns, ASTGlobalDecl* global) noexcept {

  Global* glob = comp->new_global();

  CompilationUnit* glob_unit;
  {
    auto compilation = comp->services.compilation.get();
    State* state = compilation->states.allocate();
    GlobalExtra* extra = compilation->global_extras.allocate();
    extra->global = glob;

    glob_unit = new_compilation_unit(compilation._ptr,
                                     COMPILATION_EMIT_TYPE::GLOBAL, global, ns,
                                     state, (void*)extra,
                                     comp->print_options.comp_units);
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
  Function* const func = comp->new_function();
  lambda->function = func;
  ((ASTFuncSig*)lambda->sig)->sig = &func->signature;
  func->declaration = lambda;

  ((ASTFuncSig*)lambda->sig)->convention = comp->build_options.default_calling_convention;

  //Set the compilation units
  CompilationUnit* sig_unit;
  CompilationUnit* body_unit;
  {
    auto compilation = comp->services.compilation.get();

    State* shared_state = compilation->states.allocate();
    sig_unit = new_compilation_unit(compilation._ptr,
                                    COMPILATION_EMIT_TYPE::FUNC_SIG, lambda->sig, ns,
                                    shared_state, nullptr,
                                    comp->print_options.comp_units);

    FuncBodyExtra* func_extra = compilation->func_body_extras.allocate();
    func_extra->func = func;

    body_unit = new_compilation_unit(compilation._ptr,
                                     COMPILATION_EMIT_TYPE::FUNC_BODY, lambda, ns,
                                     shared_state, func_extra,
                                     comp->print_options.comp_units);
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
    State* state = compilation->states.allocate();

    unit = new_compilation_unit(compilation._ptr,
                                COMPILATION_EMIT_TYPE::STRUCTURE, struct_body, ns,
                                state, nullptr, comp->print_options.comp_units);

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
      if (comp->services.file_loader._ptr->unparsed_files.size > 0) {
        thead_doing_work(comp, comp_thread);

        compile_current_unparsed_files(comp, comp_thread, comp->services.file_loader._ptr);
        if (comp_thread->is_panic()) {
          return;
        }

        ASSERT(!comp_thread->is_depends());
      }

      comp->services.file_loader._mutex.release();
    }
  }

  CompilationUnit* unit = nullptr;


  if (comp->pipelines.emit_import.try_pop_front(&unit)) {
    TRACING_SCOPE("Import loop");
    thead_doing_work(comp, comp_thread);

    ASSERT(!comp_thread->is_depends() && !comp_thread->is_panic());

    ASSERT(unit->waiting_on_count == 0);

    State* state = unit->state;

    Context context = {};
    context.comptime_compilation = false;
    context.dependency_load_pipe = &comp->pipelines.emit_import;
    context.current_unit = unit;

    ASSERT(unit->ast != nullptr);
    ASSERT(unit->ast->ast_type == AST_TYPE::IMPORT);

    ASTImport* imp = (ASTImport*)unit->ast;


    AST_LOCAL expr = imp->expr_location;
    ASSERT(expr != nullptr);

    //Only called if this isnt already a constant literal
    if (can_compile_const_value(expr)) {
      UnitID id = compile_and_execute(comp, unit->available_names, expr, {});
      set_dependency(comp_thread, &context, id);

      ASSERT(comp_thread->is_depends() && !comp_thread->is_panic());
      try_dispatch_dependencies(comp, comp_thread, &context);
      return;
    }

    ImportExtra* imp_extra = (ImportExtra*)unit->extra;

    compile_import_file(comp, comp_thread, &imp_extra->src_loc, imp, unit->available_names);
    if (comp_thread->is_panic()) {
      return;
    }

    //Finished
    auto compilation = comp->services.compilation.get();

    compilation->states.free(state);
    compilation->import_extras.free(imp_extra);
    close_compilation_unit(comp_thread, compilation._ptr, unit);
    return;
  }

  if (comp->pipelines.emit_global.try_pop_front(&unit)) {
    TRACING_SCOPE("Emit Global");
    thead_doing_work(comp, comp_thread);

    ASSERT(!comp_thread->is_depends() && !comp_thread->is_panic());

    ASSERT(unit->waiting_on_count == 0);

    State* state = unit->state;

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

    compile_init_expr_of_global(comp, comp_thread, &context, state, decl, global);
    if (comp_thread->is_panic()) {
      return;
    }

    //Finished
    auto compilation = comp->services.compilation.get();

    compilation->states.free(state);
    compilation->global_extras.free(global_extra);
    close_compilation_unit(comp_thread, compilation._ptr, unit);
    return;
  }

  if (comp->pipelines.emit_function.try_pop_front(&unit)) {
    TRACING_SCOPE("Emit Function Body");
    thead_doing_work(comp, comp_thread);

    ASSERT(!comp_thread->is_depends() && !comp_thread->is_panic());

    ASSERT(unit->waiting_on_count == 0);

    State* state = unit->state;

    Context context = {};
    context.dependency_load_pipe = &comp->pipelines.emit_function;
    context.current_unit = unit;

    ASTLambda* lambda = (ASTLambda*)unit->ast;
    FuncBodyExtra* func_body_extra = (FuncBodyExtra*)unit->extra;

    compile_function_body_code(comp, comp_thread,
                               &context,
                               state,
                               lambda,
                               func_body_extra->func);
    ASSERT(!comp_thread->is_panic());

    //Finished
    auto compilation = comp->services.compilation.get();

    compilation->states.free(state);
    compilation->func_body_extras.free(func_body_extra);
    close_compilation_unit(comp_thread, compilation._ptr, unit);
    return;
  }

  if (comp->pipelines.exec_code.try_pop_front(&unit)) {
    TRACING_SCOPE("Exec Code Unit");
    thead_doing_work(comp, comp_thread);

    ASSERT(!comp_thread->is_depends() && !comp_thread->is_panic());

    ASSERT(unit->waiting_on_count == 0);

    State* state = unit->state;

    Context context = {};
    context.current_unit = unit;
    context.comptime_compilation = true;
    context.dependency_load_pipe = &comp->pipelines.exec_code;

    ExecCodeExtra* exec_code_extra = (ExecCodeExtra*)unit->extra;

    {
      AST_LOCAL ast = unit->ast;
      const Type& cast_to = exec_code_extra->cast_to;

      if (cast_to.is_valid()) {
        ASSERT(ast->node_type == cast_to);
      }

      //maybe need??
      //unit->constants = std::move(comp->working_state->constants);

      CodeBlock block = {};
      block.label = comp->labels++;


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

      init_state_regs(convention, state);
      state->return_label = comp->labels++;

      //Set up new flow
      state->control_flow.new_flow();

      const Type type = ast->node_type;

      //Do we need to pass as a parameter
      const bool return_as_ptr = register_passed_as_pointer(type);

      const Structure* actual_return_type;
      {
        AtomicLock<Structures> structures = {};
        AtomicLock<StringInterner> strings = {};
        comp->services.get_multiple(&structures, &strings);
        actual_return_type = find_or_make_pointer_structure(structures._ptr, strings._ptr,
                                                            comp_thread->build_options.ptr_size, type);
      }

      uint8_t* const_val = comp->new_constant(type.structure->size);

      {
        CallingConvParamIterator param_itr = {
          convention,
          0,
          convention->shadow_space_size
        };

        if (return_as_ptr) {
          state->return_val = advance_runtime_param(state, &param_itr, actual_return_type);

          UnOpArgs args = {};
          args.comp = comp;
          args.state = state;
          args.code = &block;
          args.prim = &state->return_val;

          //Should fix any issues later on
          state->return_val = args.emit_deref();

        }
        else {
          state->return_val.type = RVT::REGISTER;
          state->return_val.reg = state->new_value();

          auto* ret_val = state->get_val(state->return_val.reg);
          ret_val->value_type = ValueType::FIXED;
          ret_val->reg = convention->return_register;
        }
      }

      compile_bytecode_of_expression_existing(comp,
                                              &context,
                                              state,
                                              &block,
                                              ast,
                                              &state->return_val);

      ASSERT(!comp_thread->is_panic() && !comp_thread->is_depends());

      if (state->return_val.type == RVT::REGISTER) {
        state->use_value(state->return_val.reg);
      }

      ByteCode::EMIT::JUMP_TO_FIXED(block.code, state->return_label);

      //Graph colour
      graph_colour_algo(comp_thread, convention, &block, state);

      //Backend
      Program prog = {};
      compile_backend_single_func(comp, comp_thread, &prog, &block, &system_vm);
      if (comp_thread->is_panic()) {
        return;
      }

      if (comp_thread->print_options.comptime_exec) {
        IO_Single::lock();
        IO_Single::print("\nAbout to execute Compile Time Code:\n");
        print_full_ast(ast);
        IO_Single::print("\n\nWhich produced this bytecode:\n");
        ByteCode::print_bytecode(&vm_regs_name_from_num, stdout, prog.code.ptr, prog.code_size);
        IO_Single::unlock();
      }

      {
        auto vm = comp->services.vm.get();

        //Run the VM
        if (return_as_ptr) {
          X64_UNION pass_param = const_val;
          vm_set_parameters(convention, vm._ptr, pass_param);
        }

        vm->errors = &comp_thread->errors;

        vm_rum(vm._ptr, &prog);
        if (comp_thread->is_panic()) {
          return;
        }

        //Get the value back
        if (return_as_ptr) {
          if (comp_thread->print_options.comptime_res) {
            IO::print("\nComptime Res In Bytes: ");
            print_as_bytes(const_val, type.structure->size);
            putc('\n', stdout);
          }
        }
        else {
          //Effectively stored in RAX
          uint64_t val = vm->registers[convention_vm.return_register].b64.reg;
          x64_to_bytes(val, const_val);

          if (comp_thread->print_options.comptime_res) {
            printf("\nComptime Res: %llx\n", val);
          }
        }
      }

      if (cast_to.is_valid()) {
        uint8_t* res = comp->new_constant(cast_to.structure->size);
        do_literal_cast(comp, comp_thread, ast, cast_to, const_val, res);
        if (comp_thread->is_panic()) {
          return;
        }

        comp->free_constant(const_val);
        const_val = res;

        ast->node_type = cast_to;
      }

      ast->value = const_val;

      //Swap back
      std::swap(options, comp_thread->build_options);
    }

    ASSERT(!comp_thread->is_panic() && !comp_thread->is_depends());

    //Finished
    auto compilation = comp->services.compilation.get();

    compilation->states.free(state);
    compilation->exec_code_extras.free(exec_code_extra);
    close_compilation_unit(comp_thread, compilation._ptr, unit);
    return;
  }

  if (comp->pipelines.type_check.try_pop_front(&unit)) {
    TRACING_SCOPE("Type check");
    thead_doing_work(comp, comp_thread);

    ASSERT(!comp_thread->is_depends() && !comp_thread->is_panic());


    ASSERT(unit->waiting_on_count == 0);

    State* state = unit->state;

    Context context = {};
    context.dependency_load_pipe = &comp->pipelines.type_check;
    context.current_unit = unit;

    ASSERT(unit->ast != nullptr);

    //HACK: deal with these dependencies better - perhaps transfer them?
    if (!unit->ast->node_type.is_valid()) {
      type_check_ast(comp, comp_thread, &context, state, unit->ast);
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
          compilation->states.free(state);

          close_compilation_unit(comp_thread, compilation._ptr, unit);
          break;
        }
      case COMPILATION_EMIT_TYPE::EXEC_CODE: {
          comp->pipelines.exec_code.push_back(unit);
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

    DependencyCheckStateAndContext st = {};

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

      comp->pipelines.exec_code.mutex.acquire();
      format_to_array(error, "- Exec Code Size: {}\n", comp->pipelines.exec_code.size);
      comp->pipelines.exec_code.mutex.release();

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

  //compiler_loop(comp, comp_thread);
  compiler_loop_threaded(comp, comp_thread);
  if (comp->is_global_panic()) {
    return;
  }

  ASSERT(comp->services.compilation.get()->in_flight_units == 0);
  {
    TRACING_SCOPE("Sort Imports");

    constexpr auto import_sorter = [](const LibraryImport& l, const LibraryImport& r) {
      return !is_alphabetical_order(r.path, l.path);
    };

    sort_range(comp->lib_import.mut_begin(), comp->lib_import.mut_end(), import_sorter);

  }
  //{
  //  TRACING_SCOPE("test load dlls");

  //  //Sort the dlls
  //  constexpr auto import_sorter = [](const LibraryImport& l, const LibraryImport& r) {
  //    return !is_alphabetical_order(r.path, l.path);
  //  };

  //  auto i = comp->lib_import.begin();
  //  auto end = comp->lib_import.end();

  //  while (i < end) {
  //    const InternString* path = i->path;
  //    PEFile single_dll ={};

  //    load_portable_executable_from_file(comp, Span{}, &single_dll, path->string);
  //    if (comp->is_panic()) {
  //      return comp->services.errors->print_all();
  //    }

  //    while (i < end && i->path == path) {
  //      if (!single_dll.export_table.names.contains(i->name)) {
  //        comp->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
  //                           "Dll '{}' does export anything named '{}'",
  //                           i->path, i->name);
  //      }

  //      i++;
  //    }

  //    if (comp->is_panic()) {
  //      return comp->services.errors->print_all();
  //    }
  //  }
  //}
}

void print_compiled_functions(CompilerGlobals* const comp) {
  comp->functions_mutex.acquire();

  auto i = comp->functions_single_threaded.begin_const_iter();
  const auto end = comp->functions_single_threaded.end_const_iter();

  for (; i != end; i.next()) {
    const Function* func = i.get();

    printf("FUNCTION %s:\n", func->signature.name->string);
    ByteCode::print_bytecode(comp->build_options.endpoint_system->reg_name_from_num, stdout, func->code_block.code.data, func->code_block.code.size);
    IO::print('\n');
  }

  comp->functions_mutex.release();
}

//Make sure that t_struct is already created!!!
Type create_named_type(CompilerGlobals* comp, CompilerThread* comp_thread, NameManager* names, const Span& span, Namespace* ns,
                       const InternString* name, const Structure* s) {
  Global* g = comp->new_global();

  ASSERT(comp->builtin_types->t_type.is_valid());

  Type type = { name, s };

  g->decl.meta_flags = (u8)META_FLAG::COMPTIME;
  g->decl.name = name;
  g->decl.type = comp->builtin_types->t_type;
  g->decl.span = span;
  g->constant_value.ptr = comp->new_constant<Type>();
  g->constant_value.size = sizeof(Type);

  memcpy_ts((Type*)g->constant_value.ptr, 1, &type, 1);

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
  g->constant_value.ptr = comp->new_constant<const EnumValue*>();
  g->constant_value.size = sizeof(const EnumValue*);

  memcpy_ts((const EnumValue**)g->constant_value.ptr, 1, &v, 1);

  names->add_global_name(comp_thread, ns, v->name, NULL_ID, g);
}

void init_compiler(const APIOptions& options, CompilerGlobals* comp, CompilerThread* comp_thread) {
  TRACING_FUNCTION();

  //Setup the built in namespace
  Namespace* builtin_namespace = comp->new_namespace();
  comp->builtin_namespace = builtin_namespace;

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
    s_type->size = sizeof(Type);
    s_type->alignment = alignof(Type);
    builtin_types->t_type = to_type(s_type);

    /*_ = */ register_builtin_type(s_type);
  }


  {
    const auto base_type = [&](const auto& name, STRUCTURE_TYPE ty, u32 size, Type* t) {
      Structure* s = STRUCTS::new_base_structure(structures._ptr, strings->intern(name));
      s->type = ty;
      s->size = size;
      s->alignment = size;

      *t = register_builtin_type(s);
    };

    base_type("void", STRUCTURE_TYPE::VOID, 0, &builtin_types->t_void);
    base_type("ascii", STRUCTURE_TYPE::ASCII_CHAR, 1, &builtin_types->t_ascii);
  }

  {
    const auto int_type = [&](const auto& name, bool is_signed, u32 size, Type* t) {
      IntegerStructure* s = STRUCTS::new_int_structure(structures._ptr, strings->intern(name));
      s->is_signed = is_signed;
      s->size = size;
      s->alignment = size;

      *t = register_builtin_type(s);
    };

    int_type("u8", false, 1, &builtin_types->t_u8);
    int_type("i8", true, 1, &builtin_types->t_i8);
    int_type("u32", false, 4, &builtin_types->t_u32);
    int_type("i32", true, 4, &builtin_types->t_i32);
    int_type("u64", false, 8, &builtin_types->t_u64);
    int_type("i64", true, 8, &builtin_types->t_i64);
  }

  {
    Structure* const s_void_ptr = STRUCTS::new_pointer_structure(structures._ptr, strings._ptr, comp->build_options.ptr_size, builtin_types->t_void);
    builtin_types->t_void_ptr = register_builtin_type(s_void_ptr);
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

    g->constant_value.ptr = comp->new_constant<const void*>();
    g->constant_value.size = sizeof(void*);
    *(const void**)g->constant_value.ptr = 0;

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

  //Systems
  comp->system_names.sys_vm = strings->intern(system_vm.name);
  comp->system_names.sys_x86_64 = strings->intern(system_x86_64.name);

  //Calling Conventionss
  comp->system_names.conv_vm = strings->intern(convention_vm.name);
  comp->system_names.conv_x64 = strings->intern(convention_microsoft_x64.name);
  comp->system_names.conv_stdcall = strings->intern(convention_stdcall.name);


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

  if (options.build.output_file != nullptr) {
    comp->build_options.output_file = strings->intern(options.build.output_file);
  }


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
    const InternString* system_name = strings->intern(options.build.system_name);

    if (system_name == comp->system_names.sys_vm) {
      comp->build_options.endpoint_system = &system_vm;
    }
    else if (system_name == comp->system_names.sys_x86_64) {
      comp->build_options.endpoint_system = &system_x86_64;
    }
    else {
      comp_thread->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                                "Invalid system '{}'", system_name);
      return;
    }
  }

  {
    const InternString* conv_name = strings->intern(options.build.default_calling_convention);

    if (conv_name == comp->system_names.conv_vm) {
      comp->build_options.default_calling_convention = &convention_vm;

      if (comp->build_options.endpoint_system != &system_vm) {
        comp_thread->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                                  "Invalid system and calling convention combo '{}' and '{}'",
                                  comp->build_options.endpoint_system->name, conv_name);
      }
    }
    else if (conv_name == comp->system_names.conv_x64) {
      comp->build_options.default_calling_convention = &convention_microsoft_x64;

      if (comp->build_options.endpoint_system != &system_x86_64) {
        comp_thread->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                                  "Invalid system and calling convention combo '{}' and '{}'",
                                  comp->build_options.endpoint_system->name, conv_name);
      }
    }
    else if (conv_name == comp->system_names.conv_stdcall) {
      comp_thread->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                                "Invalid system and calling convention combo '{}' and '{}'",
                                comp->build_options.endpoint_system->name, conv_name);
    }
    else {
      comp_thread->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                                "Invalid default calling convention '{}'", conv_name);
    }
  }
}