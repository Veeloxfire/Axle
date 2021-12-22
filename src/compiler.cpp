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

Function* Compiler::new_function() {
  Function* func =  functions.insert();
  func->func_type = FUNCTION_TYPE::DEFAULT;
  return func;
}

ImportUnit* Compiler::new_import_unit(NamespaceIndex ns) {
  ImportUnit* unit = import_units.allocate();
  unit->type  = COMPILATION_TYPE::IMPORT;
  unit->stage = IMPORT_COMP_STAGE::UNTYPED;
  unit->available_names = ns;

  to_compile.insert(unit);
  return unit;
}

ConstantExprUnit* Compiler::new_const_expr_unit(NamespaceIndex ns) {
  ConstantExprUnit* unit = const_expr_units.allocate();
  unit->type  = COMPILATION_TYPE::CONST_EXPR;
  unit->stage = EXPR_COMP_STAGE::UNTYPED;
  unit->available_names = ns;

  to_compile.insert(unit);
  return unit;
}

FunctionUnit* Compiler::new_function_unit(NamespaceIndex ns) {
  FunctionUnit* unit = function_units.allocate();
  unit->type  = COMPILATION_TYPE::FUNCTION;
  unit->stage = FUNCTION_COMP_STAGE::UNINIT;
  unit->available_names = ns;

  to_compile.insert(unit);
  return unit;
}

SignatureUnit* Compiler::new_signature_unit(NamespaceIndex ns) {
  SignatureUnit* unit = signature_units.allocate();
  unit->type  = COMPILATION_TYPE::SIGNATURE;
  unit->stage = SIGNATURE_COMP_STAGE::UNTYPED;
  unit->available_names = ns;

  to_compile.insert(unit);
  return unit;
}

StructureUnit* Compiler::new_structure_unit(NamespaceIndex ns) {
  StructureUnit* unit = structure_units.allocate();
  unit->type  = COMPILATION_TYPE::STRUCTURE;
  unit->stage = STRUCTURE_COMP_STAGE::UNTYPED;
  unit->available_names = ns;

  to_compile.insert(unit);
  return unit;
}

GlobalUnit* Compiler::new_global_unit(NamespaceIndex ns) {
  GlobalUnit* unit = global_units.allocate();
  unit->type  = COMPILATION_TYPE::GLOBAL;
  unit->stage = GLOBAL_COMP_STAGE::UNTYPED;
  unit->available_names = ns;

  to_compile.insert(unit);
  return unit;
}

void Compiler::set_dep(Context* context, CompilationUnit* unit) {
  ASSERT(unit != nullptr);
  unfound_deps.panic = true;

  context->current_unit->dependencies.insert(unit);
  unit->dependency_of.insert(context->current_unit);
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
  }
}

void State::use_value(const ValueIndex index) {
  use_value(index, index);
}

void State::use_value(const ValueIndex index, const ValueIndex related) {
  struct Lambda {
    const ControlFlow* flow;

    bool operator()(const ValueUse& p) const noexcept {
      return flow->test_a_flows_to_b(p.time.flow, flow->current_flow)
        || p.time.flow == flow->current_flow;
    }
  };

  auto& val = *get_val(index);

  ASSERT(val.has_value);

  val.last_uses.remove_if(Lambda{ &control_flow });
  val.last_uses.insert(
    ValueUse{
      related,
      control_flow.now(),
    }
  );

  val.crosses_call = val.crosses_call //still crossed a call
    || (control_flow.had_call //had a call
        && val.creation.time.flow == control_flow.current_flow //same flow
        && val.creation.time.time < control_flow.last_call) // created before last call
    || (control_flow.had_call //had a call
        && val.creation.time.flow != control_flow.current_flow);//not same flow must lead here
}

MemIndex State::new_mem() {
  MemIndex i ={};
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

Local* State::find_local(const InternString* i_s) {
  auto i = active_locals.begin();
  const auto end = active_locals.end();

  for (; i < end; i++) {
    Local* l = all_locals.data + *i;
    if (l->decl.name == i_s) {
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

const Structure* find_or_make_array_structure(Compiler* const comp, Context* const context,
                                              const Type& base, size_t length) {
  Structures* structures = comp->services.structures;

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
  return STRUCTS::new_array_structure(comp, base, length);
}

static ConstantVal copy_constant_value(Compiler* comp, ConstantVal val) {
  u8* data = comp->constants.alloc_no_construct(val.size);

  memcpy_ts(data, val.size, val.ptr, val.size);
  return { data, val.size };
}

const Structure* find_or_make_pointer_structure(Compiler* const comp, Context* const context,
                                                const Type& base) {
  Structures* structures = comp->services.structures;

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
  return STRUCTS::new_pointer_structure(comp, base);
}

const Structure* find_or_make_tuple_structure(Compiler* const comp, Context* const context,
                                              Array<Type>&& els) {

  Structures* structures = comp->services.structures;

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



  //Doesnt exist - need to make new type
  return STRUCTS::new_tuple_structure(comp, std::move(els));
}

const SignatureStructure* find_or_make_lamdba_structure(Compiler* const comp, Context* const context,
                                                        const CallingConvention* conv,
                                                        Array<Type>&& params,
                                                        Type ret_type) {
  Structures* structures = comp->services.structures;
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


  SignatureStructure* sig_struct = STRUCTS::new_lambda_structure(comp, conv, std::move(params), ret_type);

  sig_struct->return_via_addres = register_passed_as_pointer(sig_struct->return_type.structure);
  if (sig_struct->return_via_addres) {
    sig_struct->actual_parameter_types.insert(
      to_type(find_or_make_pointer_structure(comp, context, sig_struct->return_type))
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
          to_type(find_or_make_pointer_structure(comp, context, *i))
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

//Note: Recursive for array types
void compile_type(Compiler* const comp,
                  Context* const context,
                  ASTType* type) {
  if (type->type.is_valid()) {
    //Just in case function re-called
    return;
  }

  switch (type->type_type) {
    case TYPE_TYPE::LAMBDA: {
        FOR_MUT(type->lambda.types, i) {
          compile_type(comp, context, i);
          if (comp->is_panic()) {
            return;
          }
        }

        Array<Type> args ={};

        FOR(type->lambda.types, i) {
          args.insert(i->type);
        }

        Type ret = *args.back();
        args.pop();


        const Structure* s = find_or_make_lamdba_structure(comp, context, comp->build_options.default_calling_convention, std::move(args), ret);

        type->type.structure = s;
        type->type.name = s->struct_name;

        break;
      }
    case TYPE_TYPE::NORMAL: {
        const NamedElement* name = comp->services.names->find_name(context->current_namespace, type->name);

        if (name == nullptr) {
          UnknownName unknown ={};
          unknown.all_names = false;
          unknown.ident = type->name;
          unknown.namespace_index = context->current_namespace;
          unknown.num_knowns = name == 0;
          unknown.num_unknowns = name == 0;

          comp->set_unfound_name(context, std::move(unknown),
                                 ERROR_CODE::NAME_ERROR, type->span,
                                 "Could not find name '{}'", type->name);
          return;
        }
        else if (name->unknowns > 0) {
          UnknownName unknown ={};
          unknown.all_names = false;
          unknown.ident = type->name;
          unknown.namespace_index = context->current_namespace;
          unknown.num_knowns = name->globals.size;
          unknown.num_unknowns = name->unknowns;

          comp->set_unfound_name(context, std::move(unknown),
                                 ERROR_CODE::NAME_ERROR, type->span,
                                 "Could not resolve name '{}'\n"
                                 "This name had multiple unknown dependencies",
                                 type->name);
        }
        else {
          if (name->globals.size != 1) {
            comp->report_error(ERROR_CODE::NAME_ERROR, type->span,
                               "'{}' is an ambiguous name",
                               type->name);
            return;
          }

          const Global* global = name->globals.data[0];

          if (global->decl.type.struct_type() != STRUCTURE_TYPE::TYPE) {
            comp->report_error(ERROR_CODE::NAME_ERROR, type->span,
                               "'{}' was not a type",
                               type->name);
            return;
          }

          if (global->constant_value.ptr == nullptr) {
            comp->set_dep(context, global->compilation_unit);
            return;
          }

          memcpy_ts(&type->type, 1, (const Type*)global->constant_value.ptr, 1);
        }
        break;
      }
    case TYPE_TYPE::ARRAY: {
        compile_type(comp, context, type->arr.base);
        if (comp->is_panic()) {
          return;
        }

        ASSERT(type->arr.base->type.structure != nullptr);
        ASSERT(type->arr.expr != nullptr);

        if (type->arr.expr->const_val == nullptr) {
          ConstantExprUnit* unit = comp->new_const_expr_unit(context->current_namespace);

          unit->type = COMPILATION_TYPE::CONST_EXPR;
          unit->stage = EXPR_COMP_STAGE::UNTYPED;
          unit->available_names = context->current_namespace;
          unit->expr = type->arr.expr;
          unit->cast_to = comp->services.builtin_types->t_u64;

          comp->set_dep(context, unit);
          return;
        }
        else {
          if (!TYPE_TESTS::is_int(type->arr.expr->type)) {
            comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, type->arr.expr->span,
                               "Expected an integer type value for array length\n"
                               "Instead found: {}", type->arr.expr->type.name);
            return;
          }

          uint64_t length;
          if (TYPE_TESTS::is_signed_int(type->arr.expr->type)) {
            int64_t i_length = *(const int64_t*)type->arr.expr->const_val;
            if (i_length < 0) {
              comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, type->arr.expr->span,
                                 "Length of array must positive\n"
                                 "Instead found: {}", i_length);
              return;
            }
            else {
              length = (uint64_t)i_length;
            }
          }
          else {
            length = *(const uint64_t*)type->arr.expr->const_val;
          }

          const Structure* s = find_or_make_array_structure(comp, context, type->arr.base->type, length);

          type->type.name = s->struct_name;
          type->type.structure = s;
          break;
        }

        break;
      }
    case TYPE_TYPE::PTR: {
        compile_type(comp, context, type->ptr.base);
        if (comp->is_panic()) {
          return;
        }

        ASSERT(type->ptr.base != nullptr);

        const Structure* s = find_or_make_pointer_structure(comp, context, type->ptr.base->type);

        type->type.name = s->struct_name;
        type->type.structure = s;
        break;
      }
    default:
      comp->report_error(ERROR_CODE::INTERNAL_ERROR, type->span,
                         "Invalid type union id '{}'", (int)type->type_type);
      return;
  }

  ASSERT(type->type.name != nullptr);
  ASSERT(type->type.structure != nullptr);
}

static OverloadSet generate_overload_set(Compiler* const comp,
                                         Context* const context,
                                         State* const state,
                                         const Span& span,
                                         const CallSignature* sig) {
  Array<NamespaceElement> names = comp->services.names->find_all_names(context->current_namespace, sig->name);

  OverloadSet set ={};

  const auto test_func = [&](const SignatureStructure* sig_struct) {
    set.num_options_checked += 1;

    //Correct name and number of args
    if (sig->arguments.size == sig_struct->parameter_types.size) {
      auto p_call = sig->arguments.begin();
      const auto end_call = sig->arguments.end();

      auto p_func = sig_struct->parameter_types.begin();

      bool requires_cast = false;

      while (p_call < end_call) {
        if (p_call->type == *p_func) {
          //Do nothing
        }
        else if (!set.complete_match
                 && TYPE_TESTS::check_implicit_cast(p_call->flags, p_call->type, *p_func)) {
          requires_cast = true;
        }
        else {
          //Escape out
          return;
        }

        p_call++;
        p_func++;
      }

      if (!set.complete_match && requires_cast) {
        //Insert possible overload
        set.valid_overloads.insert(sig_struct);
      }
      else {
        //Complete match
        if (!set.complete_match) {
          set.complete_match = true;
          set.valid_overloads.clear();
        }

        set.valid_overloads.insert(sig_struct);
      }
    }
  };

  //Globals
  {
    auto n_i = names.begin();
    const auto n_end = names.end();
    for (; n_i < n_end; n_i++) {
      const NamedElement* possible_func = n_i->named_element;
      NamespaceIndex ns = n_i->ns_index;

      if (possible_func->unknowns > 0) {
        UnknownName unknown ={};
        unknown.all_names = true;
        unknown.ident = sig->name;
        unknown.namespace_index = context->current_namespace;
        unknown.num_knowns = possible_func->globals.size;
        unknown.num_unknowns = possible_func->unknowns;

        comp->set_unfound_name(context, std::move(unknown),
                               ERROR_CODE::NAME_ERROR, span,
                               "'{}' has several meanings\n"
                               "Some of these had unresolved dependencies",
                               sig->name);
      }

      if (possible_func->globals.size > 0) {
        auto f_i = possible_func->globals.begin();
        auto f_end = possible_func->globals.end();

        for (; f_i < f_end; f_i++) {
          const Global* global = *f_i;

          if (global->decl.type.struct_type() != STRUCTURE_TYPE::LAMBDA) { continue; }

          const auto* sig_struct = global->decl.type.unchecked_base<SignatureStructure>();

          /*if (global->constant_value == nullptr) {
            comp->set_dep(context, global->compilation_unit);
            return {};
          }*/

          test_func(sig_struct);
        }
      }
    }
  }

  //Locals
  {
    auto i = state->active_locals.begin();
    const auto end = state->active_locals.end();

    for (; i < end; i++) {
      const Local* loc = state->all_locals.data + *i;

      if (loc->decl.type.struct_type() == STRUCTURE_TYPE::LAMBDA) {
        const auto* sig_struct = loc->decl.type.unchecked_base<SignatureStructure>();
        test_func(sig_struct);
      }

    }
  }

  return set;
}

static void compile_find_function_call(Compiler* const comp,
                                       Context* context,
                                       State* const state,
                                       ASTExpression* const expr) {
  ASSERT(expr->expr_type == EXPRESSION_TYPE::FUNCTION_CALL);
  FunctionCallExpr* const call = &expr->call;

  //TODO: local functions

  CallSignature sig ={};

  sig.name = call->function_name;
  sig.arguments.reserve_total(call->arguments.size);

  //Load all the types
  {
    auto i = call->arguments.begin();
    const auto end = call->arguments.end();

    for (; i < end; i++) {
      sig.arguments.insert({ i->meta_flags, i->type });
    }
  }

  sig.arguments.shrink();

  OverloadSet set = generate_overload_set(comp, context, state, expr->span, &sig);
  if (comp->is_panic()) {
    return;
  }

  if (set.valid_overloads.size == 0) {
    UnknownName unknown ={};
    unknown.all_names = true;
    unknown.ident = sig.name;
    unknown.namespace_index = context->current_namespace;
    unknown.num_knowns = set.num_options_checked;
    unknown.num_unknowns = 0;

    comp->set_unfound_name(context, std::move(unknown),
                           ERROR_CODE::TYPE_CHECK_ERROR, expr->span,
                           "'{}' has no valid overloads that match the call signature\n"
                           "Call Signature: '{}'",
                           sig.name, sig);
    return;
  }
  else if (set.valid_overloads.size == 1) {
    //Success!
    //Function* func = set.valid_overloads.data[0];
    call->sig = set.valid_overloads.data[0];
    //func->is_called = true;//tell it to be included in the final result
  }
  else if (set.complete_match) {
    comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->span,
                       "More than one function exists that exactly matches signature '{}'",
                       call->function_name);
  }
  else {
    Array<char> options_message ={};

    //Load all the options into a string
    for (size_t i = 0; i < set.valid_overloads.size; i++) {
      const SignatureStructure* sig = set.valid_overloads.data[i];

      format_to_array(options_message, "Option {}: {}\n", i + 1, PrintSignatureType{ sig });
    }

    //Format to array doesnt null terminate
    //Instead replace the last '\n' with '\0'
    *options_message.back() = '\0';

    comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->span,
                       "Found more than one function '{}' with signature that required implicit casts\n"
                       "Expcted: {}\n"
                       "{}", call->function_name, sig, options_message);
  }

}

RuntimeValue take_address(Compiler* comp, State* state, CodeBlock* code, const RuntimeValue* val) {
  UnOpArgs args ={};
  args.comp = comp;
  args.state = state;
  args.code = code;
  args.prim = val;

  return args.emit_address();
}

static void cast_operator_type(Compiler* const comp,
                               Context* const context,
                               State* const state,
                               ASTExpression* const expr) {
  ASSERT(expr->expr_type == EXPRESSION_TYPE::CAST);

  const Structures* const structures = comp->services.structures;

  const Type& cast_to = expr->cast.type.type;
  META_FLAGS from_flags = expr->cast.expr->meta_flags;
  const Type& cast_from = expr->cast.expr->type;

  DEFER(&) { if (!comp->is_panic()) ASSERT(expr->cast.emit != nullptr); };

  const auto emit_cast_func = [from_flags, &cast_to](ASTExpression* expr, CAST_FUNCTION cast) {
    expr->cast.emit = cast;
    expr->type = cast_to;
    expr->meta_flags = from_flags;
  };

  if (TYPE_TESTS::check_implicit_cast(from_flags, cast_from, cast_to)) {
    if (cast_from != cast_to) {
      TypeHint hint  ={};
      hint.tht = THT::EXACT;
      hint.type = cast_to;

      compile_type_of_expression(comp, context, state, expr->cast.expr, &hint);
    }

    emit_cast_func(expr, CASTS::no_op);
    return;
  }

  switch (cast_from.struct_type()) {
    case STRUCTURE_TYPE::ENUM: {
        const auto* en = cast_from.unchecked_base<EnumStructure>();

        if (cast_to == en->base) {
          emit_cast_func(expr, CASTS::no_op);
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

          if (TYPE_TESTS::check_implicit_cast(from_flags, from_arr->base, to_ptr->base)) {
            //Must be in memory to cast like this
            set_runtime_flags(expr->cast.expr, state, false, (uint8_t)RVT::MEMORY);
            emit_cast_func(expr, take_address);
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
            emit_cast_func(expr, CASTS::no_op);
            return;
          }
          else {
            //Cast up in size
            //Need specific instructions for different sizes
            //Can cast up to r64 and then "cast down" which is free

            if (from_int->size == 1) {
              if (from_int->is_signed) {
                emit_cast_func(expr, CASTS::i8_to_r64);
                return;
              }
              else {
                emit_cast_func(expr, CASTS::u8_to_r64);
                return;
              }
            }
            else if (from_int->size == 4) {
              if (from_int->is_signed) {
                emit_cast_func(expr, CASTS::i32_to_r64);
                return;
              }
              else {
                emit_cast_func(expr, CASTS::u32_to_r64);
                return;
              }
            }
          }

          comp->report_error(ERROR_CODE::INTERNAL_ERROR, expr->span,
                             "Cannot cast type '{}' to type '{}'\n"
                             "They are both integers and this should be implemented",
                             cast_from.name, cast_to.name);
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
            emit_cast_func(expr, CASTS::no_op);
            return;
          }

          if (TYPE_TESTS::match_sizes(from_ptr->base, to_ptr->base)) {
            emit_cast_func(expr, CASTS::no_op);
            return;
          }
        }

        break;
      }
    case STRUCTURE_TYPE::VOID: {
        comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->span,
                           "Cannot cast '{}' to any type\n"
                           "Attempted to cast '{}' to '{}'",
                           cast_from.name, cast_from.name, cast_to.name);
        break;
      }
  }

  comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->span,
                     "Cannot cast type '{}' to type '{}'",
                     cast_from.name, cast_to.name);
  return;
}

constexpr static bool already_const_type(EXPRESSION_TYPE et) {
  return et == EXPRESSION_TYPE::ENUM
    || et == EXPRESSION_TYPE::VALUE
    || et == EXPRESSION_TYPE::ASCII_STRING
    || et == EXPRESSION_TYPE::ASCII_CHAR
    || et == EXPRESSION_TYPE::NULLPTR
    || et == EXPRESSION_TYPE::STRUCT;
}

constexpr static bool can_compile_const_value(const ASTExpression* const expr) {
  return expr->const_val == nullptr
    && TEST_MASK(expr->meta_flags, META_FLAG::COMPTIME)
    && !already_const_type(expr->expr_type);
}

void force_load_const_value(Compiler* const comp, ASTExpression* expr) {
  ASSERT(TEST_MASK(expr->meta_flags, META_FLAG::COMPTIME));

  if (expr->const_val != nullptr) {
    return;
  }

  //If not already const then must be an "already const type"
  ASSERT(already_const_type(expr->expr_type));

  switch (expr->expr_type) {
    case EXPRESSION_TYPE::ENUM: {
        uint8_t* enum_c = comp->constants.alloc_no_construct(8);
        x64_to_bytes(expr->enum_value.enum_value->representation, enum_c);

        expr->const_val = enum_c;
        break;
      }
    case EXPRESSION_TYPE::VALUE: {
        const size_t size = expr->type.structure->size;

        uint8_t* val_c = comp->constants.alloc_no_construct(size);
        memcpy_ts(val_c, size, (uint8_t*)&expr->value.value, size);

        expr->const_val = val_c;
        break;
      }
    case EXPRESSION_TYPE::ASCII_STRING: {
        const auto* const arr_type = expr->type.extract_base<ArrayStructure>();
        ASSERT(arr_type != nullptr);

        char* string_c = (char*)comp->constants.alloc_no_construct(arr_type->size);

        expr->const_val = (uint8_t*)string_c;
        break;
      }
  }
}

static void do_literal_cast(Compiler* const comp,
                            const ASTExpression* expr, const Type& to_type,
                            const uint8_t* from, uint8_t* to) {
  //This should not be reached if the cast doesnt work

  const Type& from_type = expr->type;

  if (from_type == to_type) {
    const size_t size = from_type.structure->size;
    memcpy_ts(to, size, from, size);
    return;
  }

  switch (from_type.structure->type) {
    case STRUCTURE_TYPE::FIXED_ARRAY: {
        const auto* from_arr = from_type.unchecked_base<ArrayStructure>();
        const auto* to_arr = to_type.unchecked_base<ArrayStructure>();

        ASSERT(to_arr->type == STRUCTURE_TYPE::FIXED_ARRAY);
        ASSERT(from_arr->length == to_arr->length);
        ASSERT(expr->expr_type == EXPRESSION_TYPE::ARRAY_EXPR);

        const size_t from_base_size = from_arr->base.structure->size;
        const size_t to_base_size = to_arr->base.structure->size;

        for (size_t i = 0; i < from_arr->length; i++) {
          auto* base_expr = expr->array_expr.elements.data + i;

          do_literal_cast(comp, base_expr, to_arr->base, from + (i * from_base_size), to + (i * to_base_size));
          if (comp->is_panic()) {
            return;
          }
        }

        break;
      }
    default: {
        comp->report_error(ERROR_CODE::INTERNAL_ERROR, expr->span,
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

static void compile_unary_operator_emit(Compiler* const comp, Context* context, State* const state, ASTExpression* expr) {
  ASSERT(expr->expr_type == EXPRESSION_TYPE::UNARY_OPERATOR);

  switch (expr->un_op.op) {
    case UNARY_OPERATOR::NEG:
      compile_unary_operator(comp, context, state, expr, neg_operators);
      break;
    case UNARY_OPERATOR::ADDRESS:
      compile_take_address(comp, context, state, expr);
      break;
    case UNARY_OPERATOR::DEREF:
      compile_deref(comp, expr);
      break;
    default: {
        const char* name = UNARY_OP_STRING::get(expr->un_op.op);

        comp->report_error(ERROR_CODE::INTERNAL_ERROR, expr->span,
                           "Type checking is not implemented for unary operator '{}'",
                           name);
        return;
      }
  }
}

static void compile_binary_operator_emit(Compiler* const comp, Context* context, State* const state, ASTExpression* const expr,
                                         const TypeHint* hint) {
  switch (expr->bin_op.op) {
    case BINARY_OPERATOR::ADD:
      compile_binary_operator(comp, context, state, expr, add_operators, hint);
      break;
    case BINARY_OPERATOR::SUB:
      compile_binary_operator(comp, context, state, expr, sub_operators, hint);
      break;
    case BINARY_OPERATOR::MUL:
      compile_binary_operator(comp, context, state, expr, mul_operators, hint);
      break;
    case BINARY_OPERATOR::DIV:
      compile_binary_operator(comp, context, state, expr, div_operators, hint);
      break;
    case BINARY_OPERATOR::EQUIVALENT:
      compile_binary_operator(comp, context, state, expr, eq_operators);
      break;
    case BINARY_OPERATOR::NOT_EQ:
      compile_binary_operator(comp, context, state, expr, neq_operators);
      break;
    case BINARY_OPERATOR::LESSER:
      compile_binary_operator(comp, context, state, expr, lesser_operators);
      break;
    case BINARY_OPERATOR::GREATER:
      compile_binary_operator(comp, context, state, expr, greater_operators);
      break;
    case BINARY_OPERATOR::OR:
      compile_binary_operator(comp, context, state, expr, or_operators, hint);
      break;
    case BINARY_OPERATOR::XOR:
      compile_binary_operator(comp, context, state, expr, xor_operators, hint);
      break;
    case BINARY_OPERATOR::AND:
      compile_binary_operator(comp, context, state, expr, and_operators, hint);
      break;
    case BINARY_OPERATOR::LEFT_SHIFT:
      compile_binary_operator(comp, context, state, expr, left_shift_operators);
      break;
    case BINARY_OPERATOR::RIGHT_SHIFT:
      compile_binary_operator(comp, context, state, expr, right_shift_operators);
      break;
    default: {
        const char* const name = BINARY_OP_STRING::get(expr->bin_op.op);

        comp->report_error(ERROR_CODE::INTERNAL_ERROR, expr->span,
                           "Type Checking is not implemented for binary operator '{}'",
                           name);
        return;
      }
  }
}

void set_runtime_flags(ASTExpression* const expr, State* const state, bool modified, uint8_t valid_rvts) {
  if (modified) {
    expr->meta_flags &= ~META_FLAG::COMPTIME;
  }
  expr->valid_rvts &= valid_rvts;

  switch (expr->expr_type) {
    case EXPRESSION_TYPE::LOCAL: {
        Local* local = state->all_locals.data + expr->local;

        local->valid_rvts &= expr->valid_rvts;
        break;
      }
    case EXPRESSION_TYPE::MEMBER: {
        set_runtime_flags(expr->member.expr, state, modified, valid_rvts);
        break;
      }
    case EXPRESSION_TYPE::INDEX: {
        set_runtime_flags(expr->index.expr, state, modified, valid_rvts);
        break;
      }
  }
}

static bool test_type_satisfies_hint(META_FLAGS flags, const Type& ty, const TypeHint* hint) {
  ASSERT(ty.is_valid());

  if (hint == nullptr) return true;


  if (hint->tht == THT::EXACT) {
    ASSERT(hint->type.is_valid());

    return ty == hint->type;
  }
  else if (hint->tht == THT::IMPLICIT) {
    ASSERT(hint->type.is_valid());

    return TYPE_TESTS::check_implicit_cast(flags, ty, hint->type);
  }
  else if (hint->tht == THT::BASE) {
    ASSERT(hint->type.is_valid());
    const Type* base = nullptr;

    switch (ty.struct_type()) {
      case STRUCTURE_TYPE::FIXED_ARRAY:
        base = &ty.unchecked_base<ArrayStructure>()->base;
        break;
      case STRUCTURE_TYPE::POINTER:
        base = &ty.unchecked_base<PointerStructure>()->base;
        break;
      default:
        return false;
    }

    return TYPE_TESTS::check_implicit_cast(flags, *base, hint->type);
  }
  else if (hint->tht == THT::BASE_HINT) {
    const Type* ty_p = &ty;

    do {
      switch (ty_p->struct_type()) {
        case STRUCTURE_TYPE::FIXED_ARRAY:
          ty_p = &ty.unchecked_base<ArrayStructure>()->base;
          break;
        case STRUCTURE_TYPE::POINTER:
          ty_p = &ty.unchecked_base<PointerStructure>()->base;
          break;
        default:
          return false;
      }

      hint = hint->other_hint;
      ASSERT(hint != nullptr);
    } while (hint->tht == THT::BASE_HINT);

    return test_type_satisfies_hint(flags, *ty_p, hint);
  }

  INVALID_CODE_PATH("Type hint union option not covered");
}

static void compile_test_type_satisfies_hint(Compiler* const comp, const Span& span,
                                             META_FLAGS flags, const Type& ty, const TypeHint* hint) {
  ASSERT(ty.is_valid());

  if (hint == nullptr) return;

  if (hint->tht == THT::EXACT) {
    ASSERT(hint->type.is_valid());

    if (ty != hint->type) {
      comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, span,
                         "Expected type: '{}'. Found type: '{}'",
                         ty.name, hint->type.name);
      return;
    }
  }
  else if (hint->tht == THT::IMPLICIT) {
    ASSERT(hint->type.is_valid());

    if (!TYPE_TESTS::check_implicit_cast(flags, ty, hint->type)) {
      comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, span,
                         "Cannot implicitly case between type: '{}' and type: '{}'",
                         ty.name, hint->type.name);
      return;
    }
  }
  else if (hint->tht == THT::BASE) {
    ASSERT(hint->type.is_valid());
    const Type* base = nullptr;

    switch (ty.struct_type()) {
      case STRUCTURE_TYPE::FIXED_ARRAY:
        base = &ty.unchecked_base<ArrayStructure>()->base;
        break;
      case STRUCTURE_TYPE::POINTER:
        base = &ty.unchecked_base<PointerStructure>()->base;
        break;
      default:
        comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, span,
                           "'{}' is not an array or pointer type so has no base type",
                           ty.name);
        return;
    }

    if (!TYPE_TESTS::check_implicit_cast(flags, *base, hint->type)) {
      comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, span,
                         "Could not implicitly cast '{}' to '{}'",
                         hint->type.name, base->name);
      return;
    }
  }
  else if (hint->tht == THT::BASE_HINT) {
    const Type* ty_p = &ty;

    do {
      switch (ty_p->struct_type()) {
        case STRUCTURE_TYPE::FIXED_ARRAY:
          ty_p = &ty.unchecked_base<ArrayStructure>()->base;
          break;
        case STRUCTURE_TYPE::POINTER:
          ty_p = &ty.unchecked_base<PointerStructure>()->base;
          break;
        default:
          comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, span,
                             "'{}' is not an array or pointer type so has no base type",
                             ty_p->name);
          return;
      }

      hint = hint->other_hint;
      ASSERT(hint != nullptr);
    } while (hint->tht == THT::BASE_HINT);

    compile_test_type_satisfies_hint(comp, span, flags, ty, hint);
  }
}

static Type build_pointer_type_from_type_hint(Compiler* const comp, Context* context, const Span& span, const TypeHint* t_hint) {
  if (t_hint->tht == THT::EXACT) {
    return t_hint->type;
  }
  else if (t_hint->tht == THT::BASE) {
    return to_type(find_or_make_pointer_structure(comp, context, t_hint->type));
  }
  else {
    return to_type(find_or_make_pointer_structure(comp, context, build_pointer_type_from_type_hint(comp, context, span, t_hint->other_hint)));
  }
}

//Note: Recursive
void compile_type_of_expression(Compiler* const comp,
                                Context* const context,
                                State* const state,
                                ASTExpression* const expr,
                                const TypeHint* const hint) {
  //Already typed
  if (expr->type.is_valid() && test_type_satisfies_hint(expr->meta_flags, expr->type, hint)) {
    return;
  }

  ASSERT(expr->valid_rvts != 0);//shouldnt start 0
  DEFER(&) { ASSERT(expr->valid_rvts != 0); };//shouldnt end 0

  switch (expr->expr_type) {
    case EXPRESSION_TYPE::STRUCT: {
        ASTStructBody* struct_body = expr->struct_body.body;

        if (!struct_body->type.is_valid()) {
          comp->set_dep(context, struct_body->compilation_unit);
          return;
        }

        SET_MASK(expr->meta_flags, META_FLAG::COMPTIME);
        expr->type = comp->services.builtin_types->t_type;

        break;
      }
    case EXPRESSION_TYPE::LAMBDA: {
        //Never type the actual lambda here, its typed elsewhere
        ASTLambda* lambda = expr->lambda.lambda;

        if (lambda->sig.sig->sig_struct == nullptr) {
          comp->set_dep(context, lambda->function->compilation_unit);
          return;
        }

        SET_MASK(expr->meta_flags, META_FLAG::COMPTIME);
        expr->type = to_type(lambda->sig.sig->sig_struct);
        break;
      }
    case EXPRESSION_TYPE::MEMBER: {
        expr->member.expr->call_leaf = expr->call_leaf;

        //Compile the object first
        compile_type_of_expression(comp, context, state, expr->member.expr, nullptr);
        if (comp->is_panic()) {
          return;
        }

        //Assignable if the base is assignable
        expr->meta_flags = expr->index.expr->meta_flags;

        ASSERT(expr->member.expr->type.is_valid());

        if (expr->member.expr->type.struct_type() != STRUCTURE_TYPE::COMPOSITE) {
          comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->span,
                             "Type '{}' does not have any members (it is not a composite type)",
                             expr->member.expr->type.name);
          return;
        }

        const Type& cmp_t = expr->member.expr->type;

        const auto* cs = cmp_t.unchecked_base<CompositeStructure>();

        auto i = cs->elements.begin();
        auto end = cs->elements.end();

        expr->type.structure ={};//reset

        for (; i < end; i++) {
          if (i->name == expr->member.name) {
            expr->type = i->type;
            expr->member.offset = i->offset;
            break;
          }
        }

        if (!expr->type.is_valid()) {
          comp->report_error(ERROR_CODE::NAME_ERROR, expr->span,
                             "Type '{}' has no member '{}'",
                             cmp_t.name, expr->member.name);
          return;
        }

        //TODO: Make this work for other types
        set_runtime_flags(expr->member.expr, state, false, (uint8_t)RVT::MEMORY);

        expr->makes_call =  expr->member.expr->makes_call;

        compile_test_type_satisfies_hint(comp, expr->span, expr->meta_flags, expr->type, hint);
        if (comp->is_panic()) {
          return;
        }

        break;
      }
    case EXPRESSION_TYPE::INDEX: {
        expr->index.expr->call_leaf = expr->call_leaf;
        expr->index.index->call_leaf = expr->call_leaf;

        if (hint == nullptr) {
          compile_type_of_expression(comp, context, state, expr->index.expr, nullptr);
        }
        else {
          TypeHint index_hint ={};
          if (hint->tht == THT::BASE || hint->tht == THT::BASE_HINT) {
            index_hint.tht = THT::BASE_HINT;
            index_hint.other_hint = hint;
          }
          else {
            index_hint.tht = THT::BASE;
            index_hint.type = hint->type;
          }

          compile_type_of_expression(comp, context, state, expr->index.expr, &index_hint);
        }

        if (comp->is_panic()) {
          return;
        }

        expr->meta_flags = expr->index.expr->meta_flags;

        ASSERT(expr->index.expr->type.is_valid());

        compile_type_of_expression(comp, context, state, expr->index.index, nullptr);
        if (comp->is_panic()) {
          return;
        }
        ASSERT(expr->index.index->type.is_valid());

        ASSERT(expr->expr_type == EXPRESSION_TYPE::INDEX);

        auto* arr_expr = expr->index.expr;
        auto* index_expr = expr->index.index;

        if (!TYPE_TESTS::can_index(arr_expr->type)) {
          comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, arr_expr->span,
                             "Cannot take index of type: {}",
                             arr_expr->type.name);
          return;
        }

        if (!TYPE_TESTS::is_int(index_expr->type)) {
          comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, index_expr->span,
                             "An index must be in integer\n"
                             "Found non-integer type: {}",
                             index_expr->type.name);
          return;
        }


        if (TEST_MASK(index_expr->meta_flags, META_FLAG::LITERAL)) {
          TypeHint inner_hint ={};
          inner_hint.tht = THT::EXACT;
          inner_hint.type = comp->services.builtin_types->t_u64;

          compile_type_of_expression(comp, context, state, expr->index.index, &inner_hint);
          if (comp->is_panic()) {
            return;
          }
        }

        if (!TEST_MASK(index_expr->meta_flags, META_FLAG::COMPTIME)) {
          //If the index is not known at compile time then the array must be in memory or in a register
          uint8_t valids = (uint8_t)RVT::MEMORY;

          if (arr_expr->type.structure->size <= 8) {
            //Can be in a register if it will fit
            valids |= RVT::REGISTER;
          }

          set_runtime_flags(arr_expr, state, false, valids);
        }

        expr->type = arr_expr->type.unchecked_base<ArrayStructure>()->base;
        expr->makes_call = index_expr->makes_call || arr_expr->makes_call;

        compile_test_type_satisfies_hint(comp, expr->span, expr->meta_flags, expr->type, hint);
        if (comp->is_panic()) {
          return;
        }
        break;
      }
    case EXPRESSION_TYPE::TUPLE_LIT: {
        auto i = expr->tuple_lit.elements.mut_begin();
        const auto end = expr->tuple_lit.elements.end();

        //Assume comptime to start with
        SET_MASK(expr->meta_flags, META_FLAG::COMPTIME);


        if (hint == nullptr) {
          Array<Type> element_types ={};

          for (; i < end; i++) {
            i->call_leaf = expr->call_leaf;

            compile_type_of_expression(comp, context, state, i, nullptr);
            if (comp->is_panic()) {
              return;
            }
            ASSERT(i->type.is_valid());

            element_types.insert(i->type);

            expr->makes_call |= i->makes_call;
            expr->meta_flags &= i->meta_flags;
          }

          const Structure* tuple_s
            = find_or_make_tuple_structure(comp,
                                           context,
                                           std::move(element_types));

//Create the type
          expr->type = to_type(tuple_s);
        }
        else {
          if (hint->tht != THT::EXACT) {
            comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->span,
                               "Tuple expressions have no base type");
            return;
          }

          if (hint->type.struct_type() != STRUCTURE_TYPE::COMPOSITE) {
            comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->span,
                               "'{}' is not a composite type",
                               hint->type.name);
            return;
          }

          const auto* s = hint->type.unchecked_base<CompositeStructure>();

          auto s_i = s->elements.begin();

          for (; i < end; i++, s_i++) {
            i->call_leaf = expr->call_leaf;

            TypeHint inner_h ={};
            inner_h.tht = THT::EXACT;
            inner_h.type = s_i->type;

            compile_type_of_expression(comp, context, state, i, &inner_h);
            if (comp->is_panic()) {
              return;
            }
            ASSERT(i->type.is_valid());

            expr->makes_call |= i->makes_call;
            expr->meta_flags &= i->meta_flags;
          }

          expr->type = hint->type;
        }

        //Temp?
        expr->meta_flags &= ~META_FLAG::ASSIGNABLE;

        //Can currently on load into memory
        expr->valid_rvts &= RVT::MEMORY;

        compile_test_type_satisfies_hint(comp, expr->span, expr->meta_flags, expr->type, hint);
        if (comp->is_panic()) {
          return;
        }

        break;
      }
    case EXPRESSION_TYPE::ARRAY_EXPR: {
        expr->meta_flags &= ~META_FLAG::ASSIGNABLE;

        auto i = expr->array_expr.elements.mut_begin();
        const auto end = expr->array_expr.elements.end();

        //Assume true to start with
        expr->meta_flags |= META_FLAG::COMPTIME;

        Type base ={};

        TypeHint inner_hint_holder ={};

        bool fixed_hint = false;

        if (hint != nullptr) {
          if (hint->tht == THT::BASE_HINT) {
            const auto* b_h = hint->other_hint;

            //Set inner_h_ptr
            inner_hint_holder = *b_h;

            fixed_hint = hint->other_hint->tht == THT::EXACT;
          }
          else if (hint->tht == THT::BASE) {
            inner_hint_holder.tht = THT::EXACT;
            inner_hint_holder.type = hint->type;
            fixed_hint = true;
          }
          else if (hint->tht == THT::EXACT) {
            if (hint->type.struct_type() != STRUCTURE_TYPE::FIXED_ARRAY) {
              comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->span,
                                 "Could not implicity cast array to non-array type: '{}'",
                                 hint->type.name);
              return;
            }

            const auto* arr_s = hint->type.unchecked_base<ArrayStructure>();

            inner_hint_holder.tht = THT::EXACT;
            inner_hint_holder.type = arr_s->base;
            base = arr_s->base;

            fixed_hint = true;
          }
        }

        const TypeHint* inner_hint = &inner_hint_holder;

        if (!fixed_hint) {
          for (; i < end; i++) {
            i->call_leaf = expr->call_leaf;

            compile_type_of_expression(comp, context, state, i, inner_hint);
            if (comp->is_panic()) {
              return;
            }
            ASSERT(i->type.is_valid());

            if (!base.is_valid()) {
              base = i->type;
            }
            else {
              //Check new element matches
              if (TYPE_TESTS::check_implicit_cast(i->meta_flags, i->type, base)) {
                base = i->type;//Will always be either same or more specific
              }
            }

            i = expr->array_expr.elements.mut_begin();
            inner_hint_holder.tht = THT::EXACT;
            inner_hint_holder.type = base;
            inner_hint = &inner_hint_holder;

            for (; i < end; i++) {
              if (i->type != base) {
                if (!TYPE_TESTS::check_implicit_cast(i->meta_flags, i->type, base)) {
                  comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, i->span,
                                     "Array type was inferred as '{}'\n"
                                     "Cannot implicity cast '{}' to '{}'",
                                     base.name, i->type.name, base.name);
                  return;
                }
                else {
                  compile_type_of_expression(comp, context, state, i, inner_hint);
                  if (comp->is_panic()) {
                    return;
                  }
                }
              }
            }

            expr->makes_call |= i->makes_call;
            expr->meta_flags &= i->meta_flags;
          }

          if (!base.is_valid()) {
            comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, i->span,
                               "Array type could not inferred\n"
                               "This is probably because the array was empty (i.e. [])");
            return;
          }
          else {
            expr->type = to_type(find_or_make_array_structure(comp, context, base, expr->array_expr.elements.size));
          }
        }
        else {
          ASSERT(hint != nullptr);

          for (; i < end; i++) {
            i->call_leaf = expr->call_leaf;

            compile_type_of_expression(comp, context, state, i, inner_hint);
            if (comp->is_panic()) {
              return;
            }
            ASSERT(i->type.is_valid());

            expr->makes_call |= i->makes_call;
            expr->meta_flags &= i->meta_flags;
          }

          if (hint->tht == THT::EXACT) {
            expr->type = hint->type;
          }
          else {
            expr->type = to_type(find_or_make_array_structure(comp, context, base, expr->array_expr.elements.size));
          }
        }

        compile_test_type_satisfies_hint(comp, expr->span, expr->meta_flags, expr->type, hint);
        if (comp->is_panic()) {
          return;
        }

        break;
      }
    case EXPRESSION_TYPE::ASCII_CHAR: {
        expr->type = comp->services.builtin_types->t_ascii;
        expr->meta_flags |= META_FLAG::COMPTIME;

        compile_test_type_satisfies_hint(comp, expr->span, expr->meta_flags, expr->type, hint);
        if (comp->is_panic()) {
          return;
        }
        break;
      }
    case EXPRESSION_TYPE::ASCII_STRING: {

        const size_t len = expr->ascii_string->len + 1;

        expr->type = to_type(find_or_make_array_structure(comp, context, comp->services.builtin_types->t_ascii, len));
        expr->meta_flags |= META_FLAG::COMPTIME;

        compile_test_type_satisfies_hint(comp, expr->span, expr->meta_flags, expr->type, hint);
        if (comp->is_panic()) {
          return;
        }

        break;
      }
    case EXPRESSION_TYPE::ENUM: {
        const NamedElement* name = comp->services.names->find_name(context->current_namespace, expr->enum_value.name);

        if (name == nullptr) {
          UnknownName unknown ={};
          unknown.all_names = false;
          unknown.ident = expr->enum_value.name;
          unknown.namespace_index = context->current_namespace;
          unknown.num_unknowns = 0;

          comp->set_unfound_name(context, std::move(unknown),
                                 ERROR_CODE::UNFOUND_DEPENDENCY, expr->span,
                                 "Expected an enum but '{}' did not exist",
                                 expr->enum_value.name);
          return;
        }

        INVALID_CODE_PATH("Not yet implemented enums???");

        compile_test_type_satisfies_hint(comp, expr->span, expr->meta_flags, expr->type, hint);
        if (comp->is_panic()) {
          return;
        }

        //TODO: Is this valid?
        expr->meta_flags |= META_FLAG::COMPTIME;

        break;
      }
    case EXPRESSION_TYPE::VALUE: {
        constexpr auto smallest_type_for_integer = [](const BuiltinTypes* types, u64 val) -> Type {
          if (val < 0) {
            if (val >= INT8_MIN) {
              return types->t_i8;
            }
            /*else if (val >= INT8_MIN) {
              return types->t_i16;
            }*/
            else if (val >= INT32_MIN) {
              return types->t_i32;
            }
            else if (val >= INT64_MIN) {
              return types->t_i64;
            }
            else {
              INVALID_CODE_PATH("There isnt a big enough integer");
            }
          }
          else {
            if (val < UINT8_MAX) {
              return types->t_u8;
            }
            /*else if (val <= UINT16_MAX) {
              return types->t_u16;
            }*/
            else if (val <= UINT32_MAX) {
              return types->t_u32;
            }
            else if (val <= UINT64_MAX) {
              return types->t_u64;
            }
            else {
              INVALID_CODE_PATH("There isnt a big enough integer");
            }
          }

          return {};
        };

        ValueExpr* const val = &expr->value;

        expr->meta_flags |= META_FLAG::LITERAL;
        expr->meta_flags |= META_FLAG::COMPTIME;

        if (val->suffix == nullptr) {
          const Type min_size_int = smallest_type_for_integer(comp->services.builtin_types,
                                                              val->value);

          if (hint != nullptr) {
            if (hint->tht != THT::EXACT) {
              comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->span,
                                 "This literal has no base type");
              return;
            }

            expr->type = hint->type;

            if (!TYPE_TESTS::check_implicit_cast(expr->meta_flags, min_size_int, expr->type)) {
              comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->span,
                                 "Could not implicity cast literal of type '{}' to type '{}'\n"
                                 "Integer types are decided by the smallest value that can hold it\n"
                                 "If you want a different value try using an integer suffix",
                                 min_size_int.name, expr->type.name);
              return;
            }
          }
          else {
            expr->type = min_size_int;
          }
        }
        else {
          if (val->suffix == comp->services.builtin_types->t_i64.name) {
            expr->type = comp->services.builtin_types->t_i64;
          }
          else if (val->suffix == comp->services.builtin_types->t_u64.name) {
            expr->type = comp->services.builtin_types->t_u64;
          }
          else {
            comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->span,
                               "Invalid integer literal suffix type '{}'",
                               val->suffix);
            return;
          }

          compile_test_type_satisfies_hint(comp, expr->span, expr->meta_flags, expr->type, hint);
          if (comp->is_panic()) {
            return;
          }
        }

        break;
      }
    case EXPRESSION_TYPE::NULLPTR: {
        expr->meta_flags |= META_FLAG::LITERAL;
        expr->meta_flags |= META_FLAG::COMPTIME;

        if (hint != nullptr) {
          if (hint->tht == THT::EXACT) {
            if (hint->type.struct_type() != STRUCTURE_TYPE::POINTER) {
              comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->span,
                                 "'{}' is not a valid pointer type",
                                 hint->type.name);
              return;
            }

            expr->type = hint->type;
          }
          else if (hint->tht == THT::BASE) {
            expr->type = to_type(find_or_make_pointer_structure(comp, context, hint->type));
          }
          else {
            expr->type = build_pointer_type_from_type_hint(comp, context, expr->span, hint->other_hint);
          }

          if (!TYPE_TESTS::check_implicit_cast(expr->meta_flags,
                                               comp->services.builtin_types->t_void_ptr, expr->type)) {
            comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->span,
                               "Could not implicitly cast type '{}' to type '{}'",
                               comp->services.builtin_types->t_void_ptr.name, expr->type.name);
            return;
          }
        }
        else {
          expr->type = comp->services.builtin_types->t_void_ptr;
        }

        break;
      }
    case EXPRESSION_TYPE::NAME: {
        const InternString* name = expr->name;

        Local* const loc = state->find_local(name);

        const Decl* decl = nullptr;
        if (loc != nullptr) {
          expr->set_union(EXPRESSION_TYPE::LOCAL);
          expr->local = loc - state->all_locals.data;
          expr->type = loc->decl.type;

          loc->valid_rvts &= expr->valid_rvts;
          decl = &loc->decl;
        }
        else {
          NamedElement* non_local = comp->services.names->find_name(context->current_namespace, name);
          if (non_local == nullptr || non_local->unknowns > 0) {
            UnknownName unknown ={};
            unknown.all_names = false;
            unknown.ident = expr->enum_value.name;
            unknown.namespace_index = context->current_namespace;
            unknown.num_knowns = non_local == nullptr ? 0 : non_local->globals.size;
            unknown.num_unknowns = non_local == nullptr ? 0 : non_local->unknowns;

            comp->set_unfound_name(context, std::move(unknown),
                                   ERROR_CODE::UNFOUND_DEPENDENCY, expr->span,
                                   "'{}' was used but a it has not matching declaration",
                                   name);
            return;
          }

          if (non_local->globals.size != 1) {
            comp->report_error(ERROR_CODE::NAME_ERROR, expr->span,
                               "Name '{}' is ambiguous in this context", name);
            return;
          }

          const Global* glob = non_local->globals.data[0];

          if (!glob->decl.type.is_valid()) {
            comp->set_dep(context, glob->compilation_unit);
            return;
          }

          expr->set_union(EXPRESSION_TYPE::GLOBAL);
          expr->global = glob;
          expr->type   = glob->decl.type;

          decl = &glob->decl;
        }

        ASSERT(expr->type.is_valid());
        ASSERT(decl != nullptr);

        compile_test_type_satisfies_hint(comp, expr->span, expr->meta_flags, expr->type, hint);
        if (comp->is_panic()) {
          return;
        }

        //Can be comptime
        expr->meta_flags = decl->meta_flags;

        break;
      }
    case EXPRESSION_TYPE::LOCAL: {
        //Can be reached during type inference

        Local* const loc = state->all_locals.data + expr->local;

        compile_test_type_satisfies_hint(comp, expr->span, loc->decl.meta_flags, loc->decl.type, hint);
        if (comp->is_panic()) {
          return;
        }

        expr->meta_flags = loc->decl.meta_flags;
        break;
      }
    case EXPRESSION_TYPE::GLOBAL: {
        //Can be reached during type inference
        compile_test_type_satisfies_hint(comp, expr->span, expr->global->decl.meta_flags, expr->global->decl.type, hint);
        if (comp->is_panic()) {
          return;
        }

        expr->meta_flags = expr->global->decl.meta_flags;
        break;
      }
    case EXPRESSION_TYPE::CAST: {
        expr->cast.expr->call_leaf = expr->call_leaf;
        expr->meta_flags |= META_FLAG::COMPTIME;

        if (!expr->cast.type.type.is_valid()) {
          compile_type(comp, context, &expr->cast.type);
          if (comp->is_panic()) {
            return;
          }
        }

        compile_type_of_expression(comp, context, state, expr->cast.expr, nullptr);
        if (comp->is_panic()) {
          return;
        }

        ASSERT(expr->cast.expr->type.is_valid());

        //may set comtime to false
        cast_operator_type(comp, context, state, expr);
        if (comp->is_panic()) {
          return;
        }

        compile_test_type_satisfies_hint(comp, expr->span, expr->meta_flags, expr->type, hint);
        if (comp->is_panic()) {
          return;
        }

        expr->meta_flags &= expr->cast.expr->meta_flags;
        expr->makes_call = expr->cast.expr->makes_call;
        expr->type = expr->cast.type.type;
        break;
      }
    case EXPRESSION_TYPE::UNARY_OPERATOR: {
        expr->un_op.expr->call_leaf = expr->call_leaf;

        TypeHint inner_hint_holder ={};
        const TypeHint* inner_hint = nullptr;

        if (hint != nullptr) {
          inner_hint = &inner_hint_holder;

          switch (expr->un_op.op) {
            case UNARY_OPERATOR::NEG: {
                if (hint->tht == THT::EXACT) {
                  //TODO: This may be wrong
                  inner_hint = hint;
                }
                else {
                  //Whatever this might entail
                  //No idea how this can even be called but maybe it will
                  INVALID_CODE_PATH("Some weird stuff with type hints is happening");
                }
                break;
              }
            case UNARY_OPERATOR::ADDRESS: {
                switch (hint->tht) {
                  case THT::EXACT: {
                      if (hint->type.struct_type() != STRUCTURE_TYPE::POINTER) {
                        inner_hint = nullptr;
                      }
                      else {
                        inner_hint_holder.tht = THT::EXACT;
                        const auto* ps = hint->type.unchecked_base<PointerStructure>();
                        inner_hint_holder.type = ps->base;
                      }

                      break;
                    }
                  case THT::BASE_HINT: {
                      inner_hint = hint->other_hint;
                      break;
                    }
                  case THT::BASE: {
                      inner_hint_holder.tht = THT::EXACT;
                      inner_hint_holder.type = hint->type;
                      break;
                    }
                }
                break;
              }
            case UNARY_OPERATOR::DEREF: {
                expr->meta_flags |= META_FLAG::ASSIGNABLE;

                if (hint->tht == THT::EXACT) {
                  inner_hint_holder.tht = THT::EXACT;
                  inner_hint_holder.type = to_type(find_or_make_pointer_structure(comp, context, hint->type));
                }
                else {
                  inner_hint_holder.tht = THT::BASE_HINT;
                  inner_hint_holder.other_hint = hint;
                }
                break;
              }
          }
        }

        compile_type_of_expression(comp, context, state, expr->un_op.expr, inner_hint);
        if (comp->is_panic()) {
          return;
        }

        expr->meta_flags = expr->un_op.expr->meta_flags;
        expr->makes_call = expr->un_op.expr->makes_call;

        compile_unary_operator_emit(comp, context, state, expr);
        if (comp->is_panic()) {
          return;
        }

        compile_test_type_satisfies_hint(comp, expr->span, expr->meta_flags, expr->type, hint);
        if (comp->is_panic()) {
          return;
        }

        break;
      }
    case EXPRESSION_TYPE::BINARY_OPERATOR: {
        BinaryOperatorExpr* const bin_op = &expr->bin_op;

        bin_op->left->call_leaf = expr->call_leaf;
        bin_op->right->call_leaf = expr->call_leaf;

        //Do hints later

        compile_type_of_expression(comp, context, state, bin_op->left, nullptr);
        if (comp->is_panic()) {
          return;
        }

        compile_type_of_expression(comp, context, state, bin_op->right, nullptr);
        if (comp->is_panic()) {
          return;
        }

        expr->meta_flags = bin_op->left->meta_flags & bin_op->right->meta_flags;
        expr->makes_call = bin_op->left->makes_call || bin_op->right->makes_call;

        if (!TEST_MASK(expr->meta_flags, META_FLAG::COMPTIME)) {
          if (can_compile_const_value(bin_op->left)) {
            ConstantExprUnit* unit = comp->new_const_expr_unit(context->current_namespace);
            unit->expr = bin_op->left;

            comp->set_dep(context, unit);
            return;//guaranteed to have panic
          }
          else if (can_compile_const_value(bin_op->right)) {
            ConstantExprUnit* unit = comp->new_const_expr_unit(context->current_namespace);
            unit->expr = bin_op->right;

            comp->set_dep(context, unit);
            return;//guaranteed to have panic
          }
        }

        //pass in the hint for some operands
        compile_binary_operator_emit(comp, context, state, expr, hint);
        if (comp->is_panic()) {
          return;
        }

        compile_test_type_satisfies_hint(comp, expr->span, expr->meta_flags, expr->type, hint);
        if (comp->is_panic()) {
          return;
        }

        break;
      }
    case EXPRESSION_TYPE::FUNCTION_CALL: {
        FunctionCallExpr* const call = &expr->call;

        //TODO: Allow function execution at compile time
        //Means we need to have a way to know which functions to load
        //Currently it just expects to find a function but doesnt and calls the start of the code
        expr->meta_flags |= ~META_FLAG::COMPTIME;

        expr->makes_call = true;

        {
          auto i = call->arguments.mut_begin();
          const auto end = call->arguments.mut_end();

          for (; i < end; i++) {
            i->call_leaf = true;

            compile_type_of_expression(comp, context, state, i, nullptr);
            if (comp->is_panic()) {
              return;
            }

            expr->meta_flags &= i->meta_flags;
          }
        }

        if (call->sig == nullptr) {
          compile_find_function_call(comp, context, state, expr);
          if (comp->is_panic()) {
            return;
          }
        }

        const auto* sig = call->sig;

        {
          const size_t size = sig->parameter_types.size;

          if (call->arguments.size != size) {
            comp->report_error(ERROR_CODE::INTERNAL_ERROR, expr->span,
                               "Compiler linked a function with {} parameters for a call with {} arguments!",
                               size, call->arguments.size);
            return;
          }

          //Do implicit casts
          for (size_t i = 0; i < size; i++) {
            const Type& param_t = sig->parameter_types.data[i];
            ASTExpression* arg_expr = call->arguments.data + i;

            TypeHint inner_hint ={};

            inner_hint.tht = THT::EXACT;
            inner_hint.type = param_t;

            compile_type_of_expression(comp, context, state, arg_expr, &inner_hint);
            if (comp->is_panic()) {
              return;
            }
          }
        }

        if (!TEST_MASK(expr->meta_flags, META_FLAG::COMPTIME)) {
          auto i = call->arguments.mut_begin();
          const auto end = call->arguments.mut_end();

          for (; i < end; i++) {
            if (can_compile_const_value(i)) {
              ConstantExprUnit* unit = comp->new_const_expr_unit(context->current_namespace);
              unit->expr = i;

              comp->set_dep(context, unit);
              return;
            }
          }
        }

        //Last thing to do it set return type
        expr->type = sig->return_type;

        compile_test_type_satisfies_hint(comp, expr->span, expr->meta_flags, expr->type, hint);
        if (comp->is_panic()) {
          return;
        }
        break;
      }
    default: {
        comp->report_error(ERROR_CODE::INTERNAL_ERROR, expr->span,
                           "Invalid Expression type found! Expression id: '{}'",
                           (int)expr->expr_type);
        return;
      }
  }

  ASSERT(expr->type.is_valid());
}

static void compile_type_of_decl(Compiler* const comp,
                                 Context* const context,
                                 State* const state,
                                 ASTDecl* const decl) {
  if (decl->type_ast != nullptr) {
    if (!decl->type_ast->type.is_valid()) {
      compile_type(comp, context, decl->type_ast);
      if (comp->is_panic()) {
        return;
      }
    }

    if (!decl->expr->type.is_valid()) {
      TypeHint type_hint ={};
      type_hint.tht = THT::EXACT;
      type_hint.type = decl->type_ast->type;

      compile_type_of_expression(comp, context, state, decl->expr, &type_hint);
      if (comp->is_panic()) {
        return;
      }
    }

    decl->type = decl->type_ast->type;
  }
  else {
    if (!decl->expr->type.is_valid()) {
      compile_type_of_expression(comp, context, state, decl->expr, nullptr);
      if (comp->is_panic()) {
        return;
      }
    }

    decl->type = decl->expr->type;
  }

  ASSERT(decl->type.is_valid());

  //if (decl->structure->type == STRUCTURE_TYPE::SIMPLE_LITERAL
  //    || decl->structure->type == STRUCTURE_TYPE::TUPLE_LITERAL) {

  //  comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, decl->span,
  //                     "The type of '{}' was infered as '{}' which does not have a given size\n"
  //                     "Please use a different type (hopefully temporary)",
  //                     decl->name, decl->structure->name);
  //  return;
  //}

  if (decl->compile_time_const
      && (decl->expr == nullptr || !TEST_MASK(decl->expr->meta_flags, META_FLAG::COMPTIME))) {
    comp->report_error(ERROR_CODE::CONST_ERROR, decl->span,
                       "Compile time declaration '{}' must be initialized by a compile time expression",
                       decl->name);
    return;
  }
}

static void compile_type_of_statement(Compiler* const comp,
                                      Context* context,
                                      Function* const func,
                                      State* const state,
                                      UntypedCode* untyped,
                                      ASTStatement* const statement) {
  switch (statement->type) {
    case STATEMENT_TYPE::ASSIGN: {
        compile_type_of_expression(comp, context, state, statement->assign.assign_to, nullptr);
        if (comp->is_panic()) {
          return;
        }

        if (!TEST_MASK(statement->assign.assign_to->meta_flags, META_FLAG::ASSIGNABLE)) {
          comp->report_error(ERROR_CODE::CONST_ERROR, statement->assign.assign_to->span,
                             "Cannot assign to non-assignable expression");
          return;
        }

        set_runtime_flags(statement->assign.assign_to, state, true, (uint8_t)RVT::MEMORY);

        TypeHint hint ={};
        hint.tht = THT::EXACT;
        hint.type = statement->assign.assign_to->type;

        compile_type_of_expression(comp, context, state, statement->assign.value, &hint);
        if (comp->is_panic()) {
          return;
        }

        return;
      }
    case STATEMENT_TYPE::IF_ELSE: {
        ASTIfElse* const if_else = &statement->if_else;

        TypeHint hint ={};
        hint.tht = THT::EXACT;
        hint.type = comp->services.builtin_types->t_bool;

        compile_type_of_expression(comp, context, state, if_else->condition, &hint);
        if (comp->is_panic()) {
          return;
        }


        new_scope(&untyped->itr, if_else->if_statement, if_else->if_statement + 1, state);
        new_scope(&untyped->itr, if_else->else_statement, if_else->else_statement + 1, state);
        return;
      }
    case STATEMENT_TYPE::WHILE: {
        ASTWhile* const while_loop = &statement->while_loop;

        TypeHint hint ={};
        hint.tht = THT::EXACT;
        hint.type = comp->services.builtin_types->t_bool;

        compile_type_of_expression(comp, context, state, while_loop->condition, &hint);
        if (comp->is_panic()) {
          return;
        }

        new_scope(&untyped->itr, while_loop->statement, while_loop->statement + 1, state);
        return;
      }
    case STATEMENT_TYPE::BLOCK: {
        auto locals = state->active_locals.size;
        DEFER(&) { state->active_locals.size = locals; };

        new_scope(&untyped->itr, statement->block.block.mut_begin(), statement->block.block.end(), state);
        return;
      }
    case STATEMENT_TYPE::LOCAL: {
        ASTDecl* const decl = &statement->local;

        compile_type_of_decl(comp, context, state, decl);
        if (comp->is_panic()) {
          return;
        }

        if (can_compile_const_value(decl->expr)) {
          ConstantExprUnit* unit = comp->new_const_expr_unit(context->current_namespace);
          unit->expr = decl->expr;

          comp->set_dep(context, unit);
          return;
        }

        assert_empty_name(comp, statement->span, context->current_namespace, decl->name);
        if (comp->is_panic()) {
          return;
        }

        //Check for shadowing

        const Local* shadowing = state->find_local(decl->name);

        if (shadowing != nullptr) {
          comp->report_error(ERROR_CODE::NAME_ERROR, statement->span,
                             "Attempted to shadow the local variable '{}'",
                             decl->name);
          return;
        }


        size_t loc_index = state->all_locals.size;
        decl->local_index = loc_index;

        state->all_locals.insert_uninit(1);
        auto* loc = state->all_locals.back();

        loc->decl.name = decl->name;
        loc->decl.type = decl->type;
        loc->decl.source = decl;
        if (decl->compile_time_const) {
          loc->decl.meta_flags |= META_FLAG::COMPTIME;
        }
        else {
          loc->decl.meta_flags |= META_FLAG::ASSIGNABLE;
        }

        ASSERT(loc->decl.type.is_valid());

        if (TEST_MASK(loc->decl.meta_flags, META_FLAG::COMPTIME)) {
          if (!TEST_MASK(decl->expr->meta_flags, META_FLAG::COMPTIME)) {
            comp->report_error(ERROR_CODE::CONST_ERROR, decl->span,
                               "Cannot initialize a compile time constant with "
                               "a non compile time constant value");
            return;
          }

          //Load as constant
          force_load_const_value(comp, decl->expr);
          ASSERT(decl->expr->const_val != nullptr);

          loc->val.type = RVT::CONST;
          loc->val.constant = ConstantVal{ decl->expr->const_val, loc->decl.type.structure->size };
        }

        state->active_locals.insert(decl->local_index);

        return;
      }
    case STATEMENT_TYPE::EXPRESSION: {

        compile_type_of_expression(comp, context, state, statement->expression.expr, nullptr);
        if (comp->is_panic()) {
          return;
        }

        return;
      }
    case STATEMENT_TYPE::RETURN: {
        TypeHint hint ={};
        hint.tht  = THT::EXACT;
        hint.type = func->signature.sig_struct->return_type;

        ASTExpression* const expr = statement->expression.expr;
        compile_type_of_expression(comp, context, state, expr, &hint);
        if (comp->is_panic()) {
          return;
        }

        if (can_compile_const_value(expr)) {
          ConstantExprUnit* unit = comp->new_const_expr_unit(context->current_namespace);
          unit->expr = expr;

          comp->set_dep(context, unit);
          return;
        }
        return;
      }
  }

  comp->report_error(ERROR_CODE::INTERNAL_ERROR, statement->span,
                     "Reached end of statement type checking without exiting\n"
                     "Statement type: {}\n", statement->type);
}

static RuntimeValue advance_runtime_arg(State* state,
                                        CallingConvArgIterator* itr,
                                        const Structure* s) {
  RuntimeValue val ={};
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
  RuntimeValue val ={};
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

static RuntimeValue load_to_argument_itr(Compiler* comp,
                                         Context* context,
                                         State* state,
                                         CodeBlock* const code,
                                         const Structure* s,
                                         RuntimeValue* val,
                                         CallingConvArgIterator* itr) {
  RuntimeValue param = advance_runtime_arg(state, itr, s);

  const bool reg_passed_as_ptr = register_passed_as_pointer(s);

  if (param.type == RVT::REGISTER && reg_passed_as_ptr) {
    //LEA
    UnOpArgs args ={};
    args.comp = comp;
    args.state = state;
    args.code = code;
    args.prim = val;

    //Load as pointer
    RuntimeValue address = args.emit_address();
    const Structure* ptr_type = find_or_make_pointer_structure(comp, context, to_type(s));
    ASSERT(!comp->is_panic());

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
  else if (param.type == RVT::MEMORY &&  itr->conv->stack_pass_type == STACK_PASS_TYPE::POINTER
           && reg_passed_as_ptr) {
    //LEA
    UnOpArgs args ={};
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

static void load_const_to_mem(Compiler* const comp,
                              State* const state,
                              CodeBlock* const code,
                              const Structure* s,
                              ConstantVal constant,
                              MemIndex mem) {

  const uint32_t size = s->size;
  const uint32_t align = s->alignment;

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
  comp->constants.free_no_destruct((void*)constant.ptr);
  state->control_flow.expression_num++;
}

static void copy_mem_to_mem(Compiler* const comp,
                            State* const state,
                            CodeBlock* const code,
                            const Structure* s,
                            MemIndex from,
                            MemIndex to) {

  const uint32_t size = s->size;
  const uint32_t align = s->alignment;

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

static void load_const_to_reg(Compiler* const comp,
                              State* const state,
                              CodeBlock* const code,
                              ConstantVal constant,
                              ValueIndex reg) {
  ASSERT(constant.size <= 8);

  switch (constant.size) {
    case 1: {
        uint8_t u8 = *constant.ptr;
        ByteCode::EMIT::SET_R8_TO_8(code->code, (uint8_t)reg.val, u8);
        break;
      }
    case 2: {
        uint16_t u16 = x16_from_bytes(constant.ptr);
        ByteCode::EMIT::SET_R16_TO_16(code->code, (uint8_t)reg.val, u16);
        break;
      }
    case 4: {
        uint32_t u32 = x32_from_bytes(constant.ptr);
        ByteCode::EMIT::SET_R32_TO_32(code->code, (uint8_t)reg.val, u32);
        break;
      }
    case 8: {
        uint64_t u64 = x64_from_bytes(constant.ptr);
        ByteCode::EMIT::SET_R64_TO_64(code->code, (uint8_t)reg.val, u64);
        break;
      }
    default:
      printf("ERROR: Unsupported constant size: %zu\n", constant.size);
      INVALID_CODE_PATH("Cant handle register sizes that arent a power of 2");
  }

  comp->constants.free_no_destruct((void*)constant.ptr);

  state->set_value(reg);
  state->control_flow.expression_num++;
}

RuntimeValue new_const_in_reg(Compiler* const comp, State* const state, CodeBlock* const code, const uint8_t* data, size_t size) {
  ValueIndex val = state->new_value();

  RuntimeValue r ={};
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

void load_const_to_runtime_val(Compiler* const comp,
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

static void copy_mem_to_runtime(Compiler* const comp,
                                State* const state,
                                CodeBlock* const code,
                                const Structure* type,
                                MemIndex mem,
                                RuntimeValue* to) {
  ASSERT(to->type != RVT::CONST);
  DEFER(&) { state->control_flow.expression_num++; };

  switch (to->type) {
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

static void copy_reg_to_runtime(Compiler* const comp,
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

void copy_runtime_to_runtime(Compiler* const comp,
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

static void load_runtime_hint(Compiler* const comp,
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

void copy_runtime_to_runtime_hint(Compiler* const comp,
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

static void compile_bytecode_of_expression(Compiler* const comp,
                                           Context* const context,
                                           State* const state,
                                           CodeBlock* const code,
                                           const ASTExpression* const expr,
                                           RuntimeHint* hint);

static RuntimeValue compile_bytecode_of_expression_new(Compiler* const comp,
                                                       Context* const context,
                                                       State* const state,
                                                       CodeBlock* const code,
                                                       const ASTExpression* const expr,
                                                       uint8_t hint) {
  RuntimeHint rt_hint ={};
  rt_hint.is_hint = true;
  rt_hint.hint_types = hint;
  compile_bytecode_of_expression(comp, context, state, code, expr, &rt_hint);

  ASSERT(!rt_hint.is_hint);
  ASSERT(((uint8_t)rt_hint.val.type & hint) > 0);
  return rt_hint.val;
}

static void compile_bytecode_of_expression_existing(Compiler* const comp,
                                                    Context* const context,
                                                    State* const state,
                                                    CodeBlock* const code,
                                                    const ASTExpression* const expr,
                                                    RuntimeValue* hint) {
  ASSERT(hint->type != RVT::CONST);//Cant load to an existing constant

  RuntimeHint rt_hint ={};
  rt_hint.is_hint = false;
  rt_hint.val = *hint;

  compile_bytecode_of_expression(comp, context, state, code, expr, &rt_hint);

  ASSERT(rt_hint.val == *hint);
}

static void compile_function_call(Compiler* const comp,
                                  Context* const context,
                                  State* const state,
                                  CodeBlock* const code,
                                  const ASTExpression* const expr,
                                  RuntimeHint* hint) {

  ASSERT(expr->expr_type == EXPRESSION_TYPE::FUNCTION_CALL);
  const FunctionCallExpr* const call = &expr->call;

  auto save_stack_params = state->stack.current_passed;
  DEFER(&) { state->stack.current_passed = save_stack_params; };

  state->made_call = true;
  state->stack.require_call_alignment(16);//TODO: Actually work out the alignment

  const CallingConvention* convention = call->sig->calling_convention;

  bool has_return = call->sig->return_type != comp->services.builtin_types->t_void;
  bool return_via_pointer = has_return && register_passed_as_pointer(call->sig->return_type);

  struct StructuredVal {
    RuntimeValue rv ={};
    const Structure* str = nullptr;
  };

  Array<StructuredVal> parameter_vals;
  parameter_vals.reserve_total(call->arguments.size + return_via_pointer);

  if (return_via_pointer) {
    parameter_vals.insert_uninit(1);
  }


  //Compile expression for arguments
  {
    const size_t size = call->arguments.size;

    for (size_t i = 0; i < size; i++) {
      const ASTExpression* inner_expr = call->arguments.data + i;
      const Type call_type = call->sig->parameter_types.data[i];


      const RuntimeValue val = compile_bytecode_of_expression_new(comp, context, state, code, inner_expr, ALL_RVTS);

      parameter_vals.insert({ val, call_type.structure });
    }
  }

  state->control_flow.expression_num++;

  //Set argument registers and stack


  if (return_via_pointer) {
    ASSERT(hint != nullptr);
    //Load the return on the stack and then pass a pointer
    load_runtime_hint(comp, state, call->sig->return_type.structure, hint, (uint8_t)RVT::MEMORY);

    UnOpArgs args ={};
    args.comp = comp;
    args.state = state;
    args.code = code;
    args.prim = &hint->val;

    //First element is reserved ahead of time
    parameter_vals.data[0].rv = args.emit_address();
    parameter_vals.data[0].str = comp->services.builtin_types->t_void_ptr.structure;//just any pointer type

    state->control_flow.expression_num++;
  }

  CallingConvArgIterator conv_iter ={
    convention,
    0
  };

  auto i = parameter_vals.mut_begin();
  const auto end = parameter_vals.end();

  //Load the actual arguments
  for (; i < end; i++) {
    RuntimeValue param_val = load_to_argument_itr(comp, context, state, code, i->str, &i->rv, &conv_iter);

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

  size_t stack_params = state->stack.current_passed;

  Function* func = nullptr;
  {
    const auto names = comp->services.names->find_all_names(context->current_namespace, call->function_name);
    auto ni = names.begin();
    const auto nend = names.end();

    for (; ni < nend; ni++) {
      const auto& globals = ni->named_element->globals;
      auto gi = globals.begin();
      const auto gend = globals.end();

      for (; gi < gend; gi++) {
        const Global* glob = *gi;
        if (glob->decl.type.structure == call->sig) {
          ASSERT(glob->constant_value.ptr != nullptr);
          func = *(Function**)glob->constant_value.ptr;
          goto FOUND_FUNC;
        }
      }
    }
  }

  {
    auto li = state->active_locals.begin();
    const auto lend = state->active_locals.end();

    for (; li < lend; li++) {
      const Local* loc = state->all_locals.data + *li;

      if (loc->decl.type.structure == call->sig && loc->decl.name == call->function_name) {
        ASSERT(TEST_MASK(loc->decl.meta_flags, META_FLAG::COMPTIME));
        ASSERT(loc->val.type == RVT::CONST);
        ASSERT(loc->val.constant.size == 8);
        func = *(Function**)loc->val.constant.ptr;
        goto FOUND_FUNC;
      }
    }
  }

FOUND_FUNC:
  ASSERT(func != nullptr);


  func->is_called = true;
  ByteCode::EMIT::CALL(code->code, func);


  state->control_flow.had_call = true;
  state->control_flow.last_call = state->control_flow.expression_num;

  state->control_flow.expression_num++;

  if (has_return && !return_via_pointer) {
    ASSERT(hint != nullptr);

    //Need to reserve RAX if we didnt already pass a pointer in
    const ValueIndex rax = state->new_value();
    state->set_value(rax);//set by the called function

    {
      auto* rax_val = state->value_tree.values.data + rax.val;

      rax_val->value_type = ValueType::FIXED;
      rax_val->reg        = convention->return_register;
    }

    //Fake copy so dont need to insert copy later if one is needed
    state->control_flow.expression_num++;

    if (hint->is_hint) {
      load_runtime_hint(comp, state, expr->type.structure, hint, expr->valid_rvts & NON_CONST_RVTS);
    }

    copy_reg_to_runtime(comp, state, code, expr->type.structure, rax, &hint->val);
  }
}

//Note: Recursive 
static void compile_bytecode_of_expression(Compiler* const comp,
                                           Context* const context,
                                           State* const state,
                                           CodeBlock* const code,
                                           const ASTExpression* const expr,
                                           RuntimeHint* hint) {
  ASSERT(hint != nullptr);

  DEFER(&) {
    ASSERT(!hint->is_hint);

    state->control_flow.expression_num++;
  };

  if (expr->const_val != nullptr) {
    //Compile time expression

    //Copy the value to a new constant
    const size_t size = expr->type.structure->size;

    uint8_t* bytes = (uint8_t*)comp->constants.alloc_no_construct(size);
    const uint8_t* c_bytes = (const uint8_t*)expr->const_val;

    memcpy_ts(bytes, size, c_bytes, size);

    const ConstantVal constant ={ bytes, size };

    if (hint->is_hint) {
      load_runtime_hint(comp, state, expr->type.structure, hint, expr->valid_rvts);
    }

    load_const_to_runtime_val(comp, state, code, expr->type.structure, constant, &hint->val);
    return;
  }


  switch (expr->expr_type) {
    case EXPRESSION_TYPE::STRUCT: {
        ASSERT(state->comptime_compilation);

        if (hint->is_hint) {
          load_runtime_hint(comp, state, expr->type.structure, hint, expr->valid_rvts);
        }

        Type* struct_c = (Type*)comp->constants.alloc_no_construct(sizeof(Type));
        memcpy_ts(struct_c, 1, &expr->struct_body.body->type, 1);

        load_const_to_runtime_val(comp, state, code, expr->type.structure,
                                  ConstantVal{ (uint8_t*)struct_c, 8 },
                                  &hint->val);
        break;
      }
    case EXPRESSION_TYPE::LAMBDA: {
        ASSERT(state->comptime_compilation);

        if (hint->is_hint) {
          load_runtime_hint(comp, state, expr->type.structure, hint, expr->valid_rvts);
        }

        Function** func_c = (Function**)comp->constants.alloc_no_construct(8);
        *func_c = expr->lambda.lambda->function;

        load_const_to_runtime_val(comp, state, code, expr->type.structure,
                                  ConstantVal{ (uint8_t*)func_c, 8 },
                                  &hint->val);
        break;
      }
    case EXPRESSION_TYPE::MEMBER: {
        ASSERT(hint != nullptr);

        RuntimeValue obj = compile_bytecode_of_expression_new(comp,
                                                              context,
                                                              state,
                                                              code,
                                                              expr->index.expr,
                                                              (uint8_t)RVT::MEMORY);

        ASSERT(obj.type == RVT::MEMORY);

        RuntimeValue member ={};
        member.type = RVT::MEMORY;
        member.mem = state->new_mem();

        //Set up memory as offset from the original object memory
        {
          MemValue* member_mem = state->get_mem(member.mem);
          const MemValue* obj_mem = state->get_mem(obj.mem);

          member_mem->mem = obj_mem->mem;

          member_mem->mem.disp += expr->member.offset;
          member_mem->size = expr->type.structure->size;
        }

        copy_runtime_to_runtime_hint(comp, state, code, expr->type.structure, &member, hint, expr->valid_rvts & NON_CONST_RVTS);
        break;
      }
    case EXPRESSION_TYPE::INDEX: {
        ASSERT(hint != nullptr);
        const size_t base_size = expr->type.structure->size;

        ASSERT(TYPE_TESTS::can_index(expr->index.expr->type));

        RuntimeValue arr = compile_bytecode_of_expression_new(comp,
                                                              context,
                                                              state,
                                                              code,
                                                              expr->index.expr,
                                                              RVT::REGISTER | RVT::MEMORY);

        RuntimeValue index_val = compile_bytecode_of_expression_new(comp,
                                                                    context,
                                                                    state,
                                                                    code,
                                                                    expr->index.index,
                                                                    RVT::REGISTER | RVT::CONST);

        if (TYPE_TESTS::is_pointer(expr->index.expr->type)) {
          //Dereference the pointer to an array

          UnOpArgs args ={};
          args.comp = comp;
          args.state = state;
          args.code = code;
          args.prim = &arr;

          arr = args.emit_deref();
        }
        else {
          ASSERT(TYPE_TESTS::is_array(expr->index.expr->type));
        }

        if (arr.type == RVT::REGISTER) {
          //If its in a register then a shift is equivalent to the 
          RuntimeValue use_val ={};
          use_val.type = RVT::REGISTER;
          use_val.reg = state->new_value();

          BinOpArgs args ={};
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

          copy_runtime_to_runtime_hint(comp, state, code, expr->type.structure, &res, hint, expr->valid_rvts & NON_CONST_RVTS);
        }
        else {
          RuntimeValue index_mem ={};

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

          copy_runtime_to_runtime_hint(comp, state, code, expr->type.structure, &index_mem, hint, expr->valid_rvts & NON_CONST_RVTS);
        }

        break;
      }
    case EXPRESSION_TYPE::TUPLE_LIT: {
        ASSERT(hint != nullptr);
        ASSERT(expr->type.struct_type() == STRUCTURE_TYPE::COMPOSITE);

        const auto* cpst = expr->type.unchecked_base<CompositeStructure>();

        //Load the hint to mem
        if (hint->is_hint) {
          load_runtime_hint(comp, state, expr->type.structure, hint, expr->valid_rvts);
        }

        ASSERT(hint->val.type == RVT::MEMORY);

        RuntimeValue tup_single ={};
        tup_single.type = RVT::MEMORY;
        tup_single.mem  = state->new_mem();
        uint32_t base_disp = 0;

        {
          const MemValue* arr_mem = state->get_mem(hint->val.mem);
          MemValue* el_mem = state->get_mem(tup_single.mem);

          el_mem->mem = arr_mem->mem;
          base_disp = el_mem->mem.disp;
        }

        auto i = expr->array_expr.elements.begin();
        auto i_t = cpst->elements.begin();
        const auto end = expr->array_expr.elements.end();

        //save stack as expression stack cant be used by anything - its copied anyway
        auto save_stack = state->stack.current;

        for (; i < end; i++, i_t++) {
          //Set the correct offset
          MemValue* el_mem = state->get_mem(tup_single.mem);

          el_mem->mem.disp = base_disp + i_t->offset;
          el_mem->size = i_t->type.structure->size;

          //Load to that location
          compile_bytecode_of_expression_existing(comp, context, state, code, i, &tup_single);

          state->stack.current = save_stack;//reset stack
        }
        break;
      }
    case EXPRESSION_TYPE::ARRAY_EXPR: {
        ASSERT(hint != nullptr);
        const auto* const arr_type = expr->type.unchecked_base<ArrayStructure>();

        const size_t base_size = arr_type->base.structure->size;

        const size_t full_align = arr_type->alignment;
        const size_t full_size = arr_type->size;

        if (hint->is_hint) {
          load_runtime_hint(comp, state, expr->type.structure, hint, expr->valid_rvts);
        }

        //TODO: Constants

        if (full_size <= 8) {
          ASSERT(hint->val.type != RVT::CONST);

          //Its shifting time

          auto i = expr->array_expr.elements.begin();
          const auto end = expr->array_expr.elements.end();

          uint8_t shift_dst = 0;

          ASSERT(i < end);

          //Set up mask
          RuntimeValue mask ={};

          if (full_size > base_size) {
            mask.type = RVT::REGISTER;
            mask.reg = state->new_value();
            state->set_value(mask.reg);

            //Fill the bottom bits as a mask
            ByteCode::EMIT::SET_R64_TO_64(code->code,
                                          (uint8_t)mask.reg.val, bit_fill_lower<uint64_t>((uint8_t)base_size));

            state->control_flow.expression_num++;
          }

          BinOpArgs args ={};
          args.comp = comp;
          args.state = state;
          args.code = code;

          args.info = nullptr;

          //First element doesnt need shifting and works as the base value to shift into
          RuntimeValue res = compile_bytecode_of_expression_new(comp,
                                                                context,
                                                                state,
                                                                code,
                                                                i,
                                                                (uint8_t)RVT::REGISTER);

          args.left = &res;
          args.right = &mask;

          if (full_size > base_size) {
            res = args.emit_add_64s();
          }

          i++;

          for (; i < end; i++) {
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

            RuntimeValue shift_val ={};
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

          copy_runtime_to_runtime(comp, state, code, expr->type.structure, &res, &hint->val);
        }
        else {
          //Can only load large arrays into memory

          ASSERT(hint->val.type == RVT::MEMORY);

          RuntimeValue arr_single ={};
          arr_single.type = RVT::MEMORY;
          arr_single.mem  = state->new_mem();

          {
            const MemValue* arr_mem = state->get_mem(hint->val.mem);
            MemValue* index_mem = state->get_mem(arr_single.mem);

            index_mem->mem = arr_mem->mem;
            index_mem->size = base_size;
          }

          auto i = expr->array_expr.elements.begin();
          const auto end = expr->array_expr.elements.end();

          //save stack as expression stack cant be used by anything - its copied anyway
          auto save_stack = state->stack.current;

          for (; i < end; i++) {
            compile_bytecode_of_expression_existing(comp, context, state, code, i, &arr_single);

            state->stack.current = save_stack;//reset stack
            state->get_mem(arr_single.mem)->mem.disp += (int32_t)base_size;
          }
        }
        break;
      }
    case EXPRESSION_TYPE::ASCII_CHAR: {
        ASSERT(hint != nullptr);
        if (hint->is_hint) {
          load_runtime_hint(comp, state, expr->type.structure, hint, expr->valid_rvts);
        }

        char* char_c = (char*)comp->constants.alloc_no_construct(1);
        *char_c = expr->ascii_char;

        load_const_to_runtime_val(comp, state, code, expr->type.structure,
                                  ConstantVal{ (uint8_t*)char_c, 1 },
                                  &hint->val);
        break;
      }
    case EXPRESSION_TYPE::ASCII_STRING: {
        ASSERT(hint != nullptr);
        const auto* const arr_type = expr->type.unchecked_base<ArrayStructure>();

        const size_t size = arr_type->size;
        char* string_c = (char*)comp->constants.alloc_no_construct(size);

        const ConstantVal constant ={ (uint8_t*)string_c, size };

        memcpy_ts(string_c, size, expr->ascii_string->string, size);

        if (hint->is_hint) {
          load_runtime_hint(comp, state, expr->type.structure, hint, expr->valid_rvts);
        }

        load_const_to_runtime_val(comp, state, code, expr->type.structure, constant, &hint->val);
        break;
      }
    case EXPRESSION_TYPE::ENUM: {
        ASSERT(hint != nullptr);
        ASSERT(expr->type.structure->size == 8);
        uint8_t* enum_c = comp->constants.alloc_no_construct(8);
        x64_to_bytes(expr->enum_value.enum_value->representation, enum_c);

        const ConstantVal constant ={ (uint8_t*)enum_c, 8 };

        if (hint->is_hint) {
          load_runtime_hint(comp, state, expr->type.structure, hint, expr->valid_rvts);
        }

        load_const_to_runtime_val(comp, state, code, expr->type.structure, constant, &hint->val);
        break;
      }
    case EXPRESSION_TYPE::VALUE: {
        ASSERT(hint != nullptr);
        const size_t size = expr->type.structure->size;

        uint8_t* val_c = comp->constants.alloc_no_construct(size);
        memcpy_ts(val_c, size, (uint8_t*)&expr->value.value, size);

        const ConstantVal constant ={ val_c, size };

        if (hint->is_hint) {
          load_runtime_hint(comp, state, expr->type.structure, hint, expr->valid_rvts);
        }

        load_const_to_runtime_val(comp, state, code, expr->type.structure, constant, &hint->val);

        break;
      }
    case EXPRESSION_TYPE::NULLPTR: {
        ASSERT(hint != nullptr);
        uint8_t* val_c = comp->constants.alloc_no_construct(8);
        x64_to_bytes((uint64_t)0, val_c);

        const ConstantVal constant ={ val_c, 8 };

        if (hint->is_hint) {
          load_runtime_hint(comp, state, expr->type.structure, hint, expr->valid_rvts);
        }

        load_const_to_runtime_val(comp, state, code, expr->type.structure, constant, &hint->val);
        break;
      }
    case EXPRESSION_TYPE::LOCAL: {
        ASSERT(hint != nullptr);
        Local* local = state->all_locals.data + expr->local;

        RuntimeValue local_val = local->val;
        if (local_val.type == RVT::CONST) {
          //Need to actively copy the constant otherwise it will be freed twice
          ConstantVal copy_to ={};
          const ConstantVal to_copy = local_val.constant;

          copy_to.ptr = comp->constants.alloc_no_construct(to_copy.size);
          copy_to.size = to_copy.size;

          memcpy_ts(copy_to.ptr, copy_to.size, to_copy.ptr, copy_to.size);

          local_val.constant = copy_to;
        }

        copy_runtime_to_runtime_hint(comp, state, code, expr->type.structure, &local->val, hint, expr->valid_rvts & NON_CONST_RVTS);
        break;
      }
    case EXPRESSION_TYPE::GLOBAL: {
        ASSERT(hint != nullptr);
        const Global* glob = expr->global;

        if (glob->constant_value.ptr != nullptr) {
          if (hint->is_hint) {
            load_runtime_hint(comp, state, expr->type.structure, hint, expr->valid_rvts);
          }

          ConstantVal copied_val = copy_constant_value(comp, glob->constant_value);

          load_const_to_runtime_val(comp,
                                    state,
                                    code,
                                    glob->decl.type.structure, copied_val,
                                    &hint->val);
        }
        else {
          const auto reg_mem = state->new_value();

          ByteCode::EMIT::LOAD_GLOBAL_MEM(code->code, (uint8_t)reg_mem.val, glob);//changed later to be the actual location
          state->set_value(reg_mem);

          state->control_flow.expression_num++;

          RuntimeValue global_mem ={};
          global_mem.type = RVT::MEMORY;
          global_mem.mem = state->new_mem();

          MemValue* mem_val = state->get_mem(global_mem.mem);
          mem_val->size = glob->decl.type.structure->size;
          mem_val->mem.base = (uint8_t)reg_mem.val;

          copy_runtime_to_runtime_hint(comp, state, code, expr->type.structure, &global_mem, hint, expr->valid_rvts & NON_CONST_RVTS);
        }
        break;
      }
    case EXPRESSION_TYPE::CAST: {
        ASSERT(hint != nullptr);
        const CastExpr* const cast = &expr->cast;
        RuntimeValue val = compile_bytecode_of_expression_new(comp,
                                                              context,
                                                              state,
                                                              code,
                                                              cast->expr, ALL_RVTS);

        val = expr->cast.emit(comp, state, code, &val);

        copy_runtime_to_runtime_hint(comp, state, code, expr->type.structure, &val, hint, expr->valid_rvts);
        return;
      }
    case EXPRESSION_TYPE::UNARY_OPERATOR: {
        ASSERT(hint != nullptr);
        const UnaryOperatorExpr* const un_op = &expr->un_op;

        RuntimeValue temp = compile_bytecode_of_expression_new(comp,
                                                               context,
                                                               state,
                                                               code,
                                                               un_op->expr,
                                                               ALL_RVTS);

        UnOpArgs args ={};
        args.comp = comp;
        args.state = state;
        args.code = code;
        args.prim = &temp;

        RuntimeValue rt = (args.*un_op->emit)();

        copy_runtime_to_runtime_hint(comp, state, code, expr->type.structure, &rt, hint, expr->valid_rvts & NON_CONST_RVTS);
        break;
      }
    case EXPRESSION_TYPE::BINARY_OPERATOR: {
        ASSERT(hint != nullptr);
        const BinaryOperatorExpr* const bin_op = &expr->bin_op;
        const ASTExpression* const left = bin_op->left;
        const ASTExpression* const right = bin_op->right;

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

        BinOpArgs args ={};
        args.comp = comp;
        args.state = state;
        args.code = code;
        args.left = &temp_left;
        args.right = &temp_right;

        args.info = &bin_op->info;

        RuntimeValue res = (args.*(bin_op->emit))();

        copy_runtime_to_runtime_hint(comp, state, code, expr->type.structure, &res, hint, expr->valid_rvts & NON_CONST_RVTS);
        break;
      }
    case EXPRESSION_TYPE::FUNCTION_CALL:
      compile_function_call(comp, context, state, code, expr, hint);
      break;
    default: {
        //Invalid enum type
        //probably just didnt get around to supporting it
        INVALID_CODE_PATH("Invalid EXPRESSION_TYPE enum");
      }
  }
}

void compile_bytecode_of_statement(Compiler* const comp,
                                   Context* const context,
                                   ASTStatement* const statement,
                                   State* const state,
                                   CodeBlock* const code) {
  switch (statement->type) {
    case STATEMENT_TYPE::ASSIGN: {
        RuntimeValue assign_to = compile_bytecode_of_expression_new(comp,
                                                                    context,
                                                                    state,
                                                                    code,
                                                                    statement->assign.assign_to,
                                                                    (uint8_t)RVT::MEMORY);

        //Load into 'assign_to'
        compile_bytecode_of_expression_existing(comp,
                                                context,
                                                state,
                                                code,
                                                statement->assign.value,
                                                &assign_to);
        return;
      }
    case STATEMENT_TYPE::BLOCK: {
        const auto num_locals = state->active_locals.size;
        DEFER(&) { state->active_locals.size = num_locals; };

        auto i = statement->block.block.mut_begin();
        const auto end = statement->block.block.mut_end();

        for (; i < end; i++) {
          compile_bytecode_of_statement(comp, context, i, state, code);
        }

        return;
      }
    case STATEMENT_TYPE::RETURN: {
        compile_bytecode_of_expression_existing(comp,
                                                context,
                                                state,
                                                code,
                                                statement->expression.expr,
                                                &state->return_val);

        if (state->return_val.type == RVT::REGISTER) {
          state->use_value(state->return_val.reg);
        }

        ByteCode::EMIT::JUMP_TO_FIXED(code->code, state->return_label);
        return;
      }
    case STATEMENT_TYPE::EXPRESSION: {
        if (statement->expression.expr->type == comp->services.builtin_types->t_void) {
          compile_bytecode_of_expression(comp, context, state, code, statement->expression.expr, nullptr);
        }
        else {
          auto a = compile_bytecode_of_expression_new(comp, context, state, code, statement->expression.expr, ALL_RVTS);
        }

        return;
      }
    case STATEMENT_TYPE::WHILE: {
        ASTWhile* const while_loop = &statement->while_loop;

        const uint64_t base_label = comp->labels++;
        ByteCode::EMIT::LABEL(code->code, base_label);//to jump back to in the loop

        RuntimeValue cond = compile_bytecode_of_expression_new(comp,
                                                               context,
                                                               state,
                                                               code,
                                                               while_loop->condition,
                                                               RVT::CONST | RVT::REGISTER);

        if (cond.type == RVT::CONST) {
          //Only compile loop if its true

          if (*(uint64_t*)cond.constant.ptr != 0) {
            const auto locals = state->active_locals.size;
            DEFER(&) { state->active_locals.size = locals; };

            //Compile while loop as a fixed loop with no condition
            const uint64_t base_label = comp->labels++;

            compile_bytecode_of_statement(comp, context, while_loop->statement, state, code);
            ByteCode::EMIT::JUMP_TO_FIXED(code->code, base_label);
          }
        }
        else {
          //Conditional jump out of the loop
          const uint64_t exit_label = comp->labels++;
          ByteCode::EMIT::JUMP_TO_FIXED_IF_VAL_ZERO(code->code, (uint8_t)cond.reg.val, exit_label);
          state->use_value(cond.reg);

          const size_t start_flow = state->control_flow.current_flow;

          const auto locals = state->active_locals.size;

          //loop branch
          state->control_flow.new_flow();
          state->control_flow.set_a_flows_to_b(start_flow, state->control_flow.current_flow);
          compile_bytecode_of_statement(comp, context, while_loop->statement, state, code);

          state->active_locals.size = locals;

          ByteCode::EMIT::JUMP_TO_FIXED(code->code, base_label);
          const size_t loop_flow = state->control_flow.current_flow;

          //After the loop
          state->control_flow.new_flow();
          state->control_flow.set_a_flows_to_b(loop_flow, state->control_flow.current_flow);

          ByteCode::EMIT::LABEL(code->code, exit_label);
        }
        return;
      }
    case STATEMENT_TYPE::IF_ELSE: {
        ASTIfElse* const if_else = &statement->if_else;

        RuntimeValue cond = compile_bytecode_of_expression_new(comp,
                                                               context,
                                                               state,
                                                               code,
                                                               if_else->condition,
                                                               RVT::CONST | RVT::REGISTER);

        if (cond.type == RVT::CONST) {
          //Just becomes a block - no need for flows and stuff

          const auto locals = state->active_locals.size;
          DEFER(&) { state->active_locals.size = locals; };

          if (*(uint64_t*)cond.constant.ptr != 0) {
            //Compile if branch
            compile_bytecode_of_statement(comp, context, if_else->if_statement, state, code);
          }
          else {
            //Compile else branch
            compile_bytecode_of_statement(comp, context, if_else->else_statement, state, code);
          }
        }
        else {
          //Condition jump
          const uint64_t else_label = comp->labels++;

          ByteCode::EMIT::JUMP_TO_FIXED_IF_VAL_ZERO(code->code, (uint8_t)cond.reg.val, else_label);
          state->use_value(cond.reg);

          const size_t start_flow = state->control_flow.current_flow;

          const auto locals = state->active_locals.size;

          //If branch
          state->control_flow.new_flow();
          state->control_flow.set_a_flows_to_b(start_flow, state->control_flow.current_flow);
          compile_bytecode_of_statement(comp, context, if_else->if_statement, state, code);

          state->active_locals.size = locals;

          const size_t end_if_flow = state->control_flow.current_flow;

          //Jump from if branch to after the else branch
          const uint64_t escape_label = comp->labels++;
          ByteCode::EMIT::JUMP_TO_FIXED(code->code, escape_label);
          ByteCode::EMIT::LABEL(code->code, else_label);

          //Else branch
          state->control_flow.new_flow();
          state->control_flow.set_a_flows_to_b(start_flow, state->control_flow.current_flow);
          compile_bytecode_of_statement(comp, context, if_else->else_statement, state, code);

          ByteCode::EMIT::JUMP_TO_FIXED(code->code, escape_label);

          state->active_locals.size = locals;

          const size_t end_else_flow = state->control_flow.current_flow;

          //Leave the if
          state->control_flow.new_flow();
          state->control_flow.set_a_flows_to_b(end_if_flow, state->control_flow.current_flow);
          state->control_flow.set_a_flows_to_b(end_else_flow, state->control_flow.current_flow);

          ByteCode::EMIT::LABEL(code->code, escape_label);
        }
        return;
      }
    case STATEMENT_TYPE::LOCAL: {
        ASTDecl* const decl = &statement->local;

        Local* const local = state->all_locals.data + decl->local_index;

        if (!TEST_MASK(local->decl.meta_flags, META_FLAG::COMPTIME)) {
          RuntimeHint hint ={};
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


        //Needed for function calls
        state->active_locals.insert(decl->local_index);
        return;
      }
  }
}

static void map_values(const System* sys,
                       const CallingConvention* const conv,
                       CodeBlock* const code,
                       const State* const state,
                       uint64_t regs) {
  //Only non volatiles
  regs &= conv->non_volatiles_bit_mask;

  Array<uint8_t> temp ={};

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

  const auto UNROLL_COALESCE =[values = &state->value_tree.values](uint8_t index)->const Value* {
    const Value* val = values->data + index;

    while (val->is_coalesced()) {
      val = values->data + val->index.val;
    }

    return val;
  };

  const auto check_mem = [&](const MemComplex& mem) -> MemComplex {
    const Value* const b = UNROLL_COALESCE(mem.base);

    MemComplex out_mem ={};
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

#define X(name, structure) case ByteCode:: ## name: {\
      structure(ByteCode::PARSE:: ## name ## (bytecode));\
      bytecode += ByteCode::SIZE_OF:: ## name;\
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
        MemComplex mem ={};
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


static void compute_value_intersections(ValueTree& tree, const ControlFlow& flow) {
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

      auto j_last = j_val->last_uses.begin();
      const auto j_end = j_val->last_uses.end();

      for (; j_last < j_end; j_last++) {
        if (flow.test_a_flows_to_b(j_last->time.flow, i_val->creation.time.flow)
            || (j_last->time.flow == i_val->creation.time.flow && j_last->time.time < i_val->creation.time.time)) {
            //i created after j last use
            //if j_last flows to i_val then j_last must be before i_val
          continue;
        }
        else {
            //i created before j last use
          auto i_last = i_val->last_uses.begin();
          const auto i_end = i_val->last_uses.end();

          for (; i_last < i_end; i_last++) {
            if (flow.test_a_flows_to_b(i_last->time.flow, j_val->creation.time.flow)
                || (i_last->time.flow == j_val->creation.time.flow && i_last->time.time < j_val->creation.time.time)) {
                //i last used before j created
              continue;
            }
            else {
                //i and j dont not overlap
                //therefore overlap
              tree.set_intersection(ValueIndex{ i }, ValueIndex{ j });
              goto OVERLAP_DONE;
            }
          }
        }
      }
    OVERLAP_DONE:
      continue;
    }
  }
}

static uint64_t select(const CallingConvention* conv, State* const state) {
  const ValueTree& tree = state->value_tree;

  struct UnindexedAdjacency {
    ValueIndex current ={};
    Array<ValueIndex> adjacent ={};
  };

  Array<UnindexedAdjacency> a_l ={};

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

  //Load colours
  for (size_t iter = stack_max; iter > 0; iter--) {
    const size_t i = stack[iter - 1];
    auto* i_val = tree.values.data + i;

    if (i_val->fixed()) {
      continue;
    }

    uint64_t regs = 0;//not going to have more than 64 registers ... hopefully

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

    uint8_t colour = 0;
    if (i_val->crosses_call) {
      //requires non volatile reg
      colour += conv->num_volatile_registers;
    }

    //Find first index that is 0 (i.e. a free colour/reg)
    //Search in order of options->calling_convention->all_regs_unordered because this will do
    //volatile registers first and then non volatile registers if required
    while ((regs & ((uint64_t)1 << conv->all_regs_unordered[colour])) != 0) {
      colour++;
    }

    i_val->value_type = ValueType::FIXED;
    i_val->reg        = conv->all_regs_unordered[colour];
  }

  free_no_destruct(stack);

  uint64_t used_regs = 0;
  {
    auto i = tree.values.begin();
    const auto end = tree.values.end();

    for (; i < end; i++) {
      ASSERT(i->value_type != ValueType::FREE);

      if (i->fixed()) {
        used_regs |= ((uint64_t)1 << (i->reg));
      }
    }

    //Managed separately
    auto bp = conv->base_pointer_reg;
    auto sp = conv->stack_pointer_reg;

    used_regs &= ~(bp | sp);
  }

  return used_regs;
}

static void combine_last_uses(Array<ValueUse>& arr1, const Array<ValueUse>& arr2, const ControlFlow* const c_flow) {
  Array<ValueUse> temp ={};

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


void coalesce(const Compiler* const comp, const CallingConvention* conv, State* const state) {
  ValueTree& tree = state->value_tree;
  const ControlFlow& c_flow = state->control_flow;

  const size_t size = state->value_tree.adjacency_list.size;
  for (size_t l1 = 0; l1 < size; l1++) {
    auto& l1_val = tree.values.data[l1];

    ValueIndex created_by = resolve_coalesced(l1_val.creation.related_index, tree);
    auto& possible_parent = tree.values.data[created_by.val];

    Array<size_t> ignore_vals ={};

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
      l1_val.index.val  = created_by.val;

      parent_val.crosses_call |= l1_val.crosses_call;
      parent_val.is_modified  |= l1_val.is_modified;



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
void graph_colour_algo(Compiler* const comp,
                       const CallingConvention* const conv,
                       CodeBlock* const code,
                       State* const state) noexcept {
  if (comp->print_options.pre_reg_alloc) {
    IO::print("\n=== Pre Register Allocation Bytecode ===\n\n");
    ByteCode::print_bytecode(&reg_num_as_string, stdout, code->code.data, code->code.size);
    IO::print("\n=============================\n\n");
  }

  //Computers all the intersections in the value tree based on control flow
  //Build section
  compute_value_intersections(state->value_tree, state->control_flow);
  coalesce(comp, conv, state);
  const uint64_t regs = select(conv, state);

  //map values and function prolog and epilog
  map_values(comp->build_options.endpoint_system, conv, code, state, regs);

  code->code.shrink();

  if (comp->print_options.normal_bytecode) {
    IO::print("\n=== Normal Bytecode ===\n\n");
    ByteCode::print_bytecode(comp->build_options.endpoint_system->reg_name_from_num, stdout, code->code.data, code->code.size);
    IO::print("\n=============================\n\n");
  }
}

void new_scope(UntypedIterator* itr,
               ASTStatement* begin, const ASTStatement* end,
               State* state) {
  itr->scopes.insert_uninit(1);

  auto* sc = itr->scopes.back();

  sc->num_outer_locals = state->active_locals.size;

  sc->scope.i = begin;
  sc->scope.end = end;
}

ASTStatement* advance_scopes(UntypedIterator* itr, State* state) {
  if (itr->scopes.size == 0) {
    return nullptr;
  }

  auto* current_scope = itr->scopes.back();

  while (current_scope->scope.i >= current_scope->scope.end) {
    // Pop Locals
    state->active_locals.size = current_scope->num_outer_locals;

    // Pop scope
    itr->scopes.pop();

    //Finished?
    if (itr->scopes.size == 0) {
      return nullptr;
    }

    //Old scope
    current_scope = itr->scopes.back();
  }

  //return the statement
  return current_scope->scope.i++;
}

void compile_function_body_types(Compiler* const comp,
                                 Context* const context,
                                 ASTLambda* const ast_lambda,
                                 Function* const func,
                                 UntypedCode* const untyped,
                                 State* const state) {

  while (untyped->current_statement != nullptr) {
    compile_type_of_statement(comp, context, func, state, untyped, untyped->current_statement);
    if (comp->is_panic()) {
      return;
    }

    //Next statement!
    untyped->current_statement = advance_scopes(&untyped->itr, state);
  }
}

void compile_function_body_init(Compiler* const comp,
                                ASTLambda* const ast_lambda,
                                Function* const func,
                                UntypedCode* const untyped,
                                State* const state) {
  ASTFuncSig* ast_sig = &ast_lambda->sig;

  //Enter the body
  state->control_flow.new_flow();

  //Load parameters as locals

  state->all_locals.insert_uninit(ast_sig->parameters.size);

  size_t index = 0;
  const size_t size = ast_sig->parameters.size;

  for (; index < size; index++) {
    auto i = ast_sig->parameters.data + index;
    auto l_i = state->all_locals.data + index;
    state->active_locals.insert(index);

    l_i->decl.name = i->name;
    l_i->decl.type = i->type;
    l_i->decl.meta_flags |= META_FLAG::ASSIGNABLE;
    l_i->valid_rvts &= NON_CONST_RVTS;//Cant have const parameters
    l_i->val = RuntimeValue();
  }

  //Set up the scopes
  new_scope(&untyped->itr, ast_lambda->body.block.mut_begin(), ast_lambda->body.block.end(), state);
  untyped->current_statement = advance_scopes(&untyped->itr, state);
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
void compile_function_body_code(Compiler* const comp,
                                Context* const context,
                                ASTLambda* const ast_lambda,
                                Function* const func,
                                State* const state) {
  //Should never be called twice on the same function
  ASSERT(func->code_block.code.size == 0);

  //Enter the body - setup
  func->code_block.label = comp->labels++;
  state->return_label = comp->labels++;
  state->control_flow.new_flow();

  //Useful values
  init_state_regs(func->signature.sig_struct->calling_convention, state);

  bool return_as_ptr = func->signature.sig_struct->return_via_addres;

  const Type actual_return_type = return_as_ptr
    ? func->signature.sig_struct->actual_parameter_types.data[0]
    : func->signature.sig_struct->return_type;

  const CallingConvention* convention = func->signature.sig_struct->calling_convention;

  //Parameter set values
  {
    CallingConvParamIterator param_itr ={
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

    auto* i_loc = state->all_locals.mut_begin();
    const auto end_loc = i_loc + func->signature.sig_struct->parameter_types.size;

    for (; i_param < end_param; (i_param++, i_loc++)) {
      ASSERT(i_loc < end_loc);

      i_loc->val = advance_runtime_param(state, &param_itr, i_param->structure);
    }
  }

  state->control_flow.expression_num++;

  //Actual values for the parameters - allows them to be different from parameter registers
  {
    int32_t used_shadow_space = 0;

    if (return_as_ptr) {
      //Should fix any issues later on
      UnOpArgs args ={};
      args.comp = comp;
      args.state = state;
      args.code = &func->code_block;
      args.prim = &state->return_val;

      state->return_val = args.emit_deref();
    }

    if (return_as_ptr && state->return_val.type == RVT::REGISTER) {
      if (comp->optimization_options.non_stack_locals) {
        RuntimeHint load_hint ={};
        load_hint.hint_types = NON_CONST_RVTS;

        load_runtime_hint(comp, state, actual_return_type.structure, &load_hint, NON_CONST_RVTS);

        copy_runtime_to_runtime(comp, state, &func->code_block, actual_return_type.structure, &state->return_val, &load_hint.val);

        state->return_val = std::move(load_hint.val);
      }
      else {
        RuntimeValue rt_v ={};

        rt_v.type = RVT::MEMORY;
        rt_v.mem  = state->new_mem();

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

    auto i_loc = state->all_locals.mut_begin();
    const auto end_loc = state->all_locals.end();

    for (; i_param < end_param; (i_loc++, i_param++, i_act_param++)) {
      ASSERT(i_loc < end_loc);
      ASSERT(i_act_param < end_act_param);

      const Type& param_t = *i_param;
      const Type& act_param_t = *i_act_param;

      ASSERT(i_loc->decl.type == param_t);

      RuntimeValue rt_p ={};

      if (param_t == act_param_t) {
        if (i_loc->val.type != RVT::REGISTER) {
          //Dont need to save somehere else
          continue;
        }


        //Not passed by pointer
        if (comp->optimization_options.non_stack_locals) {
          RuntimeHint load_hint ={};
          load_hint.hint_types = i_loc->valid_rvts;

          load_runtime_hint(comp, state, param_t.structure, &load_hint, NON_CONST_RVTS);

          copy_runtime_to_runtime(comp, state, &func->code_block, param_t.structure, &i_loc->val, &load_hint.val);

          rt_p = std::move(load_hint.val);
        }
        else {
          ASSERT((i_loc->valid_rvts & RVT::MEMORY) > 0);
          rt_p.type = RVT::MEMORY;
          rt_p.mem  = state->new_mem();

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

          copy_reg_to_runtime(comp, state, &func->code_block, param_t.structure, i_loc->val.reg, &rt_p);
        }
      }
      else if (act_param_t.struct_type() == STRUCTURE_TYPE::POINTER
               && act_param_t.unchecked_base<PointerStructure>()->base == param_t) {
        //passed as pointer
        //Just do a simple deref
        //This should actually solves all issues with lifetimes later in the system
        UnOpArgs args ={};
        args.comp = comp;
        args.state = state;
        args.code = &func->code_block;
        args.prim = &i_loc->val;

        rt_p = args.emit_deref();
      }
      else {
        //Should never be here
        INVALID_CODE_PATH("Signature mismatch in code generation");
      }

      i_loc->val = std::move(rt_p);
    }
  }

  state->control_flow.expression_num++;

  //Calculate all the values
  //and compile the bytecode
  {
    Array<ASTStatement>& statements = ast_lambda->body.block;

    auto i = statements.mut_begin();
    const auto end = statements.mut_end();
    for (; i < end; i++) {
      compile_bytecode_of_statement(comp, context, i, state, &func->code_block);
    }
  }

  graph_colour_algo(comp, convention, &func->code_block, state);
}

static void compile_function_signature_type(Compiler* const comp,
                                            Context* const context,
                                            ASTFuncSig* const ast_sig,
                                            FunctionSignature* const sig) {
  DO_NOTHING;

  {
    auto i = ast_sig->parameters.mut_begin();
    auto end = ast_sig->parameters.end();

    for (; i < end; i++) {
      ASSERT(i->type_ast != nullptr);
      compile_type(comp, context, i->type_ast);
      if (comp->is_panic()) {
        return;
      }

      //Need to load the type
      i->type = i->type_ast->type;
    }
  }

  compile_type(comp, context, &ast_sig->return_type);
  if (comp->is_panic()) {
    return;
  }

  Array<Type> params ={};
  params.reserve_total(ast_sig->parameters.size);

  {
    auto i = ast_sig->parameters.begin();
    auto end = ast_sig->parameters.end();
    for (; i < end; i++) {
      params.insert(i->type);
    }
  }

  sig->sig_struct = find_or_make_lamdba_structure(comp,
                                                  context,
                                                  ast_sig->convention,
                                                  std::move(params),
                                                  ast_sig->return_type.type);
}

void compile_untyped_structure_declaration(Compiler* comp, Context* context, UntypedStructureElements* untyped) {
  for (; untyped->i < untyped->end; untyped->i++) {
    compile_type(comp, context, &untyped->i->type);
    if (comp->is_panic()) {
      return;
    }
  }
}

void compile_import_expression_type(Compiler* comp, Context* context, State* state,
                                    ASTExpression* expr) {
  compile_type_of_expression(comp, context, state, expr, nullptr);
  if (comp->is_panic()) { return; }

  if (expr->type.struct_type() != STRUCTURE_TYPE::FIXED_ARRAY) {
    comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->span,
                       "#{} expression must be a character array\n"
                       "Instead found: {}",
                       comp->intrinsics.import, expr->type.name);
    return;
  }

  const auto* arr = expr->type.unchecked_base<ArrayStructure>();

  if (arr->base != comp->services.builtin_types->t_ascii) {
    comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->span,
                       "#{} expression must be a character array\n"
                       "Expected base type: {}\n"
                       "Instead found: {}",
                       comp->intrinsics.import, comp->services.builtin_types->t_ascii.name, arr->base.name);
    return;
  }

  if (!TEST_MASK(expr->meta_flags, META_FLAG::COMPTIME)) {
    comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->span,
                       "#{} expression must be a compile time constant",
                       comp->intrinsics.import);
    return;
  }
}

static void compile_import_file(Compiler* comp, Context* context, ASTImport* imp, NamespaceIndex import_to) {

  const char* path = nullptr;

  if (already_const_type(imp->expr_location.expr_type)) {
    ASSERT(imp->expr_location.expr_type == EXPRESSION_TYPE::ASCII_STRING);

    path = imp->expr_location.ascii_string->string;
  }
  else {
    ASSERT(imp->expr_location.const_val != nullptr);

    path = (const char*)imp->expr_location.const_val;
  }

  ASSERT(path != nullptr);

  Namespace* ns = comp->services.names->get_raw_namespace(import_to);

  FileLocation loc = parse_file_location(ns->source_file.directory->string, path,
                                         comp->services.strings);

  if (imp->expr_location.const_val != nullptr) {
    comp->constants.free_no_destruct(imp->expr_location.const_val);
    imp->expr_location.const_val = nullptr;
  }

  const auto is_correct_file =[&loc](const ASTFile* f) {
    return f->file_loc == loc;
  };

  const ASTFile* imported_file = comp->parsed_files.find_if(is_correct_file);

  if (imported_file) {
    ns = comp->services.names->get_raw_namespace(import_to);
    ns->imported.insert(imported_file->namespace_index);
  }
  else {
    FileImport file_import ={};
    file_import.file_loc = std::move(loc);
    file_import.ns_index = comp->services.names->new_namespace();
    file_import.span = imp->span;

    ns = comp->services.names->get_raw_namespace(import_to);
    ns->imported.insert(file_import.ns_index);

    comp->services.file_loader->unparsed_files.insert(std::move(file_import));
  }
}

static void compile_untyped_global(Compiler* comp, Context* context, State* state, ASTDecl* decl, Global* global) {
  compile_type_of_decl(comp, context, state, decl);
  if (comp->is_panic()) {
    return;
  }

  global->decl.type = decl->type;
  if (decl->compile_time_const) {
    global->decl.meta_flags |= META_FLAG::COMPTIME;
  }
  else {
    global->decl.meta_flags |= META_FLAG::ASSIGNABLE;
  }
}

void compile_init_expr_of_global(Compiler* comp, Context* context, State* state, ASTDecl* decl, Global* global) {
  if (TEST_MASK(global->decl.meta_flags, META_FLAG::COMPTIME)) {
    if (decl->expr->const_val == nullptr) {
      ConstantExprUnit* unit = comp->new_const_expr_unit(context->current_namespace);
      unit->expr = decl->expr;

      comp->set_dep(context, unit);
      return;
    }

    global->constant_value.ptr = decl->expr->const_val;
    global->constant_value.size = global->decl.type.structure->size;
    decl->expr->const_val = nullptr;
  }
  else {
    ASSERT(global->decl.type.is_valid());

    state->control_flow.new_flow();

    global->init.label = comp->labels++;
    state->return_label = comp->labels++;

    ValueIndex reg_mem = state->new_value();

    ByteCode::EMIT::LOAD_GLOBAL_MEM(global->init.code, (uint8_t)reg_mem.val, global);//changed later to be the actual location
    state->set_value(reg_mem);

    state->control_flow.expression_num++;

    RuntimeValue global_mem ={};
    global_mem.type = RVT::MEMORY;
    global_mem.mem = state->new_mem();

    MemValue* mem_val = state->get_mem(global_mem.mem);
    mem_val->size = global->decl.type.structure->size;
    mem_val->mem.base = (uint8_t)reg_mem.val;

    compile_bytecode_of_expression_existing(comp, context, state, &global->init, decl->expr, &global_mem);

    graph_colour_algo(comp, comp->build_options.default_calling_convention, &global->init, state);
  }

  //Now it can exist
  NamedElement* el = comp->services.names->find_name(context->current_namespace, decl->name);

  if (decl->type.struct_type() == STRUCTURE_TYPE::LAMBDA) {
    //Special stuff to allow overloads

    {
      auto i = el->globals.begin();
      auto end = el->globals.end();

      for (; i < end; i++) {
        const Global* g = *i;
        if (g->decl.type.struct_type() != STRUCTURE_TYPE::LAMBDA) {
          comp->report_error(ERROR_CODE::NAME_ERROR, g->decl.source->span,
                             "Cannot overload the non-function '{}'", decl->name);
          return;
        }

        //TODO: check the functions are ok to be overloaded
      }
    }

  }
  else {
    if (el->globals.size + el->unknowns != 1) {
      comp->report_error(ERROR_CODE::NAME_ERROR, decl->span,
                         "Cannot overload the non-function '{}'", decl->name);
      return;
    }
  }

  el->unknowns--;
  el->globals.insert(global);
}

void compile_new_composite_structure(Compiler* comp, Context* context, ASTStructBody* struct_body) {
  CompositeStructure* cmp_s = STRUCTS::new_composite_structure(comp);

  uint32_t current_size = 0;
  uint32_t current_alignment = 0;

  auto i = struct_body->elements.begin();
  auto end = struct_body->elements.end();

  for (; i < end; i++) {
    cmp_s->elements.insert_uninit(1);
    auto* b = cmp_s->elements.back();

    b->type = i->type.type;
    b->name = i->name;
    b->offset = current_size;

    uint32_t this_align = b->type.structure->alignment;

    current_size = (uint32_t)ceil_to_n(current_size, this_align);
    current_size += b->type.structure->size;

    current_alignment = larger(this_align, current_alignment);
  }

  cmp_s->declaration = struct_body;
  cmp_s->size = current_size;
  cmp_s->alignment = current_alignment;

  struct_body->type = to_type(cmp_s);
}


void compile_current_unparsed_files(Compiler* const comp) {
  while (comp->services.file_loader->unparsed_files.size > 0) {
    //still have files to parse

    //parse the last file
    const FileImport* file_import = comp->services.file_loader->unparsed_files.back();

    const InternString* full_path = file_import->file_loc.full_name;

    if (comp->print_options.file_loads) {
      IO::print("Loading file \"", full_path->string, "\" ...\n");
    }

    //Just a sanity check - should alread have been set
    if (file_import->file_loc.extension == comp->services.file_loader->axl
        || file_import->file_loc.extension == nullptr) {
      //Load a source file

      OwnedPtr<const char> text_source = FILES::load_file_to_string(full_path->string);

      if (text_source.ptr == nullptr) {
        comp->report_error(ERROR_CODE::FILE_ERROR, comp->services.file_loader->unparsed_files.back()->span,
                           "File '{}' could not be opened, perhaps it does not exist",
                           full_path);
        return;
      }

      //Loaded file can pop of the file stack thing
      //It will shortly be loaded into the parsed files
      comp->services.file_loader->unparsed_files.pop();

      //Reset the parser for this file
      reset_parser(comp, full_path, text_source.ptr);

      // ^ Can error (in the lexing)
      if (comp->is_panic()) {
        return;
      }

      //Parse
      comp->parsed_files.insert_uninit(1);
      ASTFile* ast_file = comp->parsed_files.back();
      ast_file->file_loc = file_import->file_loc;

      ast_file->namespace_index = file_import->ns_index;
      comp->services.parser->current_namespace = file_import->ns_index;

      parse_file(comp, comp->services.parser, ast_file);


      //Test for errors
      if (comp->is_panic()) {
        return;
      }

      //Should no longer be needed now - can free the file
      text_source.free_no_destruct();

      ASSERT(comp->services.parser->current.type != AxleTokenType::Error);

      if (comp->print_options.ast) {
        IO::print("\n=== Print Parsed AST ===\n\n");
        print_ast(comp, ast_file);
        IO::print("\n========================\n\n");
      }

      if (comp->is_panic()) {
        return;
      }
      process_parsed_file(comp, ast_file);
    }
    else {
      comp->report_error(ERROR_CODE::FILE_ERROR, file_import->span,
                         "'{}' is not a loadable file extension",
                         file_import->file_loc.extension);
      return;
    }
  }
}

void process_parsed_file(Compiler* const comp, ASTFile* const file) {
  Namespace* ns = comp->services.names->get_raw_namespace(file->namespace_index);

  ns->source_file = file->file_loc;

  {
    auto i = file->imports.mut_begin();
    const auto end = file->imports.mut_end();

    for (; i < end; i++) {
      ImportUnit* imp = comp->new_import_unit(file->namespace_index);
      imp->import_ast = i;
    }
  }

  {
    ASTDecl* i = file->decls.mut_begin();
    const auto end = file->decls.end();

    for (; i < end; i++) {
      NamedElement* el = ns->names.get_val(i->name);
      if (el == nullptr) {
        el = ns->names.insert(i->name);
      }

      //Not typed yet
      el->unknowns++;

      GlobalUnit* unit = comp->new_global_unit(file->namespace_index);
      Global* glob =  comp->globals.insert();

      glob->decl.name = i->name;
      glob->decl.source = i;

      unit->global = glob;
      unit->source = i;
    }
  }

  //Might as well try and save some memory
  ns->imported.shrink();
}

void add_comp_unit_for_lambda(Compiler* const comp, NamespaceIndex namespace_index, ASTLambda* lambda) noexcept {
  //Make  new function
  Function* const func = comp->new_function();

  SignatureUnit* const unit = comp->new_signature_unit(namespace_index);

  func->compilation_unit = unit;

  //Link up the ast and function
  lambda->function = func;
  lambda->sig.sig = &func->signature;
  func->declaration = lambda;

  unit->source = lambda;
  unit->sig = &func->signature;
  unit->func = func;

  lambda->sig.parameters.shrink();
  lambda->sig.convention = comp->build_options.default_calling_convention;
}

void add_comp_unit_for_struct(Compiler* const comp, NamespaceIndex namespace_index, ASTStructBody* struct_body) noexcept {
  StructureUnit* const unit = comp->new_structure_unit(namespace_index);

  unit->source = struct_body;

  unit->untyped.i = struct_body->elements.mut_begin();
  unit->untyped.end = struct_body->elements.end();

  struct_body->compilation_unit = unit;
}

void free_compilation_unit(Compiler* const comp, CompilationUnit* unit) {
  FOR(unit->dependency_of, u) {
    free_compilation_unit(comp, *u);
  }

  switch (unit->type) {
    case COMPILATION_TYPE::FUNCTION:
      comp->function_units.free((const FunctionUnit*)unit);
      break;
    case COMPILATION_TYPE::STRUCTURE:
      comp->structure_units.free((const StructureUnit*)unit);
      break;
    case COMPILATION_TYPE::SIGNATURE:
      comp->signature_units.free((const SignatureUnit*)unit);
      break;
    case COMPILATION_TYPE::CONST_EXPR:
      comp->const_expr_units.free((const ConstantExprUnit*)unit);
      break;
    case COMPILATION_TYPE::GLOBAL:
      comp->global_units.free((const GlobalUnit*)unit);
      break;
    case COMPILATION_TYPE::IMPORT:
      comp->import_units.free((const ImportUnit*)unit);
      break;
    default:
      INVALID_CODE_PATH("Invalid compilation type");
  }
}

void close_compilation_unit(Compiler* const comp, const CompilationUnit* unit) {
  //Remove as dependency
  auto i = unit->dependency_of.begin();
  const auto end = unit->dependency_of.end();

  for (; i < end; i++) {
    CompilationUnit* dep_of = *i;

    const auto is_unit = [unit](const CompilationUnit* dep) -> bool { return dep == unit; };
    dep_of->dependencies.remove_if(is_unit);

    if (dep_of->dependencies.size == 0) {
      //No more dependencies - can add back to the compiling
      comp->to_compile.insert(dep_of);
    }
  }

  switch (unit->type) {
    case COMPILATION_TYPE::FUNCTION:
      comp->function_units.free((const FunctionUnit*)unit);
      break;
    case COMPILATION_TYPE::STRUCTURE:
      comp->structure_units.free((const StructureUnit*)unit);
      break;
    case COMPILATION_TYPE::SIGNATURE:
      comp->signature_units.free((const SignatureUnit*)unit);
      break;
    case COMPILATION_TYPE::CONST_EXPR:
      comp->const_expr_units.free((const ConstantExprUnit*)unit);
      break;
    case COMPILATION_TYPE::GLOBAL:
      comp->global_units.free((const GlobalUnit*)unit);
      break;
    case COMPILATION_TYPE::IMPORT:
      comp->import_units.free((const ImportUnit*)unit);
      break;
    default:
      INVALID_CODE_PATH("Invalid compilation type");
  }
}

ERROR_CODE compile_all(Compiler* const comp) {
  Array<CompilationUnit*> to_compile ={};
  CompilationUnit** to_comp_i = nullptr;
  CompilationUnit** to_comp_end = nullptr;

  DEFER(comp, &to_comp_i, &to_comp_end) {
    for (; to_comp_i < to_comp_end; to_comp_i++) {
      free_compilation_unit(comp, *to_comp_i);
    }

    FOR(comp->to_compile, it) {
      free_compilation_unit(comp, *it);
    }

    FOR(comp->unfound_deps.unfound, it) {
      free_compilation_unit(comp, it->unit_waiting);
    }
  };


  while (comp->is_compiling()) {
    //Compile waiting
    {
      if (comp->services.file_loader->unparsed_files.size > 0) {
        compile_current_unparsed_files(comp);
        if (comp->is_panic()) {
          return comp->services.errors->print_all();
        }
      }

      to_compile = std::move(comp->to_compile);

      to_comp_i = to_compile.mut_begin();
      to_comp_end = to_compile.mut_end();

      for (; to_comp_i < to_comp_end; to_comp_i++) {
        CompilationUnit* comp_u = *to_comp_i;

        Context context ={};
        context.current_unit = comp_u;
        context.current_namespace = comp_u->available_names;

        //TODO: Determine system and calling convention
        context.system = nullptr;
        context.calling_convention = nullptr;

        if (comp->print_options.start_comp_unit) {
          format_print("Running Comp unit {}\n",
                       PrintPtr{ comp_u });
        }

        switch (comp_u->type) {
          case COMPILATION_TYPE::IMPORT: {
              ImportUnit* const unit = (ImportUnit*)comp_u;

              switch (unit->stage) {
                case IMPORT_COMP_STAGE::UNTYPED: {
                    compile_import_expression_type(comp, &context, &unit->state,
                                                   &unit->import_ast->expr_location);
                    if (comp->is_panic()) {
                      if (comp->is_fatal()) {
                        return comp->services.errors->print_all();
                      }
                      else {
                        comp->reset_panic();
                      }
                    }
                    else {
                      unit->stage = IMPORT_COMP_STAGE::UNPARSED;

                      //Only called if this isnt already a constant literal
                      if (can_compile_const_value(&unit->import_ast->expr_location)) {
                        ConstantExprUnit* const_u = comp->new_const_expr_unit(unit->available_names);

                        const_u->expr = &unit->import_ast->expr_location;
                        const_u->state = std::move(unit->state);

                        const_u->cast_to ={};

                        comp->set_dep(&context, const_u);
                        ASSERT(comp->is_panic() && !comp->is_fatal());
                        comp->reset_panic();

                        //dont redo this one straight away as it has a dependency
                      }
                      else {
                        //redo this one straight away as already loaded
                        to_comp_i--;
                      }

                    }
                    break;
                  }
                case IMPORT_COMP_STAGE::UNPARSED: {
                    compile_import_file(comp, &context, unit->import_ast, unit->available_names);
                    if (comp->is_panic()) {
                      if (comp->is_fatal()) {
                        return comp->services.errors->print_all();
                      }
                      else {
                        comp->reset_panic();
                      }
                    }

                    close_compilation_unit(comp, comp_u);
                    break;
                  }
              }

              break;
            }
          case COMPILATION_TYPE::GLOBAL: {
              GlobalUnit* const unit = (GlobalUnit*)comp_u;

              switch (unit->stage) {
                case GLOBAL_COMP_STAGE::UNTYPED: {
                    compile_untyped_global(comp, &context, &unit->state, unit->source, unit->global);
                    if (comp->is_panic()) {
                      if (comp->is_fatal()) {
                        return comp->services.errors->print_all();
                      }
                      else {
                        comp->reset_panic();
                      }
                    }
                    else {
                      unit->stage = GLOBAL_COMP_STAGE::TYPED;
                      to_comp_i--;//redo straight away
                    }
                    break;
                  }
                case GLOBAL_COMP_STAGE::TYPED: {
                    compile_init_expr_of_global(comp, &context, &unit->state, unit->source, unit->global);
                    if (comp->is_panic()) {
                      if (comp->is_fatal()) {
                        return comp->services.errors->print_all();
                      }
                      else {
                        comp->reset_panic();
                      }
                    }
                    else {
                      close_compilation_unit(comp, comp_u);
                    }
                    break;
                  }
              }

              break;
            }
          case COMPILATION_TYPE::STRUCTURE: {
              StructureUnit* const unit = (StructureUnit*)comp_u;

              switch (unit->stage) {
                case STRUCTURE_COMP_STAGE::UNTYPED: {
                    compile_untyped_structure_declaration(comp, &context, &unit->untyped);
                    if (comp->is_panic()) {
                      if (comp->is_fatal()) {
                        return comp->services.errors->print_all();
                      }
                      else {
                        comp->reset_panic();
                      }
                    }
                    else {
                      unit->stage = STRUCTURE_COMP_STAGE::TYPED;
                      to_comp_i--;//redo straight away
                    }
                    break;
                  }
                case STRUCTURE_COMP_STAGE::TYPED: {
                    compile_new_composite_structure(comp, &context, unit->source);
                    if (comp->is_panic()) {
                      //Should only be called once
                      return comp->services.errors->print_all();
                    }

                    //Finished
                    close_compilation_unit(comp, unit);
                    break;
                  }
                default: {
                    comp->report_error(ERROR_CODE::INTERNAL_ERROR, Span{},
                                       "A compilation unit was created for a completed structure");
                    return comp->services.errors->print_all();
                  }
              }

              break;
            }
          case COMPILATION_TYPE::SIGNATURE: {
              SignatureUnit* const unit = (SignatureUnit*)comp_u;

              if (unit->stage != SIGNATURE_COMP_STAGE::UNTYPED) {
                comp->report_error(ERROR_CODE::INTERNAL_ERROR, Span{},
                                   "A compilation unit was created for a completed signature");
                return comp->services.errors->print_all();
              }

              compile_function_signature_type(comp, &context, &unit->source->sig, unit->sig);
              //Error handling
              if (comp->is_panic()) {
                if (comp->is_fatal()) {
                  return comp->services.errors->print_all();
                }
                else {
                  comp->reset_panic();
                }
              }
              else if (unit->func != nullptr) {
                //Set up new compilation unit for the body
                FunctionUnit* func_unit = comp->new_function_unit(unit->available_names);
                func_unit->source = unit->source;
                func_unit->func = unit->func;

                unit->func->compilation_unit = func_unit;
                default_init(&func_unit->state);

                //Finished
                close_compilation_unit(comp, unit);
              }
              else {
                //DLL functions should have no body
                close_compilation_unit(comp, unit);
              }
              break;
            }

          case COMPILATION_TYPE::FUNCTION: {
              FunctionUnit* const unit = (FunctionUnit*)comp_u;

              switch (unit->stage) {
                case FUNCTION_COMP_STAGE::UNINIT: {
                    compile_function_body_init(comp,
                                               unit->source,
                                               unit->func,
                                               &unit->untyped,
                                               &unit->state);
                    if (comp->is_panic()) {
                      if (comp->is_fatal()) {
                        return comp->services.errors->print_all();
                      }
                      else {
                        comp->reset_panic();
                      }
                    }
                    else {
                      unit->stage = FUNCTION_COMP_STAGE::UNTYPED_BODY;
                      to_comp_i--;//Try it again straight away
                    }

                    break;
                  }
                case FUNCTION_COMP_STAGE::UNTYPED_BODY: {
                    compile_function_body_types(comp,
                                                &context,
                                                unit->source,
                                                unit->func,
                                                &unit->untyped,
                                                &unit->state);

                    if (comp->is_panic()) {
                      if (comp->is_fatal()) {
                        return comp->services.errors->print_all();
                      }
                      else {
                        comp->reset_panic();
                      }
                    }
                    else {
                      unit->stage = FUNCTION_COMP_STAGE::TYPED_BODY;
                      to_comp_i--;//Try it again straight away
                    }
                    break;
                  }
                case FUNCTION_COMP_STAGE::TYPED_BODY: {
                    compile_function_body_code(comp,
                                               &context,
                                               unit->source,
                                               unit->func,
                                               &unit->state);

                    if (comp->is_panic()) {
                      if (comp->is_fatal()) {
                        return comp->services.errors->print_all();
                      }
                      else {
                        comp->reset_panic();
                      }
                    }
                    else {
                      //Finished
                      close_compilation_unit(comp, unit);
                    }
                    break;
                  }
              }
              break;
            }
          case COMPILATION_TYPE::CONST_EXPR: {
              ConstantExprUnit* const unit = (ConstantExprUnit*)comp_u;

              switch (unit->stage) {
                case EXPR_COMP_STAGE::UNTYPED: {
                    TypeHint hint ={};
                    hint.tht = THT::EXACT;
                    hint.type = unit->cast_to;

                    compile_type_of_expression(comp,
                                               &context,
                                               &unit->state,
                                               unit->expr,
                                               (unit->cast_to.is_valid() ?
                                                &hint : nullptr));
                    if (comp->is_panic()) {
                      if (comp->is_fatal()) {
                        return comp->services.errors->print_all();
                      }
                      else {
                        comp->reset_panic();
                      }
                    }
                    else {
                      unit->stage = EXPR_COMP_STAGE::TYPED;
                      to_comp_i--;
                    }
                    break;
                  }
                case EXPR_COMP_STAGE::TYPED: {
                    if (unit->cast_to.is_valid()) {
                      ASSERT(unit->expr->type == unit->cast_to);
                    }

                    //maybe need??
                    //unit->constants = std::move(comp->working_state->constants);

                    CodeBlock block ={};
                    block.label = comp->labels++;


                    //Have to compile to vm
                    BuildOptions options;
                    options.default_calling_convention = &convention_vm;
                    options.endpoint_system            = &system_vm;
                    options.entry_point = comp->build_options.entry_point;
                    options.output_file = comp->build_options.output_file;
                    options.file_name   = comp->build_options.file_name;

                    //Swap forward
                    std::swap(options, comp->build_options);

                    const CallingConvention* convention = comp->build_options.default_calling_convention;

                    State* state = &unit->state;
                    init_state_regs(convention, state);
                    state->return_label = comp->labels++;

                    //Set up new flow
                    size_t new_flow = state->control_flow.new_flow();
                    state->control_flow.current_flow = new_flow;

                    //Compile bytecode
                    state->comptime_compilation = true;

                    const Type type = unit->expr->type;

                    //Do we need to pass as a parameter
                    const bool return_as_ptr = register_passed_as_pointer(type);


                    const Structure* actual_return_type = find_or_make_pointer_structure(comp, &context, type);

                    uint8_t* const_val = comp->constants.alloc_no_construct(type.structure->size);

                    {
                      CallingConvParamIterator param_itr ={
                        convention,
                        0,
                        convention->shadow_space_size
                      };

                      if (return_as_ptr) {
                        state->return_val = advance_runtime_param(state, &param_itr, actual_return_type);

                        UnOpArgs args ={};
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
                                                            unit->expr,
                                                            &state->return_val);

                    if (state->return_val.type == RVT::REGISTER) {
                      state->use_value(state->return_val.reg);
                    }

                    ByteCode::EMIT::JUMP_TO_FIXED(block.code, state->return_label);

                    //Graph colour
                    graph_colour_algo(comp, convention, &block, state);

                    //Backend
                    Program prog ={};
                    compile_backend_single_func(&prog, &block, comp, &system_vm);

                    if (comp->is_panic()) {
                      return comp->services.errors->print_all();
                    }

                    if (comp->print_options.comptime_exec) {
                      Printer printer ={};

                      IO::print("\nAbout to execute Compile Time Code:\n");
                      print_ast_expression(&printer, unit->expr);
                      IO::print("\n\nWhich produced this bytecode:\n");
                      ByteCode::print_bytecode(&vm_regs_name_from_num, stdout, prog.code.ptr, prog.code_size);
                    }

                    //Run the VM
                    if (return_as_ptr) {
                      X64_UNION pass_param = const_val;
                      vm_set_parameters(convention, comp->services.vm, pass_param);
                    }

                    comp->services.vm->errors = comp->services.errors;

                    vm_rum(comp->services.vm, &prog);
                    if (comp->is_panic()) {
                      return comp->services.errors->print_all();
                    }

                    //Get the value back
                    if (return_as_ptr) {
                      if (comp->print_options.comptime_res) {
                        IO::print("\nComptime Res In Bytes: ");
                        print_as_bytes(const_val, type.structure->size);
                        putc('\n', stdout);
                      }
                    }
                    else {
                      //Effectively stored in RAX
                      uint64_t val = comp->services.vm->registers[convention_vm.return_register].b64.reg;
                      x64_to_bytes(val, const_val);

                      if (comp->print_options.comptime_res) {
                        printf("\nComptime Res: %llx\n", val);
                      }
                    }

                    if (unit->cast_to.is_valid()) {
                      uint8_t* res = comp->constants.alloc_no_construct(unit->cast_to.structure->size);
                      do_literal_cast(comp, unit->expr, unit->cast_to, const_val, res);
                      if (comp->is_panic()) {
                        return comp->services.errors->print_all();
                      }

                      comp->constants.free_no_destruct(const_val);
                      const_val = res;

                      unit->expr->type = unit->cast_to;
                    }

                    unit->expr->const_val = const_val;

                    //Swap back
                    std::swap(options, comp->build_options);

                    //Finished
                    close_compilation_unit(comp, unit);
                    break;
                  }
              }
              break;
            }
        }
      }
    }

    if (!comp->is_compiling()) {
      //Wait for there to be no compiling to check unfound deps - best chance they exist

      if (comp->unfound_deps.unfound.size > 0) {
        const size_t num_deps = comp->unfound_deps.unfound.size;
        //Remove units if dependency has been found
        comp->unfound_deps.unfound.remove_if([comp](const UnfoundDep& dep) {
          usize globals = 0;
          usize unknowns = 0;

          if (dep.name.all_names) {
            Array<NamespaceElement> el = comp->services.names->find_all_names(dep.name.namespace_index, dep.name.ident);

            FOR(el, it) {
              unknowns += it->named_element->unknowns;
              globals += it->named_element->globals.size;
            }
          }
          else {
            NamedElement* el = comp->services.names->find_name(dep.name.namespace_index, dep.name.ident);
            if (el != nullptr) {
              unknowns += el->unknowns;
              globals += el->globals.size;
            }
          }

          if (globals != dep.name.num_knowns || unknowns != dep.name.num_unknowns) {
            //Success!
            comp->to_compile.insert(dep.unit_waiting);
            return true;
          }
          else {
            return false;
          }
                                             });

        if (num_deps == comp->unfound_deps.unfound.size) {
          //All dependencies are still unfound
          auto i = comp->unfound_deps.unfound.mut_begin();
          auto end = comp->unfound_deps.unfound.mut_end();

          comp->services.errors->error_messages.reserve_extra(comp->unfound_deps.unfound.size);

          for (; i < end; i++) {
            comp->services.errors->error_messages.insert(std::move(i->as_error));
            free_compilation_unit(comp, i->unit_waiting);
          }
          return comp->services.errors->print_all();
        }
      }
    }
  }

  {
    auto i = comp->dlls_import.begin();
    auto end = comp->dlls_import.end();

    for (; i < end; i++) {
      PEFile single_dll ={};

      load_portable_executable_from_file(comp, i->span, &single_dll, i->name->string);
      if (comp->is_panic()) {
        return comp->services.errors->print_all();
      }

      auto i_el = i->imports.begin();
      auto end_el = end->imports.begin();
      for (; i_el < end_el; i_el++) {
        if (!single_dll.export_table.names.contains(i_el->name)) {
          //Now thats a lot of indirection
          comp->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, i_el->ptr->declaration->sig.signature_span,
                             "Dll '{}' does export anything named '{}'",
                             i->name, i_el->name);
        }
      }

      //Let it emit multiple errors
      if (comp->is_panic()) {
        return comp->services.errors->print_all();
      }
    }
  }

  return ERROR_CODE::NO_ERRORS;
}

void print_compiled_functions(const Compiler* const comp) {
  auto i = comp->functions.begin_const_iter();
  const auto end = comp->functions.end_const_iter();

  for (; i != end; i.next()) {
    const Function* func = i.get();

    printf("FUNCTION %s:\n", func->signature.name->string);
    ByteCode::print_bytecode(comp->build_options.endpoint_system->reg_name_from_num, stdout, func->code_block.code.data, func->code_block.code.size);
    IO::print('\n');
  }
}

//Make sure that t_struct is already created!!!
Type create_named_type(Compiler* comp, const Span& span, NamespaceIndex ns,
                       const InternString* name, const Structure* s) {
  NamedElement* ne = comp->services.names->create_name(ns, name);

  if (ne == nullptr) {
    comp->report_error(ERROR_CODE::NAME_ERROR, span,
                       "Tried to create a type of name '{}', but this name is already used",
                       name);
    return { nullptr, nullptr };
  }

  Global* g = comp->globals.insert();

  ASSERT(comp->services.builtin_types->t_type.is_valid());

  Type type ={ name, s };

  g->decl.meta_flags = (u8)META_FLAG::COMPTIME;
  g->decl.name = name;
  g->decl.type = comp->services.builtin_types->t_type;
  g->constant_value.ptr = comp->constants.alloc_no_construct<Type>();
  g->constant_value.size = sizeof(Type);

  memcpy_ts((Type*)g->constant_value.ptr, 1, &type, 1);

  ne->globals.insert(g);

  return type;
}

void create_named_enum_value(Compiler* comp, const Span& span, NamespaceIndex ns, const EnumValue* v) {
  NamedElement* ne = comp->services.names->create_name(ns, v->name);

  if (ne == nullptr) {
    comp->report_error(ERROR_CODE::NAME_ERROR, span,
                       "Tried to create an enum value of name '{}', but this name is already used",
                       v->name);
    return;
  }

  Global* g = comp->globals.insert();

  ASSERT(v->type.is_valid());

  g->decl.meta_flags = (u8)META_FLAG::COMPTIME;
  g->decl.name = v->name;
  g->decl.type = v->type;
  g->constant_value.ptr = comp->constants.alloc_no_construct<const EnumValue*>();
  g->constant_value.size = sizeof(const EnumValue*);

  memcpy_ts((const EnumValue**)g->constant_value.ptr, 1, &v, 1);

  ne->globals.insert(g);
}

void init_compiler(const APIOptions& options, Compiler* comp) {
  //Setup the built in namespace
  //comp->builtin_namespace = comp->names->builtin_namespace;

  NamespaceIndex builtin_namespace = comp->services.names->builtin_namespace;

  //Init the types
  auto* structures = comp->services.structures;
  auto* builtin_types = comp->services.builtin_types;
  auto* strings = comp->services.strings;

  const auto register_builtin_type = [comp, builtin_namespace](const Structure* s) -> Type {
    Type t = create_named_type(comp, Span{}, builtin_namespace, s->struct_name, s);
    ASSERT(!comp->is_panic());
    return t;
  };

  const auto register_builtin_enum_value = [comp, builtin_namespace](const EnumValue* v) {
    create_named_enum_value(comp, Span{}, builtin_namespace, v);
    ASSERT(!comp->is_panic());
  };

  {
    Structure* const s_type = STRUCTS::new_base_structure(comp,
                                                          strings->intern("type"));
    s_type->type = STRUCTURE_TYPE::TYPE;
    builtin_types->t_type = to_type(s_type);

    /*_ = */ register_builtin_type(s_type);
  }


  {
    const auto base_type = [&](const auto& name, STRUCTURE_TYPE ty, u32 size, Type* t) {
      Structure* s = STRUCTS::new_base_structure(comp, strings->intern(name));
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
      IntegerStructure* s = STRUCTS::new_int_structure(comp, strings->intern(name));
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
    Structure* const s_void_ptr = STRUCTS::new_pointer_structure(comp, builtin_types->t_void);
  }

  {
    EnumStructure* const s_bool = STRUCTS::new_enum_structure(comp, builtin_types->t_u8);
    const InternString* bool_name = strings->intern("bool");

    builtin_types->t_bool = create_named_type(comp, Span{}, builtin_namespace, bool_name, s_bool);
    ASSERT(!comp->is_panic());

    s_bool->enum_values.reserve_extra(2);
    {
      EnumValue* const e_true = STRUCTS::new_enum_value(comp, s_bool, bool_name,
                                                        strings->intern("true"));
      e_true->representation = 1;
      builtin_types->e_true = e_true;
      register_builtin_enum_value(e_true);


      EnumValue* const e_false = STRUCTS::new_enum_value(comp, s_bool, bool_name,
                                                         strings->intern("false"));

      e_true->representation = 0;
      builtin_types->e_false = e_false;
      register_builtin_enum_value(e_false);

    }
    s_bool->enum_values.shrink();
  }


  //Intrinsics
#define MOD(n) comp->intrinsics. ## n = strings->intern(#n);
  INTRINSIC_MODS;
#undef MOD

  //File extensions
  comp->services.file_loader->axl = strings->intern("axl");
  comp->services.file_loader->dll = strings->intern("dll");

  //Systems
  comp->system_names.sys_vm = strings->intern(system_vm.name);
  comp->system_names.sys_x86_64 = strings->intern(system_x86_64.name);

  //Calling Conventionss
  comp->system_names.conv_vm = strings->intern(convention_vm.name);
  comp->system_names.conv_x64 = strings->intern(convention_microsoft_x64.name);
  comp->system_names.conv_stdcall = strings->intern(convention_stdcall.name);

  //Build options
  comp->print_options        = options.print;
  comp->optimization_options = options.optimize;

  if (options.build.file_name == nullptr) {
    comp->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                       "Expected input file name");
    return;
  }

  comp->build_options.file_name = strings->intern(options.build.file_name);

  if (options.build.entry_point == nullptr) {
    comp->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                       "Expected entry point");
    return;
  }

  comp->build_options.entry_point = strings->intern(options.build.entry_point);

  if (options.build.output_file != nullptr) {
    comp->build_options.output_file = strings->intern(options.build.output_file);
  }


  if (options.build.std_lib_folder == nullptr) {
    comp->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                       "Expected std lib folder");
    return;
  }
  comp->build_options.std_lib_folder = strings->intern(options.build.std_lib_folder);

  {
    const InternString* system_name = strings->intern(options.build.system_name);

    if (system_name == comp->system_names.sys_vm) {
      comp->build_options.endpoint_system = &system_vm;
    }
    else if (system_name == comp->system_names.sys_x86_64) {
      comp->build_options.endpoint_system = &system_x86_64;
    }
    else {
      comp->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                         "Invalid system '{}'", system_name);
      return;
    }
  }

  {
    const InternString* conv_name = strings->intern(options.build.default_calling_convention);

    if (conv_name == comp->system_names.conv_vm) {
      comp->build_options.default_calling_convention = &convention_vm;

      if (comp->build_options.endpoint_system != &system_vm) {
        comp->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                           "Invalid system and calling convention combo '{}' and '{}'",
                           comp->build_options.endpoint_system->name, conv_name);
      }
    }
    else if (conv_name == comp->system_names.conv_x64) {
      comp->build_options.default_calling_convention = &convention_microsoft_x64;

      if (comp->build_options.endpoint_system != &system_x86_64) {
        comp->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                           "Invalid system and calling convention combo '{}' and '{}'",
                           comp->build_options.endpoint_system->name, conv_name);
      }
    }
    else if (conv_name == comp->system_names.conv_stdcall) {
      comp->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                         "Invalid system and calling convention combo '{}' and '{}'",
                         comp->build_options.endpoint_system->name, conv_name);
    }
    else {
      comp->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                         "Invalid default calling convention '{}'", conv_name);
    }
  }
}

void build_data_section_for_exec(Program* prog, Compiler* const comp) {

}

void build_data_section_for_vm(Program* prog, Compiler* const comp) {
  InternHashTable<size_t> loaded_strings ={};

  Array<uint8_t> data ={};

  //Writes a string if its not alread written
  const auto write_string = [&](Array<uint8_t>& bytes, const InternString* s) -> size_t {
    if (!loaded_strings.contains(s)) {
      //doesnt contain this string
      //have to write it to data

      bytes.reserve_extra(s->len + 1);

      size_t position = bytes.size;

      memcpy_ts(bytes.data + bytes.size, bytes.capacity - bytes.size, (const uint8_t*)s->string, s->len + 1);
      bytes.size += s->len + 1;


      size_t ret_position = position;

      loaded_strings.insert(s, std::move(position));
      return ret_position;
    }
    else {
      //Already written
      return *loaded_strings.get_val(s);
    }
  };

  //Write a 64 bit number aligned to a 64 bit boundary
  const auto write_u64 = [](Array<uint8_t>& bytes, uint64_t a)->size_t {
    //Align
    const auto align_to = ceil_to_8(bytes.size);
    bytes.insert_uninit(align_to - bytes.size);

    size_t position = bytes.size;
    serialise_to_array(bytes, a);

    return position;
  };

  const auto write_num_bytes = [](Array<uint8_t>& bytes, size_t num_bytes, size_t alignment)->size_t {
    //Align
    const auto align_to = ceil_to_n(bytes.size, alignment);
    bytes.insert_uninit(align_to - bytes.size);

    size_t position = bytes.size;
    bytes.insert_uninit(num_bytes);

    return position;
  };

  //0 is an invalid position
  write_u64(data, 0);

  //Function pointers
  {
    auto i = comp->functions.begin_iter();
    auto end = comp->functions.end_iter();

    for (; i != end; i.next()) {
      Function* func = i.get();

      if (func->is_called && func->func_type == FUNCTION_TYPE::EXTERN) {
        //Reserve the location
        func->data_index = write_u64(data, 0);
      }
    }
  }

  // Globals
  {
    auto i = comp->globals.begin_iter();
    auto end = comp->globals.end_iter();

    for (; i != end; i.next()) {
      Global* glob = i.get();

      if (glob->constant_value.ptr == nullptr) {
        glob->data_index = write_num_bytes(data, glob->decl.type.structure->size, glob->decl.type.structure->alignment);
      }
    }
  }


  Array<uint8_t> imports ={};

  //Dll imports
  {
    auto i_dll = comp->dlls_import.begin();
    auto end_dll = comp->dlls_import.end();

    for (; i_dll < end_dll; i_dll++) {
      //Dont need to write the header unless a function is used
      bool written_header = false;

      auto i_func = i_dll->imports.begin();
      const auto end_func = i_dll->imports.end();

      for (; i_func < end_func; i_func++) {
        Function* func = i_func->ptr;
        const InternString* name = i_func->name;

        if (func->is_called && func->func_type == FUNCTION_TYPE::EXTERN) {
          //Function is used in this dll
          if (!written_header) {
            //Need to put in the name of the dll before any functions
            //Dont know we need it until this point (might not use any of the functions)
            written_header = true;
            size_t name_position = write_string(data, i_dll->name);

            //write header
            write_u64(imports, name_position);
          }

          //Write the function name
          size_t name_position = write_string(data, name);
          write_u64(imports, name_position);

          //The write the data index
          write_u64(imports, func->data_index);
        }
      }

      //Null terminate for functions in a dll
      write_u64(imports, 0);
    }

    //2nd null terminate for dlls in import
    write_u64(imports, 0);
  }

  prog->data_size = data.size;
  prog->data = std::move(data);
  if (imports.size != 8) {
    prog->imports = std::move(imports);
  }
}