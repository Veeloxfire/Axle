#include "type_check.h"
#include "ir.h"
#include "compiler.h"
#include "type.h"

#include <Axle/tracing_wrapper.h>

struct Typer {
  Type return_type = {};

  const Namespace* available_names;
};

//Gets the underlying type of a type-node e.g. if its an array type node it gets the cached array type
//Errors if it was not a type
static Type get_type_value(CompilerThread* const comp_thread, AST_LOCAL a) {
  ASSERT(a.ast->node_type == comp_thread->builtin_types->t_type);
  ASSERT(VC::is_comptime(a.ast->value_category));

// putting in all switch cases even with default
#pragma warning(push)
#pragma warning(disable: 4061)

  switch (a.ast->ast_type) {
    case AST_TYPE::NAMED_TYPE: return downcast_ast<ASTNamedType>(a)->actual_type;
    case AST_TYPE::ARRAY_TYPE: return downcast_ast<ASTArrayType>(a)->actual_type;
    case AST_TYPE::PTR_TYPE: return downcast_ast<ASTPtrType>(a)->actual_type;
    case AST_TYPE::SLICE_TYPE: return downcast_ast<ASTSliceType>(a)->actual_type;
    case AST_TYPE::LAMBDA_TYPE: return downcast_ast<ASTLambdaType>(a)->actual_type;
    case AST_TYPE::TUPLE_TYPE: return downcast_ast<ASTTupleType>(a)->actual_type;
    case AST_TYPE::STRUCT: return downcast_ast<ASTStructBody>(a)->actual_type;
    case AST_TYPE::STRUCT_EXPR: {
        AST_LOCAL body = downcast_ast<ASTStructExpr>(a)->struct_body;
        return downcast_ast<ASTStructBody>(body)->actual_type;
      }
    case AST_TYPE::IDENTIFIER_EXPR: {
        ASTIdentifier* ident = downcast_ast<ASTIdentifier>(a);
        switch (ident->id_type) {
          case ASTIdentifier::LOCAL: {
              const Local* l = ident->local;
              if (!VC::is_comptime(l->decl.value_category)) {
                comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, ident->node_span,
                                          "Runtime types are invalid");
                return {};
              }

              ASSERT(l->decl.type == comp_thread->builtin_types->t_type);
              ASSERT(l->decl.init_value != nullptr);

              Type t = {};
              Axle::memcpy_ts(&t, 1, (const Type*)(l->decl.init_value), 1);
              return t;
            }
          case ASTIdentifier::GLOBAL: {
              const Global* g = ident->global;
              if (!VC::is_comptime(g->decl.value_category)) {
                comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, ident->node_span,
                                          "Runtime types are invalid");
                return {};
              }

              ASSERT(g->decl.type == comp_thread->builtin_types->t_type);
              ASSERT(g->decl.init_value != nullptr);

              Type t = {};
              Axle::memcpy_ts(&t, 1, (const Type*)(g->decl.init_value), 1);
              return t;
            }
        }

        comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, ident->node_span,
                                  "Invalid identifier type");
        return {};
      }

    default: {
        comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, a.ast->node_span,
                                  "Invalid type node: {}", ast_type_string(a.ast->ast_type));
        return {};
      }
  }

// putting in all switch cases even with default
#pragma warning(pop)
}

template<AST_VISIT_STEP>
static void run_step_impl(CompilerGlobals*, CompilerThread*, Typer*, AST_LOCAL);

#define EXPAND_THIS(asttype, name) asttype* const name = downcast_ast<asttype>(this_node);
#define TC_STAGE(stage) template<> void run_step_impl<AST_VISIT_STEP:: stage>([[maybe_unused]] CompilerGlobals* const comp, [[maybe_unused]] CompilerThread* const comp_thread, [[maybe_unused]] Typer* const typer, AST_LOCAL this_node)

TC_STAGE(NAMED_TYPE_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTNamedType, nt);

  nt->value_category = VALUE_CATEGORY::VARIABLE_CONSTANT;
  
  //TODO: local types
  const Global* global = nt->global;

  ASSERT(global != nullptr);
  ASSERT(global->decl.type.is_valid());

  if (global->decl.type.struct_type() != STRUCTURE_TYPE::TYPE) {
    comp_thread->report_error(ERROR_CODE::NAME_ERROR, nt->node_span,
                              "'{}' was not a type",
                              nt->name);
    return;
  }

  ASSERT(global->decl.init_value != nullptr);
  ASSERT(VC::is_comptime(global->decl.value_category));

  nt->actual_type = *(const Type*)global->decl.init_value;
  nt->node_type = comp_thread->builtin_types->t_type;
  return;
}

TC_STAGE(ARRAY_TYPE_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTArrayType, at);
  
  Type base_type = get_type_value(comp_thread, at->base);
  if (comp_thread->is_panic()) {
    return;
  }

  ASSERT(at->expr.ast->node_type == comp_thread->builtin_types->t_u64);
  ASSERT(VC::is_comptime(at->expr.ast->value_category));

  IR::EvalPromise eval = {};
  eval.type = comp_thread->builtin_types->t_u64;
  eval.data = (u8*)&at->array_length;

  IR::eval_ast(comp, comp_thread, at->expr, &eval);
  if (comp_thread->is_panic()) {
    return;
  }

  if (at->array_length == 0) {
    comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, at->expr.ast->node_span,
                              "Length of array must be larger than 0");
    return;
  }

  const Structure* s;
  {
    Axle::AtomicLock<Structures> structures = {};
    Axle::AtomicLock<Axle::StringInterner> strings = {};
    comp->services.get_multiple({ .structures = &structures, .strings = &strings});

    s = find_or_make_array_structure(structures._ptr,
                                     strings._ptr,
                                     base_type, at->array_length);
  }

  at->actual_type = to_type(s);
  at->node_type = comp_thread->builtin_types->t_type;

  return;
}

TC_STAGE(ARRAY_TYPE_DOWN_LEN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTArrayType, at);
  
  ASSERT(at->expr.ast->node_type == comp_thread->builtin_types->t_u64);
  if (!VC::is_comptime(at->expr.ast->value_category)) {
    comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, at->expr.ast->node_span,
                              "array size expression must be a compile time constant");
    return;
  }

  IR::EvalPromise eval = {};
  eval.type = comp_thread->builtin_types->t_u64;
  eval.data = (u8*)&at->array_length;

  IR::eval_ast(comp, comp_thread, at->expr, &eval);
  if (comp_thread->is_panic()) {
    return;
  }

  return;
}

TC_STAGE(ARRAY_TYPE_UP) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTArrayType, at);
  
  at->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  at->base.ast->node_infer_type = comp_thread->builtin_types->t_type;
  at->expr.ast->node_infer_type = comp_thread->builtin_types->t_u64;

  return;
}

TC_STAGE(PTR_TYPE_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTPtrType, ptr);

  Type base_type = get_type_value(comp_thread, ptr->base);
  if (comp_thread->is_panic()) {
    return;
  }

  const Structure* s;
  {
    Axle::AtomicLock<Structures> structures = {};
    Axle::AtomicLock<Axle::StringInterner> strings = {};
    comp->services.get_multiple({ .structures = &structures, .strings = &strings});

    s = find_or_make_pointer_structure(structures._ptr,
                                       strings._ptr,
                                       base_type);
  }

  ptr->actual_type = to_type(s);
  ptr->node_type = comp_thread->builtin_types->t_type;
  return;
}

TC_STAGE(PTR_TYPE_UP) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTPtrType, ptr);
  
  ptr->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  ptr->base.ast->node_infer_type = comp_thread->builtin_types->t_type;

  return;
}

TC_STAGE(SLICE_TYPE_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTSliceType, ptr);

  Type base_type = get_type_value(comp_thread, ptr->base);
  if (comp_thread->is_panic()) {
    return;
  }

  const Structure* s;
  {
    Axle::AtomicLock<Structures> structures = {};
    Axle::AtomicLock<Axle::StringInterner> strings = {};
    comp->services.get_multiple({ .structures = &structures, .strings = &strings});

    s = find_or_make_slice_structure(structures._ptr,
                                       strings._ptr,
                                       base_type);
  }

  ptr->actual_type = to_type(s);
  ptr->node_type = comp_thread->builtin_types->t_type;
  return;
}

TC_STAGE(SLICE_TYPE_UP) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTSliceType, ptr);
  
  ptr->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  ptr->base.ast->node_infer_type = comp_thread->builtin_types->t_type;

  return;
}

TC_STAGE(LAMBDA_TYPE_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTLambdaType, lt);
  Axle::Array<Type> args = {};
  args.reserve_total(lt->args.size);

  for (AST_LOCAL i: lt->args) {
    Type i_type = get_type_value(comp_thread, i);
    if (comp_thread->is_panic()) {
      return;
    }

    args.insert(i_type);
  }

  Type ret = get_type_value(comp_thread, lt->ret);
  if (comp_thread->is_panic()) {
    return;
  }

  const SignatureStructure* s;
  {
    Axle::AtomicLock<Structures> structures = {};
    Axle::AtomicLock<Axle::StringInterner> strings = {};
    comp->services.get_multiple({ .structures = &structures, .strings = &strings});

    s = find_or_make_lambda_structure(structures._ptr,
                                      strings._ptr,
                                      comp_thread->build_options.default_calling_convention,
                                      bake_arr(std::move(args)), ret);
  }

  lt->actual_type = to_type(static_cast<const Structure*>(s));
  lt->node_type = comp_thread->builtin_types->t_type;
  return;
}

TC_STAGE(LAMBDA_TYPE_UP) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTLambdaType, lt);
  
  lt->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  for (AST_LOCAL ty: lt->args) {
    ty.ast->node_infer_type = comp_thread->builtin_types->t_type;
  }

  lt->ret.ast->node_infer_type = comp_thread->builtin_types->t_type;

  return;
}

TC_STAGE(TUPLE_TYPE_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTTupleType, tt);
  Axle::Array<Type> args = {};
  args.reserve_total(tt->types.size);

  for (AST_LOCAL i: tt->types) {
    Type i_type = get_type_value(comp_thread, i);
    if (comp_thread->is_panic()) {
      return;
    }

    args.insert(i_type);
  }

  const Structure* s;
  {
    Axle::AtomicLock<Structures> structures = {};
    Axle::AtomicLock<Axle::StringInterner> strings = {};
    comp->services.get_multiple({ .structures = &structures, .strings = &strings});

    s = find_or_make_tuple_structure(structures._ptr,
                                     strings._ptr,
                                     view_arr(args));
  }

  tt->actual_type = to_type(s);
  tt->node_type = comp_thread->builtin_types->t_type;
  return;
}

TC_STAGE(TUPLE_TYPE_UP) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTTupleType, tt);
  
  tt->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  for (AST_LOCAL ty: tt->types) {
    ty.ast->node_infer_type = comp_thread->builtin_types->t_type;
  }

  return;
}

TC_STAGE(STRUCT_EXPR_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTStructExpr, se);
  
  se->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  ASTStructBody* struct_body = downcast_ast<ASTStructBody>(se->struct_body);

  ASSERT(struct_body->actual_type.is_valid());

  se->node_type = comp_thread->builtin_types->t_type;
  return;
}

TC_STAGE(LAMBDA_EXPR_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTLambdaExpr, le);
  
  ASTLambda* lambda = downcast_ast<ASTLambda>(le->lambda);
  const SignatureStructure* sig_struct = lambda->function->sig_struct;
  ASSERT(sig_struct != nullptr);//should be done in the signature unit

  le->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;
  le->node_type = to_type(sig_struct);

  return;
}

TC_STAGE(FUNCTION_SIGNATURE_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTFuncSig, ast_sig);
  Axle::Array<Type> params = {};
  params.reserve_total(ast_sig->parameters.size);

  for (AST_LOCAL i: ast_sig->parameters) {
    params.insert(i.ast->node_type);
  }

  Type ret_type = get_type_value(comp_thread, ast_sig->return_type);
  if (comp_thread->is_panic()) {
    return;
  }

  const SignatureStructure* sig_struct;
  {
    Axle::AtomicLock<Structures> structures = {};
    Axle::AtomicLock<Axle::StringInterner> strings = {};
    comp->services.get_multiple({ .structures = &structures, .strings = &strings});

    sig_struct = find_or_make_lambda_structure(structures._ptr,
                                               strings._ptr,
                                               ast_sig->convention,
                                               bake_arr(std::move(params)), ret_type);
  }

  ast_sig->ir_function->label = comp->next_function_label(sig_struct, ast_sig->node_span, NULL_ID);
  ast_sig->ir_function->sig_struct = sig_struct;
  
  ast_sig->node_type = comp_thread->builtin_types->t_void;

  return;
}

TC_STAGE(FUNCTION_SIGNATURE_UP) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTFuncSig, ast_sig);

  ast_sig->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  for (AST_LOCAL i: ast_sig->parameters) {
    i.ast->node_infer_type = {};
  }

  ast_sig->return_type.ast->node_infer_type = comp_thread->builtin_types->t_type;

  return;
}

TC_STAGE(LAMBDA_UP) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTLambda, lambda);

  lambda->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  ASTFuncSig* ast_sig = lambda->sig;

  const SignatureStructure* sig_struct = ast_sig->ir_function->sig_struct;
  ASSERT(sig_struct != nullptr);//should be done in the signature unit
  
  for (AST_LOCAL it: ast_sig->parameters) {
    ASSERT(it.ast->ast_type == AST_TYPE::TYPED_NAME);

    ASTTypedName* tn = downcast_ast<ASTTypedName>(it);
    ASSERT(tn != nullptr);
    ASSERT(tn->local_ptr != nullptr);
    ASSERT(tn->local_ptr->decl.type.is_valid());
    ASSERT(tn->local_ptr->decl.value_category == VALUE_CATEGORY::VARIABLE_MUTABLE);
  }

  lambda->node_type = to_type(sig_struct);

  typer->return_type = sig_struct->return_type;
  lambda->body.ast->node_infer_type = {};

  return;
}

TC_STAGE(MEMBER_ACCESS_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTMemberAccessExpr, member);

  AST_LOCAL base = member->expr;
  ASSERT(base.ast->node_type.is_valid());

  STRUCTURE_TYPE struct_type = base.ast->node_type.struct_type();

  if (struct_type == STRUCTURE_TYPE::COMPOSITE) {
    const Type& cmp_t = base.ast->node_type;

    const auto* cs = cmp_t.unchecked_base<CompositeStructure>();

    auto i = cs->elements.begin();
    auto end = cs->elements.end();

    member->node_type.structure = nullptr;//reset

    for (; i < end; i++) {
      if (i->name == member->name) {
        member->node_type = i->type;
        member->offset = i->offset;
        break;
      }
    }

    if (!member->node_type.is_valid()) {
      comp_thread->report_error(ERROR_CODE::NAME_ERROR, member->node_span,
                                "Type '{}' has no member '{}'",
                                cmp_t.name, member->name);
      return;
    }

    same_category(member, base);
  }
  else if (struct_type == STRUCTURE_TYPE::FIXED_ARRAY) {
    const Type& arr_t = base.ast->node_type;

    const ArrayStructure* as = arr_t.unchecked_base<ArrayStructure>();

    if (member->name == comp_thread->important_names.ptr) {
      if (!VC::is_addressable(base.ast->value_category)) {
        comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, member->node_span,
                                  "Cannot get a pointer to a value with no address (this was likely a temporary)");
        return;
      }

      {
        Axle::AtomicLock<Structures> structures = {};
        Axle::AtomicLock<Axle::StringInterner> strings = {};
        comp->services.get_multiple({ .structures = &structures, .strings = &strings});
        member->node_type = to_type(find_or_make_pointer_structure(structures._ptr,
                                                                   strings._ptr,
                                                                   as->base));
      }

      reduce_category(member, base);
    }
    else  if (member->name == comp_thread->important_names.len) {
      member->node_type = comp_thread->builtin_types->t_u64;

      member->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;
    }
    else {
      comp_thread->report_error(ERROR_CODE::NAME_ERROR, member->node_span,
                                "Type '{}' has no member '{}'",
                                arr_t.name, member->name);
      return;
    }
  }
  else if (struct_type == STRUCTURE_TYPE::SLICE) {
    const Type& slice_t = base.ast->node_type;

    const SliceStructure* as = slice_t.unchecked_base<SliceStructure>();

    if (member->name == comp_thread->important_names.ptr) {
      {
        Axle::AtomicLock<Structures> structures = {};
        Axle::AtomicLock<Axle::StringInterner> strings = {};
        comp->services.get_multiple({ .structures = &structures, .strings = &strings});
        member->node_type = to_type(find_or_make_pointer_structure(structures._ptr,
                                                                   strings._ptr,
                                                                   as->base));
      }

      reduce_category(member, base);
    }
    else  if (member->name == comp_thread->important_names.len) {
      member->node_type = comp_thread->builtin_types->t_u64;
      reduce_category(member, base);
    }
    else {
      comp_thread->report_error(ERROR_CODE::NAME_ERROR, member->node_span,
                                "Type '{}' has no member '{}'",
                                slice_t.name, member->name);
      return;
    }
  }
  else {
    comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, member->node_span,
                              "Type '{}' does not have any members (it is not a composite type)",
                              base.ast->node_type.name);
    return;
  }

  ASSERT(member->node_type.is_valid());
  return;
}

TC_STAGE(MEMBER_ACCESS_UP) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTMemberAccessExpr, member);
  
  member->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  AST_LOCAL base = member->expr;
  base.ast->node_infer_type = {};

  return;
}

TC_STAGE(INDEX_EXPR_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTIndexExpr, index_expr);

  AST_LOCAL base = index_expr->expr;
  const usize arg_count = index_expr->arguments.size;

  same_category(index_expr, base);
  for (AST_LOCAL it: index_expr->arguments) {
    reduce_category(index_expr, it);
    ASSERT(it.ast->node_type == comp_thread->builtin_types->t_u64);
  }

  constexpr auto index_or_slice_base = [](const Type& t) {
    if (t.struct_type() == STRUCTURE_TYPE::FIXED_ARRAY) {
      return t.unchecked_base<ArrayStructure>()->base;
    }
    else if (t.struct_type() == STRUCTURE_TYPE::SLICE) {
      return t.unchecked_base<SliceStructure>()->base;
    }
    else {
      INVALID_CODE_PATH("Cannot index or slice this type");
    }
  };

  if(arg_count == 1) {
    if (!TYPE_TESTS::can_index(base.ast->node_type)) {
      comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, base.ast->node_span,
          "Cannot take index of type: {}",
          base.ast->node_type.name);
      return;
    }

    index_expr->node_type = index_or_slice_base(base.ast->node_type);
    return;
  }
  else if(arg_count == 0 || arg_count == 2) {
    if (!TYPE_TESTS::can_slice(base.ast->node_type)) {
      comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, base.ast->node_span,
          "Cannot take slice of type: {}",
          base.ast->node_type.name);
      return;
    }

    const Type t = index_or_slice_base(base.ast->node_type);
   
    {
      Axle::AtomicLock<Structures> structures;
      Axle::AtomicLock<Axle::StringInterner> strings;
      comp->services.get_multiple({ .structures = &structures, .strings = &strings});

      index_expr->node_type = to_type(find_or_make_slice_structure(structures._ptr, strings._ptr, t));
    }
    return;
  }
  else {
    comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, base.ast->node_span,
          "Cannot index with {} arguments", arg_count);
    return;
  }
}

TC_STAGE(INDEX_EXPR_UP) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTIndexExpr, index_expr);

  index_expr->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  AST_LOCAL base = index_expr->expr;

  base.ast->node_infer_type = {};
  for (AST_LOCAL it: index_expr->arguments) {
    it.ast->node_infer_type = comp_thread->builtin_types->t_u64;
  }

  return;
}

TC_STAGE(TUPLE_LIT_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTTupleLitExpr, tup);

  if (tup->node_infer_type.is_valid()) {
    for (AST_LOCAL it: tup->elements) {
      reduce_category(tup, it);
    }

    tup->node_type = tup->node_infer_type;
    return;
  }

  Axle::Array<Type> element_types = {};
  element_types.reserve_total(tup->elements.size);

  for (AST_LOCAL it: tup->elements) {
    reduce_category(tup, it);
    element_types.insert(it.ast->node_type);
  }

  const Structure* ts;
  {
    Axle::AtomicLock<Structures> structures = {};
    Axle::AtomicLock<Axle::StringInterner> strings = {};
    comp->services.get_multiple({ .structures = &structures, .strings = &strings});
    ts = find_or_make_tuple_structure(structures._ptr,
                                      strings._ptr,
                                      view_arr(element_types));
  }

  tup->node_type = to_type(ts);
  return;
}

TC_STAGE(TUPLE_LIT_UP_ELEMENTS) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTTupleLitExpr, tup);

  ASSERT(!tup->node_type.is_valid());

  if (tup->prefix != NULL_AST_NODE) {
    const Type infer_type = tup->node_infer_type;
    
    Type prefix_type = get_type_value(comp_thread, tup->prefix);
    if (comp_thread->is_panic()) {
      return;
    }

    if (infer_type.is_valid() && infer_type != prefix_type) {
      comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, tup->node_span,
                                "Expected type: \"{}\"\nFound type: \"{}\"", infer_type.name, tup->node_type.name);
      return;
    }

    // infer_type is either invalid, or the same
    // so this is safe
    tup->node_infer_type = prefix_type;
  }
  
  const Type infer_type = tup->node_infer_type;

  if (infer_type.is_valid()) {
    STRUCTURE_TYPE st = infer_type.struct_type();

    if (st == STRUCTURE_TYPE::COMPOSITE) {
      const CompositeStructure* cs = infer_type.unchecked_base<CompositeStructure>();

      if (cs->elements.size != tup->elements.size) {
        comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, tup->node_span,
                                  "'{}' expected {} elements. Received: {}",
                                  infer_type.name, cs->elements.size, tup->elements.size);
        return;
      }

      auto cs_i = cs->elements.begin();
      const auto cs_end = cs->elements.end();

      for (AST_LOCAL it: tup->elements) {
        ASSERT(cs_i != cs_end);

        it.ast->node_infer_type = cs_i->type;

        cs_i++;
      }

      ASSERT(cs_i == cs_end);
      return;
    }
    else if (st == STRUCTURE_TYPE::TUPLE) {
      const TupleStructure* ts = infer_type.unchecked_base<TupleStructure>();

      if (ts->elements.size != tup->elements.size) {
        comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, tup->node_span,
                                  "'{}' expected {} elements. Received: {}",
                                  infer_type.name, ts->elements.size, tup->elements.size);
        return;
      }

      auto ts_i = ts->elements.begin();
      const auto ts_end = ts->elements.end();


      for (AST_LOCAL it: tup->elements) {
        ASSERT(ts_i != ts_end);

        it.ast->node_infer_type = ts_i->type;

        ts_i++;
      }

      ASSERT(ts_i == ts_end);
    }
    else {
      comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, tup->node_span,
                                "Could not build composite literal for type: '{}'",
                                infer_type.name);
      return;
    }
  }
  else {
    for (AST_LOCAL it: tup->elements) {
      it.ast->node_infer_type = {};
    }
  }
}

TC_STAGE(TUPLE_LIT_UP_PREFIX) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTTupleLitExpr, tup);

  tup->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  if (tup->prefix != NULL_AST_NODE) {
    tup->prefix.ast->node_infer_type = comp_thread->builtin_types->t_type;
  }
}

TC_STAGE(ARRAY_EXPR_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTArrayExpr, arr_expr);

  if (arr_expr->node_infer_type.is_valid()) {

    for (AST_LOCAL it: arr_expr->elements) {
      reduce_category(arr_expr, it);
    }

    arr_expr->node_type = arr_expr->node_infer_type;
  }

  auto l = arr_expr->elements.begin();
  const auto e = arr_expr->elements.end();

  ASSERT(l < e);

  AST_LOCAL first = *l;
  ++l;
  
  const Type base = first.ast->node_type;
  ASSERT(base.is_valid());

  reduce_category(arr_expr, first);


  for (; l < e; ++l) {
    AST_LOCAL it = *l;

    reduce_category(arr_expr, it);
  }

  const Structure* arr_s;
  {
    Axle::AtomicLock<Structures> structures = {};
    Axle::AtomicLock<Axle::StringInterner> strings = {};
    comp->services.get_multiple({ .structures = &structures, .strings = &strings});

    arr_s = find_or_make_array_structure(structures._ptr,
                                         strings._ptr,
                                         base, arr_expr->elements.size);
  }

  //Create the type
  arr_expr->node_type = to_type(arr_s);
  return;
}

TC_STAGE(ARRAY_EXPR_UP_REST) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTArrayExpr, arr_expr);
  
  if (arr_expr->node_infer_type.is_valid()) {
    return;// already covered
  }

  auto l = arr_expr->elements.begin();
  const auto e = arr_expr->elements.end();

  ASSERT(l < e);

  AST_LOCAL base_test = *l;
  ++l;

  const Type base = base_test.ast->node_type;

  for (; l < e; ++l) {
    AST_LOCAL it = *l;

    it.ast->node_infer_type = base;
  }
}

TC_STAGE(ARRAY_EXPR_UP_FIRST) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTArrayExpr, arr_expr);

  arr_expr->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  const Type infer_type = arr_expr->node_infer_type;

  if (infer_type.is_valid()) {
    if (infer_type.struct_type() != STRUCTURE_TYPE::FIXED_ARRAY) {
      comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, arr_expr->node_span,
                                "Tried to infer an array literal as a non-array type: {}",
                                infer_type.name);
      return;
    }

    const ArrayStructure* as = infer_type.unchecked_base<ArrayStructure>();

    if (as->length != arr_expr->elements.size) {
      comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, arr_expr->node_span,
                                "Array expected size {}. Actual size: {}",
                                as->length, arr_expr->elements.size);
      return;
    }

    Type base = as->base;

    for (AST_LOCAL it: arr_expr->elements) {
      it.ast->node_infer_type = base;
    }
  }
  else {
    if (arr_expr->elements.size > 0) {
      AST_LOCAL base_test = arr_expr->elements[0];
      base_test.ast->node_infer_type = {};
    }
  }
}

TC_STAGE(ASCII_CHAR_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTAsciiChar, a);
  a->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;
  a->node_type = comp_thread->builtin_types->t_ascii;
  return;
}

TC_STAGE(ASCII_STRING_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTAsciiString, ascii);
  ascii->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;
  
  const size_t len = ascii->string->len;

  const Structure* s;
  {
    Axle::AtomicLock<Structures> structures = {};
    Axle::AtomicLock<Axle::StringInterner> strings = {};
    comp->services.get_multiple({ .structures = &structures, .strings = &strings});

    s = find_or_make_array_structure(structures._ptr,
                                     strings._ptr,
                                     comp_thread->builtin_types->t_ascii, len);
  }

  ascii->node_type = to_type(s);
  return;
}

TC_STAGE(NUMBER_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTNumber, num);
  num->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  const Type infer_type = num->node_infer_type;

  if (num->suffix == nullptr) {
    if (infer_type.is_valid()) {
      if (infer_type.struct_type() != STRUCTURE_TYPE::INTEGER) {
        comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, num->node_span,
                                  "Can not infer a number as type '{}'",
                                  infer_type.name);
        return;
      }

      const IntegerStructure* is = infer_type.unchecked_base<IntegerStructure>();

      u64 max_val = 0;
      if (is->is_signed) {
        max_val = Axle::bit_fill_lower<u64>((is->size * 8) - 1);
      }
      else {
        max_val = Axle::bit_fill_lower<u64>(is->size * 8);
      }

      if (num->num_value > max_val) {
        comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, num->node_span,
                                  "'{}' is too small to contain '{}'",
                                  infer_type.name, num->num_value);
        return;
      }

      num->node_type = infer_type;
    }
    else {
      num->node_type = comp_thread->builtin_types->t_u64;
    }
  }
  else {
    if (num->suffix == comp_thread->builtin_types->t_i32.name) {
      num->node_type = comp_thread->builtin_types->t_i32;
    }
    else if (num->suffix == comp_thread->builtin_types->t_u32.name) {
      num->node_type = comp_thread->builtin_types->t_u32;
    }
    else if (num->suffix == comp_thread->builtin_types->t_i64.name) {
      num->node_type = comp_thread->builtin_types->t_i64;
    }
    else if (num->suffix == comp_thread->builtin_types->t_u64.name) {
      num->node_type = comp_thread->builtin_types->t_u64;
    }
    else {
      comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, num->node_span,
                                "Invalid integer literal suffix type '{}'",
                                num->suffix);
      return;
    }
  }

  ASSERT(num->node_type.is_valid());
  return;
}

TC_STAGE(EXPORT_SINGLE_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTExportSingle, es);

  const Type& es_type = es->value.ast->node_type;

  if (es_type.struct_type() != STRUCTURE_TYPE::LAMBDA) {
    comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, es->node_span,
                              "Cannot export a non-function. Found: \"{}\"", es_type.name);
    return;
  }

  es->node_type = comp_thread->builtin_types->t_void;
  return;
}

TC_STAGE(EXPORT_SINGLE_UP) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTExportSingle, es);

  es->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;
  es->value.ast->node_infer_type = {};

  return;
}

TC_STAGE(EXPORT_UP) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTExport, e);
  e->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  for (AST_LOCAL it: e->export_list) {
    it.ast->node_infer_type = {};
  }

  e->node_type = comp_thread->builtin_types->t_void;
  return;
}

TC_STAGE(LINK_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTLink, imp);

  imp->node_type = get_type_value(comp_thread, imp->import_type);
  if (comp_thread->is_panic()) {
    return;
  }

  ASSERT(imp->dynamic);//TODO: only support dynamic links
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
      import_lib.label = comp->next_function_label(t.unchecked_base<SignatureStructure>(), imp->node_span, NULL_ID);

      comp->dyn_lib_imports.insert(std::move(import_lib));
      imp->import_index = comp->dyn_lib_imports.size;
    }

  ALREADY_IMPORTED:
    ASSERT(imp->node_type.is_valid());
  }

  return;
}

TC_STAGE(LINK_UP) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTLink, imp);
  imp->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  imp->import_type.ast->node_infer_type = comp_thread->builtin_types->t_type;

  return;
}

TC_STAGE(IDENTIFIER_EXPR_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTIdentifier, ident);

  if (ident->id_type == ASTIdentifier::LOCAL) {
    Local* local = ident->local;
    ASSERT(local != nullptr);
    ASSERT(local->decl.type.is_valid());

    ident->node_type = local->decl.type;
    ident->value_category = local->decl.value_category;
  }
  else if (ident->id_type == ASTIdentifier::GLOBAL) {
    Global* glob = ident->global;
    ASSERT(glob != nullptr);
    ASSERT(glob->decl.type.is_valid());

    ident->node_type = glob->decl.type;
    ident->value_category = glob->decl.value_category;
  }
  else {
    comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, ident->node_span,
                              "Identifer type was invalid");
    return;
  }

  ASSERT(ident->node_type.is_valid());
  return;
}

TC_STAGE(CAST_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTCastExpr, cast);
  AST_LOCAL expr = cast->expr;
  reduce_category(cast, expr);

  const Type cast_to = get_type_value(comp_thread, cast->type);
  if (comp_thread->is_panic()) {
    return;
  }
  ASSERT(cast_to.is_valid());

  const Type cast_from = cast->expr.ast->node_type;
  ASSERT(cast_from.is_valid());

  cast->node_type = cast_to;

  DEFER(&) { if (!comp_thread->is_panic()) ASSERT(cast->emit != nullptr); };

  switch (cast_from.struct_type()) {
    case STRUCTURE_TYPE::ENUM: {
        const auto* en = cast_from.unchecked_base<EnumStructure>();

        if (cast_to == en->base) {
          cast->emit = CASTS::no_op;
          return;
        }

        break;
      }
    case STRUCTURE_TYPE::FIXED_ARRAY: {
        const auto* from_arr = cast_from.unchecked_base<ArrayStructure>();

        if (cast_to.struct_type() == STRUCTURE_TYPE::POINTER) {
          const auto* to_ptr = cast_to.unchecked_base<PointerStructure>();

          if (from_arr->base == to_ptr->base) {
            if (!VC::is_addressable(cast->expr.ast->value_category)) {
              comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, cast->node_span,
                                        "Value is not addressible");
              return;
            }

            cast->value_category = VALUE_CATEGORY::TEMPORARY_IMMUTABLE;
            cast->emit = CASTS::take_address;
            return;
          }
        }

        break;
      }
    case STRUCTURE_TYPE::INTEGER: {
        //const auto* from_int = cast_from.unchecked_base<IntegerStructure>();

        if (cast_to.struct_type() == STRUCTURE_TYPE::INTEGER) {
          cast->emit = CASTS::int_to_int;
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
            cast->emit = CASTS::no_op;
            return;
          }

          if (TYPE_TESTS::match_sizes(from_ptr->base, to_ptr->base)) {
            cast->emit = CASTS::no_op;
            return;
          }
        }

        break;
      }
    case STRUCTURE_TYPE::SLICE: {
        const auto* from_slice = cast_from.unchecked_base<SliceStructure>();
        if (cast_to.struct_type() == STRUCTURE_TYPE::SLICE) {
          const auto* to_slice = cast_to.unchecked_base<SliceStructure>();

          if (TYPE_TESTS::match_sizes(from_slice->base, to_slice->base)) {
            cast->emit = CASTS::no_op;
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
        return;
      }
    
    case STRUCTURE_TYPE::TYPE:
    case STRUCTURE_TYPE::TUPLE:
    case STRUCTURE_TYPE::LAMBDA:
    case STRUCTURE_TYPE::COMPOSITE: {
        comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, cast->node_span,
                                  "Cannot cast type '{}' to type '{}'",
                                  cast_from.name, cast_to.name);
        return;
      }
  }

  INVALID_CODE_PATH("Invalid STRUCTURE_TYPE");
}

TC_STAGE(CAST_UP_EXPR) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTCastExpr, cast);
  AST_LOCAL expr = cast->expr;

  expr.ast->node_infer_type = {};

  return;
}

TC_STAGE(CAST_UP_TYPE) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTCastExpr, cast);
  cast->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  AST_LOCAL ty = cast->type;

  ty.ast->node_infer_type = comp_thread->builtin_types->t_type;

  return;
}

static void run_un_up_neg_down(CompilerThread* const comp_thread,
                               AST_LOCAL this_node) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTUnaryOperatorExpr, expr);
  ASSERT(expr->op == UNARY_OPERATOR::NEG);

  AST_LOCAL prim = expr->expr;

  expr->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;
  reduce_category(expr, prim);

  Type ty = prim.ast->node_type;

  if (ty.struct_type() != STRUCTURE_TYPE::INTEGER) {
    comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, expr->node_span,
                              "Cannot negate type '{}'. It was not an integer",
                              ty.name);
    return;
  }

  const IntegerStructure* is = ty.unchecked_base<IntegerStructure>();

  if (!is->is_signed) {
    comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, expr->node_span,
                              "Cannot negate type '{}'. It was unsigned!",
                              ty.name);
    return;
  }

  expr->node_type = ty;
  expr->emit_info = { prim.ast->node_type, expr->node_type, UnOpFull::neg_int };
  return;
}

static void run_un_up_addr_down(CompilerGlobals* const comp, AST_LOCAL this_node) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTUnaryOperatorExpr, expr);
  ASSERT(expr->op == UNARY_OPERATOR::ADDRESS);

  AST_LOCAL prim = expr->expr;

  expr->value_category = VALUE_CATEGORY::TEMPORARY_IMMUTABLE;

  const Structure* ptr;
  {
    Axle::AtomicLock<Structures> structures = {};
    Axle::AtomicLock<Axle::StringInterner> strings = {};
    comp->services.get_multiple({ .structures = &structures, .strings = &strings});

    ptr = find_or_make_pointer_structure(structures._ptr, strings._ptr,
                                         expr->expr.ast->node_type);
  }

  expr->node_type = to_type(ptr);
  expr->emit_info = { prim.ast->node_type, expr->node_type, UnOpFull::address };
  return;
}

static void run_un_up_deref_down(CompilerThread* const comp_thread, AST_LOCAL this_node) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTUnaryOperatorExpr, expr);
  ASSERT(expr->op == UNARY_OPERATOR::DEREF);

  AST_LOCAL prim = expr->expr;

  if (prim.ast->node_type.struct_type() == STRUCTURE_TYPE::POINTER) {
    const auto* ptr = prim.ast->node_type.unchecked_base<PointerStructure>();

    if (ptr->is_mut) {
      expr->value_category = VALUE_CATEGORY::VARIABLE_MUTABLE;
    }
    else {
      expr->value_category = VALUE_CATEGORY::VARIABLE_IMMUTABLE;
    }

    expr->node_type = ptr->base;
    expr->emit_info = { prim.ast->node_type, expr->node_type, UnOpFull::deref_ptr };
    return;
  }
  else {
    const Axle::ViewArr<const char> op_string = UNARY_OP_STRING::get(expr->op);

    comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->node_span,
                              "No unary operator '{}' exists for type: '{}'",
                              op_string, prim.ast->node_type.name);

    return;
  }
}

TC_STAGE(UNARY_OPERATOR_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTUnaryOperatorExpr, expr);

  ASSERT(expr->expr.ast->node_type.is_valid());

  switch(expr->op) {
    case UNARY_OPERATOR::NEG: {
      return run_un_up_neg_down(comp_thread, this_node);
    }
    case UNARY_OPERATOR::ADDRESS: {
      return run_un_up_addr_down(comp, this_node);
    }
    case UNARY_OPERATOR::DEREF: {
      return run_un_up_deref_down(comp_thread, this_node);
    }
  }
}

TC_STAGE(UNARY_OPERATOR_UP) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTUnaryOperatorExpr, expr);
  expr->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  AST_LOCAL prim = expr->expr;

  Type infer_type = expr->node_infer_type;

  switch (expr->op) {
    case UNARY_OPERATOR::NEG: {
        if (infer_type.is_valid()) {
          if (infer_type.struct_type() == STRUCTURE_TYPE::INTEGER) {
            const IntegerStructure* is = infer_type.unchecked_base<IntegerStructure>();

            if (is->is_signed) {
              //Can infer

              prim.ast->node_infer_type = infer_type;
              return;
            }
          }
          comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->node_span,
                                    "Cannot infer negation of type: \"{}\"", infer_type.name);
          return;
        }
        else {
          prim.ast->node_infer_type = {};
          return;
        }
      }
    case UNARY_OPERATOR::ADDRESS: {
        //TODO: can we infer anything here??
        prim.ast->node_infer_type = {};
        return;
      }
    case UNARY_OPERATOR::DEREF: {
        //TODO: can we infer anything here
        prim.ast->node_infer_type = {};
        return;
      }
  }

  const Axle::ViewArr<const char> name = UNARY_OP_STRING::get(expr->op);

  comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, expr->node_span,
                            "Type checking is not implemented for unary operator '{}'",
                            name);
  return;
}

void type_check_binary_operator(CompilerGlobals* comp,
                                CompilerThread* comp_thread,
                                AST_LOCAL this_node) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTBinaryOperatorExpr, expr);

  AST_LOCAL left_ast = expr->left;
  AST_LOCAL right_ast = expr->right;

  const Type& left = left_ast.ast->node_type;
  const Type& right = right_ast.ast->node_type;

#pragma warning(push)
#pragma warning(disable: 4061)

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
                      expr->emit_info.op_full = BinOpFull::add_ints;
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
                      expr->emit_info.op_full = BinOpFull::add_int_to_ptr;
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
                  expr->emit_info.op_full = BinOpFull::add_int_to_ptr;
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
                      expr->emit_info.op_full = BinOpFull::sub_ints;
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
                      expr->emit_info.op_full = BinOpFull::sub_ptrs;
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
                      expr->emit_info.op_full = BinOpFull::mul_ints;
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
                      expr->emit_info.op_full = BinOpFull::div_ints;
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
                      expr->emit_info.op_full = BinOpFull::mod_ints;
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
          case STRUCTURE_TYPE::ENUM: {
              if (left == right) {
                const auto* en = left.unchecked_base<EnumStructure>();
                ASSERT(en->base.struct_type() == STRUCTURE_TYPE::INTEGER);

                expr->node_type = comp_thread->builtin_types->t_bool;
                expr->emit_info.main_side = MainSide::LEFT;
                expr->emit_info.dest_type = expr->node_type;
                expr->emit_info.op_full = BinOpFull::eq_ints;
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
                      expr->emit_info.op_full = BinOpFull::eq_ints;
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
                      expr->emit_info.op_full = BinOpFull::eq_ints;
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
          case STRUCTURE_TYPE::ENUM: {
              if (left == right) {
                const auto* en = left.unchecked_base<EnumStructure>();
                ASSERT(en->base.struct_type() == STRUCTURE_TYPE::INTEGER);


                expr->node_type = comp_thread->builtin_types->t_bool;
                expr->emit_info.main_side = MainSide::LEFT;
                expr->emit_info.dest_type = expr->node_type;
                expr->emit_info.op_full = BinOpFull::neq_ints;
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
                      expr->emit_info.op_full = BinOpFull::neq_ints;
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
                      expr->emit_info.op_full = BinOpFull::neq_ints;
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
                    expr->emit_info.op_full = BinOpFull::lesser_ints;

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
                      expr->emit_info.op_full = BinOpFull::greater_ints;

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

    case BINARY_OPERATOR::OR: {
        switch (left.struct_type()) {
          case STRUCTURE_TYPE::ENUM: {
              if (left == right) {
                const auto* en = left.unchecked_base<EnumStructure>();
                ASSERT(en->base.struct_type() == STRUCTURE_TYPE::INTEGER);

                expr->node_type = left;
                expr->emit_info.main_side = MainSide::LEFT;
                expr->emit_info.dest_type = expr->node_type;
                expr->emit_info.op_full = BinOpFull::or_ints;
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
                      expr->emit_info.op_full = BinOpFull::or_ints;
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

    case BINARY_OPERATOR::XOR: {
        switch (left.struct_type()) {
          case STRUCTURE_TYPE::INTEGER: {
              switch (right.struct_type()) {
                case STRUCTURE_TYPE::INTEGER: {
                    if (left == right) {
                      expr->node_type = left;
                      expr->emit_info.main_side = MainSide::LEFT;
                      expr->emit_info.dest_type = expr->node_type;
                      expr->emit_info.op_full = BinOpFull::xor_ints;
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

    case BINARY_OPERATOR::AND: {
        switch (left.struct_type()) {
          case STRUCTURE_TYPE::ENUM: {
              if (left == right) {
                const auto* en = left.unchecked_base<EnumStructure>();
                ASSERT(en->base.struct_type() == STRUCTURE_TYPE::INTEGER);

                expr->node_type = left;
                expr->emit_info.main_side = MainSide::LEFT;
                expr->emit_info.dest_type = expr->node_type;
                expr->emit_info.op_full = BinOpFull::and_ints;
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
                      expr->emit_info.op_full = BinOpFull::and_ints;
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
    case BINARY_OPERATOR::LEFT_SHIFT: {
      break;
#if 0
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
                      expr->emit_info.op_full = BinOpFull::shift_l_64_by_8;
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
#endif
      }

    case BINARY_OPERATOR::RIGHT_SHIFT: {
      break;
#if 0
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
                        expr->emit_info.op_full = BinOpFull::shift_r_i64_by_8;
                      }
                      else {
                        expr->emit_info.op_full = BinOpFull::shift_r_u64_by_8;
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
#endif
      }
  }

#pragma warning(pop)

  const Axle::ViewArr<const char> op_string = BINARY_OP_STRING::get(expr->op);

  comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->node_span,
                            "No binary operator '{}' exists for left type: '{}', and right type: '{}'",
                            op_string, left.name, right.name);
  return;
}

TC_STAGE(BINARY_OPERATOR_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTBinaryOperatorExpr, bin_op);
  AST_LOCAL left = bin_op->left;
  AST_LOCAL right = bin_op->right;

  ASSERT(left.ast->node_type.is_valid());
  ASSERT(right.ast->node_type.is_valid());

  reduce_category(bin_op, left);
  reduce_category(bin_op, right);

  //TODO: constant folding
  //TODO: inference
  return type_check_binary_operator(comp, comp_thread, this_node);
}

TC_STAGE(BINARY_OPERATOR_UP) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTBinaryOperatorExpr, bin_op);

  bin_op->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  AST_LOCAL left = bin_op->left;
  AST_LOCAL right = bin_op->right;

  //TODO: Can we do type inference?

  left.ast->node_infer_type = {};
  right.ast->node_infer_type = {};

  return;
}

TC_STAGE(IMPORT_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTImport, imp);
  AST_LOCAL expr = imp->expr_location;
  ASSERT(expr.ast->node_type.is_valid());

  if (expr.ast->node_type.struct_type() != STRUCTURE_TYPE::FIXED_ARRAY) {
    comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr.ast->node_span,
                              "#{} expression must be a character array\n"
                              "Instead found: {}",
                              comp_thread->intrinsics.import, expr.ast->node_type.name);
    return;
  }

  const auto* array_type = expr.ast->node_type.unchecked_base<ArrayStructure>();

  if (array_type->base != comp_thread->builtin_types->t_ascii) {
    comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr.ast->node_span,
                              "#{} expression must be a character array\n"
                              "Expected base type: {}\n"
                              "Instead found: {}",
                              comp_thread->intrinsics.import, comp_thread->builtin_types->t_ascii.name, array_type->base.name);
    return;
  }

  if (!VC::is_comptime(expr.ast->value_category)) {
    comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr.ast->node_span,
                              "#{} expression must be a compile time constant",
                              comp_thread->intrinsics.import);
    return;
  }

  imp->node_type = comp_thread->builtin_types->t_void;
  return;
}

TC_STAGE(IMPORT_UP) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTImport, imp);
  imp->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;
  
  AST_LOCAL expr = imp->expr_location;
  expr.ast->node_infer_type = {};

  return;
}

static bool test_function_overload(const CallSignature* sig, const SignatureStructure* sig_struct) {
  AXLE_TELEMETRY_FUNCTION();

  //Correct name and number of args
  if (sig->arguments.size == sig_struct->parameter_types.size) {
    auto p_call = sig->arguments.begin();
    const auto end_call = sig->arguments.end();

    auto p_func = sig_struct->parameter_types.begin();

    while (p_call < end_call) {
      if (*p_call == *p_func) {
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

static void check_call_arguments(CompilerThread* const comp_thread,
                                 ASTFunctionCallExpr* const call) {
  AXLE_TELEMETRY_FUNCTION();

  ASSERT(call->sig != nullptr);
  const SignatureStructure* sig_struct = call->sig;

  CallSignature sig = {};

  sig.arguments.reserve_total(call->arguments.size);

  //Load all the types
  for (AST_LOCAL it: call->arguments) {
    ASSERT(it.ast->node_type.is_valid());
    sig.arguments.insert({ it.ast->node_type });
  }

  bool matches = test_function_overload(&sig, sig_struct);
  if (!matches) {
    comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, call->node_span,
                              "Arguments mismatch\nArgs: {}\nDecl: {}", sig, PrintSignatureType{ sig_struct });

  }
}

TC_STAGE(FUNCTION_CALL_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTFunctionCallExpr, call);

  const Type& func_type = call->function.ast->node_type;
  call->sig = func_type.unchecked_base<SignatureStructure>();

  for (AST_LOCAL it: call->arguments) {
    reduce_category(call, it);
  }

  check_call_arguments(comp_thread, call);
  if (comp_thread->is_panic()) {
    return;
  }

  const auto* sig = call->sig;
  ASSERT(sig);

  const size_t size = sig->parameter_types.size;

  if (call->arguments.size != size) {
    comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, call->node_span,
                              "Compiler linked a function with {} parameters for a call with {} arguments!",
                              size, call->arguments.size);
    return;
  }

  IR::GlobalLabel label = IR::NULL_GLOBAL_LABEL;
  {
    IR::EvalPromise eval = {};
    eval.type = func_type;
    eval.data = (u8*)&label;

    IR::eval_ast(comp, comp_thread, call->function, &eval);
    if (comp_thread->is_panic()) {
      return;
    }
  }


  ASSERT(label != IR::NULL_GLOBAL_LABEL);
  call->label = label;

  //Last thing to do it set return type
  call->node_type = sig->return_type;

  return;
}

TC_STAGE(FUNCTION_CALL_UP_ARGS) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTFunctionCallExpr, call);

  const Type& func_type = call->function.ast->node_type;
  if (func_type.struct_type() != STRUCTURE_TYPE::LAMBDA) {
    comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, call->function.ast->node_span,
                              "Attempted to call a non-function. Found type {}", func_type.name);
    return;
  }

  ASSERT(VC::is_comptime(call->function.ast->value_category));

  for (AST_LOCAL it: call->arguments) {
    //TODO: try to infer arguments
    it.ast->node_infer_type = {};
  }
}

TC_STAGE(FUNCTION_CALL_UP_FUNCTION) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTFunctionCallExpr, call);

  call->value_category = VALUE_CATEGORY::TEMPORARY_IMMUTABLE;

  call->function.ast->node_infer_type = {};

  return;
}

TC_STAGE(ASSIGN_UP_RIGHT) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTAssign, assign);

  AST_LOCAL assign_to = assign->assign_to;
  ASSERT(assign_to.ast->node_type.is_valid());
  ASSERT(!assign->value.ast->node_type.is_valid());

  if (!VC::is_mutable(assign_to.ast->value_category)) {
    comp_thread->report_error(ERROR_CODE::CONST_ERROR, assign_to.ast->node_span,
                              "Cannot assign to \"const\" expression");
    return;
  }

  assign->value.ast->node_infer_type = assign_to.ast->node_type;

  assign->node_type = comp_thread->builtin_types->t_void;
}

TC_STAGE(ASSIGN_UP_LEFT) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTAssign, assign);
  
  AST_LOCAL assign_to = assign->assign_to;
  
  ASSERT(!assign_to.ast->node_type.is_valid());
  assign_to.ast->node_infer_type = {};

  return;
}

TC_STAGE(DECL_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTDecl, ast_decl);

  AST_LOCAL decl_expr = ast_decl->expr;

  if (!ast_decl->type.is_valid()) {
    ASSERT(decl_expr != NULL_AST_NODE);
    ast_decl->type = ast_decl->expr.ast->node_type;
  }

  ASSERT(ast_decl->type.is_valid());

  if (ast_decl->compile_time_const
      && (decl_expr == NULL_AST_NODE || !VC::is_comptime(decl_expr.ast->value_category))) {
    comp_thread->report_error(ERROR_CODE::CONST_ERROR, ast_decl->node_span,
                              "Compile time declaration '{}' must be initialized by a compile time expression",
                              ast_decl->name);
    return;
  }

  Decl* decl;

  switch (ast_decl->decl_type) {
    case ASTDecl::TYPE::LOCAL: {
        Local* const loc = ast_decl->local_ptr;
        decl = &loc->decl;
        break;
      }
    case ASTDecl::TYPE::GLOBAL: {
        ASSERT(ast_decl->global_ptr != nullptr);
        Global* global = ast_decl->global_ptr;

        decl = &global->decl;
        break;
      }
    default: {
        INVALID_CODE_PATH("Declaration was somehow not a global or local variable ...");
      }
  }

  decl->type = ast_decl->type;

  if (ast_decl->compile_time_const) {
    decl->value_category = VALUE_CATEGORY::VARIABLE_CONSTANT;
  }
  else {
    decl->value_category = VALUE_CATEGORY::VARIABLE_MUTABLE;
  }

  if (VC::is_comptime(decl->value_category) || VC::is_comptime(decl_expr.ast->value_category)) {
    if (!VC::is_comptime(decl_expr.ast->value_category)) {
      comp_thread->report_error(ERROR_CODE::CONST_ERROR, ast_decl->node_span,
                                "Cannot initialize a compile time constant with "
                                "a non compile time constant value");
      return;
    }

    IR::EvalPromise p = {};
    p.data = nullptr;
    p.type = decl->type;

    IR::eval_ast(comp, comp_thread, decl_expr, &p);
    if (comp_thread->is_panic()) {
      return;
    }

    ASSERT(p.data != nullptr);
    ASSERT(p.type == decl->type);

    decl->init_value = p.data;
  }

  ASSERT(VC::is_variable(decl->value_category));

  ast_decl->value_category = decl->value_category;
  ast_decl->node_type = comp_thread->builtin_types->t_void;
  return;
}

TC_STAGE(DECL_UP_EXPR) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTDecl, decl);

  Type expr_type = {};
  if (decl->type_ast != NULL_AST_NODE) {
    decl->type = get_type_value(comp_thread, decl->type_ast);
    if (comp_thread->is_panic()) {
      return;
    }

    expr_type = decl->type;
  }
  else {
    decl->type = {};
  }

  if (decl->expr != NULL_AST_NODE) {
    decl->expr.ast->node_infer_type = expr_type;
  }

  return;
}

TC_STAGE(DECL_UP_TYPE) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTDecl, decl);

  if (decl->type_ast != NULL_AST_NODE) {
    decl->type_ast.ast->node_infer_type = comp_thread->builtin_types->t_type;
  }
}

TC_STAGE(IF_ELSE_UP) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTIfElse, if_else);
  
  if_else->condition.ast->node_infer_type = comp_thread->builtin_types->t_bool;
  if_else->if_statement.ast->node_infer_type = {};

  if (if_else->else_statement != NULL_AST_NODE) {
    if_else->else_statement.ast->node_infer_type = {};
  }

  if_else->node_type = comp_thread->builtin_types->t_void;
  return;
}

TC_STAGE(WHILE_UP) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTWhile, while_loop);

  while_loop->condition.ast->node_infer_type = comp_thread->builtin_types->t_bool;
  while_loop->statement.ast->node_infer_type = {};

  while_loop->node_type = comp_thread->builtin_types->t_void;
  return;
}

TC_STAGE(BLOCK_UP) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTBlock, block);
  
  for (AST_LOCAL it: block->block) {
    it.ast->node_infer_type = {};
  }

  block->node_type = comp_thread->builtin_types->t_void;
  return;
}

TC_STAGE(RETURN_UP) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTReturn, ret);
  
  ASSERT(typer->return_type.is_valid());

  if (ret->expr != NULL_AST_NODE) {
    ret->expr.ast->node_infer_type = typer->return_type;

    ret->node_type = comp_thread->builtin_types->t_void;
    return;
  }
  else {
    if (typer->return_type != comp_thread->builtin_types->t_void) {
      comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, ret->node_span,
                                "Return type was {}, yet the expression was empty\n",
                                typer->return_type.name);
      return;
    }
    ret->node_type = comp_thread->builtin_types->t_void;
    return;
  }
}

TC_STAGE(STRUCT_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTStructBody, body);

  //Build the new structure
  {
    CompositeStructure* cmp_s;
    {
      Axle::AtomicLock<Structures> structures = {};
      Axle::AtomicLock<Axle::StringInterner> strings = {};
      comp->services.get_multiple({ .structures = &structures, .strings = &strings});
      cmp_s = STRUCTS::new_composite_structure(structures._ptr,
                                               strings._ptr);
    }
    uint32_t current_size = 0;
    uint32_t current_alignment = 0;

    Axle::Array<StructElement> elements = {};
    elements.reserve_total(body->elements.size);

    for (AST_LOCAL it: body->elements) {
      ASTTypedName* tn = downcast_ast<ASTTypedName>(it);

      elements.insert_uninit(1);
      auto* b = elements.back();

      b->type = get_type_value(comp_thread, tn->type);
      if (comp_thread->is_panic()) {
        return;
      }
      b->name = tn->name;
      b->offset = current_size;

      uint32_t this_align = b->type.structure->alignment;

      current_size = (uint32_t)Axle::ceil_to_n(current_size, this_align);
      current_size += b->type.structure->size;

      current_alignment = Axle::larger(this_align, current_alignment);
    }

    cmp_s->elements = bake_arr(std::move(elements));
    cmp_s->declaration = body;
    cmp_s->size = current_size;
    cmp_s->alignment = current_alignment;

    body->actual_type = to_type(cmp_s);
  }

  body->node_type = comp_thread->builtin_types->t_type;

  return;
}

TC_STAGE(STRUCT_UP) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTStructBody, body);

  for (AST_LOCAL it: body->elements) {
    it.ast->node_infer_type = {};
  }
}

TC_STAGE(TYPED_NAME_DOWN) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTTypedName, name);

  name->node_type = get_type_value(comp_thread, name->type);
  if (comp_thread->is_panic()) {
    return;
  }

  ASSERT(name->node_type.is_valid());

  Local* loc = name->local_ptr;

  ASSERT(loc->decl.name == name->name);
  loc->decl.type = name->node_type;
  loc->decl.value_category = VALUE_CATEGORY::VARIABLE_MUTABLE;
  loc->decl.init_value = nullptr;

  return;
}

TC_STAGE(TYPED_NAME_UP) {
  AXLE_TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTTypedName, name);
  
  name->type.ast->node_infer_type = comp_thread->builtin_types->t_type;

  return;
}

static void run_step(CompilerGlobals* const comp,
                     CompilerThread* const comp_thread,
                     Typer* const typer,
                     AST_LOCAL node, AST_VISIT_STEP step) {
  AXLE_TELEMETRY_FUNCTION();

  switch (step) {
#define MOD(s, a, ...) case AST_VISIT_STEP:: s : \
    static_assert(AST_TYPE:: a == ast_visit_step_ast_type(AST_VISIT_STEP:: s)); \
    ASSERT(node.ast->ast_type == AST_TYPE:: a); \
    ASSERT(node.ast->ast_type == ast_visit_step_ast_type(AST_VISIT_STEP:: s)); \
    return run_step_impl<AST_VISIT_STEP:: s>(comp, comp_thread, typer, node);
      AST_VISIT_STEP_MOD;
#undef MOD
  }

  INVALID_CODE_PATH("Invalid visit step");
}

void TC::type_check_ast(CompilerGlobals* const comp,
                        CompilerThread* const comp_thread,
                        TC::TypeCheckContext& context) {
  AXLE_TELEMETRY_FUNCTION();

  ASSERT(!comp_thread->is_panic());
  ASSERT(!comp_thread->is_depends());
  ASSERT(!context.finished());

  usize& index = context.next_index;
  const Axle::ViewArr<const AstVisit> visit_arr = context.visit_arr;
  const Namespace* ns = context.ns;

  Typer typer = {};
  typer.available_names = ns;
  

  for (; index < visit_arr.size; ++index) {
    const AstVisit& v = visit_arr[index];

    ASSERT(!v.node.ast->node_type.is_valid());// this is what signals completion

    run_step(comp, comp_thread, &typer, v.node, v.step);
    if (comp_thread->is_panic() || comp_thread->is_depends()) {
      return;
    }

    AST_LOCAL n = v.node;
    const Type& n_type = n.ast->node_type;
    const Type& n_infer = n.ast->node_infer_type;
    if (n_type.is_valid() && n_infer.is_valid()
        && n_infer != n_type) {
      const Type& expected = n.ast->node_infer_type;
      const Type& actual = n.ast->node_type;
      comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, n.ast->node_span,
                                "Expected type: {}. Actual type: {}", expected.name, actual.name);
      return;
    }
  }

  for (const AstVisit v: visit_arr) {
    ASSERT(v.node.ast->node_type.is_valid());// this is what signals completion
  }

  ASSERT(context.finished());
}
