#include "type_check.h"
#include "ir.h"
#include "compiler.h"
#include "type.h"

#ifdef AXLE_TRACING
#include <Tracer/trace.h>
#endif

struct Typer {
  Type return_type = {};

  Namespace* available_names;
};

//Gets the underlying type of a type-node e.g. if its an array type node it gets the cached array type
//Errors if it was not a type
static Type get_type_value(CompilerThread* const comp_thread, AST_LOCAL a) {
  ASSERT(a->node_type == comp_thread->builtin_types->t_type);
  ASSERT(VC::is_comptime(a->value_category));

// putting in all switch cases even with default
#pragma warning(push)
#pragma warning(disable: 4061)

  switch (a->ast_type) {
    case AST_TYPE::NAMED_TYPE: return static_cast<ASTNamedType*>(a)->actual_type;
    case AST_TYPE::ARRAY_TYPE: return static_cast<ASTArrayType*>(a)->actual_type;
    case AST_TYPE::PTR_TYPE: return static_cast<ASTPtrType*>(a)->actual_type;
    case AST_TYPE::SLICE_TYPE: return static_cast<ASTSliceType*>(a)->actual_type;
    case AST_TYPE::LAMBDA_TYPE: return static_cast<ASTLambdaType*>(a)->actual_type;
    case AST_TYPE::TUPLE_TYPE: return static_cast<ASTTupleType*>(a)->actual_type;
    case AST_TYPE::STRUCT: return static_cast<ASTStructBody*>(a)->actual_type;
    case AST_TYPE::STRUCT_EXPR: {
        AST_LOCAL body = static_cast<ASTStructExpr*>(a)->struct_body;
        ASSERT(body->ast_type == AST_TYPE::STRUCT);
        return static_cast<ASTStructBody*>(body)->actual_type;
      }
    case AST_TYPE::IDENTIFIER_EXPR: {
        ASTIdentifier* ident = downcast_ast<ASTIdentifier>(a);
        switch (ident->id_type) {
          case ASTIdentifier::LOCAL: {
              const Local* l = ident->local;
              if (!VC::is_comptime(l->decl.value_category)) {
                comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, a->node_span,
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
                comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, a->node_span,
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

        comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, a->node_span,
                                  "Invalid identifier type");
        return {};
      }

    default: {
        comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, a->node_span,
                                  "Invalid type node: {}", ast_type_string(a->ast_type));
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
  TELEMETRY_FUNCTION();
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
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTArrayType, at);
  
  Type base_type = get_type_value(comp_thread, at->base);
  if (comp_thread->is_panic()) {
    return;
  }

  ASSERT(at->expr->node_type == comp_thread->builtin_types->t_u64);
  ASSERT(VC::is_comptime(at->expr->value_category));

  IR::EvalPromise eval = {};
  eval.type = comp_thread->builtin_types->t_u64;
  eval.data = (u8*)&at->array_length;

  IR::eval_ast(comp, comp_thread, at->expr, &eval);
  if (comp_thread->is_panic()) {
    return;
  }

  if (at->array_length == 0) {
    comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, at->expr->node_span,
                              "Length of array must be larger than 0");
    return;
  }

  const Structure* s;
  {
    Axle::AtomicLock<Structures> structures = {};
    Axle::AtomicLock<Axle::StringInterner> strings = {};
    comp->services.get_multiple(&structures, &strings);

    s = find_or_make_array_structure(structures._ptr,
                                     strings._ptr,
                                     base_type, at->array_length);
  }

  at->actual_type = to_type(s);
  at->node_type = comp_thread->builtin_types->t_type;

  return;
}

TC_STAGE(ARRAY_TYPE_UP) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTArrayType, at);
  
  at->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  at->base->node_infer_type = comp_thread->builtin_types->t_type;
  at->expr->node_infer_type = comp_thread->builtin_types->t_u64;

  return;
}

TC_STAGE(PTR_TYPE_DOWN) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTPtrType, ptr);

  Type base_type = get_type_value(comp_thread, ptr->base);
  if (comp_thread->is_panic()) {
    return;
  }

  const Structure* s;
  {
    Axle::AtomicLock<Structures> structures = {};
    Axle::AtomicLock<Axle::StringInterner> strings = {};
    comp->services.get_multiple(&structures, &strings);

    s = find_or_make_pointer_structure(structures._ptr,
                                       strings._ptr,
                                       base_type);
  }

  ptr->actual_type = to_type(s);
  ptr->node_type = comp_thread->builtin_types->t_type;
  return;
}

TC_STAGE(PTR_TYPE_UP) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTPtrType, ptr);
  
  ptr->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  ptr->base->node_infer_type = comp_thread->builtin_types->t_type;

  return;
}

TC_STAGE(SLICE_TYPE_DOWN) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTSliceType, ptr);

  Type base_type = get_type_value(comp_thread, ptr->base);
  if (comp_thread->is_panic()) {
    return;
  }

  const Structure* s;
  {
    Axle::AtomicLock<Structures> structures = {};
    Axle::AtomicLock<Axle::StringInterner> strings = {};
    comp->services.get_multiple(&structures, &strings);

    s = find_or_make_slice_structure(structures._ptr,
                                       strings._ptr,
                                       base_type);
  }

  ptr->actual_type = to_type(s);
  ptr->node_type = comp_thread->builtin_types->t_type;
  return;
}

TC_STAGE(SLICE_TYPE_UP) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTSliceType, ptr);
  
  ptr->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  ptr->base->node_infer_type = comp_thread->builtin_types->t_type;

  return;
}

TC_STAGE(LAMBDA_TYPE_DOWN) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTLambdaType, lt);
  Axle::Array<Type> args = {};
  args.reserve_total(lt->args.count);

  FOR_AST(lt->args, i) {
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
    comp->services.get_multiple(&structures, &strings);
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
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTLambdaType, lt);
  
  lt->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  FOR_AST(lt->args, ty) {
    ty->node_infer_type = comp_thread->builtin_types->t_type;
  }

  lt->ret->node_infer_type = comp_thread->builtin_types->t_type;

  return;
}

TC_STAGE(TUPLE_TYPE_DOWN) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTTupleType, tt);
  Axle::Array<Type> args = {};
  args.reserve_total(tt->types.count);

  FOR_AST(tt->types, i) {
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
    comp->services.get_multiple(&structures, &strings);

    s = find_or_make_tuple_structure(structures._ptr,
                                     strings._ptr,
                                     view_arr(args));
  }

  tt->actual_type = to_type(s);
  tt->node_type = comp_thread->builtin_types->t_type;
  return;
}

TC_STAGE(TUPLE_TYPE_UP) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTTupleType, tt);
  
  tt->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  FOR_AST(tt->types, ty) {
    ty->node_infer_type = comp_thread->builtin_types->t_type;
  }

  return;
}

TC_STAGE(STRUCT_EXPR_DOWN) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTStructExpr, se);
  
  se->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  ASTStructBody* struct_body = downcast_ast<ASTStructBody>(se->struct_body);

  ASSERT(struct_body->actual_type.is_valid());

  se->node_type = comp_thread->builtin_types->t_type;
  return;
}

TC_STAGE(LAMBDA_EXPR_DOWN) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTLambdaExpr, le);
  
  ASTLambda* lambda = downcast_ast<ASTLambda>(le->lambda);
  ASTFuncSig* ast_sig = downcast_ast<ASTFuncSig>(lambda->sig);

  const SignatureStructure* sig_struct = ast_sig->sig->sig_struct;
  ASSERT(sig_struct != nullptr);//should be done in the signature unit

  le->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;
  le->node_type = to_type(sig_struct);

  return;
}

TC_STAGE(FUNCTION_SIGNATURE_DOWN) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTFuncSig, ast_sig);
  Axle::Array<Type> params = {};
  params.reserve_total(ast_sig->parameters.count);

  FOR_AST(ast_sig->parameters, i) {
    params.insert(i->node_type);
  }

  Type ret_type = get_type_value(comp_thread, ast_sig->return_type);
  if (comp_thread->is_panic()) {
    return;
  }

  const SignatureStructure* sig_struct;
  {
    Axle::AtomicLock<Structures> structures = {};
    Axle::AtomicLock<Axle::StringInterner> strings = {};
    comp->services.get_multiple(&structures, &strings);

    sig_struct = find_or_make_lambda_structure(structures._ptr,
                                               strings._ptr,
                                               ast_sig->convention,
                                               bake_arr(std::move(params)), ret_type);
  }

  ast_sig->sig->label = comp->next_function_label(sig_struct, ast_sig->node_span);

  ast_sig->node_type = comp_thread->builtin_types->t_void;
  ast_sig->sig->sig_struct = sig_struct;

  return;
}

TC_STAGE(FUNCTION_SIGNATURE_UP) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTFuncSig, ast_sig);

  ast_sig->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  FOR_AST(ast_sig->parameters, i) {
    i->node_infer_type = {};
  }

  ast_sig->return_type->node_infer_type = comp_thread->builtin_types->t_type;

  return;
}

TC_STAGE(LAMBDA_UP) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTLambda, lambda);

  lambda->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  ASTFuncSig* ast_sig = downcast_ast<ASTFuncSig>(lambda->sig);

  const SignatureStructure* sig_struct = ast_sig->sig->sig_struct;
  ASSERT(sig_struct != nullptr);//should be done in the signature unit
  
  FOR_AST(ast_sig->parameters, it) {
    ASSERT(it->ast_type == AST_TYPE::TYPED_NAME);

    ASTTypedName* tn = downcast_ast<ASTTypedName>(it);
    ASSERT(tn != nullptr);
    ASSERT(tn->local_ptr != nullptr);
    ASSERT(tn->local_ptr->decl.type.is_valid());
    ASSERT(tn->local_ptr->decl.value_category == VALUE_CATEGORY::VARIABLE_MUTABLE);
  }

  lambda->node_type = to_type(sig_struct);

  typer->return_type = sig_struct->return_type;
  lambda->body->node_infer_type = {};

  return;
}

TC_STAGE(MEMBER_ACCESS_DOWN) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTMemberAccessExpr, member);

  AST_LOCAL base = member->expr;
  ASSERT(base->node_type.is_valid());

  STRUCTURE_TYPE struct_type = base->node_type.struct_type();

  if (struct_type == STRUCTURE_TYPE::COMPOSITE) {
    const Type& cmp_t = base->node_type;

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
    const Type& arr_t = base->node_type;

    const ArrayStructure* as = arr_t.unchecked_base<ArrayStructure>();

    if (member->name == comp_thread->important_names.ptr) {
      if (!VC::is_addressable(base->value_category)) {
        comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, member->node_span,
                                  "Cannot get a pointer to a value with no address (this was likely a temporary)");
        return;
      }

      {
        Axle::AtomicLock<Structures> structures = {};
        Axle::AtomicLock<Axle::StringInterner> strings = {};
        comp->services.get_multiple(&structures, &strings);
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
    const Type& slice_t = base->node_type;

    const SliceStructure* as = slice_t.unchecked_base<SliceStructure>();

    if (member->name == comp_thread->important_names.ptr) {
      {
        Axle::AtomicLock<Structures> structures = {};
        Axle::AtomicLock<Axle::StringInterner> strings = {};
        comp->services.get_multiple(&structures, &strings);
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
                              base->node_type.name);
    return;
  }

  ASSERT(member->node_type.is_valid());
  return;
}

TC_STAGE(MEMBER_ACCESS_UP) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTMemberAccessExpr, member);
  
  member->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  AST_LOCAL base = member->expr;
  base->node_infer_type = {};

  return;
}

TC_STAGE(INDEX_EXPR_DOWN) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTIndexExpr, index_expr);

  AST_LOCAL base = index_expr->expr;
  const usize arg_count = index_expr->arguments.count;

  same_category(index_expr, base);
  FOR_AST(index_expr->arguments, it) {
    reduce_category(index_expr, it);
    ASSERT(it->node_type == comp_thread->builtin_types->t_u64);
  }

  constexpr auto index_or_slice_base = [](const Type& t) {
    struct V {
      Type operator()(const Structure*) const {
        INVALID_CODE_PATH("Cannot index or slice this type");
      }
      Type operator()(const ArrayStructure* as) const {
        return as->base;
      }
      Type operator()(const SliceStructure* s) const {
        return s->base;
      }
    };
    return visit_types(V{}, t.structure);
  };

  if(arg_count == 1) {
    if (!TYPE_TESTS::can_index(base->node_type)) {
      comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, base->node_span,
          "Cannot take index of type: {}",
          base->node_type.name);
      return;
    }

    index_expr->node_type = index_or_slice_base(base->node_type);
    return;
  }
  else if(arg_count == 0 || arg_count == 2) {
    if (!TYPE_TESTS::can_slice(base->node_type)) {
      comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, base->node_span,
          "Cannot take slice of type: {}",
          base->node_type.name);
      return;
    }

    const Type t = index_or_slice_base(base->node_type);
   
    {
      Axle::AtomicLock<Structures> structures;
      Axle::AtomicLock<Axle::StringInterner> strings;
      comp->services.get_multiple(&structures, &strings);

      index_expr->node_type = to_type(find_or_make_slice_structure(structures._ptr, strings._ptr, t));
    }
    return;
  }
  else {
    comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, base->node_span,
          "Cannot index with {} arguments", arg_count);
    return;
  }
}

TC_STAGE(INDEX_EXPR_UP) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTIndexExpr, index_expr);

  index_expr->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  AST_LOCAL base = index_expr->expr;

  base->node_infer_type = {};
  FOR_AST(index_expr->arguments, it) {
    it->node_infer_type = comp_thread->builtin_types->t_u64;
  }

  return;
}

TC_STAGE(TUPLE_LIT_DOWN) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTTupleLitExpr, tup);

  if (tup->node_infer_type.is_valid()) {
    FOR_AST(tup->elements, it) {
      reduce_category(tup, it);
    }

    tup->node_type = tup->node_infer_type;
    return;
  }

  Axle::Array<Type> element_types = {};
  element_types.reserve_total(tup->elements.count);

  FOR_AST(tup->elements, it) {
    reduce_category(tup, it);
    element_types.insert(it->node_type);
  }

  const Structure* ts;
  {
    Axle::AtomicLock<Structures> structures = {};
    Axle::AtomicLock<Axle::StringInterner> strings = {};
    comp->services.get_multiple(&structures, &strings);
    ts = find_or_make_tuple_structure(structures._ptr,
                                      strings._ptr,
                                      view_arr(element_types));
  }

  tup->node_type = to_type(ts);
  return;
}

TC_STAGE(TUPLE_LIT_UP_ELEMENTS) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTTupleLitExpr, tup);

  ASSERT(!tup->node_type.is_valid());

  if (tup->prefix) {
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

      if (cs->elements.size != tup->elements.count) {
        comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, tup->node_span,
                                  "'{}' expected {} elements. Received: {}",
                                  infer_type.name, cs->elements.size, tup->elements.count);
        return;
      }

      auto cs_i = cs->elements.begin();
      const auto cs_end = cs->elements.end();

      FOR_AST(tup->elements, it) {
        ASSERT(cs_i != cs_end);

        it->node_infer_type = cs_i->type;

        cs_i++;
      }

      ASSERT(cs_i == cs_end);
      return;
    }
    else if (st == STRUCTURE_TYPE::TUPLE) {
      const TupleStructure* ts = infer_type.unchecked_base<TupleStructure>();

      if (ts->elements.size != tup->elements.count) {
        comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, tup->node_span,
                                  "'{}' expected {} elements. Received: {}",
                                  infer_type.name, ts->elements.size, tup->elements.count);
        return;
      }

      auto ts_i = ts->elements.begin();
      const auto ts_end = ts->elements.end();


      FOR_AST(tup->elements, it) {
        ASSERT(ts_i != ts_end);

        it->node_infer_type = ts_i->type;

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
    FOR_AST(tup->elements, it) {
      it->node_infer_type = {};
    }
  }
}

TC_STAGE(TUPLE_LIT_UP_PREFIX) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTTupleLitExpr, tup);

  tup->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  if (tup->prefix != nullptr) {
    tup->prefix->node_infer_type = comp_thread->builtin_types->t_type;
  }
}

TC_STAGE(ARRAY_EXPR_DOWN) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTArrayExpr, arr_expr);

  if (arr_expr->node_infer_type.is_valid()) {

    FOR_AST(arr_expr->elements, it) {
      reduce_category(arr_expr, it);
    }

    arr_expr->node_type = arr_expr->node_infer_type;
  }

  AST_LINKED* l = arr_expr->elements.start;
  ASSERT(l);

  AST_LOCAL first = l->curr;
  const Type base = first->node_type;
  ASSERT(base.is_valid());

  reduce_category(arr_expr, first);

  for (; l; l = l->next) {
    AST_LOCAL it = l->curr;

    reduce_category(arr_expr, it);
  }

  const Structure* arr_s;
  {
    Axle::AtomicLock<Structures> structures = {};
    Axle::AtomicLock<Axle::StringInterner> strings = {};
    comp->services.get_multiple(&structures, &strings);

    arr_s = find_or_make_array_structure(structures._ptr,
                                         strings._ptr,
                                         base, arr_expr->elements.count);
  }

  //Create the type
  arr_expr->node_type = to_type(arr_s);
  return;
}

TC_STAGE(ARRAY_EXPR_UP_REST) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTArrayExpr, arr_expr);
  
  if (arr_expr->node_infer_type.is_valid()) {
    return;// already covered
  }

  AST_LINKED* l = arr_expr->elements.start;
  ASSERT(l);

  AST_LOCAL base_test = l->curr;

  const Type base = base_test->node_type;
  l = l->next;

  for (; l; l = l->next) {
    AST_LOCAL it = l->curr;

    it->node_infer_type = base;
  }
}

TC_STAGE(ARRAY_EXPR_UP_FIRST) {
  TELEMETRY_FUNCTION();
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

    if (as->length != arr_expr->elements.count) {
      comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, arr_expr->node_span,
                                "Array expected size {}. Actual size: {}",
                                as->length, arr_expr->elements.count);
      return;
    }

    Type base = as->base;

    FOR_AST(arr_expr->elements, it) {
      it->node_infer_type = base;
    }
  }
  else {
    AST_LINKED* l = arr_expr->elements.start;

    if (l) {
      AST_LOCAL base_test = l->curr;
      base_test->node_infer_type = {};
    }
  }
}

TC_STAGE(ASCII_CHAR_DOWN) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTAsciiChar, a);
  a->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;
  a->node_type = comp_thread->builtin_types->t_ascii;
  return;
}

TC_STAGE(ASCII_STRING_DOWN) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTAsciiString, ascii);
  ascii->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;
  
  const size_t len = ascii->string->len;

  const Structure* s;
  {
    Axle::AtomicLock<Structures> structures = {};
    Axle::AtomicLock<Axle::StringInterner> strings = {};
    comp->services.get_multiple(&structures, &strings);

    s = find_or_make_array_structure(structures._ptr,
                                     strings._ptr,
                                     comp_thread->builtin_types->t_ascii, len);
  }

  ascii->node_type = to_type(s);
  return;
}

TC_STAGE(NUMBER_DOWN) {
  TELEMETRY_FUNCTION();
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
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTExportSingle, es);

  if (es->value->node_type.struct_type() != STRUCTURE_TYPE::LAMBDA) {
    comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, es->node_span,
                              "Cannot export a non-function. Found: \"{}\"", es->value->node_type.name);
    return;
  }

  es->node_type = comp_thread->builtin_types->t_void;
  return;
}

TC_STAGE(EXPORT_SINGLE_UP) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTExportSingle, es);

  es->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;
  es->value->node_infer_type = {};

  return;
}

TC_STAGE(EXPORT_UP) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTExport, e);
  e->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  FOR_AST(e->export_list, it) {
    it->node_infer_type = {};
  }

  e->node_type = comp_thread->builtin_types->t_void;
  return;
}

TC_STAGE(LINK_DOWN) {
  TELEMETRY_FUNCTION();
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
      import_lib.label = comp->next_function_label(t.unchecked_base<SignatureStructure>(), imp->node_span);

      comp->dyn_lib_imports.insert(std::move(import_lib));
      imp->import_index = comp->dyn_lib_imports.size;
    }

  ALREADY_IMPORTED:
    ASSERT(imp->node_type.is_valid());
  }

  return;
}

TC_STAGE(LINK_UP) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTLink, imp);
  imp->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  imp->import_type->node_infer_type = comp_thread->builtin_types->t_type;

  return;
}

TC_STAGE(IDENTIFIER_EXPR_DOWN) {
  TELEMETRY_FUNCTION();
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
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTCastExpr, cast);
  AST_LOCAL expr = cast->expr;
  reduce_category(cast, expr);

  const Type cast_to = get_type_value(comp_thread, cast->type);
  if (comp_thread->is_panic()) {
    return;
  }
  ASSERT(cast_to.is_valid());

  const Type cast_from = cast->expr->node_type;
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
            if (!VC::is_addressable(cast->expr->value_category)) {
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
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTCastExpr, cast);
  AST_LOCAL expr = cast->expr;

  expr->node_infer_type = {};

  return;
}

TC_STAGE(CAST_UP_TYPE) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTCastExpr, cast);
  cast->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  AST_LOCAL ty = cast->type;

  ty->node_infer_type = comp_thread->builtin_types->t_type;

  return;
}

static void run_un_up_neg_down(CompilerThread* const comp_thread,
                               AST_LOCAL this_node) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTUnaryOperatorExpr, expr);
  ASSERT(expr->op == UNARY_OPERATOR::NEG);

  AST_LOCAL prim = expr->expr;

  expr->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;
  reduce_category(expr, prim);

  Type ty = prim->node_type;

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
  expr->emit_info = { prim->node_type, expr->node_type, &UnOpArgs::emit_neg_int };
  return;
}

static void run_un_up_addr_down(CompilerGlobals* const comp, AST_LOCAL this_node) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTUnaryOperatorExpr, expr);
  ASSERT(expr->op == UNARY_OPERATOR::ADDRESS);

  AST_LOCAL prim = expr->expr;

  expr->value_category = VALUE_CATEGORY::TEMPORARY_IMMUTABLE;

  const Structure* ptr;
  {
    Axle::AtomicLock<Structures> structures = {};
    Axle::AtomicLock<Axle::StringInterner> strings = {};
    comp->services.get_multiple(&structures, &strings);

    ptr = find_or_make_pointer_structure(structures._ptr, strings._ptr,
                                         expr->expr->node_type);
  }

  expr->node_type = to_type(ptr);
  expr->emit_info = { prim->node_type, expr->node_type, &UnOpArgs::emit_address };
  return;
}

static void run_un_up_deref_down(CompilerThread* const comp_thread, AST_LOCAL this_node) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTUnaryOperatorExpr, expr);
  ASSERT(expr->op == UNARY_OPERATOR::DEREF);

  AST_LOCAL prim = expr->expr;

  if (prim->node_type.struct_type() == STRUCTURE_TYPE::POINTER) {
    const auto* ptr = prim->node_type.unchecked_base<PointerStructure>();

    if (ptr->is_mut) {
      expr->value_category = VALUE_CATEGORY::VARIABLE_MUTABLE;
    }
    else {
      expr->value_category = VALUE_CATEGORY::VARIABLE_IMMUTABLE;
    }

    expr->node_type = ptr->base;
    expr->emit_info = { prim->node_type, expr->node_type, &UnOpArgs::emit_deref_ptr };
    return;
  }
  else {
    const Axle::ViewArr<const char> op_string = UNARY_OP_STRING::get(expr->op);

    comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->node_span,
                              "No unary operator '{}' exists for type: '{}'",
                              op_string, prim->node_type.name);

    return;
  }
}

TC_STAGE(UNARY_OPERATOR_DOWN) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTUnaryOperatorExpr, expr);

  ASSERT(expr->expr->node_type.is_valid());

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
  TELEMETRY_FUNCTION();
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

              prim->node_infer_type = infer_type;
              return;
            }
          }
          comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->node_span,
                                    "Cannot infer negation of type: \"{}\"", infer_type.name);
          return;
        }
        else {
          prim->node_infer_type = {};
          return;
        }
      }
    case UNARY_OPERATOR::ADDRESS: {
        //TODO: can we infer anything here??
        prim->node_infer_type = {};
        return;
      }
    case UNARY_OPERATOR::DEREF: {
        //TODO: can we infer anything here
        prim->node_infer_type = {};
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
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTBinaryOperatorExpr, expr);

  AST_LOCAL left_ast = expr->left;
  AST_LOCAL right_ast = expr->right;

  const Type& left = left_ast->node_type;
  const Type& right = right_ast->node_type;

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
          case STRUCTURE_TYPE::ENUM: {
              if (left == right) {
                const auto* en = left.unchecked_base<EnumStructure>();
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
          case STRUCTURE_TYPE::ENUM: {
              if (left == right) {
                const auto* en = left.unchecked_base<EnumStructure>();
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
                      expr->emit_info.func = &BinOpArgs::emit_xor_ints;
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
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTBinaryOperatorExpr, bin_op);
  AST_LOCAL left = bin_op->left;
  AST_LOCAL right = bin_op->right;

  ASSERT(left->node_type.is_valid());
  ASSERT(right->node_type.is_valid());

  reduce_category(bin_op, left);
  reduce_category(bin_op, right);

  //TODO: constant folding
  //TODO: inference
  return type_check_binary_operator(comp, comp_thread, this_node);
}

TC_STAGE(BINARY_OPERATOR_UP) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTBinaryOperatorExpr, bin_op);

  bin_op->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  AST_LOCAL left = bin_op->left;
  AST_LOCAL right = bin_op->right;

  //TODO: Can we do type inference?

  left->node_infer_type = {};
  right->node_infer_type = {};

  return;
}

TC_STAGE(IMPORT_DOWN) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTImport, imp);
  AST_LOCAL expr = imp->expr_location;
  ASSERT(expr->node_type.is_valid());

  if (expr->node_type.struct_type() != STRUCTURE_TYPE::FIXED_ARRAY) {
    comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->node_span,
                              "#{} expression must be a character array\n"
                              "Instead found: {}",
                              comp_thread->intrinsics.import, expr->node_type.name);
    return;
  }

  const auto* array_type = expr->node_type.unchecked_base<ArrayStructure>();

  if (array_type->base != comp_thread->builtin_types->t_ascii) {
    comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->node_span,
                              "#{} expression must be a character array\n"
                              "Expected base type: {}\n"
                              "Instead found: {}",
                              comp_thread->intrinsics.import, comp_thread->builtin_types->t_ascii.name, array_type->base.name);
    return;
  }

  if (!VC::is_comptime(expr->value_category)) {
    comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->node_span,
                              "#{} expression must be a compile time constant",
                              comp_thread->intrinsics.import);
    return;
  }

  imp->node_type = comp_thread->builtin_types->t_void;
  return;
}

TC_STAGE(IMPORT_UP) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTImport, imp);
  imp->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;
  
  AST_LOCAL expr = imp->expr_location;
  expr->node_infer_type = {};

  return;
}

static bool test_function_overload(const CallSignature* sig, const SignatureStructure* sig_struct) {
  TELEMETRY_FUNCTION();

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
  TELEMETRY_FUNCTION();

  ASSERT(call->sig != nullptr);
  const SignatureStructure* sig_struct = call->sig;

  CallSignature sig = {};

  sig.arguments.reserve_total(call->arguments.count);

  //Load all the types
  FOR_AST(call->arguments, it) {
    ASSERT(it->node_type.is_valid());
    sig.arguments.insert({ it->node_type });
  }

  bool matches = test_function_overload(&sig, sig_struct);
  if (!matches) {
    comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, call->node_span,
                              "Arguments mismatch\nArgs: {}\nDecl: {}", sig, PrintSignatureType{ sig_struct });

  }
}

TC_STAGE(FUNCTION_CALL_DOWN) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTFunctionCallExpr, call);

  const Type& func_type = call->function->node_type;
  call->sig = func_type.unchecked_base<SignatureStructure>();

  FOR_AST(call->arguments, it) {
    reduce_category(call, it);
  }

  check_call_arguments(comp_thread, call);
  if (comp_thread->is_panic()) {
    return;
  }

  const auto* sig = call->sig;
  ASSERT(sig);

  const size_t size = sig->parameter_types.size;

  if (call->arguments.count != size) {
    comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, call->node_span,
                              "Compiler linked a function with {} parameters for a call with {} arguments!",
                              size, call->arguments.count);
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
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTFunctionCallExpr, call);

  const Type& func_type = call->function->node_type;
  if (func_type.struct_type() != STRUCTURE_TYPE::LAMBDA) {
    comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, call->function->node_span,
                              "Attempted to call a non-function. Found type {}", func_type.name);
    return;
  }

  ASSERT(VC::is_comptime(call->function->value_category));

  FOR_AST(call->arguments, it) {
    //TODO: try to infer arguments
    it->node_infer_type = {};
  }
}

TC_STAGE(FUNCTION_CALL_UP_FUNCTION) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTFunctionCallExpr, call);

  call->value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  call->function->node_infer_type = {};

  return;
}

TC_STAGE(ASSIGN_UP_RIGHT) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTAssign, assign);

  AST_LOCAL assign_to = assign->assign_to;
  ASSERT(assign_to->node_type.is_valid());
  ASSERT(!assign->value->node_type.is_valid());

  if (!VC::is_mutable(assign_to->value_category)) {
    comp_thread->report_error(ERROR_CODE::CONST_ERROR, assign_to->node_span,
                              "Cannot assign to \"const\" expression");
    return;
  }

  assign->value->node_infer_type = assign_to->node_type;

  assign->node_type = comp_thread->builtin_types->t_void;
}

TC_STAGE(ASSIGN_UP_LEFT) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTAssign, assign);
  
  AST_LOCAL assign_to = assign->assign_to;
  
  ASSERT(!assign_to->node_type.is_valid());
  assign_to->node_infer_type = {};

  return;
}

TC_STAGE(DECL_DOWN) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTDecl, ast_decl);

  AST_LOCAL decl_expr = ast_decl->expr;

  if (!ast_decl->type.is_valid()) {
    ASSERT(decl_expr != nullptr);
    ast_decl->type = ast_decl->expr->node_type;
  }

  ASSERT(ast_decl->type.is_valid());

  if (ast_decl->compile_time_const
      && (decl_expr == nullptr || !VC::is_comptime(decl_expr->value_category))) {
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

  if (VC::is_comptime(decl->value_category) || VC::is_comptime(decl_expr->value_category)) {
    if (!VC::is_comptime(decl_expr->value_category)) {
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
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTDecl, decl);

  Type expr_type = {};
  if (decl->type_ast != nullptr) {
    decl->type = get_type_value(comp_thread, decl->type_ast);
    if (comp_thread->is_panic()) {
      return;
    }

    expr_type = decl->type;
  }
  else {
    decl->type = {};
  }

  if (decl->expr != nullptr) {
    decl->expr->node_infer_type = expr_type;
  }

  return;
}

TC_STAGE(DECL_UP_TYPE) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTDecl, decl);

  if (decl->type_ast != nullptr) {
    decl->type_ast->node_infer_type = comp_thread->builtin_types->t_type;
  }
}

TC_STAGE(IF_ELSE_UP) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTIfElse, if_else);
  
  if_else->condition->node_infer_type = comp_thread->builtin_types->t_bool;
  if_else->if_statement->node_infer_type = {};

  if (if_else->else_statement != 0) {
    if_else->else_statement->node_infer_type = {};
  }

  if_else->node_type = comp_thread->builtin_types->t_void;
  return;
}

TC_STAGE(WHILE_UP) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTWhile, while_loop);

  while_loop->condition->node_infer_type = comp_thread->builtin_types->t_bool;
  while_loop->statement->node_infer_type = {};

  while_loop->node_type = comp_thread->builtin_types->t_void;
  return;
}

TC_STAGE(BLOCK_UP) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTBlock, block);
  
  FOR_AST(block->block, it) {
    it->node_infer_type = {};
  }

  block->node_type = comp_thread->builtin_types->t_void;
  return;
}

TC_STAGE(RETURN_UP) {
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTReturn, ret);
  
  ASSERT(typer->return_type.is_valid());

  if (ret->expr != nullptr) {
    ret->expr->node_infer_type = typer->return_type;

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
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTStructBody, body);

  //Build the new structure
  {
    CompositeStructure* cmp_s;
    {
      Axle::AtomicLock<Structures> structures = {};
      Axle::AtomicLock<Axle::StringInterner> strings = {};
      comp->services.get_multiple(&structures, &strings);
      cmp_s = STRUCTS::new_composite_structure(structures._ptr,
                                               strings._ptr);
    }
    uint32_t current_size = 0;
    uint32_t current_alignment = 0;

    Axle::Array<StructElement> elements = {};
    elements.reserve_total(body->elements.count);

    FOR_AST(body->elements, it) {
      ASTTypedName* tn = (ASTTypedName*)it;

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
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTStructBody, body);

  FOR_AST(body->elements, it) {
    it->node_infer_type = {};
  }
}

TC_STAGE(TYPED_NAME_DOWN) {
  TELEMETRY_FUNCTION();
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
  TELEMETRY_FUNCTION();
  EXPAND_THIS(ASTTypedName, name);
  
  name->type->node_infer_type = comp_thread->builtin_types->t_type;

  return;
}

static void run_step(CompilerGlobals* const comp,
                     CompilerThread* const comp_thread,
                     Typer* const typer,
                     AST_LOCAL node, AST_VISIT_STEP step) {
  TELEMETRY_FUNCTION();

  switch (step) {
#define MOD(s, a, ...) case AST_VISIT_STEP:: s : \
    static_assert(AST_TYPE:: a == ast_visit_step_ast_type(AST_VISIT_STEP:: s)); \
    ASSERT(node->ast_type == AST_TYPE:: a); \
    ASSERT(node->ast_type == ast_visit_step_ast_type(AST_VISIT_STEP:: s)); \
    return run_step_impl<AST_VISIT_STEP:: s>(comp, comp_thread, typer, node);
      AST_VISIT_STEP_MOD;
#undef MOD
  }

  INVALID_CODE_PATH("Invalid visit step");
}

void TC::type_check_ast(CompilerGlobals* const comp,
                        CompilerThread* const comp_thread,
                        Namespace* const ns,
                        const Axle::ViewArr<const AstVisit> visit_arr) {
  TELEMETRY_FUNCTION();
  TELEMETRY_FUNCTION();

  Typer typer = {};
  typer.available_names = ns;

  for (const AstVisit v: visit_arr) {
    ASSERT(!v.node->node_type.is_valid());// this is what signals completion

    run_step(comp, comp_thread, &typer, v.node, v.step);
    if (comp_thread->is_panic()) {
      return;
    }

    AST_LOCAL n = v.node;
    if (n->node_type.is_valid()
     && n->node_infer_type.is_valid()
     && n->node_infer_type != n->node_type) {
      const Type& expected = n->node_infer_type;
      const Type& actual = n->node_type;
      comp_thread->report_error(ERROR_CODE::TYPE_CHECK_ERROR, n->node_span,
                                "Expected type: {}. Actual type: {}", expected.name, actual.name);
      return;
    }
  }

  for (const AstVisit v: visit_arr) {
    ASSERT(v.node->node_type.is_valid());// this is what signals completion
  }
}
