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

Function* Compiler::new_function() {
  return functions.insert();
}

void Compiler::set_unfound_name(const InternString* name, const Span& span) {
  assert(name != nullptr);
  unfound_deps.panic = true;

  unfound_deps.units.insert_uninit(1);
  UnfoundDep* dep = unfound_deps.units.back();

  dep->set_union(UnfoundDepType::Name);
  dep->name = name;
  dep->unit_waiting = current_unit;
  dep->span = span;

  switch (current_unit.type) {
    case COMPILATION_TYPE::CONSTANT: {
        (constant_units.data + current_unit.index)->unfound_dependency = true;
        break;
      }
    case COMPILATION_TYPE::FUNCTION: {
        (function_units.data + current_unit.index)->unfound_dependency = true;
        break;
      }
  }
}

void Compiler::set_unfound_signature(CallSignature&& sig, const Span& span) {
  unfound_deps.panic = true;

  unfound_deps.units.insert_uninit(1);
  UnfoundDep* dep = unfound_deps.units.back();

  dep->set_union(UnfoundDepType::Function);
  dep->signature = std::move(sig);
  dep->unit_waiting = current_unit;
  dep->span = span;

  switch (current_unit.type) {
    case COMPILATION_TYPE::CONSTANT: {
        (constant_units.data + current_unit.index)->unfound_dependency = true;
        break;
      }
    case COMPILATION_TYPE::FUNCTION: {
        (function_units.data + current_unit.index)->unfound_dependency = true;
        break;
      }
  }
}

void Compiler::set_dep(CompilationUnitCarrier unit) {
  unfound_deps.panic = true;

  switch (current_unit.type) {
    case COMPILATION_TYPE::FUNCTION:
      (function_units.data + current_unit.index)->dependencies.insert(unit);
      break;
    case COMPILATION_TYPE::CONSTANT:
      (constant_units.data + current_unit.index)->dependencies.insert(unit);
      break;
  }
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

  assert(val.has_value);

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
    if (l->name == i_s) {
      return l;
    }
  }
  return nullptr;
}

int32_t StackState::next_stack_local(uint64_t size, uint64_t alignment) {
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

void UnfoundDep::move_from(UnfoundDep&& dp) noexcept {
  unit_waiting = std::move(dp.unit_waiting);
  type = std::exchange(dp.type, UnfoundDepType::Unkown);

  switch (type) {
    case UnfoundDepType::Unkown: break;
    case UnfoundDepType::Function: {
        signature = std::move(dp.signature);
        break;
      }
    case UnfoundDepType::Name: {
        name = std::exchange(dp.name, nullptr);
        break;
      }
  }
}

void UnfoundDep::set_union(UnfoundDepType et) noexcept {
  destruct_union();
  type = et;
  switch (type) {
    case UnfoundDepType::Unkown: break;
    case UnfoundDepType::Function: {
        default_init(&signature);
        break;
      }
    case UnfoundDepType::Name: {
        name = nullptr;
        break;
      }
  }
}

void UnfoundDep::destruct_union() noexcept {
  switch (type) {
    case UnfoundDepType::Unkown: break;
    case UnfoundDepType::Function: {
        signature.~CallSignature();
        break;
      }
    case UnfoundDepType::Name: {
        name = nullptr;
        break;
      }
  }
}

UnfoundDep::~UnfoundDep() {
  destruct_single(&unit_waiting);
  destruct_union();
}

const Structure* find_or_make_array_type(Compiler* const comp, const Structure* base, size_t length) {
  Types* types = comp->types;

  {
    auto i = types->structures.begin();
    const auto end = types->structures.end();

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
  return new_array_type(comp, base, length);
}

const Structure* find_or_make_pointer_type(Compiler* const comp, const Structure* base) {
  Types* types = comp->types;

  {
    auto i = types->structures.begin();
    const auto end = types->structures.end();

    for (; i < end; i++) {
      const Structure* s = *i;
      if (s->type == STRUCTURE_TYPE::POINTER) {
        //Is array
        const PointerStructure* as = static_cast<const PointerStructure*>(s);
        if (as->base == base) {
          //Is same
          return s;
        }
      }
    }
  }

  //Doesnt exist - need to make new type
  return new_pointer_type(comp, base);
}

//Note: Recursive for array types
void compile_type(Compiler* const comp, ASTType* type) {
  if (type->type != nullptr) {
    //Incase function re-called
    return;
  }

  switch (type->type_type) {
    case TYPE_TYPE::NORMAL: {
        type->type = comp->types->structure_by_name(type->name);
        if (type->type == nullptr) {
          comp->set_unfound_name(type->name, type->span);
          return;
        }
        break;
      }
    case TYPE_TYPE::ARRAY: {
        compile_type(comp, type->arr.base);
        if (comp->is_panic()) {
          return;
        }

        assert(type->arr.base->type != nullptr);
        assert(type->arr.expr != nullptr);

        if (type->arr.expr->const_val == nullptr) {
          //New compile time constant
          ASTExpression* const expr = type->arr.expr;

          ConstantUnit unit ={};
          unit.expr   = expr;
          unit.stage  = EXPRESSION_COMPILE_STAGE::UNTYPED;

          comp->constant_units.insert(std::move(unit));

          CompilationUnitCarrier carrier ={};
          carrier.index = comp->constant_units.size - 1;
          carrier.type  = COMPILATION_TYPE::CONSTANT;

          comp->set_dep(carrier);
          return;
        }
        else {
          if (!TYPE_TESTS::is_int(type->arr.expr->type)) {
            comp->report_error(CompileCode::TYPE_CHECK_ERROR, type->arr.expr->span,
                               "Expected an integer type value for array length\n"
                               "Instead found: {}", type->arr.expr->type->name);
            return;
          }

          uint64_t length;
          if (TYPE_TESTS::is_signed_int(type->arr.expr->type)) {
            int64_t i_length = *(const int64_t*)type->arr.expr->const_val;
            if (i_length < 0) {
              comp->report_error(CompileCode::TYPE_CHECK_ERROR, type->arr.expr->span,
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

          type->type = find_or_make_array_type(comp, type->arr.base->type, length);
          break;
        }

        break;
      }
    case TYPE_TYPE::PTR: {
        compile_type(comp, type->ptr.base);
        if (comp->is_panic()) {
          return;
        }

        assert(type->ptr.base != nullptr);

        type->type = find_or_make_pointer_type(comp, type->ptr.base->type);
        break;
      }
  }

  assert(type->type != nullptr);
}

static Array<const Function*> generate_overload_set(const Compiler* const comp,
                                                    const CallSignature* call) {
  auto i = comp->functions.begin_const_iter();
  const auto end = comp->functions.end_const_iter();

  Array<const Function*> possible_overloads ={};

  for (; i != end; i.next()) {
    const Function* const func = i.get();

    //Correct name and number of args
    if (call->name == func->name
        && call->arguments.size == func->parameter_types.size) {
      auto p_call = call->arguments.begin();
      const auto end_call = call->arguments.end();

      auto p_func = func->parameter_types.begin();

      bool requires_cast = false;

      while (p_call < end_call) {
        if (*p_call == *p_func) {
          //Do nothing
        }
        else if (can_comptime_cast(*p_call, *p_func)) {
          requires_cast = true;
        }
        else {
          //Escape out
          goto FAIL;
        }

        p_call++;
        p_func++;
      }

      if (!requires_cast) {
        //Complete match
        possible_overloads.clear();
        possible_overloads.insert(func);

        return possible_overloads;
      }

      possible_overloads.insert(func);
    }

  FAIL:
    continue;
  }

  return possible_overloads;
}

static void compile_find_function_call(Compiler* const comp,
                                       ASTExpression* const expr) {
  assert(expr->expr_type == EXPRESSION_TYPE::FUNCTION_CALL);
  FunctionCallExpr* const call = &expr->call;

  CallSignature sig ={};

  sig.name = call->function_name;

  {
    sig.arguments.reserve_total(call->arguments.size);

    auto i = call->arguments.begin();
    const auto end = call->arguments.end();

    for (; i < end; i++) {
      sig.arguments.insert(i->type);
    }
  }

  Array<const Function*> possible_overloads = generate_overload_set(comp, &sig);

  if (possible_overloads.size == 0) {
    comp->set_unfound_signature(std::move(sig), expr->span);
    return;
  }
  else if (possible_overloads.size == 1) {
    call->function = possible_overloads.data[0];
  }
  else {
    Array<char> options_message ={};

    //Load all the options into a string
    for (size_t i = 0; i < possible_overloads.size; i++) {
      const Function* func = possible_overloads.data[i];

      format_to_array(options_message, "Option {}: {}", i + 1, PrintFuncSignature{ func });
    }

    comp->report_error(CompileCode::TYPE_CHECK_ERROR, expr->span,
                       "Found more than one function '{}' with signature that required implicit casts\n"
                       "Expcted: {}\n"
                       "{}", call->function_name, sig, options_message);
    return;
  }
}

static void cast_operator_type(Compiler* const comp,
                               State* const state,
                               ASTExpression* const expr) {
  assert(expr->expr_type == EXPRESSION_TYPE::CAST);

  const Types* const types = comp->types;

  const Structure* const cast_to = expr->cast.type.type;
  const Structure* const cast_from = expr->cast.expr->type;

  assert(cast_to != nullptr);
  assert(cast_from != nullptr);

  const auto emit_cast_func = [&cast_to](ASTExpression* expr, CAST_FUNCTION cast) {
    expr->cast.emit = cast;
    expr->type = cast_to;
  };

  if (can_comptime_cast(cast_from, cast_to)) {
    compile_implicit_cast(comp, expr->cast.expr, cast_to);
    emit_cast_func(expr, CASTS::no_op);
    return;
  }

  switch (cast_from->type) {
    case STRUCTURE_TYPE::ASCII_CHAR: {
        //Can only cast ascii char to a u8
        if (cast_to == types->s_u8) {
          emit_cast_func(expr, CASTS::no_op);
          return;
        }

        break;
      }
    case STRUCTURE_TYPE::ENUM: {
        const EnumStructure* en = (const EnumStructure*)cast_from;

        if (cast_to == en->base) {
          emit_cast_func(expr, CASTS::no_op);
          return;
        }

        break;
      }
    case STRUCTURE_TYPE::FIXED_ARRAY: {
        const ArrayStructure* from_arr = (const ArrayStructure*)cast_from;

        if (cast_to->type == STRUCTURE_TYPE::FIXED_ARRAY) {
          const ArrayStructure* to_arr = (const ArrayStructure*)cast_to;

          if (can_implicit_cast(from_arr->base, to_arr->base) && from_arr->size() == to_arr->size()) {
            emit_cast_func(expr, CASTS::no_op);
            return;
          }
        }
        else if (cast_to->type == STRUCTURE_TYPE::POINTER) {
          const PointerStructure* to_ptr = (const PointerStructure*)cast_to;

          if (can_implicit_cast(from_arr->base, to_ptr->base) && from_arr->size() == to_ptr->size()) {
            //Must be in memory to cast like this
            set_valid_rvts(expr->cast.expr, state, (uint8_t)RVT::MEMORY);

            emit_cast_func(expr, CASTS::no_op);
            return;
          }
        }

        return;
      }
    case STRUCTURE_TYPE::INTEGER: {
        const IntegerStructure* from_int = (const IntegerStructure*)cast_from;

        if (cast_to->type == STRUCTURE_TYPE::INTEGER) {
          const IntegerStructure* to_int = (const IntegerStructure*)cast_to;

          if (from_int->bytes >= to_int->bytes) {
            //Can cast down ints easily
            emit_cast_func(expr, CASTS::no_op);
            return;
          }
          else {
            //Cast up in size
            //Need specific instructions for different sizes
            //Can cast up to r64 and then "cast down" which is free

            if (from_int->bytes == 1) {
              if (from_int->is_signed) {
                emit_cast_func(expr, CASTS::i8_to_r64);
                return;
              }
              else {
                emit_cast_func(expr, CASTS::u8_to_r64);
                return;
              }
            }
          }

          comp->report_error(CompileCode::INTERNAL_ERROR, expr->span,
                             "Cannot cast type '{}' to type '{}'\n"
                             "They are both integers and this should be implemented",
                             cast_from->name, cast_to->name);
          return;
        }

        break;
      }
    case STRUCTURE_TYPE::LITERAL: {
        const LiteralStructure* from_lit = (const LiteralStructure*)cast_from;

        if (cast_to->type == STRUCTURE_TYPE::LITERAL) {
          const LiteralStructure* to_lit = (const LiteralStructure*)cast_to;

          if (from_lit->literal_type == LITERAL_TYPE::SIGNED_INTEGER && to_lit->literal_type == LITERAL_TYPE::INTEGER) {
            emit_cast_func(expr, CASTS::no_op);
            return;
          }
        }

        break;
      }
    case STRUCTURE_TYPE::POINTER: {
        const PointerStructure* from_ptr = (const PointerStructure*)cast_from;

        if (cast_to->type == STRUCTURE_TYPE::POINTER) {
          const PointerStructure* to_ptr = (const PointerStructure*)cast_to;

          if (can_implicit_cast(from_ptr->base, to_ptr->base) && from_ptr->size() == to_ptr->size()) {
            emit_cast_func(expr, CASTS::no_op);
            return;
          }
        }
        
        break;
      }
    case STRUCTURE_TYPE::VOID: {
        comp->report_error(CompileCode::TYPE_CHECK_ERROR, expr->span,
                           "Cannot cast '{}' to any type\n"
                           "Attempted to cast '{}' to '{}'",
                           cast_from->name, cast_from->name, cast_to->name);
        break;
      }
  }

  comp->report_error(CompileCode::TYPE_CHECK_ERROR, expr->span,
                     "Cannot cast type '{}' to type '{}'",
                     cast_from->name, cast_to->name);
  return;
}

constexpr static bool already_const_type(EXPRESSION_TYPE et) {
  return et == EXPRESSION_TYPE::ENUM
    || et == EXPRESSION_TYPE::VALUE
    || et== EXPRESSION_TYPE::ASCII_STRING;
}

constexpr static bool can_load_const_value(const ASTExpression* const expr) {
  return expr->comptime_eval && expr->const_val == nullptr && !already_const_type(expr->expr_type);
}

void force_load_const_value(Compiler* const comp, ASTExpression* expr) {
  assert(expr->comptime_eval);

  if (expr->const_val != nullptr) {
    return;
  }

  //If not already const then must be an "already const type"
  assert(already_const_type(expr->expr_type));

  switch (expr->expr_type) {
    case EXPRESSION_TYPE::ENUM: {
        uint8_t* enum_c = comp->constants.alloc_no_construct(8);
        x64_to_bytes(expr->enum_value.enum_value->representation, enum_c);

        expr->const_val = enum_c;
        break;
      }
    case EXPRESSION_TYPE::VALUE: {
        const size_t size = expr->type->size();

        uint8_t* val_c = comp->constants.alloc_no_construct(size);
        memcpy_ts(val_c, size, (uint8_t*)&expr->value.value, size);

        expr->const_val = val_c;
        break;
      }
    case EXPRESSION_TYPE::ASCII_STRING: {
        const ArrayStructure* const arr_type = (const ArrayStructure*)expr->type;

        const size_t size = arr_type->size();
        char* string_c = (char*)comp->constants.alloc_no_construct(size);

        expr->const_val = (uint8_t*)string_c;
        break;
      }
  }
}


void make_constant_typed_dependency(Compiler* const comp, ASTExpression* expr, const Structure* cast_to) {
  comp->constant_units.insert_uninit(1);
  ConstantUnit* unit = comp->constant_units.back();
  unit->expr    = expr;
  unit->stage   = EXPRESSION_COMPILE_STAGE::TYPED;
  unit->cast_to = cast_to;

  CompilationUnitCarrier carrier ={};
  carrier.index = comp->constant_units.size - 1;
  carrier.type  = COMPILATION_TYPE::CONSTANT;

  comp->set_dep(carrier);
}

static void do_literal_cast(Compiler* const comp,
                            const ASTExpression* expr, const Structure* to_type,
                            const uint8_t* from, uint8_t* to) {
  //Should be guaranteed that this cast will be ok

  const Structure* from_type = expr->type;

  if (from_type == to_type) {
    const size_t size = from_type->size();
    memcpy_ts(to, size, from, size);
    return;
  }

  switch (from_type->type) {
    case STRUCTURE_TYPE::FIXED_ARRAY: {
        const ArrayStructure* from_arr = (const ArrayStructure*)from_type;
        const ArrayStructure* to_arr = (const ArrayStructure*)to_type;

        assert(to_type->type == STRUCTURE_TYPE::FIXED_ARRAY);
        assert(from_arr->length == to_arr->length);
        assert(expr->expr_type == EXPRESSION_TYPE::ARRAY_EXPR);

        size_t from_base_size = from_arr->base->size();
        size_t to_base_size = to_arr->base->size();

        for (size_t i = 0; i < from_arr->length; i++) {
          auto* base_expr = expr->array_expr.elements.data + i;

          do_literal_cast(comp, base_expr, to_arr->base, from + (i * from_base_size), to + (i * to_base_size));
          if (comp->is_panic()) {
            return;
          }
        }

        break;
      }
    case STRUCTURE_TYPE::LITERAL: {
        const LiteralStructure* ls = (const LiteralStructure*)from_type;

        switch (ls->literal_type) {
          case LITERAL_TYPE::EMPTY_ARR: {
              assert(false);//Should fix this later
              break;
            }
          case LITERAL_TYPE::SIGNED_INTEGER: {
              const IntegerStructure* to_is = (const IntegerStructure*)to_type;
              assert(to_type->type == STRUCTURE_TYPE::INTEGER);
              assert(to_is->bytes <= 8);

              //Can still be an unsigned or signed integer

              int64_t comptime_val = x64_from_bytes(from);

              if (to_is->is_signed) {
                const int64_t max_negative_val = ~bit_fill_lower<int64_t>(to_is->bytes * 8 - 1);
                const int64_t max_positive_val = bit_fill_lower<int64_t>(to_is->bytes * 8 - 1);

                if (comptime_val < max_negative_val) {
                  comp->report_error(CompileCode::TYPE_CHECK_ERROR, expr->span,
                                     "'{}' value is too large to fit in type '{}'\n"
                                     "The maximum negative value is: '{}'\n"
                                     "Try using '% {}' to make the value small enough",
                                     comptime_val, to_is->name, max_positive_val, max_positive_val);
                  return;

                }
                else if (comptime_val > max_positive_val) {
                  comp->report_error(CompileCode::TYPE_CHECK_ERROR, expr->span,
                                     "'{}' value is too large to fit in type '{}'\n"
                                     "The maximum positive value is: '{}'\n"
                                     "Try using '% {}' to make the value small enough",
                                     comptime_val, to_is->name, max_positive_val, max_positive_val);
                  return;
                }
              }
              else {
                const uint64_t max_positive_val = bit_fill_lower<uint64_t>(to_is->bytes * 8);
                const uint64_t abs_val = absolute(comptime_val);
                if (comptime_val < 0) {
                  comp->report_error(CompileCode::TYPE_CHECK_ERROR, expr->span,
                                     "Tried to assign negative value '{}' into unsigned type '{}'\n"
                                     "Try casting to '{}' first as this legally converts it to an unsigned type",
                                     comptime_val, to_is->name, comp->types->s_int_lit->name);
                  return;
                }
                else if (abs_val > max_positive_val) {
                  comp->report_error(CompileCode::TYPE_CHECK_ERROR, expr->span,
                                     "'{}' value is too large to fit in type '{}'\n"
                                     "The maximum positive value is: '{}'\n"
                                     "Try using '% {}' to make the value small enough",
                                     abs_val, to_is->name, max_positive_val, max_positive_val);
                  return;
                }
              }

              //Should theoretically be able to just memcpy ... 
              memcpy_ts(to, to_is->bytes, from, to_is->bytes);
              break;
            }
          case LITERAL_TYPE::INTEGER: {
              const IntegerStructure* to_is = (const IntegerStructure*)to_type;
              assert(to_type->type == STRUCTURE_TYPE::INTEGER);

              assert(to_is->bytes <= 8);

              uint64_t comptime_val = x64_from_bytes(from);
              if (to_is->is_signed) {
                const uint64_t max_positive_val = bit_fill_lower<uint64_t>(to_is->bytes * 8 - 1);
                if (comptime_val > max_positive_val) {
                  comp->report_error(CompileCode::TYPE_CHECK_ERROR, expr->span,
                                     "'{}' value is too large to fit in type '{}'\n"
                                     "The maximum positive value is: '{}'\n"
                                     "Try using '% {}' to make the value small enough",
                                     comptime_val, to_is->name, max_positive_val, max_positive_val);
                  return;
                }
              }
              else {
                const uint64_t max_val = bit_fill_lower<uint64_t>(to_is->bytes * 8);
                if (comptime_val > max_val) {
                  comp->report_error(CompileCode::TYPE_CHECK_ERROR, expr->span,
                                     "'{}' value is too large to fit in type '{}'\n"
                                     "The maximum value is: '{}'"
                                     "Try using '% {}' to make the value small enough",
                                     comptime_val, to_is->name, max_val, max_val);
                  return;
                }
              }

              //Unsigned values should simply be loaded in byte by byte
              memcpy_ts(to, to_is->bytes, from, to_is->bytes);
              break;
            }
        }

        break;
      }
    default:
      assert(false);//should never be here
  }
}


void compile_implicit_cast(Compiler* const comp, ASTExpression* from_expr, const Structure* to) {
  const Structure* from = from_expr->type;
  assert(from != nullptr);
  assert(to != nullptr);

  if (can_implicit_cast(from, to)) {
    return;
  }
  else if (can_comptime_cast(from, to) && from_expr->comptime_eval) {
    if (can_load_const_value(from_expr)) {
      make_constant_typed_dependency(comp, from_expr, to);
    }
    else {
      force_load_const_value(comp, from_expr);

      uint8_t* new_const = comp->constants.alloc_no_construct(from_expr->type->size());

      do_literal_cast(comp, from_expr, to, from_expr->const_val, new_const);
      comp->constants.free_no_destruct(from_expr->const_val);

      from_expr->const_val = new_const;
    }
  }
  else {
    comp->report_error(CompileCode::TYPE_CHECK_ERROR, from_expr->span,
                       "Cannot implicilty cast from '{}' to '{}'",
                       from->name, to->name);
  }
}

void set_valid_rvts(ASTExpression* const expr, State* const state, uint8_t valid_rvts) {
  expr->valid_rvts &= valid_rvts;

  if (expr->expr_type == EXPRESSION_TYPE::LOCAL) {
    Local* local = state->all_locals.data + expr->local;

    local->valid_rvts &= expr->valid_rvts;
  }
}

//Note: Recursive
static void compile_type_of_expression(Compiler* const comp,
                                       ASTExpression* const expr,
                                       State* const state) {
  assert(expr->valid_rvts != 0);//shouldnt start 0
  DEFER(&) { assert(expr->valid_rvts != 0); };//shouldnt end 0

  //Already typed
  if (expr->type != nullptr) {
    return;
  }

  switch (expr->expr_type) {
    case EXPRESSION_TYPE::INDEX: {
        auto* arr_expr = expr->index.expr;
        auto* index_expr = expr->index.index;

        arr_expr->call_leaf = expr->call_leaf;
        index_expr->call_leaf = expr->call_leaf;

        compile_type_of_expression(comp, arr_expr, state);
        if (comp->is_panic()) {
          return;
        }

        assert(arr_expr->type != nullptr);

        if (!TYPE_TESTS::is_array(arr_expr->type)) {
          comp->report_error(CompileCode::TYPE_CHECK_ERROR, arr_expr->span,
                             "Cannot take index of non-array type: {}",
                             arr_expr->type->name);
          return;
        }

        compile_type_of_expression(comp, index_expr, state);
        if (comp->is_panic()) {
          return;
        }

        auto* index_type = index_expr->type;
        assert(index_type != nullptr);

        if (!TYPE_TESTS::is_int(index_type)) {
          comp->report_error(CompileCode::TYPE_CHECK_ERROR, index_expr->span,
                             "An index must be in integer\n"
                             "Found non-integer type: {}",
                             index_type->name);
          return;
        }

        if (!index_expr->comptime_eval) {
          //If the index is not known at compile time then the array must in memory and in a register
          uint8_t valids = (uint8_t)RVT::MEMORY;

          if (arr_expr->type->size() <= 8) {
            //Can be in a register if it will fit
            valids |= RVT::REGISTER;
          }

          set_valid_rvts(arr_expr, state, valids);
        }

        expr->type = static_cast<const ArrayStructure*>(arr_expr->type)->base;
        expr->comptime_eval = index_expr->comptime_eval && arr_expr->comptime_eval;
        expr->makes_call = index_expr->makes_call || arr_expr->makes_call;
        break;
      }
    case EXPRESSION_TYPE::ARRAY_EXPR: {
        auto i = expr->array_expr.elements.mut_begin();
        const auto end = expr->array_expr.elements.mut_end();

        const Structure* base_type = nullptr;

        //Assume true to start with
        expr->comptime_eval = true;

        for (; i < end; i++) {
          i->call_leaf = expr->call_leaf;

          compile_type_of_expression(comp, i, state);
          if (comp->is_panic()) {
            return;
          }

          if (base_type == nullptr) {
            //TEMP: First element decides type for now - will have issues with literals
            base_type = i->type;
          }
          else {
            //Check rest of the elements match
            compile_implicit_cast(comp, i, base_type);
            if (comp->is_panic()) {
              return;
            }

            base_type = i->type;//Will always be either same or more specific
          }

          expr->makes_call |= i->makes_call;
          expr->comptime_eval &= i->comptime_eval;
        }

        if (base_type == nullptr) {
          expr->type = comp->types->s_empty_arr;
        }
        else {
          expr->type = find_or_make_array_type(comp, base_type, expr->array_expr.elements.size);
        }

        break;
      }
    case EXPRESSION_TYPE::ASCII_STRING: {
        const size_t len = expr->ascii_string->len + 1;

        expr->type = find_or_make_array_type(comp, comp->types->s_ascii, len);
        expr->comptime_eval = true;
        break;
      }
    case EXPRESSION_TYPE::ENUM:
      expr->enum_value.enum_value = comp->types->enum_by_name(expr->enum_value.name);

      if (expr->enum_value.enum_value == nullptr) {
        comp->set_unfound_name(expr->enum_value.name, expr->span);
        return;
      }

      expr->type = expr->enum_value.enum_value->type;
      expr->comptime_eval = true;

      break;
    case EXPRESSION_TYPE::VALUE: {
        ValueExpr* const val = &expr->value;

        if (val->suffix == nullptr) {
          expr->type = comp->types->s_int_lit;
        }
        else if (val->suffix == comp->types->s_i64->name) {
          expr->type = comp->types->s_i64;
        }
        else if (val->suffix == comp->types->s_u64->name) {
          expr->type = comp->types->s_u64;
        }
        else {
          comp->report_error(CompileCode::TYPE_CHECK_ERROR, expr->span,
                             "Invalid integer literal suffix type '{}'",
                             val->suffix);
          return;
        }

        expr->comptime_eval = true;

        break;
      }

    case EXPRESSION_TYPE::NAME: {
        const InternString* name = expr->name;

        Local* const loc = state->find_local(name);

        if (loc != nullptr) {
          expr->expr_type = EXPRESSION_TYPE::LOCAL;
          expr->local = loc - state->all_locals.data;
          expr->type = loc->type;

          loc->valid_rvts &= expr->valid_rvts;
        }
        else {
          comp->set_unfound_name(name, expr->span);
          return;
        }

        expr->comptime_eval = false;

        break;
      }
    case EXPRESSION_TYPE::CAST: {
        expr->cast.expr->call_leaf = expr->call_leaf;

        compile_type(comp, &expr->cast.type);
        if (comp->is_panic()) {
          return;
        }

        compile_type_of_expression(comp, expr->cast.expr, state);
        if (comp->is_panic()) {
          return;
        }

        cast_operator_type(comp, state, expr);
        if (comp->is_panic()) {
          return;
        }

        expr->comptime_eval = expr->cast.expr->comptime_eval;
        expr->makes_call = expr->cast.expr->makes_call;
        expr->type = expr->cast.type.type;
        break;
      }
    case EXPRESSION_TYPE::UNARY_OPERATOR: {
        expr->un_op.primary->call_leaf = expr->call_leaf;

        compile_type_of_expression(comp, expr->un_op.primary, state);
        if (comp->is_panic()) {
          return;
        }

        expr->comptime_eval = expr->un_op.primary->comptime_eval;
        expr->makes_call = expr->un_op.primary->makes_call;

        switch (expr->un_op.op) {
          case UNARY_OPERATOR::NEG:
            compile_unary_operator(comp, expr, neg_operators);
            break;
          case UNARY_OPERATOR::ADDRESS:
            compile_take_address(comp, state, expr);
            break;
          case UNARY_OPERATOR::DEREF:
            compile_deref(comp, expr);
            break;
          default: {
              const char* name = UNARY_OP_STRING::get(expr->un_op.op);

              comp->report_error(CompileCode::INTERNAL_ERROR, expr->span,
                                 "Type checking is not implemented for unary operator '{}'",
                                 name);
              return;
            }
        }

        //Check if the op type check failed
        if (comp->is_panic()) {
          return;
        }

        break;
      }
    case EXPRESSION_TYPE::BINARY_OPERATOR: {
        BinaryOperatorExpr* const bin_op = &expr->bin_op;

        bin_op->left->call_leaf = expr->call_leaf;
        bin_op->right->call_leaf = expr->call_leaf;


        compile_type_of_expression(comp, bin_op->left, state);
        if (comp->is_panic()) {
          return;
        }

        compile_type_of_expression(comp, bin_op->right, state);
        if (comp->is_panic()) {
          return;
        }

        if (bin_op->left->comptime_eval && bin_op->right->comptime_eval) {
          expr->comptime_eval = true;
        }
        else if (can_load_const_value(bin_op->left)) {
          expr->comptime_eval = false;

          make_constant_typed_dependency(comp, bin_op->left, nullptr);
          return;//guaranteed to have panic
        }
        else if (can_load_const_value(bin_op->right)) {
          expr->comptime_eval = false;

          make_constant_typed_dependency(comp, bin_op->right, nullptr);
          return;//guaranteed to have panic
        }
        else {
          expr->comptime_eval = false;
        }

        expr->makes_call = bin_op->left->makes_call || bin_op->right->makes_call;

        switch (expr->bin_op.op) {
          case BINARY_OPERATOR::ADD:
            compile_binary_operator(comp, expr, add_operators);
            break;
          case BINARY_OPERATOR::SUB:
            compile_binary_operator(comp, expr, sub_operators);
            break;
          case BINARY_OPERATOR::MUL:
            compile_binary_operator(comp, expr, mul_operators);
            break;
          case BINARY_OPERATOR::DIV:
            compile_binary_operator(comp, expr, div_operators);
            break;
          case BINARY_OPERATOR::EQUIVALENT:
            compile_binary_operator(comp, expr, eq_operators);
            break;
          case BINARY_OPERATOR::OR:
            compile_binary_operator(comp, expr, or_operators);
            break;
          case BINARY_OPERATOR::AND:
            compile_binary_operator(comp, expr, and_operators);
            break;
          case BINARY_OPERATOR::LEFT_SHIFT:
            compile_binary_operator(comp, expr, left_shift_operators);
            break;
          case BINARY_OPERATOR::RIGHT_SHIFT:
            compile_binary_operator(comp, expr, right_shift_operators);
            break;
          default: {
              const char* const name = BINARY_OP_STRING::get(expr->bin_op.op);

              comp->report_error(CompileCode::INTERNAL_ERROR, expr->span,
                                 "Type Checking is not implemented for binary operator '{}'",
                                 name);
              return;
            }
        }

        //Type check might have failed in binary operator
        if (comp->is_panic()) {
          return;
        }

        break;
      }
    case EXPRESSION_TYPE::FUNCTION_CALL: {
        FunctionCallExpr* const call = &expr->call;

        expr->comptime_eval = true;

        {
          auto i = call->arguments.mut_begin();
          const auto end = call->arguments.mut_end();

          for (; i < end; i++) {
            i->call_leaf = true;

            compile_type_of_expression(comp, i, state);
            if (comp->is_panic()) {
              return;
            }

            expr->comptime_eval &= i->comptime_eval;
          }
        }

        if (call->function == nullptr) {
          compile_find_function_call(comp, expr);
          if (comp->is_panic()) {
            return;
          }
        }

        const auto* func = call->function;

        {
          const size_t size = func->parameter_types.size;

          if (call->arguments.size != size) {
            comp->report_error(CompileCode::INTERNAL_ERROR, expr->span,
                               "Compiler linked a function with {} parameters for a call with {} arguments!",
                               size, call->arguments.size);
            return;
          }

          for (size_t i = 0; i < size; i++) {
            const Structure* param_t = func->parameter_types.data[i];
            ASTExpression* arg_expr = call->arguments.data + i;

            compile_implicit_cast(comp, arg_expr, param_t);
            if (comp->is_panic()) {
              return;
            }
          }
        }

        //Cant currently call functions at compile time
        expr->comptime_eval = false;

        if (!expr->comptime_eval) {
          auto i = call->arguments.mut_begin();
          const auto end = call->arguments.mut_end();

          for (; i < end; i++) {
            if (can_load_const_value(i)) {
              make_constant_typed_dependency(comp, i, nullptr);
              if (comp->is_panic()) {
                return;
              }
            }
          }
        }

        expr->type = call->function->return_type;
        expr->makes_call = true;

        break;
      }
    default: {
        comp->report_error(CompileCode::INTERNAL_ERROR, expr->span,
                           "Invalid Expression type found! Expression id: '{}'",
                           (int)expr->expr_type);
        return;
      }
  }

  assert(expr->type != nullptr);
}

static void compile_type_of_statement(Compiler* const comp,
                                      Function* const func,
                                      State* const state,
                                      ASTStatement* const statement) {
  switch (statement->type) {
    case STATEMENT_TYPE::IF_ELSE: {
        ASTIfElse* const if_else = &statement->if_else;

        if (if_else->condition.type == nullptr) {
          compile_type_of_expression(comp, &if_else->condition, state);
          if (comp->is_panic()) {
            return;
          }

          compile_implicit_cast(comp, &if_else->condition, comp->types->s_bool);
          if (comp->is_panic()) {
            return;
          }
        }

        compile_type_of_statement(comp, func, state, if_else->if_statement);
        if (comp->is_panic()) {
          return;
        }

        compile_type_of_statement(comp, func, state, if_else->else_statement);
        if (comp->is_panic()) {
          return;
        }

        return;
      }
    case STATEMENT_TYPE::BLOCK: {
        auto locals = state->active_locals.size;
        DEFER(&) { state->active_locals.size = locals; };

        auto i = statement->block.block.mut_begin();
        const auto end = statement->block.block.mut_end();

        for (; i < end; i++) {
          compile_type_of_statement(comp, func, state, i);
          if (comp->is_panic()) {
            return;
          }
        }

        return;
      }
    case STATEMENT_TYPE::DECLARATION: {
        ASTDeclaration* const decl = &statement->declaration;

        if (decl->type.type == nullptr) {
          compile_type(comp, &decl->type);
          if (comp->is_panic()) {
            return;
          }
        }
        assert(decl->type.type != nullptr);

        if (decl->expression.type == nullptr) {
          compile_type_of_expression(comp, &decl->expression, state);
          if (comp->is_panic()) {
            return;
          }

          assert(decl->expression.type != nullptr);

          compile_implicit_cast(comp, &decl->expression, decl->type.type);
          if (comp->is_panic()) {
            return;
          }

          if (can_load_const_value(&decl->expression)) {
            make_constant_typed_dependency(comp, &decl->expression, nullptr);
            return;
          }
        }
        assert(decl->expression.type != nullptr);

        const Local* shadowing = state->find_local(decl->name);

        if (shadowing != nullptr) {
          comp->report_error(CompileCode::NAME_ERROR, statement->span,
                             "Attempted to shadow local variable '{}'",
                             decl->name);
          return;
        }

        size_t loc_index = state->all_locals.size;
        decl->local_index = loc_index;
        state->active_locals.insert(loc_index);

        state->all_locals.insert_uninit(1);
        auto* loc = state->all_locals.back();

        loc->name = decl->name;
        loc->type = decl->type.type;

        return;
      }
    case STATEMENT_TYPE::EXPRESSION: {
        ASTExpression* const expr = &statement->expression;
        if (expr->type == nullptr) {
          compile_type_of_expression(comp, expr, state);
          if (comp->is_panic()) {
            return;
          }

          assert(expr->type != nullptr);

          if (can_load_const_value(expr)) {
            make_constant_typed_dependency(comp, expr, nullptr);
            return;
          }
        }

        return;
      }
    case STATEMENT_TYPE::RETURN: {
        ASTExpression* const expr = &statement->expression;

        if (expr->type == nullptr) {
          compile_type_of_expression(comp, expr, state);
          if (comp->is_panic()) {
            return;
          }

          assert(expr->type != nullptr);

          compile_implicit_cast(comp, expr, func->return_type);
          if (comp->is_panic()) {
            return;
          }

          if (can_load_const_value(expr)) {
            make_constant_typed_dependency(comp, expr, nullptr);
            return;
          }
        }

        assert(expr->type != nullptr);
        return;
      }
  }

  comp->report_error(CompileCode::INTERNAL_ERROR, statement->span,
                     "Reached end of statement type checking without exiting\n"
                     "Statement type: {}\n", statement->type);
}

static void load_const_to_mem(Compiler* const comp,
                              State* const state,
                              CodeBlock* const code,
                              const Structure* type,
                              ConstantVal constant,
                              MemIndex mem) {

  const uint32_t size = type->size();
  const uint32_t align = type->alignment();

  const size_t s_div_8 = size / 8;
  const size_t s_mod_8 = size % 8;

  assert(size == constant.size);

  //Needs to go on stack
  auto* const mem_val = state->mem_values.data + mem.index;
  assert(mem_val->size >= size);

  int64_t* iptr = (int64_t*)constant.ptr;

  MemComplex indexed_mem = mem_val->mem;

  if (comp->build_options.system == &system_x86_64) {
    auto mov_val = state->new_value();

    for (size_t itr = 0; itr < s_div_8; itr++) {
      const int64_t val = *iptr;

      if (can_be_from_sign_extension(val)) {
        //Can just load the value as 32 bits and it will be sign extended
        ByteCode::EMIT::COPY_64_TO_MEM(code->code, val, indexed_mem);
        state->control_flow.expression_num++;
      }
      else {
        ByteCode::EMIT::SET_R64_TO_64(code->code, (uint8_t)mov_val.val, *iptr);
        state->set_value(mov_val);
        state->control_flow.expression_num++;

        ByteCode::EMIT::COPY_R64_TO_MEM(code->code, (uint8_t)mov_val.val, indexed_mem);
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

  const uint64_t last = *(const uint64_t*)iptr;

  switch (s_mod_8) {
    case 0: break;//Already loaded
    case 1:
      ByteCode::EMIT::COPY_8_TO_MEM(code->code, (uint8_t)(last & 0xFF), indexed_mem);
      break;
    case 2:
      ByteCode::EMIT::COPY_16_TO_MEM(code->code, (uint16_t)(last & 0xFFFF), indexed_mem);
      break;
    case 3:
      ByteCode::EMIT::COPY_16_TO_MEM(code->code, (uint16_t)(last & 0xFFFF), indexed_mem);
      indexed_mem.disp += 2;
      ByteCode::EMIT::COPY_8_TO_MEM(code->code, (uint8_t)((last & 0xFF0000) >> 16), indexed_mem);
      break;
    case 4:
      ByteCode::EMIT::COPY_32_TO_MEM(code->code, (uint32_t)(last & 0xFFFFFFFF), indexed_mem);
      break;
    case 5:
      ByteCode::EMIT::COPY_32_TO_MEM(code->code, (uint32_t)(last & 0xFFFFFFFF), indexed_mem);
      indexed_mem.disp += 4;
      ByteCode::EMIT::COPY_8_TO_MEM(code->code, (uint8_t)((last & 0xFF00000000) >> 32), indexed_mem);
      break;
    case 6:
      ByteCode::EMIT::COPY_32_TO_MEM(code->code, (uint32_t)(last & 0xFFFFFFFF), indexed_mem);
      indexed_mem.disp += 4;
      ByteCode::EMIT::COPY_16_TO_MEM(code->code, (uint16_t)((last & 0xFFFF00000000) >> 32), indexed_mem);
      break;
    case 7:
      ByteCode::EMIT::COPY_32_TO_MEM(code->code, (uint32_t)(last & 0xFFFFFFFF), indexed_mem);
      indexed_mem.disp += 4;
      ByteCode::EMIT::COPY_16_TO_MEM(code->code, (uint16_t)((last & 0xFFFF00000000) >> 32), indexed_mem);
      indexed_mem.disp += 2;
      ByteCode::EMIT::COPY_8_TO_MEM(code->code, (uint8_t)((last & 0xFF000000000000) >> 48), indexed_mem);
      break;
    default:
      assert(false);//Logically should never ever be able to get here ...
  }

  comp->constants.free_no_destruct((void*)constant.ptr);
  state->control_flow.expression_num++;
}

static void copy_mem_to_mem(Compiler* const comp,
                            State* const state,
                            CodeBlock* const code,
                            const Structure* type,
                            MemIndex from,
                            MemIndex to) {

  const uint32_t size = type->size();
  const uint32_t align = type->alignment();

  const size_t s_div_8 = size / 8;
  size_t s_mod_8 = size % 8;

  //Needs to go on stack
  auto* const to_val = state->mem_values.data + to.index;
  assert(to_val->size >= size);

  MemComplex to_mem = to_val->mem;

  auto* const from_val = state->mem_values.data + from.index;
  assert(from_val->size >= size);

  MemComplex from_mem = from_val->mem;



  auto mov_val = state->new_value();

  for (size_t itr = 0; itr < s_div_8; itr++) {
    ByteCode::EMIT::COPY_R64_FROM_MEM(code->code, (uint8_t)mov_val.val, from_mem);
    state->set_value(mov_val);
    state->control_flow.expression_num++;

    ByteCode::EMIT::COPY_R64_TO_MEM(code->code, (uint8_t)mov_val.val, to_mem);
    state->use_value(mov_val);
    state->control_flow.expression_num++;

    from_mem.disp += 8;
    to_mem.disp += 8;
  }

  while (s_mod_8 > 0) {
    switch (s_mod_8) {
      case 1: {
          ByteCode::EMIT::COPY_R8_FROM_MEM(code->code, (uint8_t)mov_val.val, from_mem);
          state->set_value(mov_val);
          state->control_flow.expression_num++;

          ByteCode::EMIT::COPY_R8_TO_MEM(code->code, (uint8_t)mov_val.val, to_mem);
          state->use_value(mov_val);
          state->control_flow.expression_num++;

          from_mem.disp += 1;
          to_mem.disp += 1;
          s_mod_8 -= 1;
          break;
        }
      case 2: {
          ByteCode::EMIT::COPY_R16_FROM_MEM(code->code, (uint8_t)mov_val.val, from_mem);
          state->set_value(mov_val);
          state->control_flow.expression_num++;

          ByteCode::EMIT::COPY_R16_TO_MEM(code->code, (uint8_t)mov_val.val, to_mem);
          state->use_value(mov_val);
          state->control_flow.expression_num++;

          from_mem.disp += 2;
          to_mem.disp += 2;
          s_mod_8 -= 2;
          break;
        }
      case 4: {
          ByteCode::EMIT::COPY_R32_FROM_MEM(code->code, (uint8_t)mov_val.val, from_mem);
          state->set_value(mov_val);
          state->control_flow.expression_num++;

          ByteCode::EMIT::COPY_R32_TO_MEM(code->code, (uint8_t)mov_val.val, to_mem);
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
  switch (s->size()) {
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
      assert(false);//Cant handle register sizes that arent a power of 2
  }
}

static void emit_copy_r_from_mem(const Structure* s, Array<uint8_t>& arr, uint8_t r, const MemComplex& mem) {
  switch (s->size()) {
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
      assert(false);//Cant handle register sizes that arent a power of 2
  }
}

static void emit_copy_r_to_r(const Structure* s, Array<uint8_t>& arr, uint8_t r1, uint8_t r2) {
  switch (s->size()) {
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
      assert(false);//Cant handle register sizes that arent a power of 2
  }
}

static void load_const_to_reg(Compiler* const comp,
                              State* const state,
                              CodeBlock* const code,
                              ConstantVal constant,
                              ValueIndex reg) {
  assert(constant.size <= 8);

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
      assert(false);
  }

  comp->constants.free_no_destruct((void*)constant.ptr);

  state->set_value(reg);
  state->control_flow.expression_num++;
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
  assert(to->type != RVT::CONST);

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
        DEFER(&) { state->control_flow.expression_num++; };

        emit_copy_r_from_mem(type, code->code, (uint8_t)to->reg.val, state->get_mem(mem)->mem);

        state->set_value(to->reg);
        break;
      }
  }
}

static void copy_reg_to_runtime(Compiler* const comp,
                                State* const state,
                                CodeBlock* const code,
                                const Structure* type,
                                ValueIndex reg,
                                RuntimeValue* to) {
  assert(to->type != RVT::CONST);

  size_t size = type->size();
  assert(size <= 8);

  switch (to->type) {
    case RVT::MEMORY: {
        emit_copy_r_to_mem(type, code->code, (uint8_t)reg.val, state->get_mem(to->mem)->mem);
        state->use_value(reg);
        break;
      }
    case RVT::REGISTER: {
        if (reg == to->reg) {
          return;//are the same so no need to copy
        }

        emit_copy_r_to_r(type, code->code, (uint8_t)reg.val, (uint8_t)to->reg.val);
        state->value_copy(reg, to->reg);
        break;
      }
    default: {
        to->type = RVT::REGISTER;
        to->reg = state->new_value();

        emit_copy_r_to_r(type, code->code, (uint8_t)reg.val, (uint8_t)to->reg.val);
        state->value_copy(reg, to->reg);
        break;
      }
  }
}

void copy_runtime_to_runtime(Compiler* const comp,
                             State* const state,
                             CodeBlock* const code,
                             const Structure* type,
                             const RuntimeValue* from,
                             RuntimeValue* to) {
  if (*from == *to) {
    return;
  }

  DEFER(&) { state->control_flow.expression_num++; };

  assert(from->type != RVT::UNKNOWN);

  switch (from->type) {
    case RVT::CONST: {
        load_const_to_runtime_val(comp, state, code, type, from->constant, to);
        break;
      }
    case RVT::REGISTER: {
        copy_reg_to_runtime(comp, state, code, type, from->reg, to);
        break;
      }
    case RVT::MEMORY: {
        copy_mem_to_runtime(comp, state, code, type, from->mem, to);
      }
  }
}

static void load_runtime_hint(Compiler* const comp,
                              State* const state,
                              const Structure* type,
                              RuntimeHint* hint,
                              uint8_t possible) {
  assert(hint->is_hint);
  hint->is_hint = false;

  assert(possible != 0);//Cant have no options ever
  possible &= hint->hint_types;
  assert(possible != 0);//Cant have overlapping options

  if ((possible & (uint8_t)RVT::CONST) > 0) {
    hint->val.type = RVT::CONST;
  }
  else if ((possible & (uint8_t)RVT::REGISTER) > 0) {
    hint->val.type = RVT::REGISTER;
    hint->val.reg = state->new_value();
  }
  else if ((possible & (uint8_t)RVT::MEMORY) > 0) {
    hint->val.type = RVT::MEMORY;
    hint->val.mem.index = state->mem_values.size;

    state->mem_values.insert_uninit(1);

    size_t size = type->size();
    size_t alignment = type->alignment();

    const ValueIndex rbp = state->new_value();
    state->set_value(rbp);

    auto& rbp_val = state->value_tree.values.data[rbp.val];

    rbp_val.value_type = ValueType::FIXED;
    rbp_val.reg = comp->build_options.calling_convention->base_pointer_reg;

    auto* stack_val = state->mem_values.back();
    stack_val->mem.base = (uint8_t)rbp.val;
    stack_val->mem.disp = state->stack.next_stack_local(size, alignment);
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
    assert(((uint8_t)to->val.type & possible_types) > 0);
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
                                           State* const state,
                                           CodeBlock* const code,
                                           const ASTExpression* const expr,
                                           RuntimeHint* hint);

static RuntimeValue compile_bytecode_of_expression_new(Compiler* const comp,
                                                       State* const state,
                                                       CodeBlock* const code,
                                                       const ASTExpression* const expr,
                                                       uint8_t hint) {
  RuntimeHint rt_hint ={};
  rt_hint.is_hint = true;
  rt_hint.hint_types = hint;
  compile_bytecode_of_expression(comp, state, code, expr, &rt_hint);

  assert(!rt_hint.is_hint);
  assert(((uint8_t)rt_hint.val.type & hint) > 0);
  return rt_hint.val;
}

static void compile_bytecode_of_expression_existing(Compiler* const comp,
                                                    State* const state,
                                                    CodeBlock* const code,
                                                    const ASTExpression* const expr,
                                                    RuntimeValue* hint) {
  RuntimeHint rt_hint ={};
  rt_hint.is_hint = false;
  rt_hint.val = *hint;

  compile_bytecode_of_expression(comp, state, code, expr, &rt_hint);

  assert(rt_hint.val == *hint);
}

//Note: Recursive 
static void compile_bytecode_of_expression(Compiler* const comp,
                                           State* const state,
                                           CodeBlock* const code,
                                           const ASTExpression* const expr,
                                           RuntimeHint* hint) {
  DEFER(&) {
    assert(!hint->is_hint);
    state->control_flow.expression_num++;
  };

  if (expr->comptime_eval && !state->comptime_compilation && !already_const_type(expr->expr_type)) {
    //Compile time expression
    assert(expr->const_val != nullptr);

    //Copy the value to a new constant
    const size_t size = expr->type->size();

    uint8_t* bytes = (uint8_t*)comp->constants.alloc_no_construct(size);
    const uint8_t* c_bytes = (const uint8_t*)expr->const_val;

    memcpy_ts(bytes, size, c_bytes, size);

    const ConstantVal constant ={ bytes, size };

    if (hint->is_hint) {
      load_runtime_hint(comp, state, expr->type, hint, expr->valid_rvts);
    }

    load_const_to_runtime_val(comp, state, code, expr->type, constant, &hint->val);
    return;
  }

  switch (expr->expr_type) {
    case EXPRESSION_TYPE::INDEX: {

        const size_t base_size = expr->type->size();

        assert(expr->index.expr->type->type == STRUCTURE_TYPE::FIXED_ARRAY);

        RuntimeValue arr = compile_bytecode_of_expression_new(comp,
                                                              state,
                                                              code,
                                                              expr->index.expr,
                                                              RVT::REGISTER | RVT::MEMORY);

        RuntimeValue index_val = compile_bytecode_of_expression_new(comp,
                                                                    state,
                                                                    code,
                                                                    expr->index.index,
                                                                    RVT::REGISTER | RVT::CONST);

        if (hint->is_hint) {
          load_runtime_hint(comp, state, expr->type, hint, expr->valid_rvts & NON_CONST_RVTS);
        }

        if (arr.type == RVT::REGISTER) {
          //If its in a register then a shift is equivalent to the 
          RuntimeValue use_val ={};
          use_val.type = RVT::REGISTER;
          use_val.reg = state->new_value();

          RuntimeValue res;
          if (index_val.type == RVT::CONST) {
            auto index = *(uint64_t*)index_val.constant.ptr;

            const uint8_t shift = (uint8_t)(base_size * index * 8);

            ByteCode::EMIT::SET_R8_TO_8(code->code, (uint8_t)use_val.reg.val, shift);
            state->set_value(use_val.reg);
            state->control_flow.expression_num++;

            res = OP::emit_shift_r_u64_by_8(comp, state, code, &arr, &use_val);
          }
          else {
            assert(index_val.type == RVT::REGISTER);

            ByteCode::EMIT::SET_R8_TO_8(code->code, (uint8_t)use_val.reg.val, (uint8_t)(base_size * 8));
            state->set_value(use_val.reg);
            state->control_flow.expression_num++;

            RuntimeValue fixed_index = OP::emit_mul_64s(comp, state, code, &use_val, &index_val);

            res = OP::emit_shift_r_u64_by_8(comp, state, code, &arr, &fixed_index);
          }

          copy_runtime_to_runtime(comp, state, code, expr->type, &res, &hint->val);
        }
        else {

          const MemIndex arr_index = state->new_mem();

          assert(arr.type == RVT::MEMORY);
          //In memory - do a memory index

          if (index_val.type == RVT::CONST) {
            auto index = *(uint64_t*)index_val.constant.ptr;

            {
              const MemValue* arr_mem = state->get_mem(arr.mem);
              MemValue* index_mem = state->get_mem(arr_index);

              const size_t disp_index = (base_size * index);
              assert(arr_mem->size >= disp_index);//Semi Bounds checking

              index_mem->mem = arr_mem->mem;
              index_mem->size = base_size;
              index_mem->mem.disp += (int32_t)disp_index;
            }

            copy_mem_to_runtime(comp, state, code, expr->type, arr_index, &hint->val);
          }
          else {
            assert(index_val.type == RVT::REGISTER);
            ValueIndex index = index_val.reg;

            {
              const MemValue* arr_mem = state->get_mem(arr.mem);
              MemValue* index_mem = state->get_mem(arr_index);

              assert(arr_mem->mem.scale == 0);

              index_mem->mem = arr_mem->mem;
              index_mem->size = base_size;
              index_mem->mem.index = (uint8_t)index.val;
              index_mem->mem.scale = (uint8_t)base_size;
            }

            copy_mem_to_runtime(comp, state, code, expr->type, arr_index, &hint->val);
          }
        }

        break;
      }
    case EXPRESSION_TYPE::ARRAY_EXPR: {
        const ArrayStructure* const arr_type = (const ArrayStructure*)expr->type;

        const size_t base_size = arr_type->base->size();

        const size_t full_align = arr_type->alignment();
        const size_t full_size = arr_type->size();

        if (hint->is_hint) {
          load_runtime_hint(comp, state, expr->type, hint, expr->valid_rvts);
        }

        assert(hint->val.type == RVT::MEMORY);

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
          compile_bytecode_of_expression_existing(comp, state, code, i, &arr_single);

          state->stack.current = save_stack;//reset stack
          state->get_mem(arr_single.mem)->mem.disp += (int32_t)base_size;
        }
        break;
      }
    case EXPRESSION_TYPE::ASCII_STRING: {
        const ArrayStructure* const arr_type = (const ArrayStructure*)expr->type;

        const size_t size = arr_type->size();
        char* string_c = (char*)comp->constants.alloc_no_construct(size);

        const ConstantVal constant ={ (uint8_t*)string_c, size };

        memcpy_ts(string_c, size, expr->ascii_string->string, size);

        if (hint->is_hint) {
          load_runtime_hint(comp, state, expr->type, hint, expr->valid_rvts);
        }

        load_const_to_runtime_val(comp, state, code, expr->type, constant, &hint->val);
        break;
      }
    case EXPRESSION_TYPE::ENUM: {
        assert(expr->type->size() == 8);
        uint8_t* enum_c = comp->constants.alloc_no_construct(8);
        x64_to_bytes(expr->enum_value.enum_value->representation, enum_c);

        const ConstantVal constant ={ (uint8_t*)enum_c, 8 };

        if (hint->is_hint) {
          load_runtime_hint(comp, state, expr->type, hint, expr->valid_rvts);
        }

        load_const_to_runtime_val(comp, state, code, expr->type, constant, &hint->val);
        break;
      }
    case EXPRESSION_TYPE::VALUE: {
        const size_t size = expr->type->size();

        uint8_t* val_c = comp->constants.alloc_no_construct(size);
        memcpy_ts(val_c, size, (uint8_t*)&expr->value.value, size);

        const ConstantVal constant ={ val_c, size };

        if (hint->is_hint) {
          load_runtime_hint(comp, state, expr->type, hint, expr->valid_rvts);
        }

        load_const_to_runtime_val(comp, state, code, expr->type, constant, &hint->val);
        break;
      }
    case EXPRESSION_TYPE::LOCAL: {
        Local* local = state->all_locals.data + expr->local;

        if ((hint->hint_types & (uint8_t)RVT::CONST) > 0 && local->val.type == RVT::CONST) {
          //Need to actively copy the constant. To stop it from being freed

          ConstantVal copy_to ={};
          const ConstantVal to_copy = local->val.constant;

          copy_to.ptr = comp->constants.alloc_no_construct(to_copy.size);
          copy_to.size = to_copy.size;

          memcpy_ts(copy_to.ptr, copy_to.size, to_copy.ptr, copy_to.size);

          hint->is_hint = false;
          hint->val.type     = RVT::CONST;
          hint->val.constant = copy_to;
        }
        else {
          copy_runtime_to_runtime_hint(comp, state, code, expr->type, &local->val, hint, expr->valid_rvts & NON_CONST_RVTS);
        }

        break;
      }
    case EXPRESSION_TYPE::CAST: {
        const CastExpr* const cast = &expr->cast;
        RuntimeValue temp = compile_bytecode_of_expression_new(comp, state, code, cast->expr, ALL_RVTS);

        RuntimeValue rt = cast->emit(comp, state, code, &temp);

        copy_runtime_to_runtime_hint(comp, state, code, expr->type, &rt, hint, expr->valid_rvts & NON_CONST_RVTS);

        break;
      }
    case EXPRESSION_TYPE::UNARY_OPERATOR: {
        const UnaryOperatorExpr* const un_op = &expr->un_op;

        RuntimeValue temp = compile_bytecode_of_expression_new(comp,
                                                               state,
                                                               code,
                                                               un_op->primary,
                                                               ALL_RVTS);

        RuntimeValue rt = un_op->emit(comp, state, code, &temp);

        copy_runtime_to_runtime_hint(comp, state, code, expr->type, &rt, hint, expr->valid_rvts & NON_CONST_RVTS);
        break;
      }
    case EXPRESSION_TYPE::BINARY_OPERATOR: {
        const BinaryOperatorExpr* const bin_op = &expr->bin_op;
        const ASTExpression* const left = bin_op->left;
        const ASTExpression* const right = bin_op->right;

        RuntimeValue temp_left = compile_bytecode_of_expression_new(comp,
                                                                    state,
                                                                    code,
                                                                    left,
                                                                    ALL_RVTS);

        RuntimeValue temp_right = compile_bytecode_of_expression_new(comp,
                                                                     state,
                                                                     code,
                                                                     right,
                                                                     ALL_RVTS);

        RuntimeValue res = bin_op->emit(comp, state, code, &temp_left, &temp_right);

        copy_runtime_to_runtime_hint(comp, state, code, expr->type, &res, hint, expr->valid_rvts & NON_CONST_RVTS);
        break;
      }
    case EXPRESSION_TYPE::FUNCTION_CALL: {
        const FunctionCallExpr* const call = &expr->call;

        auto save_stack_params = state->stack.current_parameters;
        DEFER(&) { state->stack.current_parameters = save_stack_params; };

        state->made_call = true;

        Array<RuntimeValue> to_use_values;
        to_use_values.reserve_total(call->arguments.size);

        //Compile expression for arguments
        {
          const size_t size = call->arguments.size;

          for (size_t i = 0; i < size; i++) {
            const ASTExpression* inner_expr = call->arguments.data + i;
            const Structure* call_type = call->function->parameter_types.data[i];


            const RuntimeValue val = compile_bytecode_of_expression_new(comp, state, code, inner_expr, ALL_RVTS);

            to_use_values.insert(val);
          }
        }

        state->control_flow.expression_num++;

        //Set argument registers and stack
        {
          const auto* parameter_regs = comp->build_options.calling_convention->parameter_registers;
          const size_t max_reg_params = comp->build_options.calling_convention->num_parameter_registers;
          const size_t num_reg_params = smaller(to_use_values.size, max_reg_params - call->function->return_via_ptr);
          const size_t num_stack_params = to_use_values.size - num_reg_params;

          //Load stack stuff first
          state->stack.push_stack_params(num_stack_params);

          for (size_t index = num_reg_params; index < to_use_values.size; index++) {
            auto i = to_use_values.data + index;
            const Structure* call_type = call->function->parameter_types.data[index];

            RuntimeValue val ={};
            val.type = RVT::MEMORY;
            val.mem  = state->new_mem();
            const ValueIndex rbp = state->new_value();
            state->set_value(rbp);

            {
              auto& rbp_val = state->value_tree.values.data[rbp.val];

              rbp_val.value_type = ValueType::FIXED;
              rbp_val.reg = comp->build_options.calling_convention->base_pointer_reg;
            }

            auto* stack_val = state->get_mem(val.mem);

            stack_val->mem.base = (uint8_t)rbp.val;
            stack_val->mem.disp = (int32_t)index - (int32_t)max_reg_params + (int32_t)state->stack.current_parameters;

            copy_runtime_to_runtime(comp, state, code, call_type, i, &val);
          }

          //The Load registers

          if (call->function->return_via_ptr) {
            //Load the return on the stack and then pass a pointer
            load_runtime_hint(comp, state, call->function->return_type, hint, (uint8_t)RVT::MEMORY);

            RuntimeValue val = OP::emit_address(comp, state, code, &hint->val);
            state->control_flow.expression_num++;

            auto* arg_val = state->value_tree.values.data + val.reg.val;

            arg_val->value_type = ValueType::FIXED;
            arg_val->reg        = parameter_regs[0];
            parameter_regs++;//so that index = 0 will access element at [1]

            to_use_values.insert(val);//make sure it gets used at the call
          }

          for (size_t index = 0; index < num_reg_params; index++) {
            auto i = to_use_values.data + index;
            const Structure* arg_type = call->function->parameter_types.data[index];

            RuntimeValue val ={};
            val.type = RVT::REGISTER;
            val.reg = state->new_value();

            auto* arg_val = state->value_tree.values.data + val.reg.val;

            arg_val->value_type = ValueType::FIXED;
            arg_val->reg        = comp->build_options.calling_convention->parameter_registers[index];

            copy_runtime_to_runtime(comp, state, code, arg_type, i, &val);

            *i = val;
          }
        }

        //use all the registers
        {
          auto i = to_use_values.begin();
          const auto end = to_use_values.end();

          for (; i < end; i++) {
            //Cant exit early as the last value might be a register
            if (i->type == RVT::REGISTER) {
              state->use_value(i->reg);
            }
          }
        }

        ByteCode::EMIT::CALL(code->code, call->function->code_block.label);
        state->control_flow.had_call = true;
        state->control_flow.last_call = state->control_flow.expression_num;

        state->control_flow.expression_num++;

        if (!call->function->return_via_ptr) {
          //Need to reserve RAX if we didnt already pass a pointer in
          const ValueIndex rax = state->new_value();
          state->set_value(rax);//set by the called function

          {
            auto* rax_val = state->value_tree.values.data + rax.val;

            rax_val->value_type = ValueType::FIXED;
            rax_val->reg        = comp->build_options.calling_convention->return_register;
          }

          //Fake copy so dont need to insert copy later if one is needed
          state->control_flow.expression_num++;

          if (hint->is_hint) {
            load_runtime_hint(comp, state, expr->type, hint, expr->valid_rvts & NON_CONST_RVTS);
          }

          copy_reg_to_runtime(comp, state, code, expr->type, rax, &hint->val);
        }
        break;
      }
  }
}

void compile_bytecode_of_statement(Compiler* const comp,
                                   ASTStatement* const statement,
                                   State* const state,
                                   CodeBlock* const code) {
  switch (statement->type) {
    case STATEMENT_TYPE::BLOCK: {
        const auto num_locals = state->active_locals.size;
        DEFER(&) { state->active_locals.size = num_locals; };

        auto i = statement->block.block.mut_begin();
        const auto end = statement->block.block.mut_end();

        for (; i < end; i++) {
          compile_bytecode_of_statement(comp, i, state, code);
        }

        return;
      }
    case STATEMENT_TYPE::RETURN: {
        const ASTExpression* expr = &statement->expression;

        RuntimeValue rax_v ={};
        rax_v.type = RVT::REGISTER;
        rax_v.reg = state->new_value();

        {
          auto& rax_val = state->value_tree.values.data[rax_v.reg.val];

          rax_val.value_type = ValueType::FIXED;
          rax_val.reg        = comp->build_options.calling_convention->return_register;
        }

        compile_bytecode_of_expression_existing(comp,
                                                state,
                                                code,
                                                expr,
                                                &rax_v);

        state->use_value(rax_v.reg);

        ByteCode::EMIT::JUMP_TO_FIXED(code->code, state->return_label);
        return;
      }
    case STATEMENT_TYPE::EXPRESSION: {
        auto a = compile_bytecode_of_expression_new(comp, state, code, &statement->expression, ALL_RVTS);
        return;
      }
    case STATEMENT_TYPE::IF_ELSE: {
        ASTIfElse* const if_else = &statement->if_else;

        RuntimeValue cond = compile_bytecode_of_expression_new(comp,
                                                               state,
                                                               code,
                                                               &if_else->condition,
                                                               RVT::CONST | RVT::REGISTER);

        if (cond.type == RVT::CONST) {
          //Just becomes a block - no need for flows and stuff

          const auto locals = state->active_locals.size;
          DEFER(&) { state->active_locals.size = locals; };

          if (*(uint64_t*)cond.constant.ptr != 0) {
            //Compile if branch
            compile_bytecode_of_statement(comp, if_else->if_statement, state, code);
          }
          else {
            //Compile else branch
            compile_bytecode_of_statement(comp, if_else->else_statement, state, code);
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
          compile_bytecode_of_statement(comp, if_else->if_statement, state, code);

          state->active_locals.size = locals;

          const size_t end_if_flow = state->control_flow.current_flow;

          //Jump from if branch to after the else branch
          const uint64_t escape_label = comp->labels++;
          ByteCode::EMIT::JUMP_TO_FIXED(code->code, escape_label);
          ByteCode::EMIT::LABEL(code->code, else_label);

          //Else branch
          state->control_flow.new_flow();
          state->control_flow.set_a_flows_to_b(start_flow, state->control_flow.current_flow);
          compile_bytecode_of_statement(comp, if_else->else_statement, state, code);

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
    case STATEMENT_TYPE::DECLARATION: {
        ASTDeclaration* const decl = &statement->declaration;

        Local* const local = state->all_locals.data + decl->local_index;

        const uint8_t valid_hints = local->valid_rvts & (comp->optimization_options.non_stack_locals ?
                                                         ALL_RVTS
                                                         : (uint8_t)RVT::MEMORY);

        local->val = compile_bytecode_of_expression_new(comp,
                                                        state,
                                                        code,
                                                        &decl->expression,
                                                        valid_hints);
        return;
      }
  }
}


static void map_values(const BuildOptions* const options,
                       CodeBlock* const code, const State* const state, uint64_t regs) {
  //Only non volatiles
  regs &= options->calling_convention->non_volatiles_bit_mask;

  Array<uint8_t> temp ={};

  //Prolog

  //Call label
  ByteCode::EMIT::LABEL(temp, code->label);

  int64_t base_pointer_offset = 0;
  int64_t offset_to_prev_frame = 8;//return pointer

  if (state->needs_new_frame()) {
    ByteCode::EMIT::PUSH_FRAME(temp);

    offset_to_prev_frame += 8;//pushed rbp

    const uint8_t num_regs = options->system->num_registers;
    uint8_t non_v_regs = 0;
    for (; non_v_regs < num_regs; non_v_regs++) {
      if (regs & ((uint64_t)1 << non_v_regs)) {
        base_pointer_offset -= 8;
        ByteCode::EMIT::PUSH_R64(temp, options->system->all_registers[non_v_regs].REG);
      }
    }
  }

  //Finally allocate the stack
  const uint64_t stack_needed = state->stack.max
    + ((uint64_t)state->made_call * options->calling_convention->shadow_space_size)
    + (state->stack.max_parameters * 8)
    + (-base_pointer_offset);

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

    if (b->reg == options->calling_convention->base_pointer_reg) {
      if (!state->needs_new_frame()) {
        //No new frame, use stack pointer instead
        out_mem.base = options->calling_convention->stack_pointer_reg;
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
      assert(s == 0 || s == 1 || s == 2 || s == 4 || s == 8);

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
    uint8_t non_v_regs = options->system->num_registers + 1;
    const uint8_t num_regs = 0;
    for (; non_v_regs > num_regs; non_v_regs--) {
      if (regs & ((uint64_t)1 << (non_v_regs - 1))) {
        MemComplex mem ={};
        mem.base = options->calling_convention->base_pointer_reg;
        mem.disp = (int32_t)base_pointer_offset;
        mem.scale = 0;

        ByteCode::EMIT::COPY_R64_FROM_MEM(temp,
                                          options->system->all_registers[(non_v_regs - 1)].REG,
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

static uint64_t select(const BuildOptions* const options, State* const state) noexcept {
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
        auto* other_val = tree.values.data + intersects_i->val;
        if (other_val->fixed()) {
          regs |= ((uint64_t)1 << (other_val->reg));
        }
      }
    }

    uint8_t colour = 0;
    if (i_val->crosses_call) {
      //requires non volatile reg
      colour += options->calling_convention->num_volatile_registers;
    }

    //Find first index that is 0 (i.e. a free colour/reg)
    //Search in order of options->calling_convention->all_regs_unordered because this will do
    //volatile registers first and then non volatile registers if required
    while ((regs & ((uint64_t)1 << options->calling_convention->all_regs_unordered[colour])) != 0) {
      colour++;
    }

    i_val->value_type = ValueType::FIXED;
    i_val->reg        = options->calling_convention->all_regs_unordered[colour];
  }

  free_no_destruct(stack);

  uint64_t used_regs = 0;
  {
    auto i = tree.values.begin();
    const auto end = tree.values.end();

    for (; i < end; i++) {
      assert(i->value_type != ValueType::FREE);

      if (i->fixed()) {
        used_regs |= ((uint64_t)1 << (i->reg));
      }
    }

    //Managed separately
    auto bp = options->calling_convention->base_pointer_reg;
    auto sp = options->calling_convention->stack_pointer_reg;

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

bool test_is_child(const Compiler* const comp, const ValueTree& tree,
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
      && comp->build_options.calling_convention->is_volatile(possible_parent.reg))
      ||
      (possible_parent.crosses_call
      && possible_child.value_type == ValueType::FIXED
      && comp->build_options.calling_convention->is_volatile(possible_child.reg))) {
    return false;
  }

  //Could be child
  if (possible_child.value_type == ValueType::FIXED) {
    //Test if there is a fixed value that stops us from merging
    uint8_t fixed_reg = possible_child.reg;

    auto* vi = tree.adjacency_list.data[pos_parent_index].begin();
    const auto* end = tree.adjacency_list.data[pos_parent_index].end();

    for (; vi < end; vi++) {
      const ValueIndex other = resolve_coalesced(*vi, tree);

      if (other.val == pos_parent_index || other.val == pos_child_index || ignore_vals.contains(other.val)) continue;

      const auto& other_val = tree.values.data[other.val];

      if (other_val.fixed() && other_val.reg == fixed_reg) {
        //could be special case where the last use is fixed to the same as the creating type
        if (possible_parent.creation.related_index == other) {

          ignore_vals.insert(pos_child_index);
          bool res = test_is_child(comp, tree, pos_parent_index, possible_parent, other.val, other_val, ignore_vals);
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

        //Could be special case where other is actually a child of the possible child
        if (other_val.creation.related_index.val == pos_child_index) {

          ignore_vals.insert(pos_parent_index);
          bool res = test_is_child(comp, tree, other.val, other_val, pos_child_index, possible_child, ignore_vals);
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


void coalesce(const Compiler* const comp, State* const state) {
  ValueTree& tree = state->value_tree;
  const ControlFlow& c_flow = state->control_flow;

  const size_t size = state->value_tree.adjacency_list.size;
  for (size_t l1 = 0; l1 < size; l1++) {
    auto& l1_val = tree.values.data[l1];

    ValueIndex created_by = resolve_coalesced(l1_val.creation.related_index, tree);
    auto& possible_parent = tree.values.data[created_by.val];

    Array<size_t> ignore_vals ={};

    bool is_child = test_is_child(comp, tree,
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
void graph_colour_algo(Compiler* const comp, CodeBlock* const code, State* const state) {
  if (comp->print_options.pre_reg_alloc) {
    printf("\n=== Pre Register Allocation Bytecode ===\n\n");
    ByteCode::print_bytecode(&reg_num_as_string, stdout, code->code.data, code->code.size);
    printf("\n=============================\n\n");
  }

  //Computers all the intersections in the value tree based on control flow
  //Build section
  compute_value_intersections(state->value_tree, state->control_flow);
  coalesce(comp, state);
  const uint64_t regs = select(&comp->build_options, state);

  //map values and function prolog and epilog
  map_values(&comp->build_options, code, state, regs);

  code->code.shrink();

  if (comp->print_options.normal_bytecode) {
    printf("\n=== Normal Bytecode ===\n\n");
    ByteCode::print_bytecode(comp->build_options.system->reg_name_from_num, stdout, code->code.data, code->code.size);
    printf("\n=============================\n\n");
  }
}

void compile_function_body_unit(Compiler* const comp,
                                ASTFunctionDeclaration* const ast_func,
                                Function* const func,
                                State* const state) {
  const BuildOptions* const build_options = &comp->build_options;

  {
    const auto locals = state->active_locals.size;
    DEFER(&) { state->active_locals.size = locals; };

    Array<ASTStatement>& statements = ast_func->body.block;

    auto i = statements.mut_begin();
    const auto end = statements.mut_end();
    for (; i < end; i++) {
      compile_type_of_statement(comp, func, state, i);
      if (comp->is_panic()) {
        return;
      }
    }
  }

  //Cant error now???

  //Values for the parameters - allows them to be different from parameter registers
  {
    const size_t size = smaller(state->active_locals.size,
                                (size_t)comp->build_options.calling_convention->num_parameter_registers);

    for (size_t itr = 0; itr < size; itr++) {
      auto* i = state->all_locals.data + *(state->active_locals.data + itr);
      assert(i->val.type == RVT::REGISTER);

      RuntimeValue rt_p ={};

      const ValueIndex rbp = state->new_value();
      auto& rbp_val = state->value_tree.values.data[rbp.val];

      rbp_val.value_type = ValueType::FIXED;
      rbp_val.reg = comp->build_options.calling_convention->base_pointer_reg;

      if (comp->optimization_options.non_stack_locals) {
        RuntimeHint load_hint ={};
        load_hint.hint_types = i->valid_rvts;

        load_runtime_hint(comp, state, i->type, &load_hint, NON_CONST_RVTS);

        copy_runtime_to_runtime(comp, state, &func->code_block, i->type, &i->val, &load_hint.val);

        rt_p = std::move(load_hint.val);
      }
      else {
        assert((i->valid_rvts & RVT::MEMORY) > 0);
        rt_p.type = RVT::MEMORY;
        rt_p.mem  = state->new_mem();

        auto* stack_v = state->get_mem(rt_p.mem);
        stack_v->mem.base = (uint8_t)rbp.val;

        if (comp->build_options.calling_convention->shadow_space_size >= ((itr + 1) * 8)) {
          stack_v->mem.disp = (int32_t)(8 * itr);
        }
        else {
          stack_v->mem.disp = state->stack.next_stack_local(8, 8);
        }

        stack_v->mem.scale = 0;
        stack_v->size = i->type->size();

        copy_reg_to_runtime(comp, state, &func->code_block, i->type, i->val.reg, &rt_p);
      }

      i->val = std::move(rt_p);
    }
  }

  state->control_flow.expression_num++;

  //Calculate all the values
  //and compile the bytecode
  {
    Array<ASTStatement>& statements = ast_func->body.block;

    auto i = statements.mut_begin();
    const auto end = statements.mut_end();
    for (; i < end; i++) {
      compile_bytecode_of_statement(comp, i, state, &func->code_block);
    }
  }

  graph_colour_algo(comp, &func->code_block, state);

  //No longer needed
  state->all_locals.free();
  state->active_locals.free();
  state->value_tree.free();
  state->control_flow.free();
}

static void compile_function_signature(Compiler* const comp,
                                       ASTFunctionDeclaration* const ast_func,
                                       Function* const func,
                                       State* const state) {
  DO_NOTHING;//helps with formatting

  //Parameters
  {
    auto i_ast = ast_func->parameters.mut_begin();
    auto i = func->parameter_types.mut_begin();
    const auto end = func->parameter_types.end();

    while (i < end) {
      if (*i == nullptr) {
        compile_type(comp, &i_ast->type);
        if (comp->is_panic()) {
          return;
        }

        *i = i_ast->type.type;
      }

      i++;
      i_ast++;
    }
  }

  //Enter the body
  state->control_flow.new_flow();

  //Load signature locals
  {
    state->all_locals.insert_uninit(ast_func->parameters.size);

    size_t index = 0;
    const size_t size = ast_func->parameters.size;

    const ValueIndex rbp = state->new_value();
    state->set_value(rbp);

    auto& rbp_val = state->value_tree.values.data[rbp.val];

    rbp_val.value_type = ValueType::FIXED;
    rbp_val.reg = comp->build_options.calling_convention->base_pointer_reg;

    for (; index < size; index++) {
      auto i = ast_func->parameters.data + index;
      auto l_i = state->all_locals.data + index;
      state->active_locals.insert(index);

      l_i->name = i->name;
      l_i->type = i->type.type;
      l_i->valid_rvts &= NON_CONST_RVTS;//Cant have const parameters
      l_i->val = RuntimeValue();

      if ((size_t)comp->build_options.calling_convention->num_parameter_registers < index) {
        const size_t param_num = index - comp->build_options.calling_convention->num_parameter_registers;

        l_i->val.type = RVT::MEMORY;
        l_i->val.mem = state->new_mem();

        auto* stack_v = state->get_mem(l_i->val.mem);

        stack_v->mem.base = (uint8_t)rbp.val;
        stack_v->mem.disp = 8 * (int32_t)index + (int32_t)comp->build_options.calling_convention->shadow_space_size;
        stack_v->mem.scale = 0;
        stack_v->size = l_i->type->size();

        state->stack.push_stack_params(param_num);
      }
      else {
        l_i->val.type = RVT::REGISTER;
        l_i->val.reg = state->new_value();
        state->set_value(l_i->val.reg);//Set in function call

        auto* l_val = state->value_tree.values.data + l_i->val.reg.val;

        l_val->value_type = ValueType::FIXED;
        l_val->reg = comp->build_options.calling_convention->parameter_registers[index];
      }
    }
  }

  func->code_block.label = comp->labels++;
  state->control_flow.expression_num++;

  compile_type(comp, &ast_func->return_type);
  if (comp->is_panic()) {
    return;
  }

  func->return_type = ast_func->return_type.type;
}

void build_compilation_units(Compiler* const comp, ASTFile* const func) {
  auto i = func->functions.mut_begin();
  const auto end = func->functions.mut_end();

  for (; i < end; i++) {
    comp->function_units.insert_uninit(1);
    FunctionUnit* const unit = comp->function_units.back();

    comp->compiling.insert_uninit(1);
    CompilationUnitCarrier* const carrier = comp->compiling.back();

    Function* const func = comp->new_function();

    //Link up the ast and function
    unit->source = i;
    unit->destination = func;
    func->name = i->name;

    //Setup parameters
    func->parameter_types.insert_uninit(i->parameters.size);
    func->parameter_types.shrink();

    //Start signature stage
    unit->stage = FUNCTION_COMPILE_STAGE::SIGNATURE;
  }

  comp->function_units.shrink();
}

CompileCode print_compile_errors(const Compiler* const comp) {
  CompileCode ret = CompileCode::NO_ERRORS;

  IO::err_print("--- Compiler Encountered A Fatal Error ---\n\n");

  auto i = comp->errors.error_messages.begin();
  const auto end = comp->errors.error_messages.end();

  InternHashTable<OwnedPtr<const char>> files ={};

  for (; i < end; i++) {
    ret = i->type;

    const char* message_type = compile_code_string(i->type);
    IO::err_print(message_type);
    IO::err_print(":\n");

    OwnedPtr<char> type_set_message = format_type_set(i->message.ptr, 4, 70);

    IO::err_print(type_set_message.ptr);
    IO::err_print('\n');

    if (i->span.file_name != nullptr) {
      const Span& span = i->span;

      IO::err_print("Location: ");

      if (!files.contains(i->span.file_name)) {
        auto load_file = FILES::load_file_to_string(i->span.file_name->string);

        files.insert(i->span.file_name, std::move(load_file));
      }

      OwnedPtr<const char>* ptr_ptr = files.get_val(i->span.file_name);
      assert(ptr_ptr != nullptr);

      const char* source = ptr_ptr->ptr;
      OwnedPtr<char> string = load_span_from_file(span, source);

      IO::err_print(format("{} {}:{}\n\n", span.file_name, span.char_start, span.line_start));
      IO::err_print(string.ptr);
      IO::err_print("\n\n");
    }
  }

  IO::err_print("------------------------------------------\n");
  return ret;
}

CompileCode compile_all(Compiler* const comp) {
  while (comp->compiling.size > 0) {
    //Compile waiting
    {
      auto i = comp->compiling.mut_begin();
      const auto end = comp->compiling.mut_end();

      for (; i < end; i++) {
        comp->current_unit = *i;

        switch (i->type) {
          case COMPILATION_TYPE::FUNCTION: {
              FunctionUnit* const unit = comp->function_units.data + i->index;

              switch (unit->stage) {
                case FUNCTION_COMPILE_STAGE::SIGNATURE: {
                    compile_function_signature(comp,
                                               unit->source,
                                               unit->destination,
                                               &unit->state);

                    if (comp->is_panic()) {
                      if (comp->is_fatal()) {
                        return print_compile_errors(comp);
                      }
                      else {
                        comp->reset_panic();
                      }
                    }
                    else {
                      unit->stage = FUNCTION_COMPILE_STAGE::BODY;
                      unit->state.return_label = comp->labels++;
                      i--;//Do it again straight away
                    }
                    break;
                  }
                case FUNCTION_COMPILE_STAGE::BODY: {
                    compile_function_body_unit(comp,
                                               unit->source,
                                               unit->destination,
                                               &unit->state);

                    if (comp->is_panic()) {
                      if (comp->is_fatal()) {
                        return print_compile_errors(comp);
                      }
                      else {
                        comp->reset_panic();
                      }
                    }
                    else {
                      unit->stage = FUNCTION_COMPILE_STAGE::FINISHED;
                    }
                    break;
                  }
              }
              break;
            }
          case COMPILATION_TYPE::CONSTANT: {
              ConstantUnit* const unit = comp->constant_units.data + i->index;

              switch (unit->stage) {
                case EXPRESSION_COMPILE_STAGE::UNTYPED: {
                    compile_type_of_expression(comp, unit->expr, &unit->state);

                    if (comp->is_panic()) {
                      if (comp->is_fatal()) {
                        return print_compile_errors(comp);
                      }
                      else {
                        comp->reset_panic();
                      }
                    }
                    else {
                      unit->stage = EXPRESSION_COMPILE_STAGE::TYPED;
                      i--;
                    }
                    break;
                  }
                case EXPRESSION_COMPILE_STAGE::TYPED: {
                    if (unit->cast_to != nullptr) {
                      bool implicit_cast = can_implicit_cast(unit->expr->type, unit->cast_to);
                      bool comptime_cast = can_comptime_cast(unit->expr->type, unit->cast_to);

                      assert(implicit_cast || comptime_cast);
                    }

                    State* state = &unit->state;

                    //maybe need??
                    //unit->constants = std::move(comp->working_state->constants);

                    CodeBlock block ={};
                    block.label = comp->labels++;

                    //Have to compile to vm
                    BuildOptions options;
                    options.calling_convention = &convention_vm;
                    options.system             = &system_vm;
                    options.entry_point = comp->build_options.entry_point;
                    options.output_file = comp->build_options.output_file;
                    options.file_name   = comp->build_options.file_name;

                    //Swap forward
                    std::swap(options, comp->build_options);

                    //Set up new flow
                    size_t new_flow = state->control_flow.new_flow();
                    state->control_flow.current_flow = new_flow;

                    //Compile bytecode
                    state->comptime_compilation = true;

                    const Structure* type = unit->expr->type;
                    const bool pass_ptr = type->size() > 8;

                    uint8_t* const_val = comp->constants.alloc_no_construct(unit->expr->type->size());

                    if (!pass_ptr) {
                      //Load to rax
                      RuntimeValue ret_v ={};

                      ret_v.type = RVT::REGISTER;
                      ret_v.reg = state->new_value();

                      auto& rax_val = state->value_tree.values.data[ret_v.reg.val];

                      rax_val.value_type = ValueType::FIXED;
                      rax_val.reg        = comp->build_options.calling_convention->return_register;

                      compile_bytecode_of_expression_existing(comp, state, &block, unit->expr, &ret_v);

                      state->use_value(ret_v.reg);
                    }
                    else {
                      ValueIndex reg = state->new_value();

                      state->set_value(reg);

                      //Load RCX as a memory location
                      auto& rcx_val = state->value_tree.values.data[reg.val];

                      rcx_val.value_type = ValueType::FIXED;
                      rcx_val.reg        = comp->build_options.calling_convention->parameter_registers[0];

                      RuntimeValue ret_v ={};

                      ret_v.type = RVT::MEMORY;
                      ret_v.mem  = state->new_mem();

                      auto* passed_mem = state->get_mem(ret_v.mem);
                      passed_mem->mem.base = (uint8_t)reg.val;
                      passed_mem->mem.disp = 0;
                      passed_mem->mem.scale = 0;

                      passed_mem->size = type->size();

                      compile_bytecode_of_expression_existing(comp, state, &block, unit->expr, &ret_v);

                      state->use_value(reg);
                    }

                    ByteCode::EMIT::JUMP_TO_FIXED(block.code, state->return_label);

                    //Graph colour
                    graph_colour_algo(comp, &block, state);

                    //Backend
                    Array<uint8_t> code ={};
                    const size_t entry = vm_backend_single_func(code, &block, comp->labels);

                    if (comp->print_options.comptime_exec) {
                      printf("About to execute Compile Time Code:\n");
                      ByteCode::print_bytecode(&vm_regs_name_from_num, stdout, code.data, code.size);
                    }

                    //Run the VM
                    if (pass_ptr) {
                      X64_UNION pass_rcx = const_val;
                      vm_set_parameters(&comp->build_options, comp->vm, pass_rcx);
                    }

                    ErrorCode err = vm_rum(comp->vm, code.data, entry);
                    if (err != ErrorCode::OK) {
                      comp->report_error(CompileCode::INTERNAL_ERROR, unit->expr->span,
                                         "VM returned error code '{}' in compile time execution",
                                         err);
                      return print_compile_errors(comp);
                    }

                    //Get the value back
                    if (pass_ptr) {
                      if (comp->print_options.comptime_exec) {
                        printf("\nComptime Res In Bytes: ");
                        print_as_bytes(const_val, type->size());
                        putc('\n', stdout);
                      }
                    }
                    else {
                      //Effectively stored in RAX
                      uint64_t val = comp->vm->registers[convention_vm.return_register].b64.reg;
                      x64_to_bytes(val, const_val);

                      if (comp->print_options.comptime_exec) {
                        printf("\nComptime Res: %llu\n", val);
                      }
                    }

                    if (unit->cast_to != nullptr) {
                      uint8_t* res = comp->constants.alloc_no_construct(unit->cast_to->size());
                      do_literal_cast(comp, unit->expr, unit->cast_to, const_val, res);
                      if (comp->is_panic()) {
                        return print_compile_errors(comp);
                      }

                      comp->constants.free_no_destruct(const_val);
                      const_val = res;

                      unit->expr->type = unit->cast_to;
                    }

                    unit->expr->const_val = const_val;

                    //Swap back
                    std::swap(options, comp->build_options);

                    unit->stage = EXPRESSION_COMPILE_STAGE::FINISHED;
                    break;
                  }
              }
              break;
            }
        }
      }
    }

    //Reset compiling
    comp->compiling.size = 0;

    struct DependencyFinished {
      const Compiler* comp;

      bool operator() (const CompilationUnitCarrier& unit) {
        switch (unit.type) {
          case COMPILATION_TYPE::FUNCTION:
            return (comp->function_units.data + unit.index)->stage == FUNCTION_COMPILE_STAGE::FINISHED;
          case COMPILATION_TYPE::CONSTANT:
            return (comp->constant_units.data + unit.index)->stage == EXPRESSION_COMPILE_STAGE::FINISHED;
        }

        return false;
      }
    };

    {
      auto i = comp->function_units.mut_begin();
      const auto end = comp->function_units.mut_end();

      for (; i < end; i++) {
        i->dependencies.remove_if(DependencyFinished{ comp });

        if (i->stage != FUNCTION_COMPILE_STAGE::FINISHED && i->dependencies.size == 0 && !i->unfound_dependency) {
          comp->compiling.insert_uninit(1);
          CompilationUnitCarrier* const carrier = comp->compiling.back();

          carrier->type = COMPILATION_TYPE::FUNCTION;
          carrier->index = i - comp->function_units.data;
        }
      }
    }

    {
      auto i = comp->constant_units.mut_begin();
      const auto end = comp->constant_units.mut_end();

      for (; i < end; i++) {
        i->dependencies.remove_if(DependencyFinished{ comp });

        if (i->stage != EXPRESSION_COMPILE_STAGE::FINISHED && i->dependencies.size == 0 && !i->unfound_dependency) {
          comp->compiling.insert_uninit(1);
          CompilationUnitCarrier* const carrier = comp->compiling.back();

          carrier->type = COMPILATION_TYPE::CONSTANT;
          carrier->index = i - comp->constant_units.data;
        }
      }
    }


    if (comp->compiling.size == 0) {
      if (comp->unfound_deps.units.size > 0) {
        const size_t num_deps = comp->unfound_deps.units.size;
        //Remove units if dependency has been found
        comp->unfound_deps.units.remove_if([comp](const UnfoundDep& dep) {
          switch (dep.type) {
            case UnfoundDepType::Name: {
                if (!comp->global.names.contains(dep.name)) {
                  return false;
                }
                break;
              }
            case UnfoundDepType::Function: {
                auto overloads = generate_overload_set(comp, &dep.signature);

                if (overloads.size == 0) {
                  return false;
                }

                //if there are multiple overloads then let it error in the type check phase
                break;
              }
          }

          //Success!
          switch (dep.unit_waiting.type) {
            case COMPILATION_TYPE::FUNCTION:
              (comp->function_units.data + dep.unit_waiting.index)->unfound_dependency = false;
              break;
            case COMPILATION_TYPE::CONSTANT:
              (comp->constant_units.data + dep.unit_waiting.index)->unfound_dependency = false;
              break;
          }

          return true;
        });

        if (num_deps == comp->unfound_deps.units.size) {
          auto i = comp->unfound_deps.units.begin();
          auto end = comp->unfound_deps.units.end();
          for (; i < end; i++) {
            switch (i->type) {
              case UnfoundDepType::Name: {
                  comp->report_error(CompileCode::UNFOUND_DEPENDENCY, i->span,
                                     "'{}' does not exist", i->name);
                  break;
                }
              case UnfoundDepType::Function: {
                  comp->report_error(CompileCode::UNFOUND_DEPENDENCY, i->span,
                                     "'{}' does not exist", i->signature);
                  break;
                }
            }
          }

          return print_compile_errors(comp);
        }
      }
    }

  }

  return CompileCode::NO_ERRORS;
}


void print_compiled_functions(const Compiler* const comp) {
  auto i = comp->functions.begin_const_iter();
  const auto end = comp->functions.end_const_iter();

  for (; i != end; i.next()) {
    const Function* func = i.get();

    printf("FUNCTION %s:\n", func->name->string);
    ByteCode::print_bytecode(comp->build_options.system->reg_name_from_num, stdout, func->code_block.code.data, func->code_block.code.size);
    printf("\n");
  }
}