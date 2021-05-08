#include "compiler.h"
#include "type.h"
#include "ast.h"
#include "bytecode.h"
#include "parser.h"
#include "vm.h"
#include "backends.h"
#include "format.h"

Function* Compiler::new_function() {
  return functions.insert();
}

void State::use_value(ValueIndex index) {
  use_value(index, index);
}

void State::use_value(const ValueIndex index, const ValueIndex creates) {
  struct Lambda {
    const ControlFlow* flow;

    bool operator()(const ValueUse& p) const noexcept {
      return flow->test_a_flows_to_b(p.time.flow, flow->current_flow)
        || p.time.flow == flow->current_flow;
    }
  };

  auto& val = value_tree.values.data[index.val];

  val.last_uses.remove_if(Lambda{ &control_flow });
  val.last_uses.insert(
    ValueUse{
      creates,
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

ValueIndex State::new_value() {
  value_tree.values.insert_uninit(1);
  value_tree.adjacency_list.insert_uninit(1);

  auto val_index = ValueIndex{ value_tree.intersection_check.new_value() };


  auto* val = value_tree.values.data + val_index.val;
  val->creation = ValueUse{ val_index, control_flow.now() };

  return val_index;
}

ValueIndex State::new_value(ValueIndex created_by) {
  const ValueIndex val_index = new_value();
  auto* val = value_tree.values.data + val_index.val;
  val->creation.related_index = created_by;
  return val_index;
}

const Local* State::find_local(InternString i_s) const {
  auto i = locals.begin();
  const auto end = locals.end();

  for (; i < end; i++) {
    if (i->name == i_s) {
      return i;
    }
  }
  return nullptr;
}

uint64_t StackState::next_stack_local(uint64_t size, uint64_t alignment) {
  const uint64_t mod_align = current % alignment;

  if (mod_align > 0) {
    current += alignment - mod_align;
  }

  current += size;
  if (current > max) {
    max = current;
  }

  return current;
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

const Structure* find_or_make_array(const Compiler* comp, const Structure* base, size_t length) {
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
  {
    OwnedPtr<char> name = format("[{}; {}]", base->name, length);

    ArrayStructure* const arr = types->new_array();
    arr->base = base;
    arr->length = length;
    arr->name = comp->strings->intern(name.ptr);

    return arr;
  }
}

//Note: Recursive for array types
CompileCode compile_type(Compiler* const comp, ASTType* type) {
  if (type->type != nullptr) {
    //Incase function re-called
    return CompileCode::NO_ERRORS;
  }

  switch (type->type_type) {
    case TYPE_TYPE::NORMAL: {
        type->type = comp->types->structure_by_name(type->name);
        break;
      }
    case TYPE_TYPE::ARRAY: {
        CompileCode ret = compile_type(comp, type->arr.base);
        if (ret != CompileCode::NO_ERRORS) {
          return ret;
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

          switch (comp->current_unit.type) {
            case COMPILATION_TYPE::FUNCTION:
              (comp->function_units.data + comp->current_unit.index)->dependecies.insert(carrier);
              break;
            case COMPILATION_TYPE::CONSTANT:
              (comp->constant_units.data + comp->current_unit.index)->dependecies.insert(carrier);
              break;
          }

          return CompileCode::FOUND_DEPENDENCY;
        }
        else {
          if (!TYPE_TESTS::is_int(type->arr.expr->type)) {
            printf("TYPE ERROR: Expected an integer type!\n"
                   "            Instead found %s", type->arr.expr->type->name.string);
            return CompileCode::TYPE_CHECK_ERROR;
          }

          uint64_t length;
          if (TYPE_TESTS::is_signed_int(type->arr.expr->type)) {
            int64_t i_length = *(const int64_t*)type->arr.expr->const_val;
            if (i_length <= 0) {
              printf("TYPE ERROR: Length of array must be greater than 0"
                     "            Instead found %lld", i_length);
              return CompileCode::TYPE_CHECK_ERROR;
            }
            else {
              length = (uint64_t)i_length;
            }
          }
          else {
            length = *(const uint64_t*)type->arr.expr->const_val;
          }

          type->type = find_or_make_array(comp, type->arr.base->type, length);
          break;
        }


        printf("INTERNAL ERROR: Not yet built\n");
        return CompileCode::INTERNAL_ERROR;
        break;
      }
  }

  if (type->type == nullptr) {
    printf("ERROR: could not find structure '%s'\n", type->name.string);
    return CompileCode::UNFOUND_DEPENDENCY;
  }
  else {
    return CompileCode::NO_ERRORS;
  }
}

static void print_function_call(const FunctionCallExpr* const call) {
  printf("%s(", call->function_name.string);

  auto i = call->arguments.begin();
  const auto end = call->arguments.end();

  if (i < end) {
    for (; i < (end - 1); i++) {
      printf("%s, ", i->type->name.string);
    }

    printf("%s", i->type->name.string);
  }

  printf(")");
}

static void print_function_signature(const Function* func) {
  printf("(");

  auto i = func->parameter_types.begin();
  const auto end = func->parameter_types.end();

  if (i < end) {
    for (; i < (end - 1); i++) {
      printf("%s, ", (*i)->name.string);
    }

    printf("%s", (*i)->name.string);
  }

  printf(") -> %s", func->return_type->name.string);
}

static Array<const Function*> generate_overload_set(Compiler* const comp,
                                                    FunctionCallExpr* const call) {
  auto i = comp->functions.begin_iter();
  const auto end = comp->functions.end_iter();

  Array<const Function*> possible_overloads ={};

  for (; i != end; i.next()) {
    const Function* const func = i.get();

    if (func->return_type == nullptr) {
      continue;
    }

    //Correct name and number of args
    if (call->function_name == func->name
        && call->arguments.size == func->parameter_types.size) {
      auto p_call = call->arguments.begin();
      const auto end_call = call->arguments.end();

      auto p_func = func->parameter_types.begin();

      bool implicit_cast = false;

      while (p_call < end_call) {
        if (p_call->type == *p_func) {
          //Do nothing
        }
        else if (can_literal_cast(p_call->type, *p_func)) {
          implicit_cast = true;
        }
        else {
          //Escape out
          goto FAIL;
        }

        p_call++;
        p_func++;
      }

      if (!implicit_cast) {
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

static CompileCode find_function_for_call(Compiler* const comp,
                                          FunctionCallExpr* const call) {
  Array<const Function*> possible_overloads = generate_overload_set(comp, call);

  if (possible_overloads.size == 0) {
    printf("TYPE ERROR: Could not find function '%s' with appropriate signature\n"
           "            Call: '", call->function_name.string);
    print_function_call(call);
    printf("'\n");
    return CompileCode::UNFOUND_DEPENDENCY;
  }
  else if (possible_overloads.size == 1) {
    call->function = possible_overloads.data[0];
    return CompileCode::NO_ERRORS;
  }
  else {
    printf("TYPE ERROR: Found more than one function '%s' with signature that required implicit casts\n"
           "            Call: '", call->function_name.string);

    print_function_call(call);
    printf("'\n");

    auto i = possible_overloads.begin();
    const auto end = possible_overloads.end();

    for (size_t i = 0; i < possible_overloads.size; i++) {
      const Function* func = possible_overloads.data[i];

      printf("            Option %llu: '", i + 1);
      print_function_signature(func);
      printf("'\n");
    }

    return CompileCode::TYPE_CHECK_ERROR;
  }
}

static CompileCode binary_operator_type(const Types* const types,
                                        ASTExpression* const expr) {

  const auto op = expr->bin_op.op;

  switch (op) {
    case BINARY_OPERATOR::ADD:
      return find_binary_operator(types, expr, add_operators, array_size(add_operators));
    case BINARY_OPERATOR::SUB:
      return find_binary_operator(types, expr, sub_operators, array_size(sub_operators));
    case BINARY_OPERATOR::MUL:
      return find_binary_operator(types, expr, mul_operators, array_size(mul_operators));
    case BINARY_OPERATOR::DIV:
      return find_binary_operator(types, expr, div_operators, array_size(div_operators));
    case BINARY_OPERATOR::EQUIVALENT:
      return find_binary_operator(types, expr, eq_operators, array_size(eq_operators));
    case BINARY_OPERATOR::OR:
      return find_binary_operator(types, expr, or_operators, array_size(or_operators));
    case BINARY_OPERATOR::AND:
      return find_binary_operator(types, expr, and_operators, array_size(and_operators));
    default: {
        const char* const name = BINARY_OP_STRING::get(op);

        printf("INTERNAL ERROR: Binary Operator not implemented '%s'\n", name);
        return CompileCode::INTERNAL_ERROR;
      }
  }
}

static CompileCode unary_operator_type(const Types* const types,
                                       ASTExpression* const expr) {

  const auto op = expr->un_op.op;

  switch (op) {
    case UNARY_OPERATOR::NEG:
      return find_unary_operator(types, expr, neg_operators, array_size(neg_operators));
    default: {
        const char* const name = UNARY_OP_STRING::get(op);

        printf("INTERNAL ERROR: Unary Operator not implemented '%s'\n", name);
        return CompileCode::INTERNAL_ERROR;
      }
  }
}

static CompileCode cast_operator_type(const Types* const types,
                                      ASTExpression* const expr) {
  const Structure* const cast_to = expr->cast.type.type;
  const Structure* const cast_from = expr->cast.expr->type;

  assert(cast_to != nullptr);
  assert(cast_from != nullptr);

  if (cast_from == cast_to || can_literal_cast(cast_from, cast_to)) {
    expr->cast.emit = &CASTS::no_cast;
    return CompileCode::NO_ERRORS;
  }

  auto i = cast_from->casts.begin();
  const auto end =  cast_from->casts.end();

  for (; i < end; i++) {
    if (i->test(cast_to)) {
      expr->cast.emit = i->cast;
      return CompileCode::NO_ERRORS;
    }
  }

  printf("CAST ERROR: Cannot cast type '%s' to type '%s'\n",
         cast_from->name.string, cast_to->name.string);
  return CompileCode::TYPE_CHECK_ERROR;
}

static CompileCode check_cast(const Structure* from, const Structure* to) {
  if (from == to || can_literal_cast(from, to)) {
    return CompileCode::NO_ERRORS;
  }
  else {
    printf("CAST ERROR: Cannot implicilty cast from '%s' to '%s'\n",
           from->name.string,
           to->name.string);
    return CompileCode::TYPE_CHECK_ERROR;
  }
}

constexpr static bool already_const_value(const ASTExpression* expr) {
  return expr->expr_type == EXPRESSION_TYPE::ENUM
    || expr->expr_type == EXPRESSION_TYPE::VALUE
    || expr->expr_type == EXPRESSION_TYPE::ASCII_STRING;
}

CompileCode make_constant_typed_dependency(ASTExpression* expr, State* const state, Compiler* const comp) {
  expr->const_val = comp->constants.alloc_no_construct(expr->type->size());

  ConstantUnit unit ={};
  unit.expr   = expr;
  unit.stage  = EXPRESSION_COMPILE_STAGE::TYPED;

  comp->constant_units.insert(std::move(unit));


  CompilationUnitCarrier carrier ={};
  carrier.index = comp->constant_units.size - 1;
  carrier.type  = COMPILATION_TYPE::CONSTANT;

  switch (comp->current_unit.type) {
    case COMPILATION_TYPE::FUNCTION:
      (comp->function_units.data + comp->current_unit.index)->dependecies.insert(carrier);
      break;
    case COMPILATION_TYPE::CONSTANT:
      (comp->constant_units.data + comp->current_unit.index)->dependecies.insert(carrier);
      break;
  }

  return CompileCode::FOUND_DEPENDENCY;
}

//Note: Recursive
static CompileCode compile_type_of_expression(Compiler* const comp,
                                              ASTExpression* const expr,
                                              State* state) {
  //Already typed
  if (expr->type != nullptr) {
    return CompileCode::NO_ERRORS;
  }

  //just a lil buffer so I dont have to allocate one in every switch case
  CompileCode ret_code = CompileCode::NO_ERRORS;

  switch (expr->expr_type) {
    case EXPRESSION_TYPE::INDEX: {
        expr->index.expr->call_leaf = expr->call_leaf;
        expr->index.index->call_leaf = expr->call_leaf;

        ret_code = compile_type_of_expression(comp, expr->index.expr, state);
        if (ret_code != CompileCode::NO_ERRORS) {
          return ret_code;
        }

        assert(expr->index.expr->type != nullptr);
        if (!TYPE_TESTS::is_array(expr->index.expr->type)) {
          printf("TYPE_ERROR: Cannot take index of non-array type: %s\n", expr->index.expr->type->name.string);
          return CompileCode::TYPE_CHECK_ERROR;
        }

        ret_code = compile_type_of_expression(comp, expr->index.index, state);
        if (ret_code != CompileCode::NO_ERRORS) {
          return ret_code;
        }

        assert(expr->index.index->type != nullptr);

        if (!TYPE_TESTS::is_int(expr->index.index->type)) {
          printf("TYPE_ERROR: An index must be in integer\n"
                 "            Found non-integer type: %s\n", expr->index.index->type->name.string);
          return CompileCode::TYPE_CHECK_ERROR;
        }

        expr->type = static_cast<const ArrayStructure*>(expr->index.expr->type)->base;
        expr->compile_time_constant = expr->index.index->compile_time_constant && expr->index.expr->compile_time_constant;
        expr->makes_call = expr->index.index->makes_call || expr->index.expr->makes_call;
        break;
      }
    case EXPRESSION_TYPE::ARRAY_EXPR: {
        auto i = expr->array_expr.elements.mut_begin();
        const auto end = expr->array_expr.elements.mut_end();

        const Structure* base_type = nullptr;

        for (; i < end; i++) {
          i->call_leaf = expr->call_leaf;

          ret_code = compile_type_of_expression(comp, i, state);
          if (ret_code != CompileCode::NO_ERRORS) {
            return ret_code;
          }

          if (base_type == nullptr) {
            //TEMP: First element decides type for now - will have issues with literals
            base_type = i->type;
          }
          else {
            //Check rest of the elements match
            ret_code = check_cast(i->type, base_type);
            if (ret_code != CompileCode::NO_ERRORS) {
              return ret_code;
            }

            base_type = i->type;//Will always be either same or more specific
          }

          expr->makes_call |= i->makes_call;
          expr->compile_time_constant &= i->compile_time_constant;
        }

        if (base_type == nullptr) {
          expr->type = comp->types->s_empty_arr;
        }
        else {
          expr->type = find_or_make_array(comp, base_type, expr->array_expr.elements.size);
        }

        break;
      }
    case EXPRESSION_TYPE::ASCII_STRING: {
        const size_t len = strlen_ts(expr->ascii_string.string) + 1;

        expr->type = find_or_make_array(comp, comp->types->s_ascii, len);
        expr->compile_time_constant = true;
        break;
      }
    case EXPRESSION_TYPE::ENUM:
      expr->enum_value.enum_value = comp->types->enum_by_name(expr->enum_value.name);

      if (expr->enum_value.enum_value == nullptr) {
        printf("TYPE ERROR: Enum value not found '%s'\n", expr->enum_value.name.string);
        return CompileCode::UNFOUND_DEPENDENCY;
      }

      expr->type = expr->enum_value.enum_value->type;
      expr->compile_time_constant = true;

      break;
    case EXPRESSION_TYPE::VALUE: {
        ValueExpr* const val = &expr->value;

        if (val->suffix.string == nullptr) {
          expr->type = comp->types->s_int_lit;
        }
        else if (val->suffix == comp->types->s_i64->name) {
          expr->type = comp->types->s_i64;
        }
        else if (val->suffix == comp->types->s_u64->name) {
          expr->type = comp->types->s_u64;
        }
        else {
          printf("DEPENDENCY ERROR: Invalid integer literal suffix '%s'", val->suffix.string);
          return CompileCode::UNFOUND_DEPENDENCY;
        }

        expr->compile_time_constant = true;

        break;
      }

    case EXPRESSION_TYPE::NAME: {
        const InternString name = expr->name;

        auto i = state->locals.begin();
        const auto end = state->locals.end();

        for (; i < end; i++) {
          if (name == i->name) {

            expr->expr_type = EXPRESSION_TYPE::LOCAL;
            expr->type = i->type;
            break;
          }
        }

        if (expr->type == nullptr) {
          printf("DEPENDENCY ERROR: '%s' is not a variable in this scope\n", name.string);
          return CompileCode::UNFOUND_DEPENDENCY;
        }

        expr->compile_time_constant = false;

        break;
      }
    case EXPRESSION_TYPE::CAST: {
        expr->cast.expr->call_leaf = expr->call_leaf;

        ret_code = compile_type(comp, &expr->cast.type);
        if (ret_code != CompileCode::NO_ERRORS) {
          return ret_code;
        }

        ret_code = compile_type_of_expression(comp, expr->cast.expr, state);
        if (ret_code != CompileCode::NO_ERRORS) {
          return ret_code;
        }


        ret_code = cast_operator_type(comp->types, expr);
        if (ret_code != CompileCode::NO_ERRORS) {
          return ret_code;
        }

        expr->compile_time_constant = expr->cast.expr->compile_time_constant;
        expr->makes_call = expr->cast.expr->makes_call;
        expr->type = expr->cast.type.type;
        break;
      }
    case EXPRESSION_TYPE::UNARY_OPERATOR: {
        expr->un_op.primary->call_leaf = expr->call_leaf;

        ret_code = compile_type_of_expression(comp, expr->un_op.primary, state);
        if (ret_code != CompileCode::NO_ERRORS) {
          return ret_code;
        }

        expr->compile_time_constant = expr->un_op.primary->compile_time_constant;
        expr->makes_call = expr->un_op.primary->makes_call;

        ret_code = unary_operator_type(comp->types, expr);
        if (ret_code != CompileCode::NO_ERRORS) {
          return ret_code;
        }

        break;
      }
    case EXPRESSION_TYPE::BINARY_OPERATOR: {
        BinaryOperatorExpr* const bin_op = &expr->bin_op;

        bin_op->left->call_leaf = expr->call_leaf;
        bin_op->right->call_leaf = expr->call_leaf;


        ret_code = compile_type_of_expression(comp, bin_op->left, state);
        if (ret_code != CompileCode::NO_ERRORS) {
          return ret_code;
        }

        ret_code = compile_type_of_expression(comp, bin_op->right, state);
        if (ret_code != CompileCode::NO_ERRORS) {
          return ret_code;
        }

        if (bin_op->left->compile_time_constant && bin_op->right->compile_time_constant) {
          expr->compile_time_constant = true;
        }
        else if (bin_op->left->compile_time_constant && !already_const_value(bin_op->left)) {
          expr->compile_time_constant = false;

          return make_constant_typed_dependency(bin_op->left, state, comp);
        }
        else if (bin_op->right->compile_time_constant && !already_const_value(bin_op->right)) {
          expr->compile_time_constant = false;

          return make_constant_typed_dependency(bin_op->right, state, comp);
        }
        else {
          expr->compile_time_constant = false;
        }

        expr->makes_call = bin_op->left->makes_call || bin_op->right->makes_call;

        ret_code = binary_operator_type(comp->types, expr);
        if (ret_code != CompileCode::NO_ERRORS) {
          return ret_code;
        }

        break;
      }
    case EXPRESSION_TYPE::FUNCTION_CALL: {
        FunctionCallExpr* const call = &expr->call;

        expr->compile_time_constant = true;

        {
          auto i = call->arguments.mut_begin();
          const auto end = call->arguments.mut_end();

          for (; i < end; i++) {
            i->call_leaf = true;

            ret_code = compile_type_of_expression(comp, i, state);
            if (ret_code != CompileCode::NO_ERRORS) {
              return ret_code;
            }

            expr->compile_time_constant &= i->compile_time_constant;
          }
        }

        ret_code = find_function_for_call(comp, call);
        if (ret_code != CompileCode::NO_ERRORS) {
          return ret_code;
        }

        //Cant currently call functions at compile time
        expr->compile_time_constant = false;

        if (!expr->compile_time_constant) {
          auto i = call->arguments.mut_begin();
          const auto end = call->arguments.mut_end();

          for (; i < end; i++) {
            if (i->compile_time_constant && !already_const_value(i)) {
              return make_constant_typed_dependency(i, state, comp);
            }
          }
        }

        expr->type = call->function->return_type;
        expr->makes_call = true;

        break;
      }
    default: {
        printf("INTERNAL ERROR: Invalid expression type\n"
               "                Expression type: %d\n", (int)expr->expr_type);
        return CompileCode::INTERNAL_ERROR;
      }
  }

  assert(expr->type != nullptr);
  return CompileCode::NO_ERRORS;
}

static CompileCode compile_type_of_statement(Compiler* const comp,
                                             Function* const func,
                                             State* const state,
                                             ASTStatement* const statement) {

  CompileCode ret = CompileCode::NO_ERRORS;

  switch (statement->type) {
    case STATEMENT_TYPE::IF_ELSE: {
        ASTIfElse* const if_else = &statement->if_else;

        if (if_else->condition.type == nullptr) {
          ret = compile_type_of_expression(comp, &if_else->condition, state);
          if (ret != CompileCode::NO_ERRORS) {
            return ret;
          }

          ret = check_cast(if_else->condition.type, comp->types->s_bool);
          if (ret != CompileCode::NO_ERRORS) {
            return ret;
          }
        }

        assert(if_else->condition.type != nullptr);

        ret = compile_type_of_statement(comp, func, state, if_else->if_statement);
        if (ret != CompileCode::NO_ERRORS) {
          return ret;
        }

        ret = compile_type_of_statement(comp, func, state, if_else->else_statement);
        if (ret != CompileCode::NO_ERRORS) {
          return ret;
        }

        return CompileCode::NO_ERRORS;
      }
    case STATEMENT_TYPE::BLOCK: {
        auto locals = state->locals.size;
        DEFER(&) { state->locals.size = locals; };

        auto i = statement->block.block.mut_begin();
        const auto end = statement->block.block.mut_end();

        for (; i < end; i++) {
          ret = compile_type_of_statement(comp, func, state, i);
          if (ret != CompileCode::NO_ERRORS) {
            return ret;
          }
        }

        return ret;
      }
    case STATEMENT_TYPE::DECLARATION: {
        ASTDeclaration* const decl = &statement->declaration;

        if (decl->type.type == nullptr) {
          ret = compile_type(comp, &decl->type);
          if (ret != CompileCode::NO_ERRORS) {
            return ret;
          }
        }
        assert(decl->type.type != nullptr);

        if (decl->expression.type == nullptr) {
          ret = compile_type_of_expression(comp, &decl->expression, state);
          if (ret != CompileCode::NO_ERRORS) {
            return ret;
          }

          assert(decl->expression.type != nullptr);

          ret = check_cast(decl->expression.type, decl->type.type);
          if (ret != CompileCode::NO_ERRORS) {
            return ret;
          }

          if (decl->expression.compile_time_constant && !already_const_value(&decl->expression)) {
            return make_constant_typed_dependency(&decl->expression, state, comp);
          }
        }
        assert(decl->expression.type != nullptr);

        state->locals.insert_uninit(1);
        state->locals.back()->name = decl->name;
        state->locals.back()->type = decl->type.type;

        return CompileCode::NO_ERRORS;
      }
    case STATEMENT_TYPE::EXPRESSION: {
        ASTExpression* const expr = &statement->expression;
        if (expr->type == nullptr) {
          ret = compile_type_of_expression(comp, expr, state);
          if (ret != CompileCode::NO_ERRORS) {
            return ret;
          }

          assert(expr->type != nullptr);

          if (expr->compile_time_constant && !already_const_value(expr)) {
            return make_constant_typed_dependency(expr, state, comp);
          }
        }

        return CompileCode::NO_ERRORS;
      }
    case STATEMENT_TYPE::RETURN: {
        ASTExpression* const expr = &statement->expression;

        if (expr->type == nullptr) {
          ret = compile_type_of_expression(comp, expr, state);
          if (ret != CompileCode::NO_ERRORS) {
            return ret;
          }

          assert(expr->type != nullptr);

          ret = check_cast(expr->type, func->return_type);
          if (ret != CompileCode::NO_ERRORS) {
            return ret;
          }

          if (expr->compile_time_constant && !already_const_value(expr)) {
            return make_constant_typed_dependency(expr, state, comp);
          }
        }

        assert(expr->type != nullptr);
        return CompileCode::NO_ERRORS;
      }

  }

  printf("INTERNAL ERROR: Reached end of statement type checking without exiting\n"
         "                Statement type: %d\n", (int)statement->type);
  return CompileCode::INTERNAL_ERROR;
}

struct ValueOrConst {
  bool is_constant = false;

  union {
    ValueIndex index ={};
    void* constant;
  };
};

static void load_const_to_stack(Compiler* const comp,
                                State* const state,
                                CodeBlock* const code,
                                const Structure* type,
                                uint64_t* from,
                                ValueIndex& to,
                                uint64_t additional_offset) {

  const uint32_t size = type->size();
  const uint32_t align = type->alignment();

  const size_t s_div_8 = size / 8;
  const size_t s_mod_8 = size % 8;


  //Needs to go on stack
  auto* const to_val = state->value_tree.values.data + to.val;
  assert(to_val->on_stack());

  uint64_t offset = to_val->stack_offset + additional_offset;

  if (comp->build_options.system == &system_x86_64) {
    auto mov_val = state->new_value();

    for (size_t itr = 0; itr < s_div_8; itr++) {
      const int64_t val = (int64_t)*from;

      if (can_be_from_sign_extension(val)) {
        //Can just load the value as 32 bits and it will be sign extended
        ByteCode::EMIT::COPY_64_TO_STACK(code->code, *from, offset);
        state->control_flow.expression_num++;
      }
      else {
        ByteCode::EMIT::SET_R64_TO_64(code->code, (uint8_t)mov_val.val, *from);
        state->use_value(mov_val);
        state->control_flow.expression_num++;

        ByteCode::EMIT::COPY_R64_TO_STACK(code->code, (uint8_t)mov_val.val, offset);
        state->use_value(mov_val);
        state->use_value(to);
        state->control_flow.expression_num++;
      }



      from++;
      offset += 8;
    }
  }
  else {
    for (size_t itr = 0; itr < s_div_8; itr++) {
      ByteCode::EMIT::COPY_64_TO_STACK(code->code, *from, offset);
      state->use_value(to);
      state->control_flow.expression_num++;

      from++;
      offset += 8;
    }
  }



  uint64_t last = *from;
  uint8_t last_8 = last & 255;

  for (size_t itr = 0; itr < s_mod_8; itr++) {
    ByteCode::EMIT::COPY_8_TO_STACK(code->code, (uint8_t)(last & 255), offset);
    state->use_value(to);
    state->control_flow.expression_num++;
    last >>= 8;//Shift over to the next byte
    offset++;
  }
}

static void load_stack_to_stack(Compiler* const comp,
                                State* const state,
                                CodeBlock* const code,
                                const Structure* type,
                                const ValueIndex& from,
                                ValueIndex& to,
                                int64_t additional_offset) {

  const uint32_t size = type->size();
  const uint32_t align = type->alignment();

  const size_t s_div_8 = size / 8;
  const size_t s_mod_8 = size % 8;


  //Needs to go on stack
  auto* const to_val = state->value_tree.values.data + to.val;
  assert(to_val->on_stack());
  uint64_t to_offset = to_val->stack_offset + additional_offset;

  auto* const from_val = state->value_tree.values.data + from.val;
  assert(from_val->on_stack());
  uint64_t from_offset = from_val->stack_offset;



  auto mov_val = state->new_value();

  for (size_t itr = 0; itr < s_div_8; itr++) {
    ByteCode::EMIT::COPY_R64_FROM_STACK(code->code, (uint8_t)mov_val.val, from_offset);
    state->use_value(mov_val);
    state->use_value(from);
    state->control_flow.expression_num++;

    ByteCode::EMIT::COPY_R64_TO_STACK(code->code, (uint8_t)mov_val.val, to_offset);
    state->use_value(mov_val);
    state->use_value(to);
    state->control_flow.expression_num++;

    from_offset += 8;
    to_offset += 8;
  }

  for (size_t itr = 0; itr < s_mod_8; itr++) {
    ByteCode::EMIT::COPY_R8_FROM_STACK(code->code, (uint8_t)mov_val.val, from_offset);
    state->use_value(mov_val);
    state->use_value(from);
    state->control_flow.expression_num++;

    ByteCode::EMIT::COPY_R8_TO_STACK(code->code, (uint8_t)mov_val.val, to_offset);
    state->use_value(mov_val);
    state->use_value(to);
    state->control_flow.expression_num++;

    from_offset++;
    to_offset++;
  }
}

static void copy_val_to_stack(Compiler* const comp,
                              State* const state,
                              CodeBlock* const code,
                              const Structure* type,
                              const ValueOrConst& from,
                              ValueIndex& to,
                              int64_t additional_offset) {
  DEFER(&) { state->control_flow.expression_num++; };

  auto* const to_val = state->value_tree.values.data + to.val;
  assert(to_val->on_stack());

  if (from.is_constant) {
    load_const_to_stack(comp, state, code, type, (uint64_t*)from.constant, to, additional_offset);
    comp->constants.free_no_destruct(from.constant);
  }
  else {
    auto* const from_val = state->value_tree.values.data + from.index.val;

    if (from_val->on_stack()) {
      load_stack_to_stack(comp, state, code, type, from.index, to, additional_offset);
    }
    else {
      state->use_value(from.index);
      state->use_value(to);
      ByteCode::EMIT::COPY_R64_TO_STACK(code->code, (uint8_t)from.index.val, to_val->stack_offset + additional_offset);
    }
  }
}

static ValueIndex copy_val_to_val(Compiler* const comp,
                                  State* const state,
                                  CodeBlock* const code,
                                  const Structure* type,
                                  const ValueOrConst& from) {
  DEFER(&) { state->control_flow.expression_num++; };

  ValueIndex to;

  if (from.is_constant) {

    auto* constant = (uint64_t*)from.constant;

    const uint32_t size = type->size();
    const uint32_t align = type->alignment();
    const size_t s_div_8 = size / 8;
    const size_t s_mod_8 = size % 8;

    if (size > 8) {
      //Needs to go on stack
      to = state->new_value();

      auto* to_val = state->value_tree.values.data + to.val;
      to_val->value_type = ValueType::NORMAL_STACK;
      to_val->stack_offset = state->stack.next_stack_local(size, align);

      load_const_to_stack(comp, state, code, type, (uint64_t*)from.constant, to, 0);
    }
    else {
      //need to fill in the upper bytes with zeros if less than 8
      uint64_t zerod_c = 0;

      if (s_mod_8 == 0) {
        zerod_c = *constant;
      }
      else {
        zerod_c = *constant & slow_bit_fill_lower<uint64_t>((uint8_t)s_mod_8);
      }

      to = state->new_value();

      ByteCode::EMIT::SET_R64_TO_64(code->code, (uint8_t)to.val, zerod_c);
      state->use_value(to);
    }

    comp->constants.free_no_destruct(from.constant);
  }
  else {
    to = state->new_value(from.index);
    state->use_value(from.index, to);

    ByteCode::EMIT::COPY_R64_TO_R64(code->code, (uint8_t)from.index.val, (uint8_t)to.val);
  }

  return to;
}

//Note: Recursive
static ValueOrConst compile_bytecode_of_expression(Compiler* const comp,
                                                   const ASTExpression* const expr,
                                                   State* const state,
                                                   CodeBlock* const code) {
  DEFER(state) { state->control_flow.expression_num++; };

  ValueOrConst result ={};

  if (expr->compile_time_constant && !state->comptime_compilation && !already_const_value(expr)) {

    assert(expr->const_val != nullptr);

    //Copy the value to a new constant
    const size_t size = expr->type->size();

    uint8_t* bytes = (uint8_t*)comp->constants.alloc_no_construct(size);
    const uint8_t* c_bytes = (const uint8_t*)expr->const_val;

    memcpy_ts(bytes, size, c_bytes, size);

    result.is_constant = true;
    result.constant = bytes;

    return result;
  }

  result.is_constant = false;

  switch (expr->expr_type) {
    case EXPRESSION_TYPE::INDEX: {

        const size_t base_size = static_cast<const ArrayStructure*>(expr->index.expr->type)->base->size();

        ValueIndex expr_val_stack;
        {
          ValueOrConst expr_val = compile_bytecode_of_expression(comp, expr->index.expr, state, code);
          if (expr_val.is_constant) {
            expr_val_stack = state->new_value();

            auto* expr_stack_v = state->value_tree.values.data + expr_val_stack.val;

            expr_stack_v->value_type = ValueType::NORMAL_STACK;
            expr_stack_v->stack_offset = state->stack.next_stack_local(expr->type->size(), expr->type->alignment());

            load_const_to_stack(comp, state, code, expr->index.expr->type, (uint64_t*)expr_val.constant, expr_val_stack, 0);
            comp->constants.free_no_destruct(expr_val.constant);
          }
          else {
            expr_val_stack = expr_val.index;
          }
        }

        const ValueOrConst index_val = compile_bytecode_of_expression(comp, expr->index.index, state, code);

        const uint64_t expr_stack_offset = ([&]() {
          //inside a block to stop it being accidentally used after a new value is added - invalidades the pointer
          auto* const expr_v = state->value_tree.values.data + expr_val_stack.val;
          assert(expr_v->on_stack());//hopefully temp
          return expr_v->stack_offset;
        })();//Immediately invocated

        if (index_val.is_constant) {

          const uint64_t stack_offset = expr_stack_offset - (*(uint64_t*)index_val.constant * base_size);

          result.index = state->new_value();
          ByteCode::EMIT::COPY_R64_FROM_STACK(code->code, (uint8_t)result.index.val, stack_offset);
          state->use_value(result.index);
        }
        else {
          const ValueIndex rbp = state->new_value();
          auto& rbp_val = state->value_tree.values.data[rbp.val];

          rbp_val.value_type = ValueType::FIXED;
          rbp_val.reg = comp->build_options.calling_convention->base_pointer_reg;

          MemComplex complex ={};
          complex.base   = (uint8_t)rbp.val;
          complex.index  = (uint8_t)index_val.index.val;
          complex.scale  = (uint8_t)base_size;
          complex.disp = (int32_t)expr_stack_offset;

          result.index = state->new_value(index_val.index);
          ByteCode::EMIT::COPY_R64_FROM_MEM_COMPLEX(code->code, (uint8_t)result.index.val, complex);
          state->use_value(index_val.index, result.index);
          state->use_value(result.index);
          state->use_value(rbp);
        }


        break;
      }
    case EXPRESSION_TYPE::ARRAY_EXPR: {
        const ArrayStructure* const arr_type = (const ArrayStructure*)expr->type;

        const size_t base_size = arr_type->base->size();

        const size_t full_align = arr_type->alignment();
        const size_t full_size = arr_type->size();

        result.is_constant = false;
        result.index = state->new_value();

        auto* res_val = state->value_tree.values.data + result.index.val;

        res_val->value_type = ValueType::NORMAL_STACK;
        res_val->stack_offset = state->stack.next_stack_local(full_size, full_align);

        int64_t offset = 0;

        auto i = expr->array_expr.elements.begin();
        const auto end = expr->array_expr.elements.end();

        //save stack as expression stack cant be used by anything - its copied anyway
        auto save_stack = state->stack.current;

        for (; i < end; i++) {
          auto leaf = compile_bytecode_of_expression(comp, i, state, code);

          copy_val_to_stack(comp, state, code, arr_type->base, leaf, result.index, offset);

          state->stack.current = save_stack;//reset stack
          offset -= base_size;//for next round
        }

        state->stack.current = save_stack;
        break;
      }
    case EXPRESSION_TYPE::ASCII_STRING: {
        const ArrayStructure* const arr_type = (const ArrayStructure*)expr->type;

        uint8_t* string_c = (uint8_t*)comp->constants.alloc_no_construct(arr_type->size());

        result.is_constant = true;
        result.constant = string_c;

        auto i = expr->ascii_string.string;
        const auto end = i + arr_type->length;

        for (; i < end; (i++, string_c++)) {
          *string_c = *i;
        }


        break;
      }
    case EXPRESSION_TYPE::ENUM: {
        uint64_t* enum_c = (uint64_t*)comp->constants.alloc_no_construct(expr->type->size());
        *enum_c = expr->enum_value.enum_value->representation;

        result.is_constant = true;
        result.constant = enum_c;
        break;
      }
    case EXPRESSION_TYPE::VALUE: {
        uint64_t* val_c = (uint64_t*)comp->constants.alloc_no_construct(expr->type->size());
        *val_c = expr->value.value;

        result.is_constant = true;
        result.constant = val_c;
        break;
      }
    case EXPRESSION_TYPE::LOCAL: {
        const ValueIndex local = state->find_local(expr->name)->val;

        auto* local_val = state->value_tree.values.data + local.val;

        if (local_val->on_stack() && expr->type->size() > 8) {
          result.is_constant = false;
          result.index = local;
        }
        else {
          result.index = state->new_value(local);
          state->use_value(local, result.index);

          ByteCode::EMIT::COPY_R64_TO_R64(code->code, (uint8_t)local.val, (uint8_t)result.index.val);
        }

        break;
      }
    case EXPRESSION_TYPE::CAST: {
        const CastExpr* const cast = &expr->cast;
        const ValueOrConst temp = compile_bytecode_of_expression(comp, cast->expr, state, code);

        result.index = copy_val_to_val(comp, state, code, expr->type, temp);

        cast->emit(state, code, result.index);
        break;
      }
    case EXPRESSION_TYPE::UNARY_OPERATOR: {
        const UnaryOperatorExpr* const un_op = &expr->un_op;
        const ValueOrConst temp = compile_bytecode_of_expression(comp, un_op->primary, state, code);

        result.index = copy_val_to_val(comp, state, code, expr->type, temp);

        un_op->emit(&comp->build_options, state, code, result.index);

        state->use_value(result.index);
        state->value_tree.values.data[result.index.val].is_modified = true;

        break;
      }
    case EXPRESSION_TYPE::BINARY_OPERATOR: {
        const BinaryOperatorExpr* const bin_op = &expr->bin_op;
        const ASTExpression* const left = bin_op->left;
        const ASTExpression* const right = bin_op->right;

        const ValueOrConst temp_left = compile_bytecode_of_expression(comp, left, state, code);
        const ValueOrConst temp_right = compile_bytecode_of_expression(comp, right, state, code);

        const ValueIndex left_index = copy_val_to_val(comp, state, code, left->type, temp_left);
        const ValueIndex right_index = copy_val_to_val(comp, state, code, left->type, temp_right);

        bin_op->emit(&comp->build_options, state, code, left_index, right_index);

        state->use_value(left_index);
        state->use_value(right_index);

        state->value_tree.values.data[left_index.val].is_modified = true;

        result.is_constant = false;
        result.index = left_index;
        break;
      }
    case EXPRESSION_TYPE::FUNCTION_CALL: {
        const FunctionCallExpr* const call = &expr->call;

        auto save_stack_params = state->stack.current_parameters;
        DEFER(&) { state->stack.current_parameters = save_stack_params; };

        state->made_call = true;

        Array<ValueOrConst> to_use_values;
        to_use_values.reserve_total(call->arguments.size);

        //Compile expression for arguments
        {
          const size_t size = call->arguments.size;

          for (size_t i = 0; i < size; i++) {
            const ASTExpression* inner_expr = call->arguments.data + i;
            const Structure* call_type = call->function->parameter_types.data[i];

            const ValueOrConst val = compile_bytecode_of_expression(comp, inner_expr, state, code);

            to_use_values.insert(val);
          }
        }

        //Create argument values
        {
          auto i = to_use_values.mut_begin();
          const auto end = to_use_values.end();

          const size_t size = to_use_values.size;

          for (size_t itr = 0; itr < size; itr++) {
            auto* i = to_use_values.data + itr;
            auto* i_s = call->arguments.data[itr].type;

            const ValueIndex p = copy_val_to_val(comp, state, code, i_s, *i);

            i->is_constant = false;
            i->index = p;
          }
        }

        state->control_flow.expression_num++;

        //Set argument registers and stack
        {
          size_t index = 0;
          const size_t max_reg_params = comp->build_options.calling_convention->num_parameter_registers;
          const size_t num_params = smaller(to_use_values.size, max_reg_params);

          for (; index < num_params; index++) {
            auto i = to_use_values.data + index;

            assert(!i->is_constant);

            state->use_value(i->index);

            auto* i_val = state->value_tree.values.data + i->index.val;

            i_val->value_type = ValueType::FIXED;
            i_val->reg        = comp->build_options.calling_convention->parameter_registers[index];
          }

          state->stack.push_stack_params(to_use_values.size - index);

          for (; index < to_use_values.size; index++) {
            auto i = to_use_values.data + index;

            state->use_value(i->index);

            auto* i_val = state->value_tree.values.data + i->index.val;

            i_val->value_type = ValueType::ARGUMENT_STACK;
            i_val->arg_num    = index - max_reg_params + state->stack.current_parameters;
          }
        }


        ByteCode::EMIT::CALL(code->code, call->function->code_block.label);
        state->control_flow.had_call = true;
        state->control_flow.last_call = state->control_flow.expression_num;

        state->control_flow.expression_num++;

        const ValueIndex rax = state->new_value();
        auto* rax_val = state->value_tree.values.data + rax.val;

        rax_val->value_type = ValueType::FIXED;
        rax_val->reg        = comp->build_options.calling_convention->return_register;


        //Fake copy so dont need to insert copy later if one is needed
        state->control_flow.expression_num++;

        result.is_constant = false;
        result.index = state->new_value(rax);

        state->use_value(rax, result.index);

        ByteCode::EMIT::COPY_R64_TO_R64(code->code, (uint8_t)rax.val, (uint8_t)result.index.val);

        break;
      }
  }
  return result;
}

void compile_bytecode_of_statement(Compiler* const comp,
                                   ASTStatement* const statement,
                                   State* const state,
                                   CodeBlock* const code) {
  switch (statement->type) {
    case STATEMENT_TYPE::BLOCK: {
        const auto num_locals = state->locals.size;
        DEFER(&) { state->locals.size = num_locals; };

        auto i = statement->block.block.mut_begin();
        const auto end = statement->block.block.mut_end();

        for (; i < end; i++) {
          compile_bytecode_of_statement(comp, i, state, code);
        }

        return;
      }
    case STATEMENT_TYPE::RETURN: {
        const ASTExpression* expr = &statement->expression;
        const ValueOrConst result = compile_bytecode_of_expression(comp,
                                                                   expr,
                                                                   state,
                                                                   code);

        const ValueIndex rax = copy_val_to_val(comp, state, code, expr->type, result);

        auto& rax_val = state->value_tree.values.data[rax.val];

        rax_val.value_type = ValueType::FIXED;
        rax_val.reg        = comp->build_options.calling_convention->return_register;

        ByteCode::EMIT::JUMP_TO_FIXED(code->code, state->return_label);
        return;
      }
    case STATEMENT_TYPE::EXPRESSION:
      compile_bytecode_of_expression(comp, &statement->expression, state, code);
      return;
    case STATEMENT_TYPE::IF_ELSE: {
        ASTIfElse* const if_else = &statement->if_else;

        const ValueOrConst cond = compile_bytecode_of_expression(comp,
                                                                 &if_else->condition,
                                                                 state,
                                                                 code);

        if (cond.is_constant) {
          //Just becomes a block - no need for flows and stuff

          const auto locals = state->locals.size;

          if (*(uint64_t*)cond.constant != 0) {
            //Compile if branch
            compile_bytecode_of_statement(comp, if_else->if_statement, state, code);
          }
          else {
            //Compile else branch
            compile_bytecode_of_statement(comp, if_else->else_statement, state, code);
          }

          state->locals.size = locals;
        }
        else {
          //Condition jump
          const uint64_t else_label = comp->labels++;

          ByteCode::EMIT::JUMP_TO_FIXED_IF_VAL_ZERO(code->code, (uint8_t)cond.index.val, else_label);
          state->use_value(cond.index);

          const size_t start_flow = state->control_flow.current_flow;

          const auto locals = state->locals.size;

          //If branch
          state->control_flow.new_flow();
          state->control_flow.set_a_flows_to_b(start_flow, state->control_flow.current_flow);
          compile_bytecode_of_statement(comp, if_else->if_statement, state, code);

          state->locals.size = locals;

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

          state->locals.size = locals;

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

        const ValueOrConst val = compile_bytecode_of_expression(comp, &decl->expression, state, code);

        state->locals.insert_uninit(1);

        Local* const local = state->locals.back();

        local->name = decl->name;
        local->type = decl->type.type;

        const bool is_stack = !val.is_constant && (state->value_tree.values.data + val.index.val)->on_stack();

        if (is_stack) {
          local->val  = val.index;
        }
        else {
          //Have to copy the value

          const ValueIndex temp_val = copy_val_to_val(comp, state, code, decl->expression.type, val);

          local->val  = temp_val;
          auto* const local_val = state->value_tree.values.data + local->val.val;

          if (!comp->optimization_options.non_stack_locals && local_val->value_type != ValueType::NORMAL_STACK) {
            assert(local_val->value_type == ValueType::FREE);

            local_val->value_type   = ValueType::NORMAL_STACK;
            local_val->stack_offset = state->stack.next_stack_local(local->type->size(), local->type->alignment());
          }
        }

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

  //Non-volatile registers need to be saved in the function if used
  uint8_t non_v_regs = 0;
  int64_t base_pointer_offset = 0;

  if (state->needs_new_frame()) {
    ByteCode::EMIT::PUSH_FRAME(temp);

    const uint8_t num_regs = options->system->num_registers;
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

#define OP_R_64(name) auto* v = UNROLL_COALESCE(p.val);\
        assert(!v->on_stack());\
        ByteCode::EMIT:: ## name ## (temp, v->reg, p.u64)

#define OP_R_MEM(name) auto* v = UNROLL_COALESCE(p.val);\
        auto* base = UNROLL_COALESCE(p.mem.base);\
        auto* index = UNROLL_COALESCE(p.mem.index);\
        assert(!v->on_stack());\
        assert(!base->on_stack());\
        assert(!index->on_stack());\
        auto s = p.mem.scale;\
        assert(s == 1 || s == 2 || s == 4 || s == 8);\
        ByteCode::EMIT:: ## name ## (temp, v->reg, MemComplex{base->reg, index->reg, p.mem.scale, p.mem.disp});

#define OP_8_64(name) ByteCode::EMIT:: ## name ## (temp, p.u8, p.u64)

#define OP_64_64(name) ByteCode::EMIT:: ## name ## (temp, p.u64_1, p.u64_2)

#define OP_64(name) ByteCode::EMIT:: ## name ## (temp, p.u64)

#define OP(name) ByteCode::EMIT:: ## name ## (temp)

#define OP_R(name) auto* v = UNROLL_COALESCE(p.val);\
        assert(!v->on_stack());\
        ByteCode::EMIT:: ## name ## (temp, v->reg)

#define OP_R_R(name) auto* v1 = UNROLL_COALESCE(p.val1);\
        auto* v2 = UNROLL_COALESCE(p.val2);\
        assert(!v1->on_stack());\
        assert(!v2->on_stack());\
        ByteCode::EMIT:: ## name ## (temp, v1->reg, v2->reg) 

#define OP_R_R_R(name) auto* v1 = UNROLL_COALESCE(p.val1);\
        auto* v2 = UNROLL_COALESCE(p.val2);\
        auto* v3 = UNROLL_COALESCE(p.val3);\
        assert(!v1->on_stack());\
        assert(!v2->on_stack());\
        assert(!v3->on_stack());\
        ByteCode::EMIT:: ## name ## (temp, v1->reg, v2->reg, v3->reg) 

#define X(name, structure) case ByteCode:: ## name: {\
      const auto p = ByteCode::PARSE:: ## name ## (bytecode);\
      structure(name);\
      bytecode += ByteCode::SIZE_OF:: ## name;\
      break;\
    }

  const uint8_t* bytecode = code->code.begin();
  const uint8_t* const end = code->code.end();

  while (bytecode < end) {
    switch (*bytecode) {
      case ByteCode::COPY_R64_FROM_MEM_COMPLEX: {
          const auto p = ByteCode::PARSE::COPY_R64_FROM_MEM_COMPLEX(bytecode);

          const Value* const v = UNROLL_COALESCE(p.val);
          const Value* const b = UNROLL_COALESCE(p.mem.base);
          const Value* const i = UNROLL_COALESCE(p.mem.index);

          assert(!v->on_stack());
          assert(!b->on_stack());
          assert(!i->on_stack());
          {
            auto s = p.mem.scale;
            assert(s == 1 || s == 2 || s == 4 || s == 8);
          }

          MemComplex mem ={};
          mem.base = b->reg;
          mem.index = i->reg;
          mem.scale = p.mem.scale;
          mem.disp = -p.mem.disp;//negate for stack

          if (b->reg == options->calling_convention->base_pointer_reg) {
            mem.disp += (int32_t)base_pointer_offset;
          }

          ByteCode::EMIT::COPY_R64_FROM_MEM_COMPLEX(temp, v->reg, mem);

          bytecode += ByteCode::SIZE_OF::COPY_R64_FROM_MEM_COMPLEX;
          break;
        }

      case ByteCode::SET_R64_TO_64: {
          const auto p = ByteCode::PARSE::SET_R64_TO_64(bytecode);

          const Value* const v = UNROLL_COALESCE(p.val);
          switch (v->value_type) {
            case ValueType::NORMAL_STACK: {
                const int64_t offset = v->stack_offset < 0 ? -v->stack_offset
                  : base_pointer_offset - v->stack_offset;

                ByteCode::EMIT::COPY_64_TO_STACK(temp, p.u64, offset);
                break;
              }
            case ValueType::ARGUMENT_STACK: {
                int64_t stack_offset = -((int64_t)8 * (int64_t)v->arg_num);

                ByteCode::EMIT::COPY_64_TO_STACK_TOP(temp, p.u64, stack_offset);
                break;
              }
            default: {
                ByteCode::EMIT::SET_R64_TO_64(temp, v->reg, p.u64);
                break;
              }
          }

          bytecode += ByteCode::SIZE_OF::SET_R64_TO_64;
          break;
        }
      case ByteCode::COPY_R8_TO_STACK: {
          const auto p = ByteCode::PARSE::COPY_R8_TO_STACK(bytecode);

          auto* v = UNROLL_COALESCE(p.val);
          assert(!v->on_stack());
          ByteCode::EMIT::COPY_R8_TO_STACK(temp, v->reg, base_pointer_offset - p.u64.sig_val);

          bytecode += ByteCode::SIZE_OF::COPY_R8_TO_STACK;
          break;
        }
      case ByteCode::COPY_R8_FROM_STACK: {
          const auto p = ByteCode::PARSE::COPY_R8_FROM_STACK(bytecode);

          auto* v = UNROLL_COALESCE(p.val);
          assert(!v->on_stack());
          ByteCode::EMIT::COPY_R8_FROM_STACK(temp, v->reg, base_pointer_offset - p.u64.sig_val);

          bytecode += ByteCode::SIZE_OF::COPY_R8_FROM_STACK;
          break;
        }
      case ByteCode::COPY_R64_FROM_STACK: {
          const auto p = ByteCode::PARSE::COPY_R64_FROM_STACK(bytecode);

          auto* v = UNROLL_COALESCE(p.val);
          assert(!v->on_stack());
          ByteCode::EMIT::COPY_R64_FROM_STACK(temp, v->reg, base_pointer_offset - p.u64.sig_val);

          bytecode += ByteCode::SIZE_OF::COPY_R64_FROM_STACK;
          break;
        }
      case ByteCode::COPY_R64_TO_STACK: {
          const auto p = ByteCode::PARSE::COPY_R64_TO_STACK(bytecode);

          auto* v = UNROLL_COALESCE(p.val);
          assert(!v->on_stack());
          ByteCode::EMIT::COPY_R64_TO_STACK(temp, v->reg, base_pointer_offset - p.u64.sig_val);

          bytecode += ByteCode::SIZE_OF::COPY_R64_TO_STACK;
          break;
        }
      case ByteCode::COPY_8_TO_STACK: {
          const auto p = ByteCode::PARSE::COPY_8_TO_STACK(bytecode);

          ByteCode::EMIT::COPY_8_TO_STACK(temp, p.u8, base_pointer_offset - p.u64.sig_val);

          bytecode += ByteCode::SIZE_OF::COPY_8_TO_STACK;
          break;
        }
      case ByteCode::COPY_64_TO_STACK: {
          const auto p = ByteCode::PARSE::COPY_64_TO_STACK(bytecode);

          ByteCode::EMIT::COPY_64_TO_STACK(temp, p.u64_1, base_pointer_offset - p.u64_2.sig_val);

          bytecode += ByteCode::SIZE_OF::COPY_64_TO_STACK;
          break;
        }
      case ByteCode::COPY_R64_TO_R64: {
          const auto p = ByteCode::PARSE::COPY_R64_TO_R64(bytecode);

          const Value* v1 = UNROLL_COALESCE(p.val1);
          const Value* v2 = UNROLL_COALESCE(p.val2);

          if (v1->reg != v2->reg) {
            //Cant have both be on stack
            assert(!(v1->on_stack() && v2->on_stack()));

            if (v1->on_stack()) {
              //Shouldnt be copying FROM the argument location
              assert(v1->value_type != ValueType::ARGUMENT_STACK);

              const int64_t offset = v1->stack_offset < 0 ? -v1->stack_offset
                : base_pointer_offset - v1->stack_offset;

              ByteCode::EMIT::COPY_R64_FROM_STACK(temp, v2->reg, offset);
            }
            else if (v2->on_stack()) {
              switch (v2->value_type) {
                case ValueType::NORMAL_STACK: {
                    const int64_t offset = v2->stack_offset < 0 ? -v2->stack_offset
                      : base_pointer_offset - v2->stack_offset;

                    ByteCode::EMIT::COPY_R64_TO_STACK(temp, v1->reg, offset);
                    break;
                  }
                case ValueType::ARGUMENT_STACK: {
                    int64_t stack_offset = -((int64_t)8 * (int64_t)v2->arg_num);

                    ByteCode::EMIT::COPY_R64_TO_STACK_TOP(temp, v1->reg, stack_offset);
                    break;
                  }
              }
            }
            else {
              ByteCode::EMIT::COPY_R64_TO_R64(temp, v1->reg, v2->reg);
            }
          }

          bytecode += ByteCode::SIZE_OF::COPY_R64_TO_R64;
          break;
        }
      default:
        switch (*bytecode) {
          BYTECODES_X
        }
        break;
    }
  }

#undef OP_R_64
#undef OP_R_MEM
#undef OP_8_64
#undef OP_64_64
#undef OP_64
#undef OP
#undef OP_R
#undef OP_R_R
#undef OP_R_R_R

  //Epilog
  {
    ByteCode::EMIT::LABEL(temp, state->return_label);

    //Load the non volatile regs
    uint8_t non_v_regs = options->system->num_registers + 1;
    const uint8_t num_regs = 0;
    for (; non_v_regs > num_regs; non_v_regs--) {
      if (regs & ((uint64_t)1 << (non_v_regs - 1))) {
        ByteCode::EMIT::COPY_R64_FROM_STACK(temp,
                                            options->system->all_registers[(non_v_regs - 1)].REG,
                                            base_pointer_offset);

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

    if (i_val->on_stack()) {
      continue;
    }

    for (size_t j = i + 1; j < tree.values.size; j++) {
      auto j_val = tree.values.data + j;

      if (j_val->on_stack()) {
        continue;
      }

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

      if (i->fixed() && i->is_modified) {//is_modified stops saving rbp and rsp as they are managed separately
        used_regs |= ((uint64_t)1 << (i->reg));
      }
    }
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

void coalesce(const Compiler* const comp, State* const state) {
  ValueTree& tree = state->value_tree;
  const ControlFlow& c_flow = state->control_flow;

  const size_t size = state->value_tree.adjacency_list.size;
  for (size_t l1 = 0; l1 < size; l1++) {
    auto& l1_val = tree.values.data[l1];

    //Store info needed to coalesce later
    bool is_child = false;
    bool is_child_by_modify = false;
    size_t child_of = 0;

    auto& list = tree.adjacency_list.data[l1];

    //Find values to coalsce into
    for (size_t i_el = 0; i_el < list.size; i_el++) {
      auto* i_index = list.data + i_el;

      //Reduce indirections
      *i_index = resolve_coalesced(*i_index, tree);
      const size_t l2 = i_index->val;

      if (l2 == l1) {
        continue;//just in case
      }

      auto& l2_val = tree.values.data[l2];

      //cant merge 2 values that are fixed at different values
      if (l1_val.value_type == ValueType::FIXED
          && l2_val.value_type == ValueType::FIXED
          && l1_val.reg != l2_val.reg) {
        continue;
      }


      //cant merge values that cross function calls with a volatile register

      if ((l1_val.crosses_call
          && l2_val.value_type == ValueType::FIXED
          && comp->build_options.calling_convention->is_volatile(l2_val.reg))
          ||
          (l2_val.crosses_call
          && l1_val.value_type == ValueType::FIXED
          && comp->build_options.calling_convention->is_volatile(l1_val.reg))) {
        continue;
      }


      const size_t created_by = resolve_coalesced(l1_val.creation.related_index, tree).val;

      //Test to see if l1 is a 'child' of l2
      if (created_by == l2) {
        if (!l1_val.is_modified && !l2_val.is_modified) {
            //l1 created l2 and l1 or l2 are ever modified so can potentially merge

          if (comp->print_options.coalesce_values) {
            printf("%llu is potential child of %llu\n", l1, l2);
          }

          is_child_by_modify = true;
          child_of = l2;
        }


        //l1 created by l2
        //was it the last thing l2 did?
        for (const ValueUse& use : l2_val.last_uses) {
          const size_t use_created = resolve_coalesced(use.related_index, tree).val;

          if (use_created == l1 //last thing l2 did was related to l1
              && use.time.flow == l1_val.creation.time.flow //same block
              && use.time.time == l1_val.creation.time.time /*same time*/) {
              //Last thing l2 did is create l1

              //Coalesce l1 into l2
            if (comp->print_options.coalesce_values) {
              printf("%llu is child of %llu\n", l1, l2);
            }

            is_child_by_modify = false;
            is_child = true;
            child_of = l2;
          }
        }

        //dont need to check any others - can only be created by one value
        break;
      }
    }

    if (is_child || is_child_by_modify) {
      auto& parent_val = tree.values.data[child_of];

      //Test for modify
      if (is_child || (!parent_val.is_modified && !l1_val.is_modified)) {

        if (is_child_by_modify && comp->print_options.coalesce_values) {
          printf("%llu is child of %llu\n", l1, child_of);
        }

        if (l1_val.fixed()) {
          parent_val.value_type = ValueType::FIXED;
          parent_val.reg = l1_val.reg;
        }

        l1_val.value_type = ValueType::COALESCED;
        l1_val.index.val  = child_of;

        parent_val.crosses_call |= l1_val.crosses_call;
        parent_val.is_modified  |= l1_val.is_modified;



        combine_last_uses(parent_val.last_uses, l1_val.last_uses, &c_flow);
        tree.combine_intersection(ValueIndex{ l1 }, ValueIndex{ child_of });
      }
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

CompileCode compile_function_body_unit(Compiler* const comp,
                                       ASTFunctionDeclaration* const ast_func,
                                       Function* const func,
                                       State* const state) {
  const BuildOptions* const build_options = &comp->build_options;

  CompileCode ret = CompileCode::NO_ERRORS;

  {
    const auto locals = state->locals.size;
    DEFER(&) { state->locals.size = locals; };

    Array<ASTStatement>& statements = ast_func->body.block;

    auto i = statements.mut_begin();
    const auto end = statements.mut_end();
    for (; i < end; i++) {
      ret = compile_type_of_statement(comp, func, state, i);
      if (ret != CompileCode::NO_ERRORS) {
        state->locals.size = locals;
        return ret;
      }
    }
  }

  //Cant error now???

  //Values for the parameters - allows them to be different from parameter registers
  {
    const size_t size = smaller(state->locals.size,
                                (size_t)comp->build_options.calling_convention->num_parameter_registers);

    for (size_t itr = 0; itr < size; itr++) {
      auto* i = state->locals.data + itr;

      auto p = state->new_value(i->val);
      state->use_value(p, i->val);

      ByteCode::EMIT::COPY_R64_TO_R64(func->code_block.code, (uint8_t)i->val.val, (uint8_t)p.val);

      if (!comp->optimization_options.non_stack_locals) {
        auto* p_val = state->value_tree.values.data + p.val;
        p_val->value_type = ValueType::NORMAL_STACK;

        if (comp->build_options.calling_convention->shadow_space_size >= ((itr + 1) * 8)) {
           //pushed RBP + Return address + other params in shadow space
          p_val->stack_offset = -8 - 8 - (8 * itr);
        }
        else {
          int64_t offset = state->stack.next_stack_local(8, 8);
          p_val->stack_offset = offset;
        }
      }


      i->val = p;
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
  state->locals.free();
  state->value_tree.free();
  state->control_flow.free();

  return ret;
}

static CompileCode compile_function_signature(Compiler* const comp,
                                              ASTFunctionDeclaration* const ast_func,
                                              Function* const func,
                                              State* const state) {
  CompileCode ret = CompileCode::NO_ERRORS;

  //Parameters
  {
    auto i_ast = ast_func->parameters.mut_begin();
    auto i = func->parameter_types.mut_begin();
    const auto end = func->parameter_types.end();

    while (i < end) {
      if (*i == nullptr) {
        ret = compile_type(comp, &i_ast->type);
        if (ret != CompileCode::NO_ERRORS) {
          return ret;
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
    state->locals.insert_uninit(ast_func->parameters.size);

    size_t index = 0;
    const size_t size = ast_func->parameters.size;

    for (; index < size; index++) {
      auto i = ast_func->parameters.data + index;
      auto l_i = state->locals.data + index;

      l_i->name = i->name;
      l_i->type = i->type.type;
      l_i->val = state->new_value();

      auto* l_val = state->value_tree.values.data + l_i->val.val;

      if ((size_t)comp->build_options.calling_convention->num_parameter_registers < index) {
        const size_t param_num = index - comp->build_options.calling_convention->num_parameter_registers;

        l_val->value_type = ValueType::NORMAL_STACK;

        //pushed rbp + return address + shadow space size + other params
        l_val->stack_offset = (int64_t)-8
          - (int64_t)8
          - comp->build_options.calling_convention->shadow_space_size
          - ((int64_t)8 * param_num);

        state->stack.push_stack_params(param_num);
      }
      else {
        l_val->value_type = ValueType::FIXED;
        l_val->reg = comp->build_options.calling_convention->parameter_registers[index];
      }
    }
  }

  func->code_block.label = comp->labels++;
  state->control_flow.expression_num++;

  //Last thing we do is the return type
  //This means it is a test if return type is nullptr to see if its compiled
  ret = compile_type(comp, &ast_func->return_type);
  func->return_type = ast_func->return_type.type;
  return ret;
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

static CompileCode test_circular_dependencies_impl(Compiler* const comp,
                                                   Array<CompilationUnitCarrier>& checked,
                                                   Array<CompilationUnitCarrier>& deps_stack,
                                                   const CompilationUnitCarrier* carrier) {
  if (!checked.contains(*carrier)) {
    if (deps_stack.contains(*carrier)) {
      printf("CIRCULAR DEPENDENCY ERROR!\n");
      return CompileCode::CIRCULAR_DEPENDENCY;
    }

    const CompilationUnitCarrier* i = nullptr;
    const CompilationUnitCarrier* end = nullptr;

    switch (carrier->type) {
      case COMPILATION_TYPE::FUNCTION: {
          auto& deps = (comp->function_units.data + carrier->index)->dependecies;
          i = deps.begin();
          end = deps.end();
          break;
        }
      case COMPILATION_TYPE::CONSTANT: {
          auto& deps = (comp->constant_units.data + carrier->index)->dependecies;
          i = deps.begin();
          end = deps.end();
          break;
        }
    }

    deps_stack.insert(*carrier);

    for (; i < end; i++) {
      CompileCode ret = test_circular_dependencies_impl(comp, checked, deps_stack, i);

      if (ret != CompileCode::NO_ERRORS) {
        return ret;
      }
    }

    deps_stack.pop();

    checked.insert(*carrier);
  }

  return CompileCode::NO_ERRORS;
}

static CompileCode test_circular_dependencies(Compiler* const comp) {
  Array<CompilationUnitCarrier> checked ={};
  Array<CompilationUnitCarrier> deps_stack ={};

  {
    auto i = comp->function_units.mut_begin();
    const auto end = comp->function_units.mut_end();

    for (; i < end; i++) {
      if (i->dependecies.size > 0) {
        CompilationUnitCarrier i_carrier ={};
        i_carrier.type = COMPILATION_TYPE::FUNCTION;
        i_carrier.index = i - comp->function_units.data;

        CompileCode ret = test_circular_dependencies_impl(comp, checked, deps_stack, &i_carrier);

        if (ret != CompileCode::NO_ERRORS) {
          return ret;
        }
      }
    }
  }

  {
    auto i = comp->constant_units.mut_begin();
    const auto end = comp->constant_units.mut_end();

    for (; i < end; i++) {
      if (i->dependecies.size > 0) {
        CompilationUnitCarrier i_carrier ={};
        i_carrier.type = COMPILATION_TYPE::CONSTANT;
        i_carrier.index = i - comp->constant_units.data;

        CompileCode ret = test_circular_dependencies_impl(comp, checked, deps_stack, &i_carrier);

        if (ret != CompileCode::NO_ERRORS) {
          return ret;
        }
      }
    }
  }

  return CompileCode::NO_ERRORS;
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
                    const CompileCode ret = compile_function_signature(comp,
                                                                       unit->source,
                                                                       unit->destination,
                                                                       &unit->state);

                    switch (ret) {
                      case CompileCode::NO_ERRORS:
                        unit->stage = FUNCTION_COMPILE_STAGE::BODY;
                        unit->state.return_label = comp->labels++;
                        i--;//Do it again straight away
                        break;

                      case CompileCode::FOUND_DEPENDENCY:
                        break;

                      case CompileCode::UNFOUND_DEPENDENCY:
                        unit->unfound_dependency = true;
                        break;

                      default:
                        return ret;
                    }
                    break;
                  }
                case FUNCTION_COMPILE_STAGE::BODY: {
                    const CompileCode ret = compile_function_body_unit(comp,
                                                                       unit->source,
                                                                       unit->destination,
                                                                       &unit->state);

                    switch (ret) {
                      case CompileCode::NO_ERRORS:
                        unit->stage = FUNCTION_COMPILE_STAGE::FINISHED;
                        break;

                      case CompileCode::FOUND_DEPENDENCY:
                        break;

                      case CompileCode::UNFOUND_DEPENDENCY:
                        unit->unfound_dependency = true;
                        break;

                      default:
                        return ret;
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
                    reset_type(comp->working_state);

                    const CompileCode ret = compile_type_of_expression(comp, unit->expr, comp->working_state);

                    switch (ret) {
                      case CompileCode::NO_ERRORS:
                        unit->stage = EXPRESSION_COMPILE_STAGE::TYPED;
                        unit->expr->const_val = comp->constants.alloc_no_construct(unit->expr->type->size());
                        i--;
                        break;

                      case CompileCode::FOUND_DEPENDENCY:
                        break;

                      case CompileCode::UNFOUND_DEPENDENCY:
                        unit->unfound_dependency = true;
                        break;

                      default:
                        return ret;
                    }
                    break;
                  }
                case EXPRESSION_COMPILE_STAGE::TYPED: {
                    reset_type(comp->working_state);

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
                    size_t new_flow = comp->working_state->control_flow.new_flow();
                    comp->working_state->control_flow.current_flow = new_flow;

                    //Compile bytecode
                    comp->working_state->comptime_compilation = true;
                    const ValueOrConst result = compile_bytecode_of_expression(comp, unit->expr, comp->working_state, &block);

                    //Effectively do a return
                    const ValueIndex rax = copy_val_to_val(comp, comp->working_state, &block, unit->expr->type, result);

                    auto& rax_val = comp->working_state->value_tree.values.data[rax.val];

                    rax_val.value_type = ValueType::FIXED;
                    rax_val.reg        = comp->build_options.calling_convention->return_register;
                    comp->working_state->use_value(rax);

                    ByteCode::EMIT::JUMP_TO_FIXED(block.code, comp->working_state->return_label);

                    //Graph colour
                    graph_colour_algo(comp, &block, comp->working_state);

                    //Backend
                    Array<uint8_t> code ={};
                    const size_t entry = vm_backend_single_func(code, &block, comp->labels);

                    if (comp->print_options.comptime_exec) {
                      printf("Some Compile Time Executed Code:\n");
                      ByteCode::print_bytecode(&vm_regs_name_from_num, stdout, code.data, code.size);
                    }

                    //Run the VM
                    vm_rum(comp->vm, code.data, entry);

                    if (unit->expr->type->size() > 8) {
                      printf("INTERNAL ERROR: Size of type too large for return %u", unit->expr->type->size());
                      return CompileCode::INTERNAL_ERROR;
                    }
                    else {
                      uint64_t val = comp->vm->registers[convention_vm.return_register].b64.reg;
                      *((uint64_t*)unit->expr->const_val) = val;

                      if (comp->print_options.comptime_exec) {
                        printf("\nComptime Res: %llu\n", val);
                      }
                    }

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
        i->dependecies.remove_if(DependencyFinished{ comp });

        if (i->stage != FUNCTION_COMPILE_STAGE::FINISHED && i->dependecies.size == 0 && !i->unfound_dependency) {
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
        i->dependecies.remove_if(DependencyFinished{ comp });

        if (i->stage != EXPRESSION_COMPILE_STAGE::FINISHED && i->dependecies.size == 0 && !i->unfound_dependency) {
          comp->compiling.insert_uninit(1);
          CompilationUnitCarrier* const carrier = comp->compiling.back();

          carrier->type = COMPILATION_TYPE::CONSTANT;
          carrier->index = i - comp->constant_units.data;
        }
      }
    }


    if (comp->compiling.size == 0) {
      //Test for circular dependencies
      CompileCode ret = test_circular_dependencies(comp);
      if (ret != CompileCode::NO_ERRORS) {
        return ret;
      }

      //Cant be any defined dependencies
      if (comp->unfound_dep_info.panic
          && comp->unfound_dep_info.num_function_units == comp->function_units.size
          && comp->unfound_dep_info.num_constant_units == comp->constant_units.size) {
        printf("DEPENDENCY ERROR: 1 or more Compilation units failed due to unfound dependencies!\n");
        return CompileCode::UNFOUND_DEPENDENCY;
      }
      else {
        comp->unfound_dep_info.panic = true;

        comp->unfound_dep_info.num_function_units = comp->function_units.size;
        comp->unfound_dep_info.num_constant_units = comp->constant_units.size;

        //Remove unfound dependencies

        {
          auto i = comp->function_units.mut_begin();
          const auto end = comp->function_units.mut_end();

          for (; i < end; i++) {
            if (i->stage != FUNCTION_COMPILE_STAGE::FINISHED) {
              i->unfound_dependency = true;

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
            if (i->stage != EXPRESSION_COMPILE_STAGE::FINISHED) {
              i->unfound_dependency = false;

              comp->compiling.insert_uninit(1);
              CompilationUnitCarrier* const carrier = comp->compiling.back();

              carrier->type = COMPILATION_TYPE::CONSTANT;
              carrier->index = i - comp->constant_units.data;
            }
          }
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

    printf("FUNCTION %s:\n", func->name.string);
    ByteCode::print_bytecode(comp->build_options.system->reg_name_from_num, stdout, func->code_block.code.data, func->code_block.code.size);
    printf("\n");
  }
}