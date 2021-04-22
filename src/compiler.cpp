#include "compiler.h"
#include "type.h"
#include "ast.h"
#include "bytecode.h"
#include "parser.h"

Function* Compiler::new_function() {
  return functions.insert();
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

CompileCode get_compiled_structure(Compiler* const comp, ASTType* type, const Structure** other, size_t other_n) {
  if (type->type != nullptr) {
    //Incase recalled
    return CompileCode::NO_ERRORS;
  }

  const Structure* s = comp->types->structure_by_name(type->name);

  if (s == nullptr) {
    printf("ERROR: could not find structure '%s'\n", type->name.string);
    return CompileCode::UNFOUND_DEPENDENCY;
  }
  else {
    type->type = s;

    for (size_t i = 0; i < other_n; i++) {
      other[i] = s;
    }

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

//Note: Recursive
static CompileCode compile_type_of_expression(Compiler* const comp,
                                              ASTExpression* const expr,
                                              const State* const state) {
  //Already typed
  if (expr->type != nullptr) {
    return CompileCode::NO_ERRORS;
  }

  //just a lil buffer so I dont have to allocate one in every switch case
  CompileCode ret_code = CompileCode::NO_ERRORS;

  switch (expr->expr_type) {
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

        ret_code = get_compiled_structure(comp, &expr->cast.type, nullptr, 0);
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


        expr->compile_time_constant = bin_op->left->compile_time_constant && bin_op->right->compile_time_constant;
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
            ret_code = compile_type_of_expression(comp, i, state);
            if (ret_code != CompileCode::NO_ERRORS) {
              return ret_code;
            }

            i->call_leaf = true;

            expr->compile_time_constant &= i->compile_time_constant;
          }
        }

        ret_code = find_function_for_call(comp, call);
        if (ret_code != CompileCode::NO_ERRORS) {
          return ret_code;
        }

        expr->type = call->function->return_type;
        expr->makes_call = true;

        break;
      }
    default: {
        printf("INTERNAL ERROR: Invalid expression type\n"
               "                Expression type: %d\n", (int)expr->expr_type);
        return CompileCode::UNFOUND_DEPENDENCY;
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

        auto i = statement->block.block.mut_begin();
        const auto end = statement->block.block.mut_end();

        for (; i < end; i++) {
          ret = compile_type_of_statement(comp, func, state, i);
          if (ret != CompileCode::NO_ERRORS) {
            return ret;
          }
        }

        state->locals.size = locals;

        return ret;
      }
    case STATEMENT_TYPE::DECLARATION: {
        ASTDeclaration* const decl = &statement->declaration;

        if (decl->type.type == nullptr) {
          ret = get_compiled_structure(comp, &decl->type, nullptr, 0);
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

          ret = check_cast(decl->expression.type, decl->type.type);
          if (ret != CompileCode::NO_ERRORS) {
            return ret;
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

          ret = check_cast(expr->type, func->return_type);
          if (ret != CompileCode::NO_ERRORS) {
            return ret;
          }
        }

        assert(expr->type != nullptr);
        return CompileCode::NO_ERRORS;
      }

  }

  printf("INTERNAL ERROR: Reached end of statement type checking without exiting\n"
         "                Statement type: %d\n", (int)statement->type);
  return CompileCode::UNFOUND_DEPENDENCY;
}

//Note: Recursive
static ValueIndex compile_bytecode_of_expression(const BuildOptions* const build_options,
                                                 const ASTExpression* const expr,
                                                 State* const state,
                                                 Function* const func) {

  ValueIndex result ={};

  switch (expr->expr_type) {
    case EXPRESSION_TYPE::ENUM: {
        result = state->new_value();

        ByteCode::EMIT::SET_R64_TO_64(func->code, (uint8_t)result.val, expr->enum_value.enum_value->representation);
        break;
      }
    case EXPRESSION_TYPE::VALUE: {
        result = state->new_value();

        ByteCode::EMIT::SET_R64_TO_64(func->code, (uint8_t)result.val, expr->value.value);
        break;
      }
    case EXPRESSION_TYPE::LOCAL: {
        const ValueIndex local = state->find_local(expr->name)->val;

        result = state->new_value(local);
        state->use_value(local, result);

        ByteCode::EMIT::COPY_R64_TO_R64(func->code, (uint8_t)local.val, (uint8_t)result.val);
        break;
      }
    case EXPRESSION_TYPE::CAST: {
        const CastExpr* const cast = &expr->cast;
        const ValueIndex temp = compile_bytecode_of_expression(build_options, cast->expr, state, func);

        result = state->new_value(temp);
        state->use_value(temp, result);

        ByteCode::EMIT::COPY_R64_TO_R64(func->code, (uint8_t)temp.val, (uint8_t)result.val);

        state->control_flow.expression_num++;

        cast->emit(state, func, result);
        break;
      }
    case EXPRESSION_TYPE::UNARY_OPERATOR: {
        const UnaryOperatorExpr* const un_op = &expr->un_op;
        const ValueIndex temp = compile_bytecode_of_expression(build_options, un_op->primary, state, func);

        result = state->new_value(temp);
        state->use_value(temp, result);

        ByteCode::EMIT::COPY_R64_TO_R64(func->code, (uint8_t)temp.val, (uint8_t)result.val);

        state->control_flow.expression_num++;

        un_op->emit(build_options, state, func, result);

        state->use_value(result);
        state->value_tree.values.data[result.val].is_modified = true;

        break;
      }
    case EXPRESSION_TYPE::BINARY_OPERATOR: {
        const BinaryOperatorExpr* const bin_op = &expr->bin_op;
        const ASTExpression* const left = bin_op->left;
        const ASTExpression* const right = bin_op->right;

        const ValueIndex temp_left = compile_bytecode_of_expression(build_options, left, state, func);
        const ValueIndex temp_right = compile_bytecode_of_expression(build_options, right, state, func);

        const ValueIndex left_index = state->new_value(temp_left);
        const ValueIndex right_index = state->new_value(temp_right);

        state->use_value(temp_left, left_index);
        state->use_value(temp_right, right_index);

        ByteCode::EMIT::COPY_R64_TO_R64(func->code, (uint8_t)temp_left.val, (uint8_t)left_index.val);
        ByteCode::EMIT::COPY_R64_TO_R64(func->code, (uint8_t)temp_right.val, (uint8_t)right_index.val);

        state->control_flow.expression_num++;

        bin_op->emit(build_options, state, func, left_index, right_index);

        state->use_value(left_index);
        state->use_value(right_index);

        state->value_tree.values.data[left_index.val].is_modified = true;

        result = left_index;
        break;
      }
    case EXPRESSION_TYPE::FUNCTION_CALL: {
        const FunctionCallExpr* const call = &expr->call;

        state->made_call = true;

        Array<ValueIndex> to_use_values;
        to_use_values.reserve_total(call->arguments.size);

        //Compile expression for arguments
        {
          const size_t size = call->arguments.size;

          for (size_t i = 0; i < size; i++) {
            const ASTExpression* inner_expr = call->arguments.data + i;
            const Structure* call_type = call->function->parameter_types.data[i];

            const ValueIndex val = compile_bytecode_of_expression(build_options, inner_expr, state, func);

            to_use_values.insert(val);
          }
        }

        //Create argument values
        {
          auto i = to_use_values.mut_begin();
          const auto end = to_use_values.end();

          for (; i < end; i++) {
            auto p = state->new_value(*i);
            state->use_value(*i, p);

            ByteCode::EMIT::COPY_R64_TO_R64(func->code, (uint8_t)i->val, (uint8_t)p.val);

            *i = p;
          }
        }

        state->control_flow.expression_num++;

        //Set argument registers and stack
        {
          size_t index = 0;
          const size_t max_reg_params = build_options->calling_convention->num_parameter_registers;
          const size_t num_params = smaller(to_use_values.size, max_reg_params);

          for (; index < num_params; index++) {
            auto i = to_use_values.begin();

            state->use_value(*i);

            auto* i_val = state->value_tree.values.data + i->val;

            i_val->value_type = ValueType::FIXED;
            i_val->reg        = build_options->calling_convention->parameter_registers[index];
          }

          for (; index < to_use_values.size; index++) {
            auto i = to_use_values.begin();

            state->use_value(*i);

            auto* i_val = state->value_tree.values.data + i->val;

            i_val->value_type = ValueType::ARGUMENT_STACK;
            i_val->arg_num    = index - max_reg_params;
          }
        }


        ByteCode::EMIT::CALL(func->code, call->function->label);
        state->control_flow.had_call = true;
        state->control_flow.last_call = state->control_flow.expression_num;

        state->control_flow.expression_num++;

        const ValueIndex rax = state->new_value();
        auto* rax_val = state->value_tree.values.data + rax.val;

        rax_val->value_type = ValueType::FIXED;
        rax_val->reg        = build_options->calling_convention->return_register;


        //Fake copy so dont need to insert copy later if one is needed
        state->control_flow.expression_num++;
        result = state->new_value(rax);
        state->use_value(rax, result);

        ByteCode::EMIT::COPY_R64_TO_R64(func->code, (uint8_t)rax.val, (uint8_t)result.val);

        break;
      }
  }

  state->control_flow.expression_num++;
  return result;
}

void compile_bytecode_of_statement(Compiler* const comp,
                                   ASTStatement* const statement,
                                   State* const state,
                                   Function* const func) {
  switch (statement->type) {
    case STATEMENT_TYPE::BLOCK: {
        const auto num_locals = state->locals.size;

        auto i = statement->block.block.mut_begin();
        const auto end = statement->block.block.mut_end();

        for (; i < end; i++) {
          compile_bytecode_of_statement(comp, i, state, func);
        }

        state->locals.size = num_locals;
        return;
      }
    case STATEMENT_TYPE::RETURN: {
        const ValueIndex result = compile_bytecode_of_expression(&comp->build_options,
                                                                 &statement->expression,
                                                                 state,
                                                                 func);

        const ValueIndex rax = state->new_value(result);
        state->use_value(result, rax);
        ByteCode::EMIT::COPY_R64_TO_R64(func->code, (uint8_t)result.val, (uint8_t)rax.val);
        state->control_flow.expression_num++;

        auto& rax_val = state->value_tree.values.data[rax.val];

        rax_val.value_type = ValueType::FIXED;
        rax_val.reg   = comp->build_options.calling_convention->return_register;
        state->use_value(result);

        ByteCode::EMIT::JUMP_TO_FIXED(func->code, state->return_label);
        return;
      }
    case STATEMENT_TYPE::EXPRESSION:
      compile_bytecode_of_expression(&comp->build_options, &statement->expression, state, func);
      return;
    case STATEMENT_TYPE::IF_ELSE: {
        ASTIfElse* const if_else = &statement->if_else;

        const ValueIndex cond = compile_bytecode_of_expression(&comp->build_options,
                                                               &if_else->condition,
                                                               state,
                                                               func);

        //Condition jump
        const uint64_t else_label = comp->labels++;

        ByteCode::EMIT::JUMP_TO_FIXED_IF_VAL_ZERO(func->code, (uint8_t)cond.val, else_label);
        state->use_value(cond);

        const size_t start_flow = state->control_flow.current_flow;

        const auto locals = state->locals.size;

        //If branch
        state->control_flow.new_flow();
        state->control_flow.set_a_flows_to_b(start_flow, state->control_flow.current_flow);
        compile_bytecode_of_statement(comp, if_else->if_statement, state, func);

        state->locals.size = locals;

        const size_t end_if_flow = state->control_flow.current_flow;

        //Jump from if branch to after the else branch
        const uint64_t escape_label = comp->labels++;
        ByteCode::EMIT::JUMP_TO_FIXED(func->code, escape_label);
        ByteCode::EMIT::LABEL(func->code, else_label);

        //Else branch
        state->control_flow.new_flow();
        state->control_flow.set_a_flows_to_b(start_flow, state->control_flow.current_flow);
        compile_bytecode_of_statement(comp, if_else->else_statement, state, func);

        state->locals.size = locals;

        const size_t end_else_flow = state->control_flow.current_flow;

        //Leave the if
        state->control_flow.new_flow();
        state->control_flow.set_a_flows_to_b(start_flow, state->control_flow.current_flow);
        state->control_flow.set_a_flows_to_b(end_else_flow, state->control_flow.current_flow);

        ByteCode::EMIT::LABEL(func->code, escape_label);
        return;
      }
    case STATEMENT_TYPE::DECLARATION: {
        ASTDeclaration* const decl = &statement->declaration;

        const ValueIndex val = compile_bytecode_of_expression(&comp->build_options, &decl->expression, state, func);

        state->use_value(val);

        state->locals.insert_uninit(1);

        Local* const local = state->locals.back();

        local->name = decl->name;
        local->type = decl->type.type;

        if (!comp->optimization_options.non_stack_locals) {
          const ValueIndex temp_val = state->new_value(val);
          state->use_value(val, temp_val);
          ByteCode::EMIT::COPY_R64_TO_R64(func->code, (uint8_t)val.val, (uint8_t)temp_val.val);

          state->control_flow.expression_num++;

          local->val  = temp_val;

          auto* local_val = state->value_tree.values.data + local->val.val;

          local_val->value_type   = ValueType::NORMAL_STACK;
          local_val->stack_offset = state->stack.next_stack_local(local->type->size(), local->type->alignment());
        }
        else {
          state->use_value(val);

          local->val  = val;
        }

        return;
      }
  }
}

static void map_values(const BuildOptions* const options,
                       Function* const func, const State* const state, uint64_t regs) {
  //Only non volatiles
  regs &= options->calling_convention->non_volatiles_bit_mask;

  Array<uint8_t> temp ={};

  //Prolog

  //Call label
  ByteCode::EMIT::LABEL(temp, func->label);
  ByteCode::EMIT::PUSH_FRAME(temp);

  //Non-volatile registers need to be saved in the function if used
  uint8_t non_v_regs = 0;
  int64_t base_pointer_offset = 0;

  {
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

#define OP_R64_64(name) auto* v = UNROLL_COALESCE(p.val);\
        assert(!v->on_stack());\
        ByteCode::EMIT:: ## name ## (temp, v->reg, p.u64)

#define OP_64(name) ByteCode::EMIT:: ## name ## (temp, p.u64)
#define OP(name) ByteCode::EMIT:: ## name ## (temp)

#define OP_R64(name) auto* v = UNROLL_COALESCE(p.val);\
        assert(!v->on_stack());\
        ByteCode::EMIT:: ## name ## (temp, v->reg)

#define OP_R64_R64(name) auto* v1 = UNROLL_COALESCE(p.val1);\
        auto* v2 = UNROLL_COALESCE(p.val2);\
        assert(!v1->on_stack());\
        assert(!v2->on_stack());\
        ByteCode::EMIT:: ## name ## (temp, v1->reg, v2->reg) 

#define X(name, structure) case ByteCode:: ## name: {\
      const auto p = ByteCode::PARSE:: ## name ## (bytecode);\
      structure(name);\
      bytecode += ByteCode::SIZE_OF:: ## name;\
      break;\
    }

  const uint8_t* bytecode = func->code.begin();
  const uint8_t* const end = func->code.end();

  while (bytecode < end) {
    switch (*bytecode) {
      case ByteCode::COPY_R64_TO_R64: {
          const auto p = ByteCode::PARSE::COPY_R64_TO_R64(bytecode);

          const Value* v1 = UNROLL_COALESCE(p.val1);
          const Value* v2 = UNROLL_COALESCE(p.val2);

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
          else if (v1->reg != v2->reg) {
            ByteCode::EMIT::COPY_R64_TO_R64(temp, v1->reg, v2->reg);
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

#undef OP_R64_64
#undef OP_64
#undef OP
#undef OP_R64
#undef OP_R64_R64

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

    ByteCode::EMIT::POP_FRAME(temp);
    ByteCode::EMIT::RETURN(temp);
  }

  func->code = std::move(temp);
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
        if (i_val->value_type == ValueType::FREE) {
          from.insert(*from_i);
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

  free(stack);

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

CompileCode compile_function_body_unit(Compiler* const comp,
                                       ASTFunctionDeclaration* const ast_func,
                                       Function* const func,
                                       State* const state) {
  const BuildOptions* const build_options = &comp->build_options;

  CompileCode ret = CompileCode::NO_ERRORS;

  {
    const auto locals = state->locals.size;
    Array<ASTStatement>& statements = ast_func->body.block;

    auto i = statements.mut_begin();
    const auto end = statements.mut_end();
    for (; i < end; i++) {
      ret = compile_type_of_statement(comp, func, state, i);
      if (ret != CompileCode::NO_ERRORS) {
        return ret;
      }
    }

    state->locals.size = locals;
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

      ByteCode::EMIT::COPY_R64_TO_R64(func->code, (uint8_t)i->val.val, (uint8_t)p.val);

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
      compile_bytecode_of_statement(comp, i, state, func);
    }
  }

  if (comp->print_options.pre_reg_alloc) {
    printf("\n=== %s Pre Register Allocation Bytecode ===\n\n", func->name.string);
    ByteCode::print_bytecode(&reg_num_as_string, stdout, func->code.data, func->code.size);
    printf("\n=============================\n\n");
  }

  // Simplified Graph colouring Algorithm
  //
  // Build -> Coalesce -> Select
  //

  //Computers all the intersections in the value tree based on control flow
  //Build section
  compute_value_intersections(state->value_tree, state->control_flow);
  coalesce(comp, state);
  const uint64_t regs = select(build_options, state);

  //map values and function prolog and epilog
  map_values(&comp->build_options, func, state, regs);

  //No longer needed
  state->locals.free();
  state->value_tree.free();
  state->control_flow.free();


  func->code.shrink();

  if (comp->print_options.normal_bytecode) {
    printf("\n=== %s Normal Bytecode ===\n\n", func->name.string);
    ByteCode::print_bytecode(build_options->system->reg_name_from_num, stdout, func->code.data, func->code.size);
    printf("\n=============================\n\n");
  }

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
        ret = get_compiled_structure(comp, &i_ast->type, i, 1);
        if (ret != CompileCode::NO_ERRORS) {
          return ret;
        }
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
      }
      else {
        l_val->value_type = ValueType::FIXED;
        l_val->reg = comp->build_options.calling_convention->parameter_registers[index];
      }
    }
  }

  func->label = comp->labels++;

  state->control_flow.expression_num++;

  //Return types - no need to check if compiled as will never be here if its
  return get_compiled_structure(comp, &ast_func->return_type, &func->return_type, 1);
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

CompileCode compile_all(Compiler* const comp) {
  while (comp->compiling.size > 0) {
    //Compile waiting
    {
      auto i = comp->compiling.mut_begin();
      const auto end = comp->compiling.mut_end();

      for (; i < end; i++) {

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

                case CompileCode::INTERNAL_ERROR:
                case CompileCode::TYPE_CHECK_ERROR:
                case CompileCode::UNFOUND_DEPENDENCY:
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

                case CompileCode::INTERNAL_ERROR:
                case CompileCode::TYPE_CHECK_ERROR:
                case CompileCode::UNFOUND_DEPENDENCY:
                  return ret;
              }
              break;
            }
        }
      }
    }

    //Reset compiling
    comp->compiling.size = 0;

    {
      auto i = comp->function_units.mut_begin();
      const auto end = comp->function_units.mut_end();

      for (; i < end; i++) {
        if (i->stage != FUNCTION_COMPILE_STAGE::FINISHED) {
          comp->compiling.insert_uninit(1);
          CompilationUnitCarrier* const carrier = comp->compiling.back();

          carrier->type = COMPILATION_TYPE::FUNCTION;
          carrier->index = end - i;
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
    ByteCode::print_bytecode(comp->build_options.system->reg_name_from_num, stdout, func->code.data, func->code.size);
    printf("\n");
  }
}