#include "compiler.h"
#include "type.h"
#include "ast.h"
#include "bytecode.h"

Structure* Compiler::new_composite_structure() {
  Structure* s = structures.insert();
  s->type = STRUCTURE_TYPE::COMPOSITE;

  return s;
}

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

void load_language_builtin(Compiler* const comp) {
  {
    Structure* const s_void = comp->structures.insert();
    comp->lang->s_void = s_void;

    s_void->type = STRUCTURE_TYPE::VOID;
    s_void->name = comp->strings->intern("void");
  }

  {
    Structure* const s_bool = comp->structures.insert();
    comp->lang->s_bool = s_bool;

    s_bool->type = STRUCTURE_TYPE::ENUM;
    s_bool->name = comp->strings->intern("bool");

    s_bool->num_enum_elements = 2;


    EnumValue* const e_true = comp->enums.insert();
    comp->lang->e_true = e_true;

    e_true->type  = s_bool;
    e_true->name  = comp->strings->intern("true");
    e_true->value = 1;


    EnumValue* const e_false = comp->enums.insert();
    comp->lang->e_false = e_false;

    e_false->type  = s_bool;
    e_false->name  = comp->strings->intern("false");
    e_false->value = 0;
  }

  {
    Structure* const u8 = comp->structures.insert();
    comp->lang->s_u8 = u8;

    u8->type = STRUCTURE_TYPE::INTEGER;
    u8->name = comp->strings->intern("u8");

    u8->integer.is_signed = false;
    u8->integer.size = 1;
  }

  {
    Structure* const u64 = comp->structures.insert();
    comp->lang->s_u64 = u64;

    u64->type = STRUCTURE_TYPE::INTEGER;
    u64->name = comp->strings->intern("u64");

    u64->integer.is_signed = false;
    u64->integer.size = 8;
  }
}

Structure* Compiler::structure_by_name(const InternString name) {
  auto i = structures.begin_iter();
  const auto end = structures.end_iter();

  for (; i != end; i.next()) {
    Structure* s = i.get();

    if (s->name == name) {
      return s;
    }
  }

  return nullptr;
}

EnumValue* Compiler::enum_by_name(const InternString name) {
  auto i = enums.begin_iter();
  const auto end = enums.end_iter();

  for (; i != end; i.next()) {
    EnumValue* e = i.get();

    if (e->name == name) {
      return e;
    }
  }

  return nullptr;
}

CompileCode get_compiled_structure(Compiler* const comp, ASTType* type, const Structure** other, size_t other_n) {

  Structure* s = comp->structure_by_name(type->name);

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

static CompileCode find_function_for_call(Compiler* const comp,
                                          FunctionCallExpr* const call) {
  auto i = comp->functions.begin_iter();
  const auto end = comp->functions.end_iter();

  for (; i != end; i.next()) {
    Function* const func = i.get();

    //Correct name and number of args
    if (call->function_name == func->name
        && call->arguments.size == func->parameter_types.size) {
      auto p_call = call->arguments.begin();
      const auto end_call = call->arguments.end();

      auto p_func = func->parameter_types.begin();

      while (p_call < end_call) {
        if (p_call->type != *p_func) {
          //Escape out
          goto FAIL;
        }

        p_call++;
        p_func++;
      }

      //All matched
      call->function = func;
      return CompileCode::NO_ERRORS;
    }

  FAIL:
    continue;
  }

  printf("ERROR: Could not find function '%s' with appropriate signature\n"
         "       Call: '", call->function_name.string);
  print_function_call(call);
  printf("'\n");
  return CompileCode::UNFOUND_DEPENDENCY;
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
      expr->enum_value.enum_value = comp->enum_by_name(expr->enum_value.name);

      if (expr->enum_value.enum_value == nullptr) {
        printf("ERROR: Enum value not found '%s'\n", expr->enum_value.name.string);
        return CompileCode::UNFOUND_DEPENDENCY;
      }

      expr->type = expr->enum_value.enum_value->type;

      return CompileCode::NO_ERRORS;
    case EXPRESSION_TYPE::VALUE:
      expr->type = comp->lang->s_u64;
      return CompileCode::NO_ERRORS;

    case EXPRESSION_TYPE::NAME: {
        const InternString name = expr->name;

        auto i = state->locals.begin();
        const auto end = state->locals.end();

        for (; i < end; i++) {
          if (name == i->name) {

            expr->expr_type = EXPRESSION_TYPE::LOCAL;
            expr->type = i->type;

            return CompileCode::NO_ERRORS;
          }
        }

        printf("ERROR: '%s' is not a variable in this scope\n", name.string);
        return CompileCode::UNFOUND_DEPENDENCY;
      }
    case EXPRESSION_TYPE::BINARY_OPERATOR: {
        BinaryOperatorExpr* const bin_op = &expr->bin_op;

        bin_op->left->call_leaf  = expr->call_leaf;
        bin_op->right->call_leaf = expr->call_leaf;

        ret_code = compile_type_of_expression(comp, bin_op->left, state);
        if (ret_code != CompileCode::NO_ERRORS) {
          return ret_code;
        }

        ret_code = compile_type_of_expression(comp, bin_op->right, state);
        if (ret_code != CompileCode::NO_ERRORS) {
          return ret_code;
        }

        //TODO: left and right not the same
        if (bin_op->left->type != bin_op->right->type) {
          printf("ERROR: Expected left and right of binary operator to be same type (TEMP)\n"
                 "       Left: '%s', Right: '%s'\n",
                 bin_op->left->type->name.string, bin_op->right->type->name.string);
          return CompileCode::TYPE_CHECK_ERROR;
        }

        const auto return_type = [&]() {
          switch (bin_op->op) {
            case BINARY_OPERATOR::DIV:
            case BINARY_OPERATOR::MUL:
            case BINARY_OPERATOR::SUB:
            case BINARY_OPERATOR::ADD:
            case BINARY_OPERATOR::OR:
            case BINARY_OPERATOR::AND: return bin_op->left->type;

            case BINARY_OPERATOR::LESSER:
            case BINARY_OPERATOR::GREATER:
            case BINARY_OPERATOR::EQUIVALENT: return comp->lang->s_bool;
          }
        };

        expr->makes_call = bin_op->left->makes_call || bin_op->right->makes_call;

        expr->type = return_type();
        return CompileCode::NO_ERRORS;
      }
    case EXPRESSION_TYPE::FUNCTION_CALL: {
        FunctionCallExpr* const call = &expr->call;

        {
          auto i = call->arguments.mut_begin();
          const auto end = call->arguments.mut_end();

          for (; i < end; i++) {
            i->call_leaf = true;

            ret_code = compile_type_of_expression(comp, i, state);
            if (ret_code != CompileCode::NO_ERRORS) {
              return ret_code;
            }
          }
        }

        ret_code = find_function_for_call(comp, call);
        if (ret_code != CompileCode::NO_ERRORS) {
          return ret_code;
        }

        expr->type = call->function->return_type;
        expr->makes_call = true;

        return CompileCode::NO_ERRORS;
      }
  }

  printf("INTERNAL ERROR: Reached end of expression type checking without exiting\n"
         "                Expression type: %d\n", (int)expr->expr_type);
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

        ByteCode::EMIT::SET_VAL_TO_64(func->code, (uint8_t)result.val, expr->enum_value.enum_value->value);
        break;
      }
    case EXPRESSION_TYPE::VALUE: {
        result = state->new_value();

        ByteCode::EMIT::SET_VAL_TO_64(func->code, (uint8_t)result.val, expr->value);
        break;
      }
    case EXPRESSION_TYPE::LOCAL: {
        const ValueIndex local = state->find_local(expr->name)->val;
        result = state->new_value();

        state->use_value(local, result);
        state->value_tree.values.data[result.val].creation.related_index = local;

        ByteCode::EMIT::COPY_TO_VAL(func->code, (uint8_t)local.val, (uint8_t)result.val);
        break;
      }
    case EXPRESSION_TYPE::BINARY_OPERATOR: {
        const BinaryOperatorExpr* const bin_op = &expr->bin_op;
        const ASTExpression* const left = bin_op->left;
        const ASTExpression* const right = bin_op->right;

        const ValueIndex temp_left = compile_bytecode_of_expression(build_options, left, state, func);
        const ValueIndex temp_right = compile_bytecode_of_expression(build_options, right, state, func);

        const ValueIndex left_val = state->new_value(temp_left);
        const ValueIndex right_val = state->new_value(temp_right);

        state->use_value(temp_left, left_val);
        state->use_value(temp_right, right_val);

        ByteCode::EMIT::COPY_TO_VAL(func->code, (uint8_t)temp_left.val, (uint8_t)left_val.val);
        ByteCode::EMIT::COPY_TO_VAL(func->code, (uint8_t)temp_right.val, (uint8_t)right_val.val);

        state->control_flow.expression_num++;
        state->use_value(left_val);
        state->use_value(right_val);

        state->value_tree.values.data[left_val.val].is_modified = true;

        //Select operation
        switch (bin_op->op) {
          case BINARY_OPERATOR::ADD: {
              ByteCode::EMIT::ADD_VALS(func->code, (uint8_t)right_val.val, (uint8_t)left_val.val);
              break;
            }
          case BINARY_OPERATOR::SUB: {
              ByteCode::EMIT::SUB_VALS(func->code, (uint8_t)right_val.val, (uint8_t)left_val.val);
              break;
            }
          case BINARY_OPERATOR::MUL: {
              ByteCode::EMIT::MUL_VALS(func->code, (uint8_t)right_val.val, (uint8_t)left_val.val);
              break;
            }
          case BINARY_OPERATOR::DIV: {
              ByteCode::EMIT::DIV_VALS(func->code, (uint8_t)right_val.val, (uint8_t)left_val.val);
              break;
            }
          case BINARY_OPERATOR::EQUIVALENT: {
              ByteCode::EMIT::EQ_VALS(func->code, (uint8_t)right_val.val, (uint8_t)left_val.val);
              break;
            }
          case BINARY_OPERATOR::OR: {
              ByteCode::EMIT::OR_VALS(func->code, (uint8_t)right_val.val, (uint8_t)left_val.val);
              break;
            }
          case BINARY_OPERATOR::AND: {
              ByteCode::EMIT::AND_VALS(func->code, (uint8_t)right_val.val, (uint8_t)left_val.val);
              break;
            }
        }

        const ForcedColours colours = (build_options->system->bin_op_forced)(bin_op->op);

        state->value_map.data[right_val.val] = colours.val1;
        state->value_map.data[left_val.val] = colours.val2;

        result = left_val;

        break;
      }
    case EXPRESSION_TYPE::FUNCTION_CALL: {
        const FunctionCallExpr* const call = &expr->call;

        Array<ValueIndex> to_use_values;
        to_use_values.reserve_total(call->arguments.size);

        {
          auto i = call->arguments.begin();
          const auto end = call->arguments.end();

          for (; i < end; i++) {
            const ValueIndex val = compile_bytecode_of_expression(build_options, i, state, func);
            to_use_values.insert(val);
          }
        }

        //Create parameter values
        {
          auto i = to_use_values.mut_begin();
          const auto end = to_use_values.end();

          for (; i < end; i++) {
            auto p = state->new_value(*i);
            state->use_value(*i, p);

            ByteCode::EMIT::COPY_TO_VAL(func->code, (uint8_t)i->val, (uint8_t)p.val);

            *i = p;
          }
        }

        state->control_flow.expression_num++;

        //Set parameter registers
        {
          size_t index = 0;
          const size_t max = to_use_values.size;

          for (; index < max; index++) {
            auto i = to_use_values.begin();

            state->use_value(*i);

            state->value_map.data[i->val]
              = build_options->calling_convention->parameter_registers[index] + 1;
          }
        }


        ByteCode::EMIT::CALL(func->code, call->function->label);
        state->control_flow.had_call = true;
        state->control_flow.last_call = state->control_flow.expression_num;

        state->control_flow.expression_num++;

        const ValueIndex rax = state->new_value();
        *state->value_map.back() = build_options->calling_convention->return_register + 1;


        //Fake copy so dont need to insert copy later if one is needed
        state->control_flow.expression_num++;
        result = state->new_value(rax);
        state->use_value(rax, result);

        ByteCode::EMIT::COPY_TO_VAL(func->code, (uint8_t)rax.val, (uint8_t)result.val);

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
        size_t num_locals = state->locals.size;

        auto i = statement->block.block.mut_begin();
        const auto end = statement->block.block.mut_end();

        for (; i < end; i++) {
          compile_bytecode_of_statement(comp, i, state, func);
        }

        state->locals.size = num_locals;
        return;
      }
    case STATEMENT_TYPE::RETURN: {
        const ValueIndex result = compile_bytecode_of_expression(&comp->build_options, &statement->expression, state, func);

        const ValueIndex rax = state->new_value(result);
        state->use_value(result, rax);
        ByteCode::EMIT::COPY_TO_VAL(func->code, (uint8_t)result.val, (uint8_t)rax.val);

        state->value_map.data[rax.val] = comp->build_options.calling_convention->return_register + 1;
        state->use_value(result);

        ByteCode::EMIT::JUMP_TO_FIXED(func->code, state->return_label);
        return;
      }
    case STATEMENT_TYPE::EXPRESSION:
      compile_bytecode_of_expression(&comp->build_options, &statement->expression, state, func);
      return;
    case STATEMENT_TYPE::IF_ELSE: {
        ASTIfElse* const if_else = &statement->if_else;

        const ValueIndex cond = compile_bytecode_of_expression(&comp->build_options, &if_else->condition, state, func);

        //Condition jump
        const uint64_t else_label = comp->labels++;

        state->use_value(cond);
        ByteCode::EMIT::JUMP_TO_FIXED_IF_VAL_ZERO(func->code, (uint8_t)cond.val, else_label);

        const size_t start_flow = state->control_flow.current_flow;

        //If branch
        state->control_flow.new_flow();
        state->control_flow.set_a_flows_to_b(start_flow, state->control_flow.current_flow);
        compile_bytecode_of_statement(comp, if_else->if_statement, state, func);

        const size_t end_if_flow = state->control_flow.current_flow;

        //Jump from if branch to after the else branch
        const uint64_t escape_label = comp->labels++;
        ByteCode::EMIT::JUMP_TO_FIXED(func->code, escape_label);
        ByteCode::EMIT::LABEL(func->code, else_label);

        //Else branch
        state->control_flow.new_flow();
        state->control_flow.set_a_flows_to_b(start_flow, state->control_flow.current_flow);
        compile_bytecode_of_statement(comp, if_else->else_statement, state, func);

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

        state->locals.insert_uninit(1);

        Local* const local = state->locals.back();

        local->name = decl->name;
        local->val = val;
        local->type = decl->type.type;

        return;
      }
  }
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
          CompileCode ret = compile_type_of_expression(comp, &if_else->condition, state);
          if (ret != CompileCode::NO_ERRORS) {
            return ret;
          }
        }

        assert(if_else->condition.type != nullptr);

        //Must be bool
        if (if_else->condition.type != comp->lang->s_bool) {
          printf("ERROR: Expected type '%s' for an if condition\n"
                 "       Actual: '%s'\n",
                 comp->lang->s_bool->name.string, if_else->condition.type->name.string);
          return CompileCode::TYPE_CHECK_ERROR;
        }

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


        if (decl->expression.type == nullptr) {
          ret = compile_type_of_expression(comp, &decl->expression, state);
          if (ret != CompileCode::NO_ERRORS) {
            return ret;
          }
        }

        if (decl->type.type == nullptr) {
          ret = get_compiled_structure(comp, &decl->type, nullptr, 0);
          if (ret != CompileCode::NO_ERRORS) {
            return ret;
          }
        }

        if (decl->type.type == decl->expression.type) {
          printf("ERROR: Cannot initialize object with incorrect type!\n"
                 "       Declaration: '%s', Expression: '%s'\n",
                 decl->type.name.string, decl->expression.name.string);
          return CompileCode::TYPE_CHECK_ERROR;
        }

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
        }

        assert(expr->type != nullptr);

        if (expr->type == func->return_type) {

          return CompileCode::NO_ERRORS;
        }
        else {
          printf("ERROR: Cannot return incorrect type from function\n"
                 "       Return Type: '%s', Expression '%s'\n",
                 func->return_type->name.string, expr->type->name.string);

          return CompileCode::TYPE_CHECK_ERROR;
        }
      }

  }

  printf("INTERNAL ERROR: Reached end of statement type checking without exiting\n"
         "                Statement type: %d\n", (int)statement->type);
  return CompileCode::UNFOUND_DEPENDENCY;
}

static void load_prolog_and_epilog(const BuildOptions* const options,
                                   Function* const func, const State* const state, uint64_t regs) {
  //Only non volatiles
  regs &= options->calling_convention->non_volatiles_bit_mask;
  
  //Prolog
  {
    Array<uint8_t> temp ={};

    //Call label
    ByteCode::EMIT::LABEL(temp, func->label);
    ByteCode::EMIT::PUSH_FRAME(temp);

    //Non-volatile registers need to be saved if the function is used
    uint8_t non_v_regs = 0;
    const uint8_t num_regs = options->system->num_registers;
    for (; non_v_regs < num_regs; non_v_regs++) {
      if (regs & ((uint64_t)1 << non_v_regs)) {
        ByteCode::EMIT::PUSH_VAL(temp, options->system->all_registers[non_v_regs].REG);
      }
    }

    temp.reserve_extra(func->code.size);
    memcpy_ts(temp.data + temp.size, temp.capacity - temp.size,
              func->code.data, func->code.size);
    temp.size += func->code.size;

    //Save new function
    func->code = std::move(temp);
  }

  //Epilog

  {
    ByteCode::EMIT::LABEL(func->code, state->return_label);

    uint8_t non_v_regs = options->system->num_registers + 1;
    const uint8_t num_regs = 0;
    for (; non_v_regs > num_regs; non_v_regs--) {
      if (regs & ((uint64_t)1 << (non_v_regs - 1))) {
        ByteCode::EMIT::POP_TO_VAL(func->code, options->system->all_registers[(non_v_regs - 1)].REG);
      }
    }

    ByteCode::EMIT::POP_FRAME(func->code);
    ByteCode::EMIT::RETURN(func->code);
  }
}


constexpr static ValueIndex resolve_coalesced(ValueIndex i, const ValueTree& tree) noexcept {
  while (tree.values.data[i.val].is_coalesced) {
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
  Array<uint8_t>& value_map = state->value_map;
  const ValueTree& tree = state->value_tree;

  constexpr auto copy_array = [](const Array<Array<ValueIndex>>& from, Array<Array<ValueIndex>>& to) {
    to.reserve_total(from.size);

    auto i = from.begin();
    auto end = from.end();
    auto to_i = to.mut_begin();

    for (; i < end; (i++, to_i++)) {
      to_i->reserve_total(i->size);
      to_i->size = i->size;
      memcpy_ts(to_i->data, to_i->size, i->data, i->size);
    }
  };

  Array<Array<ValueIndex>> a_l ={};
  copy_array(tree.adjacency_list, a_l);

  size_t num_values = 0;

  //Coalesced values are converted to tombstones + count how many active values
  for (uint64_t i = 0; i < tree.values.size; i++) {
    if (!tree.values.data[i].is_coalesced) {
      num_values++;
    }
    else if (value_map.data[i] > 0) {
      //Fixed value 
      continue;
    }
    else {
      a_l.data[i].free();//make tombstones
    }
  }

  size_t* const stack = allocate_zerod<size_t>(num_values);
  size_t index = 0;
  size_t num_intersects = 0;

  //Load stack - runs while there are active values not in the stack
  while (index < num_values) {
    for (uint64_t i = 0; i < tree.values.size; i++) {

      auto& intersects = a_l.data[i];
      if (intersects.capacity == 0) {
        continue; // tombstone can be skipped
      }
      else if (intersects.size >= num_intersects) {
        //Valid to put in the stack
        stack[index++] = i;

        //Go down to previous level as this will remove intersects
        num_intersects--;

        //Remove this intersect from all others
        auto i_arr = intersects.begin();
        const auto end_arr = intersects.end();
        for (; i_arr < end_arr; i_arr++) {
          struct Lambda {
            ValueIndex test;

            bool operator()(const ValueIndex v) const {
              return test == v;
            }
          };

          //Remove i from these intersections
          a_l.data[i_arr->val].remove_if(Lambda{ ValueIndex{i} });
        }

        intersects.free();
      }
    }

    num_intersects++;
  }

  //Load colours
  {
    uint64_t regs ={};//not going to have more than 64 registers ... hopefully

    auto i = stack + num_values - 1;
    const auto end = stack;

    for (; i >= end; i--) {
      if (value_map.data[*i] > 0) {
        continue;
      }

      regs = 0;

      auto i_again = stack + num_values - 1;

      //For each adjacent check for reserved colours
      for (; i_again > end; i_again--) {
        if (tree.intersection_check.test_a_intersects_b(*i_again, *i)) {
          //If adjacent remove that colour
          const uint8_t possible_colour = value_map.data[*i_again];
          if (possible_colour > 0) {
            regs |= ((uint64_t)1 << (possible_colour - 1));
          }
        }
      }

      uint8_t colour = 0;
      if (tree.values.data[*i].crosses_call) {
        //requires non volatile reg
        colour = options->calling_convention->num_volatile_registers;
      }

      //Find first index that is 0 (i.e. a free colour/reg)
      //Search in order of options->calling_convention->all_regs_unordered because this will do
      //volatile registers first and then non volatile registers if required
      while ((regs & ((uint64_t)1 << options->calling_convention->all_regs_unordered[colour])) != 0) {
        colour++;
      }

      value_map.data[*i] = options->calling_convention->all_regs_unordered[colour] + 1;
    }
  }

  //Load coalesced values
  {
    auto i = tree.values.begin();
    const auto end = tree.values.end();

    for (size_t i = 0; i < tree.values.size; i++) {
      const auto* ptr = tree.values.data + i;
      if (ptr->is_coalesced) {
        const size_t index = resolve_coalesced(ptr->index, tree).val;

        value_map.data[i] = value_map.data[index];
      }
    }
  }

  free(stack);


  uint64_t used_regs = 0;

  {
    auto i = value_map.begin();
    const auto end = value_map.end();

    for (; i < end; i++) {
      if (*i > 0) {
        used_regs |= ((uint64_t)1 << (*i - 1));
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

      {
        //cant merge value that crosses function with a volatile register
        auto l1_colour = state->value_map.data[l1];
        auto l2_colour = state->value_map.data[l2];

        if (l1_colour != 0 && l2_colour != 0 && l1_colour != l2_colour) {
          continue;
        }


        const size_t num_volatile = comp->build_options.calling_convention->num_volatile_registers;

        if ((l1_val.crosses_call && l2_colour != 0
            && (l2_colour + 1) < num_volatile)
            || (l2_val.crosses_call && l1_colour != 0
            && (l1_colour + 1) < num_volatile)) {
          continue;
        }
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

        //Cant be child due to one being modified
        l1_val.is_coalesced = true;
        l1_val.index.val = child_of;

        parent_val.crosses_call |= l1_val.crosses_call;
        parent_val.is_modified |= l1_val.is_modified;

        auto child_colour = state->value_map.data[l1];
        if (child_colour > 0) {
          state->value_map.data[child_of] = child_colour;
        }

        combine_last_uses(parent_val.last_uses, l1_val.last_uses, &c_flow);
        tree.combine_intersection(ValueIndex{ l1 }, ValueIndex{ child_of });
      }
    }
  }
}

void map_values(Array<uint8_t>& arr, const Array<uint8_t>& value_map) noexcept {
  Array<uint8_t> temp ={};


#define OP_VAL_64(name) ByteCode::EMIT:: ## name ## (temp, value_map.data[p.val] - 1, p.u64)
#define OP_64(name) ByteCode::EMIT:: ## name ## (temp, p.u64)
#define OP(name) ByteCode::EMIT:: ## name ## (temp)
#define OP_VAL(name) ByteCode::EMIT:: ## name ## (temp, value_map.data[p.val] - 1)
#define OP_VAL_VAL(name) ByteCode::EMIT:: ## name ## (temp, value_map.data[p.val1] - 1, value_map.data[p.val2] - 1) 

#define X(name, structure) case ByteCode:: ## name: {\
      const auto p = ByteCode::PARSE:: ## name ## (bytecode);\
      structure(name);\
      bytecode += ByteCode::SIZE_OF:: ## name;\
      break;\
    }

  const uint8_t* bytecode = arr.data;
  while (bytecode < arr.data + arr.size) {
    switch (*bytecode) {
      case ByteCode::COPY_TO_VAL: {
          const auto p = ByteCode::PARSE::COPY_TO_VAL(bytecode);

          const uint8_t v1 = value_map.data[p.val1] - 1;
          const uint8_t v2 = value_map.data[p.val2] - 1;

          if (v1 != v2) {
            ByteCode::EMIT::COPY_TO_VAL(temp, v1, v2);
          }

          bytecode += ByteCode::SIZE_OF::COPY_TO_VAL;
          break;
        }
      default:
        switch (*bytecode) {
          BYTECODES_X
        }
        break;
    }
  }

#undef OP_VAL_64
#undef OP_64
#undef OP
#undef OP_VAL
#undef OP_VAL_VAL

  arr = std::move(temp);
}

CompileCode compile_function_body_unit(Compiler* const comp,
                                       ASTFunctionDeclaration* const ast_func,
                                       Function* const func,
                                       State* const state) {
  const BuildOptions* const build_options = &comp->build_options;

  CompileCode ret = CompileCode::NO_ERRORS;

  {
    Array<ASTStatement>& statements = ast_func->body.block;

    auto i = statements.mut_begin();
    const auto end = statements.mut_end();
    for (; i < end; i++) {
      ret = compile_type_of_statement(comp, func, state, i);
      if (ret != CompileCode::NO_ERRORS) {
        return ret;
      }
    }
  }

  //Cant error now???

  //Values for the parameters - allows them to be different from parameter registers
  {
    auto i = state->locals.mut_begin();
    const auto end = state->locals.mut_end();

    for (; i < end; i++) {
      auto p = state->new_value(i->val);
      state->use_value(p, i->val);

      ByteCode::EMIT::COPY_TO_VAL(func->code, (uint8_t)i->val.val, (uint8_t)p.val);

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
  map_values(func->code, state->value_map);

  //No longer needed
  state->locals.free();
  state->value_tree.free();
  state->control_flow.free();

  state->value_map.shrink();

  ////Setup registers 
  //{
  //  const auto conv = build_options->calling_convention;

  //  state->num_volatiles = conv->num_volatile_registers;
  //  state->num_non_volatiles = conv->num_non_volatile_registers;
  //  const size_t total_regs = state->num_volatiles + state->num_non_volatiles;

  //  state->registers = allocate_zerod<StateRegister>(total_regs);

  //  size_t i = 0;
  //  for (; i < state->num_volatiles; i++) {
  //    state->registers[i].reg = conv->volatile_registers[i];
  //  }

  //  for (; i < total_regs; i++) {
  //    state->registers[i].reg = conv->non_volatile_registers[i - state->num_volatiles];
  //  }


  //  //Load parameter registers
  //  const size_t num_param_regs = ast_func->parameters.size < conv->num_parameter_registers ? ast_func->parameters.size : conv->num_parameter_registers;
  //  auto p = conv->parameter_registers;
  //  auto p_ast = ast_func->parameters.begin();
  //  const auto end = p + num_param_regs;

  //  for (; p < end; (p++, p_ast++)) {
  //    for (i = 0; i < state->num_non_volatiles; i++) {
  //      if (state->registers[i].reg == *p) {
  //        const auto val = state->get_local(p_ast->name);

  //        state->values.data[val].current_reg = (uint8_t)i;
  //        state->registers[i].val = val;
  //        state->registers[i].ever_used = true;
  //        state->registers[i].occupied = true;
  //        break;
  //      }
  //    }
  //  }
  //}

  //Save the non-volatile registers
  load_prolog_and_epilog(&comp->build_options, func, state, regs);

  ////No longer need the register array
  //free(state->registers);

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
    const size_t max = ast_func->parameters.size;

    for (; index < max; index++) {
      auto i = ast_func->parameters.data + index;
      auto l_i = state->locals.data + index;

      l_i->name = i->name;
      l_i->type = i->type.type;
      l_i->val  = state->new_value();

      state->value_map.data[l_i->val.val]
        = comp->build_options.calling_convention->parameter_registers[index] + 1;
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
    unit->stage  = FUNCTION_COMPILE_STAGE::SIGNATURE;
    unit->status = COMPILE_STATUS::OK;
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
                  unit->stage  = FUNCTION_COMPILE_STAGE::BODY;
                  unit->status = COMPILE_STATUS::OK;
                  unit->state.return_label = comp->labels++;
                  i--;//Do it again straight away
                  break;

                case CompileCode::TYPE_CHECK_ERROR:
                case CompileCode::UNFOUND_DEPENDENCY:
                  unit->status = COMPILE_STATUS::HAD_ERROR; break;
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

                case CompileCode::TYPE_CHECK_ERROR:
                case CompileCode::UNFOUND_DEPENDENCY:
                  unit->status = COMPILE_STATUS::HAD_ERROR; break;
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
          if (i->status == COMPILE_STATUS::OK) {
            comp->compiling.insert_uninit(1);
            CompilationUnitCarrier* const carrier = comp->compiling.back();

            carrier->type  = COMPILATION_TYPE::FUNCTION;
            carrier->index = end - i;
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
    ByteCode::print_bytecode(comp->build_options.system->reg_name_from_num, stdout, func->code.data, func->code.size);
    printf("\n");
  }
}