#include "compiler.h"
#include "type.h"
#include "ast.h"
#include "bytecode.h"

void print_all_regs(const System* system, const Local_State* const state) {
  printf("Raw Registers: |");

  for (uint8_t i = 0; i < Local_State::NUM_REGS; i++) {
    const Local_Register& reg = state->registers[i];
    const char* name = system->reg_name_from_num(i);

    printf(" %s: %hhu |", name, reg.occupied_size);
  }

  printf("\nValue Registers: |");

  {
    auto i = state->values.begin();
    const auto end = state->values.end();

    for (; i < end; i++) {
      if (i->location.type == LOCATION_TYPE::REGISTER) {
        const Local_Register& reg = state->registers[i->location.reg];
        const char* name = system->reg_name_from_num(i->location.reg);

        printf(" %s %hhu |", name, reg.occupied_size);
      }
    }
  }

  putc('\n', stdout);
}

Structure* Compiler::new_composite_structure() {
  Structure* s = structures.insert();
  s->type = STRUCTURE_TYPE::COMPOSITE;

  return s;
}

Function* Compiler::new_function() {
  return functions.insert();
}


int64_t Local_State::get_stack_location(const Structure* structure) {
  //CONSIDER: This could probably be optimized a lot if its slow

  const uint64_t alignment = structure->alignment();
  const uint64_t size = structure->size();

  const uint64_t mod_a = next_free_stack_offset % alignment;
  if (mod_a == 0) {
    next_free_stack_offset -= size;
  }
  else {
    next_free_stack_offset -= (alignment - mod_a);
    next_free_stack_offset -= size;
  }

  if (next_free_stack_offset < max_stack) {
    max_stack = next_free_stack_offset;
  }

  return next_free_stack_offset;
}

uint8_t Local_State::get_free_reg() {
  for (uint8_t i = 0; i < NUM_REGS; i++) {
    if (registers[i].occupied_size == 0 && registers[i].modified != MODIFICATION_TYPE::MANAGED) {

      registers[i].modified      = MODIFICATION_TYPE::MODIFIED;
      registers[i].occupied_size = 8;
      return i;
    }
  }

  return NUM_REGS;
}

// "reg < Local_State::NUM_REGS" will always be true if this was successful
uint8_t Local_State::get_free_reg_from_set(const uint8_t* regs, const size_t num_regs) {
  const auto end = regs + num_regs;

  for (; regs < end; regs++) {
    Local_Register& reg = registers[*regs];

    if (reg.occupied_size == 0 && reg.modified != MODIFICATION_TYPE::MANAGED) {

      reg.modified      = MODIFICATION_TYPE::MODIFIED;
      reg.occupied_size = 8;

      return *regs;
    }
  }

  return NUM_REGS;
}

Location Local_State::get_free_space(const Structure* type) {
  Location l ={};

  if (type->size() <= 8) {
    for (uint8_t i = 0; i < NUM_REGS; i++) {
      if (registers[i].occupied_size == 0 && registers[i].modified != MODIFICATION_TYPE::MANAGED) {

        registers[i].modified      = MODIFICATION_TYPE::MODIFIED;
        registers[i].occupied_size = 8;


        l.type = LOCATION_TYPE::REGISTER;
        l.reg  = i;
        return l;
      }
    }
  }

  l.type = LOCATION_TYPE::STACK;
  l.stack_disp = get_stack_location(type);
  return l;
}

void Local_State::free_reg(uint8_t reg) {
  registers[reg].occupied_size = 0;
}

void Local_State::free_value(size_t i) {
  const auto val = get_value_location(i);
  if (val->location.type == LOCATION_TYPE::REGISTER) {
    free_reg(val->location.reg);

    val->modifiers = VALUE_MODIFIERS::NONE;
    val->location.type = LOCATION_TYPE::UNKNOWN;
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

CompileCode get_compiled_structure(Compiler* const comp, ASTType* type, Structure** other, size_t other_n) {

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

void print_function_call(const FunctionCallExpr* const call) {
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
    if (call->function_name == func->signature.name
        && call->arguments.size == func->signature.parameter_types.size) {
      auto p_call = call->arguments.begin();
      const auto end_call = call->arguments.end();

      auto p_func = func->signature.parameter_types.begin();

      while (p_call < end_call) {
        if (p_call->type != p_func->structure) {
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
                                              const Local_State* const state) {
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
            //Local variable's location is known BUT different from expression location
            expr->local = i->location;
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

        expr->type = call->function->signature.return_type.structure;
        expr->makes_call = true;

        return CompileCode::NO_ERRORS;
      }
  }

  printf("INTERNAL ERROR: Reached end of expression type checking without exiting\n"
         "                Expression type: %d\n", (int)expr->expr_type);
  return CompileCode::UNFOUND_DEPENDENCY;
}

static void emit_mov_64_to_location(Array<uint8_t>& arr, X64_UNION x64, const Location& location) {
  if (location.type == LOCATION_TYPE::REGISTER) {
    ByteCode::emit_mov_64_to_r(arr, x64, location.reg);
  }
  else if (location.type == LOCATION_TYPE::STACK) {
    ByteCode::emit_mov_64_to_m(arr, x64, RBP.REG, location.stack_disp);
  }
}

static void emit_mov_location_to_r(Array<uint8_t>& arr, const Location& location, const uint8_t reg) {
  if (location.type == LOCATION_TYPE::REGISTER) {
    //No need to move if same reg
    if (location.reg == reg) {
      return;
    }

    ByteCode::emit_mov_r_to_r(arr, location.reg, reg);
  }
  else if (location.type == LOCATION_TYPE::STACK) {
    ByteCode::emit_mov_m_to_r(arr, RBP.REG, location.stack_disp, reg);
  }
}

static void emit_mov_stack_to_stack(Local_State* const state, Array<uint8_t>& arr, const int64_t from, const int64_t to) {
  //Dont need to move if same
  if (to != from) {
    uint8_t temp = state->get_free_reg();

    //TODO: Larger than 64 bits
    ByteCode::emit_mov_m_to_r(arr, RBP.REG, from, temp);
    ByteCode::emit_mov_r_to_m(arr, temp, RBP.REG, to);

    state->free_reg(temp);
  }
}

static void emit_mov_location_to_stack(Local_State* const state, Array<uint8_t>& arr, const Location& location, const int64_t offset) {
  if (location.type == LOCATION_TYPE::REGISTER) {
    ByteCode::emit_mov_r_to_m(arr, location.reg, RBP.REG, offset);
  }
  else if (location.type == LOCATION_TYPE::STACK) {
    emit_mov_stack_to_stack(state, arr, location.stack_disp, offset);
  }
}

static void emit_mov_location_to_location(Local_State* const state, Array<uint8_t>& arr, const Location& from, const Location& to) {
  switch (to.type) {
    case LOCATION_TYPE::REGISTER: {
        emit_mov_location_to_r(arr, from, to.reg);
        break;
      }
    case LOCATION_TYPE::STACK: {
        emit_mov_location_to_stack(state, arr, from, to.stack_disp);
        break;
      }
  }
}

static void emit_mov_r_to_location(Local_State* const state, Array<uint8_t>& arr, uint8_t from, const Location& to) {
  switch (to.type) {
    case LOCATION_TYPE::REGISTER: {
        if (from != to.reg) {
          ByteCode::emit_mov_r_to_r(arr, from, to.reg);
        }
        break;
      }
    case LOCATION_TYPE::STACK: {
        ByteCode::emit_mov_r_to_m(arr, from, RBP.REG, to.stack_disp);
        break;
      }
  }
}

static void emit_mov_location_to_parameter(Local_State* const state, Array<uint8_t>& arr, const Location& from, int64_t paramter_offset) {
  constexpr auto SHADOW_OFFSET = 16;
  paramter_offset -= SHADOW_OFFSET;

  switch (from.type) {
    case LOCATION_TYPE::REGISTER: {
        ByteCode::emit_mov_r_to_m(arr, from.reg, RSP.REG, paramter_offset);
        break;
      }
    case LOCATION_TYPE::STACK: {
        const uint8_t temp = state->get_free_reg();

        ByteCode::emit_mov_m_to_r(arr, RBP.REG, from.stack_disp, temp);
        ByteCode::emit_mov_r_to_m(arr, temp, RSP.REG, paramter_offset);

        state->free_reg(temp);
        break;
      }
  }
}

constexpr bool VOLATILE_IS_SAFE(const ASTExpression* const expr) {
  return !expr->call_leaf || !expr->makes_call;
}

static void find_new_reg_home_from_set(Local_State* const state,
                                       Function* const func,
                                       ValueLocation* const vl,
                                       const ValueLocation* const end,
                                       const uint8_t* set_one,
                                       const size_t num_set_one,
                                       const uint8_t* set_two,
                                       const size_t num_set_two) {
  //Try first set
  uint8_t reg = state->get_free_reg_from_set(set_one, num_set_one);

  if (reg < Local_State::NUM_REGS) {
    ByteCode::emit_mov_r_to_r(func->bytecode, vl->location.reg, reg);

    vl->location.reg = reg;
    return;
  }

  //Try second set
  reg = state->get_free_reg_from_set(set_two, num_set_two);

  if (reg < Local_State::NUM_REGS) {
    ByteCode::emit_mov_r_to_r(func->bytecode, vl->location.reg, reg);

    vl->location.reg = reg;
    return;
  }

  //Else move to stack
  const auto stack_disp = state->get_stack_location(vl->type);
  ByteCode::emit_mov_r_to_m(func->bytecode, vl->location.reg, RBP.REG, stack_disp);

  vl->location.type       = LOCATION_TYPE::STACK;
  vl->location.stack_disp = stack_disp;
}

//if valid_outs is nullptr then the rest of regs will be searched
static uint8_t force_free_reg_from_set(Local_State* const state,
                                       Function* const func,
                                       const uint8_t* regs,
                                       const size_t num_regs,
                                       const uint8_t* valid_outs,
                                       const size_t num_valid_outs) {
  const auto maybe_reg = state->get_free_reg_from_set(regs, num_regs);

  if (maybe_reg < Local_State::NUM_REGS) {
    return maybe_reg;
  }

  auto i = state->values.mut_begin();
  const auto end = state->values.end();

  for (; i < end; i++) {
    if (i->modifiers != VALUE_MODIFIERS::FIXED && i->location.type == LOCATION_TYPE::REGISTER) {
      auto i_reg = regs;
      const auto end_reg = regs + num_regs;

      for (; i_reg < end_reg; i_reg++) {
        if (i->location.reg == *i_reg) {
          if (i->modifiers != VALUE_MODIFIERS::RESERVED) {
            //use i_reg because we know the others are occupied
            find_new_reg_home_from_set(state, func, i, end, i_reg, end_reg - i_reg, valid_outs, num_valid_outs);
          }

          return *i_reg;
        }
      }
    }
  }

  //Lets hope we never get here lol that would be bad
  printf("INTERNAL ERROR: The comment says \"Lets hope we never get here lol that would be bad\"\n"
         "                Turns out we did get here ...\n");
  //WE get here a lot for how we should never be here
  return Local_State::NUM_REGS;//return this as a safe-ish error
}

static void set_expression_location_to_reg(const BuildOptions* const build_options,
                                           ASTExpression* const expr,
                                           Local_State* const state,
                                           Function* const func) {

  if (expr->value_index != 0) {
    if (state->get_value_location(expr->value_index)->location.type == LOCATION_TYPE::REGISTER) {
      //TODO: Volatile, Non-volatile
      return;
    }
  }

  uint8_t reg = Local_State::NUM_REGS;

  //Get the correct registers
  if (VOLATILE_IS_SAFE(expr)) {
    reg = state->get_free_reg_from_set(build_options->calling_convention->volatile_registers,
                                       build_options->calling_convention->num_volatile_registers);

    if (reg >= Local_State::NUM_REGS) {
      reg = state->get_free_reg_from_set(build_options->calling_convention->non_volatile_registers,
                                         build_options->calling_convention->num_non_volatile_registers);
    }
  }
  else {
    reg = state->get_free_reg_from_set(build_options->calling_convention->non_volatile_registers,
                                       build_options->calling_convention->num_non_volatile_registers);
  }

  //Did we find a register?
  if (reg >= Local_State::NUM_REGS) {
    if (VOLATILE_IS_SAFE(expr)) {
      reg = force_free_reg_from_set(state, func,
                                    build_options->calling_convention->volatile_registers,
                                    build_options->calling_convention->num_volatile_registers,
                                    nullptr, 0);
    }
    else {
      reg = force_free_reg_from_set(state, func,
                                    build_options->calling_convention->non_volatile_registers,
                                    build_options->calling_convention->num_non_volatile_registers,
                                    nullptr, 0);
    }
  }

  //100% have a free register now

  state->values.insert_uninit(1);
  const size_t v_index = state->values.size;

  ValueLocation& vl = state->values.data[v_index - 1];

  vl.modifiers     = VALUE_MODIFIERS::LAZY;
  vl.location.type = LOCATION_TYPE::REGISTER;
  vl.location.reg  = reg;
  vl.type          = expr->type;

  if (expr->value_index != 0) {
    ValueLocation* const vl_old = state->get_value_location(expr->value_index);

    emit_mov_location_to_r(func->bytecode, vl_old->location, reg);
  }

  expr->value_index = v_index;
}


//Note: Recursive
static void compile_bytecode_of_expression(const BuildOptions* const build_options,
                                           ASTExpression* const expr,
                                           Local_State* const state,
                                           Function* const func) {

  //print_all_regs(build_options->system, state);
  ValueLocation* vl = state->get_value_location(expr->value_index);

  switch (expr->expr_type) {
    case EXPRESSION_TYPE::ENUM: {
        emit_mov_64_to_location(func->bytecode, expr->enum_value.enum_value->value, vl->location);
        break;
      }
    case EXPRESSION_TYPE::VALUE: {
        emit_mov_64_to_location(func->bytecode, expr->value, vl->location);
        break;
      }
    case EXPRESSION_TYPE::LOCAL: {
        emit_mov_location_to_location(state, func->bytecode, expr->local, vl->location);
        break;
      }
    case EXPRESSION_TYPE::BINARY_OPERATOR: {
        BinaryOperatorExpr* const bin_op = &expr->bin_op;
        ASTExpression* const left = bin_op->left;
        ASTExpression* const right = bin_op->right;

        const bool REVERSE_SAVE = vl->location.type == LOCATION_TYPE::REGISTER
          && vl->location.reg == build_options->calling_convention->return_register
          && left->makes_call
          && right->makes_call;

        constexpr auto set_as_acc =[](const BuildOptions* const build_options,
                                      ASTExpression* const expr,
                                      ValueLocation* const vl,
                                      ASTExpression* const acc,
                                      Local_State* const state,
                                      Function* const func)
        {
          if (vl->location.type == LOCATION_TYPE::REGISTER && acc->value_index == 0) {
            if (vl->modifiers == VALUE_MODIFIERS::NONE) {
              vl->modifiers = VALUE_MODIFIERS::LAZY;
            }

            acc->value_index = expr->value_index;
          }
          else {
            set_expression_location_to_reg(build_options, acc, state, func);
          }
        };

        if (REVERSE_SAVE) {
          set_expression_location_to_reg(build_options, left, state, func);
        }
        else {
          set_as_acc(build_options, expr, vl, left, state, func);
        }

        //vl could now be invalid

        compile_bytecode_of_expression(build_options, left, state, func);

        if (REVERSE_SAVE) {
          set_as_acc(build_options, expr, vl, right, state, func);
        }
        else {
          set_expression_location_to_reg(build_options, right, state, func);
        }

        compile_bytecode_of_expression(build_options, right, state, func);

        const size_t acc_index   = REVERSE_SAVE ? right->value_index : left->value_index;
        const size_t other_index = REVERSE_SAVE ? left->value_index : right->value_index;

        const uint8_t acc_reg   = state->get_value_location(acc_index)->location.reg;
        const uint8_t other_reg = state->get_value_location(other_index)->location.reg;

        //Select operation
        switch (bin_op->op) {
          case BINARY_OPERATOR::ADD: {
              ByteCode::emit_add_r_to_r(func->bytecode, other_reg, acc_reg);
              break;
            }
          case BINARY_OPERATOR::SUB: {
              ByteCode::emit_sub_r_to_r(func->bytecode, other_reg, acc_reg);
              break;
            }
          case BINARY_OPERATOR::MUL: {
              ByteCode::emit_mul_r_to_r(func->bytecode, other_reg, acc_reg);
              break;
            }
          case BINARY_OPERATOR::DIV: {
              ByteCode::emit_div_r_to_r(func->bytecode, other_reg, acc_reg);
              break;
            }
          case BINARY_OPERATOR::EQUIVALENT: {
              //TODO: Think about flags, no need to do 2 cmps
              ByteCode::emit_cmp_r_to_r(func->bytecode, other_reg, acc_reg);
              ByteCode::emit_set_r_to_zf(func->bytecode, acc_reg);
              break;
            }
          case BINARY_OPERATOR::OR: {
              ByteCode::emit_or_r_to_r(func->bytecode, other_reg, acc_reg);
              break;
            }
          case BINARY_OPERATOR::AND: {
              ByteCode::emit_and_r_to_r(func->bytecode, other_reg, acc_reg);
              break;
            }
        }

        //Manually right value
        //Should be finished wit it now??? (currently no variables kept in registers)
        state->free_value(other_index);

        //Reset vl
        vl = state->get_value_location(expr->value_index);

        if (expr->value_index != acc_index) {
          //Move result to output register and free temporary registers
          emit_mov_r_to_location(state, func->bytecode, acc_reg, vl->location);

          state->free_value(acc_index);
        }
        break;
      }
    case EXPRESSION_TYPE::FUNCTION_CALL: {
        FunctionCallExpr* const call = &expr->call;
        const CallingConvention* const conv = build_options->calling_convention;

        //Compile to argument locations
        {
          auto i = call->arguments.mut_begin();
          const auto end = call->arguments.mut_end();
          size_t index = 0;
          const size_t total = end - i;

          for (; i < end; (i++, index++)) {
            Location loc = build_options->calling_convention->get_argument_location(index, total);

            if (loc.type == LOCATION_TYPE::REGISTER) {
              //Force the register to be free, move the previous value into a non_volatile register
              force_free_reg_from_set(state, func, &loc.reg, 1,
                                      conv->non_volatile_registers, conv->num_non_volatile_registers);
            }

            state->values.insert_uninit(1);
            i->value_index = state->values.size;

            ValueLocation* arg_vl = state->get_value_location(i->value_index);

            arg_vl->type = i->type;
            arg_vl->location = loc;
            arg_vl->modifiers = VALUE_MODIFIERS::FIXED;

            compile_bytecode_of_expression(build_options, i, state, func);
          }
        }

        {
          uint64_t num_on_stack = 0;
          if (call->function->signature.parameter_types.size > 4) {
            num_on_stack = call->function->signature.parameter_types.size - 4;
          }

          const int64_t call_stack = conv->shadow_space_size + (num_on_stack * 8);
          if (call_stack > state->max_call_stack) {
            state->max_call_stack = call_stack;
          }
        }

        vl = state->get_value_location(expr->value_index);
        bool expr_is_rax = (vl->location.type == LOCATION_TYPE::REGISTER && vl->location.reg == conv->return_register);

        if (!expr_is_rax) {
          //Have to force rax to be free every time
          force_free_reg_from_set(state, func, &conv->return_register, 1,
                                  conv->non_volatile_registers, conv->num_non_volatile_registers);
        }

        //Call
        if (call->function == func) {
          //Calling itself
          //Offset back to the start of the function
          ByteCode::emit_call_offset(func->bytecode, -(int64_t)func->bytecode.size);
        }
        else {
          //Just set pointer for now
          //TODO: Is this persistent? Could this be done another way?
          ByteCode::emit_call_ptr(func->bytecode, call->function);
        }

        //Free argument locations
        {
          auto i = call->arguments.mut_begin();
          const auto end = call->arguments.mut_end();

          for (; i < end; i++) {
            state->free_value(i->value_index);
          }
        }


        if (!expr_is_rax) {
          vl = state->get_value_location(expr->value_index);

          emit_mov_r_to_location(state, func->bytecode, conv->return_register, vl->location);
          state->free_reg(conv->return_register);
        }

        break;
      }
  }
}

static void patch_jump_to_current(Function* const func, size_t offset_to_jump) {
  x64_to_bytes(func->bytecode.size - offset_to_jump, func->bytecode.data + offset_to_jump + 1);
}

void compile_bytecode_of_statement(Compiler* const comp,
                                   ASTStatement* const statement,
                                   Local_State* const state,
                                   Function* const func) {
  switch (statement->type) {
    case STATEMENT_TYPE::BLOCK: {
        auto i = statement->block.block.mut_begin();
        const auto end = statement->block.block.mut_end();

        for (; i < end; i++) {
          compile_bytecode_of_statement(comp, i, state, func);
        }

        return;
      }
    case STATEMENT_TYPE::EXPRESSION:
      //TODO: Identify which bits actually have side effects and run those in sequence
      compile_bytecode_of_expression(&comp->build_options, &statement->expression, state, func);
      return;
    case STATEMENT_TYPE::IF_ELSE: {
        ASTIfElse* const if_else = &statement->if_else;

        set_expression_location_to_reg(&comp->build_options, &if_else->condition, state, func);
        compile_bytecode_of_expression(&comp->build_options, &if_else->condition, state, func);

        uint8_t comp_reg = state->get_value_location(if_else->condition.value_index)->location.reg;
        ByteCode::emit_cmp_64_to_r(func->bytecode,
                                   comp->lang->e_true->value,//probably always 1
                                   comp_reg);

        state->free_value(if_else->condition.value_index);

        const size_t offset_to_jump = func->bytecode.size;
        ByteCode::emit_jump_by_offset_if_not_zero(func->bytecode, 0);


        //If branch
        compile_bytecode_of_statement(comp, if_else->if_statement, state, func);

        const size_t offset_to_exit_jump = func->bytecode.size;
        ByteCode::emit_jump_by_offset(func->bytecode, 0);

        //Patch the else jump
        patch_jump_to_current(func, offset_to_jump);

        //Else branch
        compile_bytecode_of_statement(comp, if_else->else_statement, state, func);

        //Patch the if exit jump
        patch_jump_to_current(func, offset_to_exit_jump);

        //Should be done???
        return;
      }
    case STATEMENT_TYPE::RETURN: {
        ASTExpression* const expr = &statement->expression;

        //Determine the expected location of the output
        const CallingConvention* conv = comp->build_options.calling_convention;

        //free rax
        force_free_reg_from_set(state, func, &conv->return_register, 1,
                                conv->non_volatile_registers, conv->num_non_volatile_registers);

        state->values.insert_uninit(1);
        expr->value_index = state->values.size;

        ValueLocation* const vl = state->get_value_location(expr->value_index);

        vl->type          = expr->type;
        vl->location.type = LOCATION_TYPE::REGISTER;
        vl->location.reg  = conv->return_register;

        if (expr->makes_call) {
          vl->modifiers = VALUE_MODIFIERS::RESERVED;//Must be rax but that sorted later
        }
        else {
          //In this case rax must be fixed
          vl->modifiers = VALUE_MODIFIERS::FIXED;
        }

        compile_bytecode_of_expression(&comp->build_options, expr, state, func);
        ByteCode::emit_jump_by_offset(func->bytecode, 0);

        state->free_value(expr->value_index);//Stops program trying to save the value in rax

        return;
      }
    case STATEMENT_TYPE::DECLARATION: {
        ASTDeclaration* const decl = &statement->declaration;

        //Set up the location of the local variable
        Location loc ={};
        loc.type       = LOCATION_TYPE::STACK;
        loc.stack_disp = state->get_stack_location(decl->type.type);

        //Reserve the stack location so nobody can use it
        state->values.insert_uninit(1);
        const size_t index = state->values.size;

        ValueLocation* const vl = state->get_value_location(index);

        vl->location  = loc;
        vl->modifiers = VALUE_MODIFIERS::FIXED;//not allowed to move because its actually a local
        vl->type      = decl->type.type;

        //Compile the bytecode so the value will be loaded into where the local will end up being
        decl->expression.value_index = index;

        compile_bytecode_of_expression(&comp->build_options, &statement->declaration.expression, state, func);

        //Insert it late so that it cant be used in the expression
        state->locals.insert_uninit(1);
        Local_Variable& local_var = *state->locals.back();

        local_var.name     = decl->name;
        local_var.type     = decl->type.type;
        local_var.location = loc;

        return;
      }
  }
}

static void start_function_signature_stage(Compiler* const comp,
                                           FunctionUnit* const unit) {

  unit->stage           = FUNCTION_COMPILE_STAGE::SIGNATURE;
  unit->status          = COMPILE_STATUS::OK;

  ASTFunctionSignature* ast_sig = &unit->source->signature;
  FunctionSignature* sig = &unit->destination->signature;

  sig->parameter_types.insert_uninit(ast_sig->parameters.size);
}

static void start_function_body_stage(Compiler* const comp,
                                      FunctionUnit* const unit) {

  unit->stage           = FUNCTION_COMPILE_STAGE::BODY;
  unit->status          = COMPILE_STATUS::OK;

  ASTFunctionDeclaration* const ast_func = unit->source;
  Function* const func = unit->destination;

  Local_State* const state = &unit->state;

  //Set fixed registers
  {
    const System* const syst = comp->build_options.system;

    state->registers[syst->stack_pointer].modified = MODIFICATION_TYPE::MANAGED;
    state->registers[syst->stack_pointer].occupied_size = 8;
    state->registers[syst->base_pointer].modified = MODIFICATION_TYPE::MANAGED;
    state->registers[syst->base_pointer].occupied_size = 8;
  }

  //Load parameters to a safe space if we have parameters
  if (ast_func->signature.parameters.size > 0) {
    const FunctionSignature* const sig = &func->signature;

    auto i_ast = ast_func->signature.parameters.begin();
    auto i = sig->parameter_types.begin();
    const auto very_end = sig->parameter_types.end();
    size_t num_to_save = 0;

    const size_t num_params = sig->parameter_types.size;
    const size_t num_reg_params = comp->build_options.calling_convention->num_parameter_registers;

    if (num_params > num_reg_params) {
      num_to_save = num_reg_params;
    }
    else {
      num_to_save = num_params;
    }

    const auto end = i + num_to_save;
    const size_t num_already_saved = num_reg_params - num_to_save;

    //8 = Size of return address, 8 = size of base pointer
    constexpr size_t OFFSET_TO_SHADOW = 8ull + 8ull;


    //Use shadow space if exists
    if (comp->build_options.calling_convention->shadow_space_size >= 8) {
      uint64_t remaining_shadow_space = comp->build_options.calling_convention->shadow_space_size;


      int64_t offset_in_shadow_space = OFFSET_TO_SHADOW;

      //Will run max 'num_reg_params' times
      while (i < end && remaining_shadow_space >= 8) {

        ByteCode::emit_mov_r_to_m(func->bytecode, i->location.reg, RBP.REG, offset_in_shadow_space);

        state->locals.insert_uninit(1);
        auto last = state->locals.back();

        last->name                = i_ast->name;
        last->type                = i->structure;
        last->location.type       = LOCATION_TYPE::STACK;
        last->location.stack_disp = offset_in_shadow_space;

        i_ast++;
        i++;
        offset_in_shadow_space += 8;
        remaining_shadow_space -= 8;
        num_to_save--;
      }
    }

    while (i < end) {
      auto offset = state->get_stack_location(i->structure);

      ByteCode::emit_mov_r_to_m(func->bytecode, i->location.reg, RBP.REG, offset);

      state->locals.insert_uninit(1);
      auto last = state->locals.back();

      last->name                = i_ast->name;
      last->type                = i->structure;
      last->location.type       = LOCATION_TYPE::STACK;
      last->location.stack_disp = offset;

      i_ast++;
      i++;
      num_to_save--;
    }


    while (i < very_end) {
      state->locals.insert_uninit(1);
      auto last = state->locals.back();

      last->name     = i_ast->name;
      last->type     = i->structure;
      last->location = i->location;

      i_ast++;
      i++;
    }
  }
}

//Fix stack offsets and jumping to the return
static void fix_put_off_things(const int64_t stack_size,
                               const int64_t added_bytecode_size,
                               uint8_t* const bytecode,
                               const uint64_t size) {
  for (uint64_t i = 0; i < size;) {
    switch (bytecode[i]) {
      case ByteCode::LEAVE_THEN_RETURN:
      case ByteCode::ENTER_FUNCTION:
        i++;
        break;
      case ByteCode::PUSH_R:
      case ByteCode::POP_R:
      case ByteCode::CALL_R:
      case ByteCode::SET_R_TO_ZF:
        i += ByteCode::OP_R::INSTRUCTION_SIZE;
        break;
      case ByteCode::CALL_PTR:
      case ByteCode::JUMP_BY_I64_IF_ZERO:
      case ByteCode::JUMP_BY_I64_IF_NOT_ZERO:
        i += ByteCode::OP_64::INSTRUCTION_SIZE;
        break;
      case ByteCode::MOV_R_TO_R:
      case ByteCode::ADD_R_TO_R:
      case ByteCode::SUB_R_TO_R:
      case ByteCode::CMP_R_TO_R:
      case ByteCode::MUL_R_TO_R:
      case ByteCode::DIV_R_TO_R:
      case ByteCode::OR_R_TO_R:
      case ByteCode::AND_R_TO_R:
        i += ByteCode::OP_R_R::INSTRUCTION_SIZE;
        break;
      case ByteCode::MOV_64_TO_R:
      case ByteCode::ADD_64_TO_R:
      case ByteCode::SUB_64_TO_R:
      case ByteCode::CMP_64_TO_R:
      case ByteCode::MUL_64_TO_R:
      case ByteCode::DIV_64_TO_R:
        i += ByteCode::OP_64_R::INSTRUCTION_SIZE;
        break;
      case ByteCode::MOV_R_TO_M:
      case ByteCode::MOV_M_TO_R: {
          int64_t offset = x64_from_bytes(bytecode + i + 3);

          if (offset < 0) {
            offset += stack_size;
            x64_to_bytes(offset, bytecode + i + 3);
          }

          i += ByteCode::OP_R_R_64::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::MOV_64_TO_M: {
          int64_t offset = x64_from_bytes(bytecode + i + 1 + 8 + 1);

          if (offset < 0) {
            offset += stack_size;
            x64_to_bytes(offset, bytecode + i + 1 + 8 + 1);
          }

          i += ByteCode::OP_64_R_64::INSTRUCTION_SIZE;
          break;
        }

      case ByteCode::JUMP_BY_I64: {
          int64_t offset = x64_from_bytes(bytecode + i + 1);

          //Actually a jump to the end
          if (offset == 0) {
            x64_to_bytes(size - i, bytecode + i + 1);
          }

          i += ByteCode::OP_R::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::CALL_OFFSET: {
          int64_t offset = x64_from_bytes(bytecode + i + 1);
          offset -= added_bytecode_size;

          x64_to_bytes(offset, bytecode + i + 1);

          i += ByteCode::OP_64::INSTRUCTION_SIZE;
        }
    }
  }
}

static CompileCode compile_type_of_statement(Compiler* const comp,
                                             Function* const func,
                                             Local_State* const state,
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

        const Structure* const expected = func->signature.return_type.structure;

        if (expr->type == expected) {
          return CompileCode::NO_ERRORS;
        }
        else {
          printf("ERROR: Cannot return incorrect type from function\n"
                 "       Return Type: '%s', Expression '%s'\n",
                 expected->name.string, expr->type->name.string);

          return CompileCode::TYPE_CHECK_ERROR;
        }
      }

  }

  printf("INTERNAL ERROR: Reached end of statement type checking without exiting\n"
         "                Statement type: %d\n", (int)statement->type);
  return CompileCode::UNFOUND_DEPENDENCY;
}

CompileCode compile_function_body_unit(Compiler* const comp,
                                       ASTFunctionDeclaration* const ast_func,
                                       Function* const func,
                                       Local_State* const state) {
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

  //Cant error now?

  {
    Array<ASTStatement>& statements = ast_func->body.block;

    auto i = statements.mut_begin();
    const auto end = statements.mut_end();
    for (; i < end; i++) {
      compile_bytecode_of_statement(comp, i, state, func);
    }
  }

  //Fix stack stuff
  {
    Array<uint8_t> temp(1 + ByteCode::OP_64_R::INSTRUCTION_SIZE);

    //Should safely emit enter now so its only added once
    ByteCode::emit_enter(temp);

    //Calling convention non volatiles
    const size_t num_non_volatile = build_options->calling_convention->num_non_volatile_registers;
    const uint8_t* const non_volatile = build_options->calling_convention->non_volatile_registers;

    //Get size of non volatile registers
    int64_t save_regs = 0;
    for (size_t i = 0; i < num_non_volatile; i++) {
      const uint8_t reg = non_volatile[i];

      save_regs += (int64_t)(state->registers[reg].modified == MODIFICATION_TYPE::MODIFIED) * 8;
    }

    //Call operation stack stuff is included in max_stack
    const int64_t STACK_SIZE = state->max_stack + state->max_call_stack + save_regs;

    // Need a stack?
    if (STACK_SIZE > 0) {
      ByteCode::emit_sub_64_to_r(temp, STACK_SIZE, RSP.REG);

      //Save non volatile registers
      int64_t used_space = 0;
      for (size_t i = 0; i < num_non_volatile; i++) {
        const uint8_t reg = non_volatile[i];

        if (state->registers[reg].modified == MODIFICATION_TYPE::MODIFIED) {
          used_space -= 8;
          ByteCode::emit_mov_r_to_m(temp, reg, RBP.REG, used_space);
        }
      }

      //Fix locations into the stack given that we just saved a bunch of registers
      //Also good point to fix jumps to the end
      fix_put_off_things(used_space, temp.size, func->bytecode.data, func->bytecode.size);
    }

    //Copy the bytecode into the new buffer
    temp.reserve_extra(func->bytecode.size);
    memcpy(temp.data + temp.size, func->bytecode.data, func->bytecode.size);
    temp.size += func->bytecode.size;


    // Did we use the stack? - might have non volatiles on there
    if (STACK_SIZE > 0) {
      //Load non volatile registers from stack
      const size_t num_non_volatile = build_options->calling_convention->num_non_volatile_registers;
      const uint8_t* non_volatile = build_options->calling_convention->non_volatile_registers;

      int64_t used_space = 0;
      for (size_t i = 0; i < num_non_volatile; i++) {
        const uint8_t reg = non_volatile[i];

        if (state->registers[reg].modified == MODIFICATION_TYPE::MODIFIED) {
          used_space -= 8;
          ByteCode::emit_mov_m_to_r(temp, RBP.REG, used_space, reg);
        }
      }
    }

    //For now just emit a return at the end just in case
    ByteCode::emit_return(temp);

    //Replace the bytecode
    func->bytecode = std::move(temp);
  }
  return ret;
}

CompileCode compile_function_signature(Compiler* const comp,
                                       ASTFunctionSignature* const ast_sig,
                                       FunctionSignature* const sig) {
  CompileCode ret = CompileCode::NO_ERRORS;

  //Parameters
  {
    auto i_ast = ast_sig->parameters.mut_begin();
    auto i = sig->parameter_types.mut_begin();
    const auto end = sig->parameter_types.end();

    while (i < end) {
      if (i->structure == nullptr) {
        ret = get_compiled_structure(comp, &i_ast->type, &i->structure, 1);
        if (ret != CompileCode::NO_ERRORS) {
          return ret;
        }
      }

      i++;
      i_ast++;
    }
  }

  //Return types - no need to check if compiled as will never be here if its
  ret = get_compiled_structure(comp, &ast_sig->return_type, &sig->return_type.structure, 1);
  if (ret != CompileCode::NO_ERRORS) {
    return ret;
  }


  //Now can't error??
  const CallingConvention* const conv = comp->build_options.calling_convention;

  if (sig->return_type.structure->size() > 8) {
    //Return wont fit
    //Have to pass in an address

    sig->return_type.location.type = LOCATION_TYPE::ADDRESS;
    sig->return_type.location.reg  = conv->return_parameter;
  }
  else {
    //Return will fit

    sig->return_type.location.type = LOCATION_TYPE::REGISTER;
    sig->return_type.location.reg  = conv->return_register;
  }

  {
    auto par = sig->parameter_types.mut_begin();
    const auto end = sig->parameter_types.mut_end();

    for (size_t i = 0; i < conv->num_parameter_registers && par < end; i++) {
      const uint8_t reg =  conv->parameter_registers[i];

      if (reg == conv->return_parameter) {
        continue;
      }

      if (par->structure->size() > 8) {
        //Address can fit in register
        par->location.type = LOCATION_TYPE::ADDRESS;
      }
      else {
        //Can fit in register
        par->location.type = LOCATION_TYPE::REGISTER;
      }

      par->location.reg = reg;
      par++;
    }

    int8_t next_stack = 0;
    int64_t stack_disp = 0;

    if (comp->build_options.calling_convention->stack_direction == STACK_DIRECTION::RIGHT_TO_LEFT) {
      stack_disp = CallingConvention::OFFSET_TO_SHADOW + comp->build_options.calling_convention->shadow_space_size;
      next_stack = 8;
    }
    else {
      stack_disp = CallingConvention::OFFSET_TO_SHADOW
        + comp->build_options.calling_convention->shadow_space_size
        + (8 * (end - par));
      next_stack = -8;
    }

    while (par < end) {
      par->location.type = LOCATION_TYPE::STACK;
      par->location.stack_disp = stack_disp;

      par++;
      stack_disp += next_stack;
    }
  }

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

    unit->source = i;
    unit->destination = func;

    //Link up the ast and function
    i->function = func;
    i->signature.signature = &func->signature;
    func->signature.name = i->name;

    start_function_signature_stage(comp, unit);
  }
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
                                                                 &unit->source->signature,
                                                                 &unit->destination->signature);

              switch (ret) {
                case CompileCode::NO_ERRORS:
                  start_function_body_stage(comp, unit);
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

void print_compiled_functions(Compiler* const comp) {
  auto begin = comp->functions.begin_iter();
  const auto end = comp->functions.end_iter();

  for (; begin != end; begin.next()) {
    Function& func = *begin.get();

    printf("function %s(", func.signature.name.string);

    auto begin = func.signature.parameter_types.begin();
    const auto last = func.signature.parameter_types.back();

    for (; begin < last; begin++) {
      printf("%s, ", begin->structure->name.string);
    }

    if (func.signature.parameter_types.size > 0) {
      printf("%s", last->structure->name.string);
    }

    printf(") -> %s\n", func.signature.return_type.structure->name.string);


    ByteCode::log_bytecode(comp->build_options.system,
                           func.bytecode.data, func.bytecode.size);

    putc('\n', stdout);
  }
}

const Function* find_entry_point(Compiler* const comp) {
  auto begin = comp->functions.begin_iter();
  const auto end = comp->functions.end_iter();

  for (; begin != end; begin.next()) {
    const Function* func = begin.get();

    if (func->signature.name == comp->build_options.entry_point
        && func->signature.parameter_types.size == 0) {
      return func;
    }
  }

  return nullptr;
}