#pragma once
#include "utility.h"
#include "strings.h"
#include "calling_conventions.h"
#include "options.h"
#include "operators.h"

#include "type.h"


struct ASTType;
struct ASTFunctionDeclaration;
struct ASTStructureDeclaration;
struct ASTStatement;
struct ASTExpression;
struct ASTDeclaration;
struct ASTFile;

struct TimePoint {
  size_t flow = 0;
  uint32_t time = 0;
};

struct ControlFlow {
  SquareBitMatrix ever_flows_to ={};
  Array<Array<size_t>> direct_flows_from ={};

  size_t current_flow = 0;
  uint32_t expression_num = 0;

  bool had_call = false;
  uint32_t last_call = 0;

  void free() {
    ever_flows_to.free();
    direct_flows_from.free();

    current_flow = 0;
    expression_num = 0;

    had_call = false;
    last_call = 0;
  }

  size_t new_flow() noexcept {
    size_t old_flow = current_flow;
    expression_num = 0;

    had_call = false;
    last_call = 0;

    direct_flows_from.insert_uninit(1);
    current_flow = ever_flows_to.new_value();

    return old_flow;
  }

  void recurse_flows_to(size_t a, size_t b) noexcept {
    auto i = direct_flows_from.data[b].begin();
    auto end = direct_flows_from.data[b].end();

    for (; i < end; i++) {
      ever_flows_to.set_a_intersects_b(*i, a);
      //Recurse the direct flows
      recurse_flows_to(a, *i);
    }
  }

  void set_a_flows_to_b(size_t a, size_t b) noexcept {
    ever_flows_to.set_a_intersects_b(a, b);
    recurse_flows_to(a, b);
    direct_flows_from.data[b].insert(a);
  }

  bool test_a_flows_to_b(size_t a, size_t b) const {
    return ever_flows_to.test_a_intersects_b(a, b);
  }

  TimePoint now() const {
    return TimePoint{ current_flow, expression_num };
  }
};

struct Local {
  InternString name ={};
  ValueIndex val ={};
  const Structure* type = nullptr;
};

struct ValueUse {
  ValueIndex related_index ={};
  TimePoint time ={};
};

enum struct ValueType : uint8_t {
  FREE = 0, FIXED, COALESCED, NORMAL_STACK, ARGUMENT_STACK, CONSTANT
};

struct Value {
  bool is_modified = false;
  bool crosses_call = false;

  ValueType value_type = ValueType::FREE;

  union {
    uint8_t reg = 0;
    ValueIndex index;
    int64_t stack_offset;
    uint64_t arg_num;
    void* constant;
  };

  ValueUse creation ={};
  Array<ValueUse> last_uses ={};

  constexpr bool fixed() const { return value_type == ValueType::FIXED; }
  constexpr bool is_coalesced() const {
    return value_type == ValueType::COALESCED;
  }
  constexpr bool on_stack() const {
    return value_type == ValueType::NORMAL_STACK
      || value_type == ValueType::ARGUMENT_STACK;
  }
};

struct ValueTree {
  Array<Value> values ={};

  SquareBitMatrix intersection_check ={};
  Array<Array<ValueIndex>> adjacency_list ={};

  void set_intersection(const ValueIndex a, const ValueIndex b) {
    //Only need to insert if not already inserted
    if (!intersection_check.test_a_intersects_b(a.val, b.val)) {
      adjacency_list.data[a.val].insert(b);
      intersection_check.set_a_intersects_b(a.val, b.val);

      adjacency_list.data[b.val].insert(a);
      intersection_check.set_a_intersects_b(b.val, a.val);
    }
  }

  void combine_intersection(const ValueIndex from, const ValueIndex to);

  void free() {
    values.free();
    intersection_check.free();
    adjacency_list.free();
  }
};

struct StackState {
  uint64_t current = 0;
  uint64_t max = 0;

  uint64_t current_parameters = 0;
  uint64_t max_parameters = 0;

  //Just for counting parameters/use of stack
  void push_stack_params(uint64_t p) {
    current_parameters += p;
    if (current_parameters > max_parameters) {
      max_parameters = current_parameters;
    }
  }
  uint64_t next_stack_local(uint64_t size, uint64_t alignment);
};

struct State {
  Array<Local> locals ={};

  ValueTree value_tree ={};
  ControlFlow control_flow ={};

  uint64_t return_label = 0;

  bool made_call = false;
  StackState stack ={};

  bool comptime_compilation = false;

  constexpr bool needs_new_frame() const { 
    return made_call || stack.max_parameters > 0 || stack.max > 0; 
  }

  ValueIndex new_value();
  ValueIndex new_value(ValueIndex created_by);
  void use_value(ValueIndex index, ValueIndex creates);
  void use_value(ValueIndex index);
  const Local* find_local(InternString i_s) const;
};

struct IteratorForASTBlock {
  struct StatementLayer {
    ASTStatement* itr;
    const ASTStatement* end;
  };

  ASTExpression* current_expression;
  StatementLayer current_statement_layer;

  Array<ASTExpression*> to_do_expressions;
  Array<StatementLayer> to_do_statements;
};


#define COMPCODEINC \
MOD(NO_ERRORS)\
MOD(UNFOUND_DEPENDENCY)\
MOD(FOUND_DEPENDENCY)\
MOD(CIRCULAR_DEPENDENCY)\
MOD(TYPE_CHECK_ERROR)\
MOD(INTERNAL_ERROR)

enum struct CompileCode : uint8_t {
#define MOD(E) E,
  COMPCODEINC
#undef MOD
};

constexpr const char* compile_code_string(CompileCode c) {
  switch (c) {
  #define MOD(E) case CompileCode:: ## E: return #E;
    COMPCODEINC
  #undef MOD
  }

  return "Invalid code";
}

enum struct COMPILATION_TYPE : uint8_t {
  SIGNATURE, FUNCTION, STRUCTURE, CONSTANT, NONE
};

enum struct EXPRESSION_COMPILE_STAGE : uint8_t {
  UNTYPED, TYPED, FINISHED,
};

enum struct FUNCTION_COMPILE_STAGE : uint8_t {
  SIGNATURE, BODY, FINISHED,
};

struct CompilationUnitCarrier {
  COMPILATION_TYPE type = COMPILATION_TYPE::NONE;
  size_t index = 0;

  constexpr bool operator == (const CompilationUnitCarrier& c) const {
    return type == c.type && index == c.index;
  }
};

struct FunctionUnit {
  FUNCTION_COMPILE_STAGE stage = FUNCTION_COMPILE_STAGE::FINISHED;
  Array<CompilationUnitCarrier> dependecies;
  bool unfound_dependency = false;

  ASTFunctionDeclaration* source = nullptr;
  Function* destination = nullptr;

  State state ={};
};

struct ConstantUnit {
  EXPRESSION_COMPILE_STAGE stage = EXPRESSION_COMPILE_STAGE::FINISHED;
  Array<CompilationUnitCarrier> dependecies ={};
  bool unfound_dependency = false;

  ASTExpression* expr = nullptr;
};

struct VM;

struct UnfoundDependenciesInfo {
  bool panic = false;

  size_t num_function_units = 0;
  size_t num_constant_units = 0;
};

struct Compiler {
  VM* vm = nullptr;
  State* working_state = nullptr;

  UnfoundDependenciesInfo unfound_dep_info;

  CompilationUnitCarrier current_unit ={};

  PrintOptions        print_options        ={};
  BuildOptions        build_options        ={};
  OptimizationOptions optimization_options ={};

  Array<FunctionUnit> function_units ={};
  Array<ConstantUnit> constant_units ={};

  Array<CompilationUnitCarrier> compiling ={};

  BucketArray<Function> functions ={};
  ArenaAllocator constants ={};

  Types* types = nullptr;
  StringInterner* strings = nullptr;

  InternString entry_point ={};
  uint64_t labels = 0;

  Function* new_function();
};

CompileCode compile_all(Compiler* const comp);
void build_compilation_units(Compiler* const comp, ASTFile* const func);

void print_compiled_functions(const Compiler* comp);


