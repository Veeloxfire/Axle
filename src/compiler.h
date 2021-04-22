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
  size_t flow;
  uint32_t time;
};

struct ControlFlow {
  SquareBitMatrix ever_flows_to;
  Array<Array<size_t>> direct_flows_from;

  size_t current_flow;
  uint32_t expression_num;

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
  InternString name;
  ValueIndex val;
  const Structure* type;
};

struct ValueUse {
  ValueIndex related_index ={};
  TimePoint time;
};

enum struct ValueType : uint8_t {
  FREE = 0, FIXED, COALESCED, NORMAL_STACK, ARGUMENT_STACK
};

struct Value {
  bool is_modified = false;
  bool crosses_call = false;

  ValueType value_type = ValueType::FREE;

  constexpr bool fixed() const { return value_type == ValueType::FIXED; }
  constexpr bool is_coalesced() const {
    return value_type == ValueType::COALESCED;
  }
  constexpr bool on_stack() const {
    return value_type == ValueType::NORMAL_STACK
      || value_type == ValueType::ARGUMENT_STACK;
  }

  union {
    uint8_t reg = 0;
    ValueIndex index;
    int64_t stack_offset;
    uint64_t arg_num;
  };

  ValueUse creation;
  Array<ValueUse> last_uses;
};

struct ValueTree {
  Array<Value> values;

  SquareBitMatrix intersection_check;
  Array<Array<ValueIndex>> adjacency_list;

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

  uint64_t max_parameters = 0;

  uint64_t next_stack_local(uint64_t size, uint64_t alignment);
};

struct State {
  Array<Local> locals;

  ValueTree value_tree;
  ControlFlow control_flow;

  uint64_t return_label;

  bool made_call = false;

  StackState stack ={};

  ValueIndex new_value() {
    value_tree.values.insert_uninit(1);
    value_tree.adjacency_list.insert_uninit(1);

    auto val_index = ValueIndex{ value_tree.intersection_check.new_value() };


    auto* val = value_tree.values.data + val_index.val;
    val->creation = ValueUse{ val_index, control_flow.now() };

    return val_index;
  }

  ValueIndex new_value(ValueIndex created_by) {
    const ValueIndex val_index = new_value();
    auto* val = value_tree.values.data + val_index.val;
    val->creation.related_index = created_by;
    return val_index;
  }

  void use_value(ValueIndex index, ValueIndex creates);
  void use_value(ValueIndex index) {
    use_value(index, index);
  }


  const Local* find_local(InternString i_s) const {
    auto i = locals.begin();
    const auto end = locals.end();

    for (; i < end; i++) {
      if (i->name == i_s) {
        return i;
      }
    }
    return nullptr;
  }
};


enum struct CompileCode : uint8_t {
  NO_ERRORS = 0,
  UNFOUND_DEPENDENCY,
  TYPE_CHECK_ERROR,
  INTERNAL_ERROR,
};

enum struct COMPILATION_TYPE : uint8_t {
  SIGNATURE, FUNCTION, STRUCTURE, NONE
};

enum struct FUNCTION_COMPILE_STAGE : uint8_t {
  SIGNATURE, BODY, FINISHED,
};

struct CompilationUnitCarrier {
  COMPILATION_TYPE type;
  size_t index;
};

struct FunctionUnit {
  FUNCTION_COMPILE_STAGE stage;

  ASTFunctionDeclaration* source;
  Function* destination;

  State state;
};

struct Compiler {
  PrintOptions        print_options        ={};
  BuildOptions        build_options        ={};
  OptimizationOptions optimization_options ={};

  Array<FunctionUnit> function_units;

  Array<CompilationUnitCarrier> compiling;

  LinkedList<Function> functions;

  Types* types;
  StringInterner* strings;

  InternString entry_point;
  uint64_t labels = 0;

  Function* new_function();
};

CompileCode compile_all(Compiler* const comp);
void build_compilation_units(Compiler* const comp, ASTFile* const func);

void print_compiled_functions(const Compiler* comp);


