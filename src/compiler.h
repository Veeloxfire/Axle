#pragma once
#include "utility.h"
#include "strings.h"
#include "calling_conventions.h"
#include "options.h"

#include "type.h"


struct ASTType;
struct ASTFunctionDeclaration;
struct ASTStructureDeclaration;
struct ASTStatement;
struct ASTExpression;
struct ASTDeclaration;
struct ASTFile;

enum struct CompileCode {
  NO_ERRORS = 0,
  UNFOUND_DEPENDENCY,
  TYPE_CHECK_ERROR,
};

enum struct COMPILATION_TYPE : uint8_t {
  SIGNATURE, FUNCTION, STRUCTURE, NONE
};

enum struct FUNCTION_COMPILE_STAGE : uint8_t {
  SIGNATURE, BODY, FINISHED,
};

enum struct COMPILE_STATUS : uint8_t {
  OK, HAD_ERROR,
};

struct CompilationUnitCarrier {
  COMPILATION_TYPE type;
  size_t index;
};

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

struct Value {
  bool is_coalesced = false;
  union {
    uint8_t reg;
    ValueIndex index;
  };

  bool is_modified = false;
  bool crosses_call = false;

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


struct State {
  Array<Local> locals;
  Array<uint8_t> value_map;//0 is special value

  ValueTree value_tree;
  ControlFlow control_flow;

  uint64_t return_label;

  ValueIndex new_value() {
    value_map.insert(0);

    value_tree.adjacency_list.insert_uninit(1);
    value_tree.values.insert_uninit(1);

    auto val = ValueIndex{ value_tree.intersection_check.new_value() };

    value_tree.values.back()->creation
      = ValueUse{ val, control_flow.now() };

    return val;
  }

  ValueIndex new_value(ValueIndex created_by) {
    const ValueIndex val = new_value();
    value_tree.values.back()->creation.related_index = created_by;
    return val;
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

struct FunctionUnit {
  FUNCTION_COMPILE_STAGE stage;
  COMPILE_STATUS status;

  ASTFunctionDeclaration* source;
  Function* destination;

  State state;
};

struct EnumValue {
  const Structure* type;
  InternString name;
  uint64_t value;
};

struct Compiler {
  Array<FunctionUnit> function_units;

  Array<CompilationUnitCarrier> compiling;

  LinkedList<Structure> structures;
  LinkedList<Function> functions;
  LinkedList<EnumValue> enums;

  Lang* lang;
  StringInterner* strings;

  uint64_t labels = 0;

  RunOptions run_options;
  PrintOptions print_options;
  BuildOptions build_options;

  Structure* structure_by_name(InternString);
  EnumValue* enum_by_name(InternString);

  Structure* new_composite_structure();
  Function* new_function();
};

void load_language_builtin(Compiler*);

CompileCode compile_all(Compiler* const comp);
void build_compilation_units(Compiler* const comp, ASTFile* const func);

void print_compiled_functions(const Compiler* comp);


