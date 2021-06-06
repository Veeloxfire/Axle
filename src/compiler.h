#pragma once
#include "utility.h"
#include "strings.h"
#include "calling_conventions.h"
#include "options.h"
#include "runtime_vals.h"
#include "format.h"
#include "parser.h"
#include "files.h"

#include "type.h"


struct VM;
struct ASTType;
struct ASTFunctionDeclaration;
struct ASTStructureDeclaration;
struct ASTStatement;
struct ASTExpression;
struct ASTLocal;
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

struct ValueUse {
  ValueIndex related_index ={};
  TimePoint time ={};
};

enum struct ValueType : uint8_t {
  FREE = 0, FIXED, COALESCED
};

struct Value {
  bool has_value = false;
  bool is_modified = false;
  bool crosses_call = false;

  ValueType value_type = ValueType::FREE;

  union {
    uint8_t reg = 0;
    ValueIndex index;
  };

  ValueUse creation ={};
  Array<ValueUse> last_uses ={};

  constexpr bool fixed() const { return value_type == ValueType::FIXED; }
  constexpr bool is_coalesced() const {
    return value_type == ValueType::COALESCED;
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
  
  int32_t next_stack_local(uint64_t size, uint64_t alignment);
};

struct MemValue {
  MemComplex mem;
  size_t size;
};

struct Local {
  const InternString* name ={};
  const Structure* type = nullptr;

  uint8_t valid_rvts = ALL_RVTS;
  RuntimeValue val ={};
};

struct State {
  Array<Local> all_locals ={};
  Array<size_t> active_locals ={};

  ValueTree value_tree ={};
  Array<MemValue> mem_values ={};
  ControlFlow control_flow ={};

  uint64_t return_label = 0;

  bool made_call = false;
  StackState stack ={};

  bool comptime_compilation = false;

  constexpr bool needs_new_frame() const {
    return made_call || stack.max_parameters > 0 || stack.max > 0;
  }

  MemIndex new_mem();
  MemValue* get_mem(const MemIndex&);
  ValueIndex new_value();
  void value_copy(ValueIndex a, ValueIndex b);
  void set_value(ValueIndex index);

  Value* get_val(const ValueIndex& i);

  void use_value(ValueIndex index);
  void use_value(ValueIndex index, ValueIndex related);

  Local* find_local(const InternString* i_s);
};

enum struct COMPILATION_TYPE : uint8_t {
  NONE, FUNCTION, SIGNATURE, CONST_EXPR
};

enum struct EXPRESSION_COMPILE_STAGE : uint8_t {
  UNTYPED, TYPED, FINISHED,
};

enum struct FUNCTION_COMPILE_STAGE : uint8_t {
  SIGNATURE, BODY, FINISHED
};

struct CompilationUnitCarrier {
  COMPILATION_TYPE type = COMPILATION_TYPE::NONE;
  size_t index = 0;

  constexpr bool operator == (const CompilationUnitCarrier& c) const {
    return type == c.type && index == c.index;
  }
};

struct FunctionUnit {
  bool unfound_dependency = false;
  FUNCTION_COMPILE_STAGE stage = FUNCTION_COMPILE_STAGE::FINISHED;

  NamespaceIndex names ={};
  Array<CompilationUnitCarrier> dependencies;

  ASTFunctionDeclaration* source = nullptr;
  Function* destination = nullptr;

  State state ={};
};

struct ConstantExprUnit {
  bool unfound_dependency = false;
  EXPRESSION_COMPILE_STAGE stage = EXPRESSION_COMPILE_STAGE::FINISHED;

  NamespaceIndex names ={};
  Array<CompilationUnitCarrier> dependencies ={};

  ASTExpression* expr = nullptr;
  const Structure* cast_to = nullptr;


  State state ={};
};

struct ErrorMessage {
  CompileCode type = CompileCode::NO_ERRORS;
  Span span ={};
  OwnedPtr<char> message ={};
};

struct Errors {
  bool panic = false;
  Array<ErrorMessage> error_messages ={};
};

struct CallSignature {
  const InternString* name = nullptr;
  Array<const Structure*> arguments ={};
};

struct UnknownName {
  const InternString* ident;
  NamespaceIndex namespace_index;
};

enum struct UnfoundDepType {
  Unkown, Name, Function
};

struct UnfoundDep {
  CompilationUnitCarrier unit_waiting ={};
  UnfoundDepType type = UnfoundDepType::Unkown;
  Span span ={};

  union {
    char _dummpy = {};
    UnknownName name;
    CallSignature signature;
  };

  UnfoundDep() = default;
  UnfoundDep(UnfoundDep&& a) noexcept {
    move_from(std::move(a));
  }
  UnfoundDep& operator=(UnfoundDep&& a) noexcept {
    this->~UnfoundDep();
    move_from(std::move(a));
    return *this;
  }

  void move_from(UnfoundDep&&) noexcept;

  void set_union(UnfoundDepType et) noexcept;
  void destruct_union() noexcept;

  ~UnfoundDep();
};

struct UnfoundDependencies {
  bool panic = false;
  Array<UnfoundDep> units ={};
};

enum struct NamedElementType {
  FUNCTION, STRUCTURE, ENUM
};

struct NamedElement {
  NamedElementType type ={};
  union {
    char _dummy = '\0';
    Array<Function*> overloads;
    const Structure* structure;
    const EnumValue* enum_value;
    //NamespaceIndex other_namespace ={ 0 };
  };

  void set_union(NamedElementType ty);
  void destruct_union();
  void move_from(NamedElement&& ne);

  NamedElement() = default;
  NamedElement(NamedElement&& ne) noexcept {
    move_from(std::move(ne));
  }

  NamedElement& operator=(NamedElement&& ne) noexcept {
    this->~NamedElement();
    move_from(std::move(ne));
    return *this;
  }

  ~NamedElement() noexcept {
    destruct_union();
  }
};

struct Namespace {
  bool is_sub_namespace = false;
  NamespaceIndex inside = {};

  InternHashTable<NamedElement> names ={};
  Array<NamespaceIndex> imported = {};
};

struct FileImport {
  FileLocation file_loc ={};
  NamespaceIndex ns_index ={};
  Span span ={};
};

struct Compiler {
  VM* vm = nullptr;

  CompilationUnitCarrier current_unit ={};

  Errors errors ={};
  UnfoundDependencies unfound_deps = {};

  NamespaceIndex builtin_namespace = {};
  NamespaceIndex current_namespace = {};
  Array<Namespace> all_namespaces ={};

  Array<FileImport> unparsed_files;
  Array<ASTFile> parsed_files;

  PrintOptions        print_options        ={};
  BuildOptions        build_options        ={};
  OptimizationOptions optimization_options ={};

  Array<FunctionUnit> function_units ={};
  Array<ConstantExprUnit> const_expr_units ={};

  Array<CompilationUnitCarrier> compiling ={};

  BucketArray<Function> functions ={};
  ArenaAllocator constants ={};

  Types* types = nullptr;
  StringInterner* strings = nullptr;

  const InternString* entry_point ={};
  uint64_t labels = 0;

  Function* new_function();

  constexpr bool is_panic() const { return errors.panic || unfound_deps.panic; }
  constexpr bool is_fatal() const { return errors.panic; }
  constexpr void reset_panic() { errors.panic = false; unfound_deps.panic = false; }

  template<typename ... T>
  void report_error(CompileCode code, const Span& span, const char* f_message, T&& ... ts) {
    errors.panic = true;

    OwnedPtr<char> message = format(f_message, std::forward<T>(ts)...);
    errors.error_messages.insert({code, span, std::move(message)});
  }

  void set_unfound_name(const InternString* name, NamespaceIndex ns, const Span& span);
  void set_unfound_signature(CallSignature&& sig, const Span& span);
  void set_dep(CompilationUnitCarrier unit);
};

CompileCode compile_all(Compiler* const comp);

void compile_implicit_cast(Compiler* const comp,
                           ASTExpression* from_expr,
                           const Structure* to);

void set_valid_rvts(ASTExpression* const expr, State* const state, uint8_t valid_rvts);

CompileCode parse_all_unparsed_files_with_imports(Compiler* const comp);

void build_compilation_units_for_file(Compiler* const comp, ASTFile* const func);

void print_compiled_functions(const Compiler* comp);

void copy_runtime_to_runtime(Compiler* const comp,
                             State* const state,
                             CodeBlock* const code,
                             const Structure* type,
                             const RuntimeValue* from,
                             RuntimeValue* to);

const Structure* find_or_make_array_type(Compiler* const comp,
                                         const Structure* base,
                                         size_t length);

const Structure* find_or_make_pointer_type(Compiler* const comp,
                                           const Structure* base);

NamedElement* find_name(Compiler* const comp,
                              NamespaceIndex ns_index,
                              const InternString* name);
Array<NamedElement*> find_all_names(Compiler* const comp,
                                          NamespaceIndex ns_index,
                                          const InternString* name);

CompileCode print_compile_errors(const Compiler* const comp);