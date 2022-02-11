#pragma once
#include "utility.h"
#include "strings.h"
#include "calling_conventions.h"
#include "comp_utilities.h"
#include "format.h"
#include "parser.h"
#include "files.h"
#include "ast.h"
#include "names.h"

#include "api.h"

#include "type.h"


struct VM;

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
  u64 current = 0;
  u64 max = 0;

  u64 current_passed = 0;
  u64 max_passed = 0;

  u64 call_alignment = 1;

  i32 pass_stack_local(u64 size, u64 alignment);
  i32 next_stack_local(u64 size, u64 alignment);
  void require_call_alignment(u64 req_align);
};

struct Decl {
  ASTDecl* source = nullptr;

  const InternString* name ={};
  META_FLAGS meta_flags ={};
  Type type ={};
};

struct MemValue {
  MemComplex mem;
  size_t size;
};

struct Local {
  Decl decl;

  uint8_t valid_rvts = ALL_RVTS;
  RuntimeValue val ={};
};

struct State {
  Type return_type;

  Array<Local> all_locals ={};
  Array<size_t> active_locals ={};

  ValueTree value_tree ={};
  Array<MemValue> mem_values ={};
  ControlFlow control_flow ={};

  ValueIndex rbp ={};
  ValueIndex rsp ={};

  RuntimeValue return_val ={};

  uint64_t return_label = 0;

  bool made_call = false;
  StackState stack ={};

  bool comptime_compilation = false;

  constexpr bool needs_new_frame() const {
    return made_call || stack.max_passed > 0 || stack.max > 0;
  }

  MemIndex new_mem();
  MemValue* get_mem(const MemIndex&);
  ValueIndex new_value();
  Value* get_val(const ValueIndex& i);

  void value_copy(ValueIndex a, ValueIndex b);
  void set_value(ValueIndex index);

  void use_value(ValueIndex index);
  void use_value(ValueIndex index, ValueIndex related);

  void use_mem(MemIndex index);

  Local* find_local(const InternString* i_s);
};

void init_state_regs(const CallingConvention* convention, State* state);

struct DependencyCheckState {
  Array<const InternString*> locals;
  
  inline bool is_local(const InternString* l) {
    return locals.contains(l);
  }

};

struct Global {
  CompilationUnit* compilation_unit = nullptr;

  Decl decl;

  size_t data_index = 0;
  ConstantVal constant_value ={};

  CodeBlock init ={};
};

enum struct COMPILATION_TYPE : uint8_t {
  NONE, STRUCTURE, FUNCTION, SIGNATURE, CONST_EXPR, GLOBAL, IMPORT
};

enum struct STRUCTURE_COMP_STAGE : uint8_t {
  DEPENDING, UNTYPED, TYPED, FINISHED
};

enum struct SIGNATURE_COMP_STAGE : uint8_t {
  DEPENDING, UNTYPED, FINISHED
};

enum struct GLOBAL_COMP_STAGE : uint8_t {
  DEPENDING, UNTYPED, TYPED, FINISHED
};

enum struct EXPR_COMP_STAGE : uint8_t {
  DEPENDING, UNTYPED, TYPED, FINISHED
};

enum struct FUNCTION_COMP_STAGE : uint8_t {
  UNINIT, DEPENDING, UNTYPED_BODY, TYPED_BODY, FINISHED
};

enum struct IMPORT_COMP_STAGE : u8 {
  DEPENDING, UNTYPED, UNPARSED, FINISHED
};

struct Dependency {
  usize num_deps = 0;

  Array<Dependency*> dependency_of;

  CompilationUnit* waiting_unit;
};

struct CompilationUnit : public Dependency {
  COMPILATION_TYPE type = COMPILATION_TYPE::NONE;

  NamespaceIndex available_names ={};
};

struct ImportUnit : public CompilationUnit {
  IMPORT_COMP_STAGE stage = IMPORT_COMP_STAGE::DEPENDING;

  State state ={};

  ASTImport* import_ast;
};

struct SignatureUnit : public CompilationUnit {
  SIGNATURE_COMP_STAGE stage = SIGNATURE_COMP_STAGE::DEPENDING;

  ASTLambda* source = nullptr;
  FunctionSignature* sig = nullptr;
  Function* func = nullptr;
};

struct StructureUnit : public CompilationUnit {
  STRUCTURE_COMP_STAGE stage = STRUCTURE_COMP_STAGE::DEPENDING;

  ASTStructBody* source = nullptr;
  AST_LINKED* untyped ={};
};

struct GlobalUnit : public CompilationUnit {
  GLOBAL_COMP_STAGE stage = GLOBAL_COMP_STAGE::DEPENDING;

  ASTDecl* source = nullptr;
  Global* global = nullptr;

  State state ={};
};

struct FunctionUnit : public CompilationUnit {
  FUNCTION_COMP_STAGE stage = FUNCTION_COMP_STAGE::UNINIT;

  ASTLambda* source = nullptr;
  Function* func = nullptr;

  State state ={};
};

struct ConstantExprUnit : public CompilationUnit {
  EXPR_COMP_STAGE stage = EXPR_COMP_STAGE::DEPENDING;

  AST_LOCAL expr = nullptr;
  Type cast_to ={};

  State state ={};
};

struct CallSignature {
  const InternString* name = nullptr;
  Array<TypeAndFlags> arguments ={};
};

struct UnknownName {
  bool all_names = false;
  const InternString* ident = nullptr;
  NamespaceIndex namespace_index ={};
  usize num_knowns = 0;
  usize num_unknowns = 0;
};


struct UnfoundDep {
  UnknownName name ={};
  CompilationUnit* unit_waiting ={};
  
  //Used if the dependency isnt found
  ErrorMessage as_error={};
};

struct UnfoundDependencies {
  bool panic = false;
  Array<UnfoundDep> unfound ={};
};

struct FileImport {
  FileLocation file_loc ={};
  NamespaceIndex ns_index ={};
  Span span ={};
};

struct FileLoader {
  const InternString* dll = nullptr;
  const InternString* axl = nullptr;

  Array<FileImport> unparsed_files ={};
};

struct SingleDllImport {
  Function* ptr;
  uint32_t rva_hint;
  const InternString* name;
};

struct ImportedDll {
  const InternString* name = nullptr;
  Span span ={};
  Array<SingleDllImport> imports ={};
};

struct SystemsAndConventionNames {
  const InternString* sys_vm      = nullptr;
  const InternString* sys_x86_64  = nullptr;

  const InternString* conv_vm      = nullptr;
  const InternString* conv_x64     = nullptr;
  const InternString* conv_stdcall = nullptr;
};

struct BuildOptions {
  u32 ptr_size = 8;

  const InternString* file_name   = nullptr;
  const InternString* entry_point = nullptr;
  const InternString* output_file = nullptr;

  const InternString* std_lib_folder = nullptr;

  const System* endpoint_system = nullptr;
  const System* vm_system = nullptr;
  const CallingConvention* default_calling_convention = nullptr;
};

struct Context {
  const System* system = nullptr;
  const CallingConvention* calling_convention = nullptr;

  Span span;

  CompilationUnit* current_unit;
  NamespaceIndex current_namespace;
};

struct ToTypeCheckData {

};

struct ToByteCodeData {

};

struct ToExecData {

};

struct CompPipes {
  Queue<ToTypeCheckData> type_check;
};

struct Services {
  Lexer* lexer = nullptr;
  Parser* parser = nullptr;
  VM* vm = nullptr;
  Errors* errors = nullptr;
  NamesHandler* names = nullptr;
  FileLoader* file_loader = nullptr;
  Structures* structures = nullptr;
  BuiltinTypes* builtin_types = nullptr;
  StringInterner* strings = nullptr;
};

struct Compiler {
  APIPrintOptions        print_options        ={};
  BuildOptions           build_options        ={};
  APIOptimizationOptions optimization_options ={};

  bool new_depends;

  SystemsAndConventionNames system_names ={};
  Intrinsics intrinsics ={};

  Services services;

  Array<Token> current_stream ={};

  //CompPipes comp_unit_pipes;
  Queue<CompilationUnit*> to_compile ={};

  UnfoundDependencies unfound_deps ={};

  NamespaceIndex build_file_namespace ={};//needs to be saved for finding main

  Array<ImportedDll> dlls_import ={};
  Array<FileAST> parsed_files ={};

  FreelistBlockAllocator<SignatureUnit> signature_units ={};
  FreelistBlockAllocator<StructureUnit> structure_units ={};
  FreelistBlockAllocator<FunctionUnit> function_units ={};
  FreelistBlockAllocator<ImportUnit> import_units ={};
  FreelistBlockAllocator<ConstantExprUnit> const_expr_units ={};
  FreelistBlockAllocator<GlobalUnit> global_units ={};

  BucketArray<Global> globals ={};
  BucketArray<Function> functions ={};

  ArenaAllocator constants ={};

  uint64_t labels = 0;

  Function* new_function();

  ImportUnit* new_import_unit(NamespaceIndex ns);
  ConstantExprUnit* new_const_expr_unit(NamespaceIndex ns);
  FunctionUnit* new_function_unit(NamespaceIndex ns);
  SignatureUnit* new_signature_unit(NamespaceIndex ns);
  StructureUnit* new_structure_unit(NamespaceIndex ns);
  GlobalUnit* new_global_unit(NamespaceIndex ns);

  inline constexpr bool is_panic() const { return services.errors->panic; }
  inline constexpr bool is_depends() const { return new_depends || unfound_deps.panic; }
  inline constexpr void reset_panic() { services.errors->panic = false; unfound_deps.panic = false; new_depends = false; }

  inline constexpr bool is_compiling() const {
    return to_compile.size > 0
      || services.file_loader->unparsed_files.size > 0;
  }

  template<typename ... T>
  void report_error(ERROR_CODE code, const Span& span, const char* f_message, T&& ... ts) {
    services.errors->register_error(code, span, f_message, std::forward<T>(ts)...);

    services.errors->panic = true;
  }

  template<typename ... T>
  void set_unfound_name(Context* context, UnknownName&& name,
                        ERROR_CODE code, const Span& span,
                        const char* f_message, T&& ... ts) {
    ASSERT(name.ident != nullptr);
    unfound_deps.panic = true;

    unfound_deps.unfound.insert_uninit(1);
    UnfoundDep* dep = unfound_deps.unfound.back();

    dep->name = std::move(name);
    dep->unit_waiting = context->current_unit;
    dep->as_error.type = code;
    dep->as_error.span = span;
    dep->as_error.message = format(f_message, std::forward<T>(ts)...);

    context->current_unit->num_deps += 1;
  }

  void set_dep(Context* context, CompilationUnit* unit);
};

void add_comp_unit_for_lambda(Compiler* const comp, NamespaceIndex ns_index, ASTLambda* lambda) noexcept;
void add_comp_unit_for_struct(Compiler* const comp, NamespaceIndex ns_index, ASTStructBody* struct_body) noexcept;

void init_compiler(const APIOptions& options, Compiler* comp);

ERROR_CODE compile_all(Compiler* const comp);

void set_runtime_flags(AST_LOCAL expr, State* const state,
                       bool modified, uint8_t valid_rvts);

void cast_operator_type(Compiler* const comp,
                        Context* const context,
                        State* const state,
                        ASTCastExpr* const expr);

void process_parsed_file(Compiler* const comp, FileAST* const func);

void print_compiled_functions(const Compiler* comp);

RuntimeValue new_const_in_reg(Compiler* const comp,
                              State* const state,
                              CodeBlock* const code,
                              const uint8_t* data,
                              size_t);

void copy_runtime_to_runtime(Compiler* const comp,
                             State* const state,
                             CodeBlock* const code,
                             const Structure* type,
                             const RuntimeValue* from,
                             RuntimeValue* to);

const Structure* find_or_make_array_structure(Compiler* const comp,
                                         Context* context,
                                         const Type& base,
                                         size_t length);

const Structure* find_or_make_pointer_structure(Compiler* const comp,
                                           Context* context,
                                           const Type& base);

const Structure* find_or_make_tuple_structure(Compiler* const comp,
                                            Context* context,
                                            Array<Type>&& elements);

void build_data_section_for_vm(Program* prog, Compiler* const comp);