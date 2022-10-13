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

  Array<TimePoint> calls = {};

  void free() {
    ever_flows_to.free();
    direct_flows_from.free();
    calls.free();

    current_flow = 0;
    expression_num = 0;
  }

  void new_flow() noexcept {
    expression_num = 0;

    direct_flows_from.insert_uninit(1);
    current_flow = ever_flows_to.new_value();
  }

  void recurse_flows_to(size_t a, size_t b) noexcept {
    auto i = direct_flows_from.data[a].begin();
    auto end = direct_flows_from.data[a].end();

    for (; i < end; i++) {
      if (*i != b && !ever_flows_to.test_a_intersects_b(*i, b)) {
        ever_flows_to.set_a_intersects_b(*i, b);
        //Recurse the direct flows
        recurse_flows_to(*i, b);
      }
    }
  }

  void set_a_flows_to_b(size_t a, size_t b) noexcept {
    ASSERT(a != b);

    if (!ever_flows_to.test_a_intersects_b(a, b)) {
      ever_flows_to.set_a_intersects_b(a, b);
      recurse_flows_to(a, b);
      direct_flows_from.data[b].insert(a);
    }
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
    //adjacency_list.free();
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
  Span span ={};

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

  ValueTree value_tree ={};
  Array<MemValue> mem_values ={};
  ControlFlow control_flow ={};

  Array<ValueIndex> captured_values = {};

  ValueIndex rbp ={};
  ValueIndex rsp ={};

  RuntimeValue return_val ={};

  uint64_t return_label = 0;

  bool made_call = false;
  StackState stack ={};

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
  void use_value_captured(ValueIndex index, ValueIndex related);

  void use_mem(MemIndex index);
};

void init_state_regs(const CallingConvention* convention, State* state);

struct Pipe : AtomicQueue<CompilationUnit*> {
  const char* _debug_name;
};

struct Context {
  bool comptime_compilation;
  CompilationUnit* current_unit;

  Pipe* dependency_load_pipe;
};

struct DependencyCheckStateAndContext : Context {
  Array<Local*> locals;

  Local* get_local(const InternString* name);
};

struct DataHolder {
  const InternString* name;
  size_t size = 0;
  size_t alignment = 0;
  size_t data_index = 0;
};

struct Global {
  Decl decl;

  size_t data_holder_index = 0;
  ConstantVal constant_value ={};

  CodeBlock init ={};
};

enum struct COMPILATION_EMIT_TYPE : uint8_t {
  STRUCTURE, FUNC_BODY, FUNC_SIG, EXEC_CODE, GLOBAL, IMPORT
};

struct CompPipes {
  Pipe depend_check;
  Pipe type_check;
  Pipe exec_code;
  Pipe emit_function;
  Pipe emit_global;
  Pipe emit_import;
};

struct DependencyListSingle {
  DependencyListSingle* next;
  CompilationUnit* waiting;
};

struct DependencyManager {
  FreelistBlockAllocator<DependencyListSingle> dependency_list_entry;
  Queue<CompilationUnit*> free_dependencies;

  void close_dependency(CompilationUnit* ptr);
  void remove_dependency_from(CompilationUnit* ptr);
  void add_dependency_to(CompilationUnit* now_waiting, CompilationUnit* waiting_on);
  void add_external_dependency(CompilationUnit* now_waiting);
};

struct CompilationUnit {
  UnitID id;

  u32 waiting_on_count;
  DependencyListSingle* dependency_list;
  Pipe* insert_to;

  COMPILATION_EMIT_TYPE emit;
  Namespace* available_names;
  AST_LOCAL ast;

  State* state;
  void* extra;
};

struct ImportExtra {
  FileLocation src_loc;
};

struct GlobalExtra {
  Global* global;
};

struct FuncBodyExtra {
  Function* func;
};

struct ExecCodeExtra {
  Type cast_to;
};

struct CallSignature {
  const InternString* name = nullptr;
  Array<TypeAndFlags> arguments ={};
};

struct UnknownName {
  const InternString* ident = nullptr;
  Namespace* ns ={};
};


struct UnfoundNameHolder {
  UnknownName name ={};
  CompilationUnit* dependency = nullptr;

  //Used if the dependency isnt found
  ErrorMessage as_error={};
};

struct UnfoundNames {
  Array<UnfoundNameHolder> names ={};
};

struct FileImport {
  FileLocation file_loc ={};
  Namespace* ns = nullptr;
  Span span ={};
};

struct FileLoader {
  FileLocation cwd = {};

  Array<FileImport> unparsed_files ={};
};

struct LibraryImport {
  size_t label;
  const InternString* path;
  const InternString* name;
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

  const InternString* lib_folder = nullptr;
  const InternString* std_lib_folder = nullptr;

  const System* endpoint_system = nullptr;
  const System* vm_system = nullptr;
  const CallingConvention* default_calling_convention = nullptr;
};

#define IMPORTANT_NAMES_INC \
MOD(ptr) \
MOD(len) \
MOD(axl) \

struct ImportantNames {
#define MOD(n) const InternString* n;
  IMPORTANT_NAMES_INC;
#undef MOD
};

struct CompilationUnitStore {
  UnitID comp_unit_counter = NULL_ID;
  
  FreelistBlockAllocator<CompilationUnit> compilation_units = {};
  Array<CompilationUnit*> active_units;

  CompilationUnit* allocate_unit();
  void free_unit(CompilationUnit* unit);
  CompilationUnit* get_unit_if_exists(u64 id) const;
};

struct Compilation {
  UnfoundNames unfound_names = {};
  DependencyManager dependencies = {};

  u64 in_flight_units = 0;//Units that are not waiting somewhere

  CompilationUnitStore store = {};
  FreelistBlockAllocator<State> states = {};

  FreelistBlockAllocator<FuncBodyExtra> func_body_extras = {};
  FreelistBlockAllocator<GlobalExtra> global_extras = {};
  FreelistBlockAllocator<ImportExtra> import_extras = {};
  FreelistBlockAllocator<ExecCodeExtra> exec_code_extras = {};
};


struct Services {
  //Must always be acquired in order!
  AtomicPtr<FileLoader> file_loader;
  AtomicPtr<Compilation> compilation;
  AtomicPtr<NameManager> names;

  AtomicPtr<VM> vm;
  AtomicPtr<Structures> structures;
  AtomicPtr<StringInterner> strings;

  void get_multiple(AtomicLock<Structures>* structs,
                    AtomicLock<StringInterner>* string_int) {
    structures._mutex.acquire();
    strings._mutex.acquire();

    structs->_mutex = &structures._mutex;
    structs->_ptr = structures._ptr;

    string_int->_mutex = &strings._mutex;
    string_int->_ptr = strings._ptr;
  }
};

struct CompilerConstants {
  APIPrintOptions        print_options = {};
  BuildOptions           build_options = {};
  APIOptimizationOptions optimization_options = {};

  BuiltinTypes* builtin_types = nullptr;

  SystemsAndConventionNames system_names = {};
  Intrinsics intrinsics = {};
  ImportantNames important_names = {};

  u32 active_threads;
};

constexpr void copy_compiler_constants(const CompilerConstants* from, CompilerConstants* to) {
  *to = *from;
}

//Things that may be modified by multiple threads
struct CompilerGlobals : CompilerConstants {
  std::atomic_uint32_t work_counter;

  Signal global_panic;
  SpinLockMutex global_errors_mutex;
  Array<ErrorMessage> global_errors;

  Services services;

  CompPipes pipelines;

  Namespace* build_file_namespace ={};//needs to be saved for finding main
  Namespace* builtin_namespace ={};

  Array<LibraryImport> lib_import ={};
  Array<FileAST> parsed_files ={};
  Array<DataHolder> data_holders ={};

  SpinLockMutex locals_mutex;
  BucketArray<Local> locals_single_threaded = {};
  SpinLockMutex globals_mutex;
  BucketArray<Global> globals_single_threaded ={};
  SpinLockMutex functions_mutex;
  BucketArray<Function> functions_single_threaded ={};
  SpinLockMutex namespaces_mutex;
  BucketArray<Namespace> namespaces_single_threaded ={};

  SpinLockMutex constants_mutex;
  ArenaAllocator constants_single_threaded ={};

  uint64_t labels = 0;

  Function* new_function();
  Local* new_local();
  Global* new_global();
  Namespace* new_namespace();

  template<typename T>
  T* new_constant() {
    //constants_mutex.acquire();
    //T* t = constants_single_threaded.alloc_no_construct<T>();
    //constants_mutex.release();
    return (T*)malloc(sizeof(T));
  }

  inline u8* new_constant(usize size) {
    //constants_mutex.acquire();
    //u8* t = constants_single_threaded.alloc_no_construct(size);
    //constants_mutex.release();
    return (u8*)malloc(size);
  }

  void free_constant(void* ptr) {
    //constants_mutex.acquire();
    //constants_single_threaded.free_no_destruct(ptr);
    //constants_mutex.release();
    free(ptr);
  }

  inline bool is_global_panic() const {
    return global_panic.test();
  }

  inline bool is_compiling() const {
    auto files = services.file_loader.get();
    auto compilation = services.compilation.get();
    return !is_global_panic()
      && (files->unparsed_files.size > 0 || compilation->store.active_units.size > 0);
  }
};

//Things that cannot be modified by other threads
struct CompilerThread : CompilerConstants {
  bool doing_work = true;

  Array<Token> current_stream = {};
  Errors errors = {};

  UnfoundNames local_unfound_names;
  Array<UnitID> new_depends;

  inline bool is_panic() const { return errors.panic; }
  inline bool is_depends() const {
    return new_depends.size > 0 || local_unfound_names.names.size > 0;
  }

  template<typename ... T>
  void report_error(ERROR_CODE code, const Span& span, const char* f_message, const T& ... ts) {
    errors.report_error(code, span, f_message, ts...);
  }
};

inline constexpr void thead_doing_work(CompilerGlobals* comp, CompilerThread* comp_thread) {
  if (!comp_thread->doing_work) {
    comp->work_counter += 1;
    comp_thread->doing_work = true;
  }
}

#if 0
void set_dependency(CompilerGlobals* const comp, Context* const context, CompilationUnit* current, CompilationUnit* waiting_on);

CompilationUnit* compile_and_execute(Compiler* const comp, Namespace* const available_names, AST_LOCAL ast, Type cast_to);


void cast_operator_type(Compiler* const comp,
                        State* const state,
                        ASTCastExpr* const expr);

void process_parsed_file(Compiler* const comp, FileAST* const func);

void print_compiled_functions(const Compiler* comp);


#endif

UnitID compile_and_execute(CompilerGlobals* const comp, Namespace* const available_names, AST_LOCAL ast, Type cast_to);

void compile_all(CompilerGlobals* const comp, CompilerThread* const comp_thread);

void init_compiler(const APIOptions& options, CompilerGlobals* comp, CompilerThread* comp_thread);

void set_runtime_flags(AST_LOCAL expr, State* const state,
                       bool modified, uint8_t valid_rvts);

void copy_runtime_to_runtime(CompilerGlobals* const comp,
                             State* const state,
                             CodeBlock* const code,
                             const Structure* type,
                             const RuntimeValue* from,
                             RuntimeValue* to);

RuntimeValue new_const_in_reg(CompilerGlobals* const comp,
                              State* const state,
                              CodeBlock* const code,
                              const uint8_t* data,
                              size_t);

void add_comp_unit_for_import(CompilerGlobals* const comp, Namespace* ns, const FileLocation& src_loc, ASTImport* imp) noexcept;

void add_comp_unit_for_lambda(CompilerGlobals* const comp, CompilerThread* const comp_thread, Namespace* ns, ASTLambda* lambda) noexcept;

void add_comp_unit_for_global(CompilerGlobals* const comp, CompilerThread* const comp_thread, Namespace* ns, ASTGlobalDecl* global) noexcept;

void add_comp_unit_for_struct(CompilerGlobals* const comp, Namespace* ns, ASTStructBody* struct_body) noexcept;