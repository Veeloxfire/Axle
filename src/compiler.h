#pragma once
#include "utility.h"
#include "strings.h"
#include "comp_utilities.h"
#include "format.h"
#include "parser.h"
#include "files.h"
#include "ast.h"
#include "names.h"

#include "api.h"
#include "type.h"
#include "ir.h"
#include "backends.h"

struct Decl {
  Span span = {};

  const InternString* name = {};
  META_FLAGS meta_flags = {};
  Type type = {};
};

struct Local {
  Decl decl;

  bool is_constant;
  union {
    IR::ValueIndex val;
    void* constant;
  };
};

struct Pipe : AtomicQueue<CompilationUnit*> {
  const char* _debug_name;
};

struct Context {
  bool comptime_compilation;
  CompilationUnit* current_unit;

  Pipe* dependency_load_pipe;
};

struct ComptimeExec {
  AST_LOCAL ast;
  u8* dest;
  Type type;
};

struct DependencyChecker : Context {
  Array<Local*> locals;
  
  Local* get_local(const InternString* name);
};

struct DynamicInitData {
  const InternString* name;
  size_t size = 0;
  size_t alignment = 0;
  size_t data_index = 0;

  IR::GlobalLabel init_expr_label;
};

struct Global {
  Decl decl;

  bool is_constant;
  union {
    u32 dynamic_init_index;
    void* constant_value = nullptr;
  };
};

enum struct COMPILATION_EMIT_TYPE : uint8_t {
  STRUCTURE, FUNC_BODY, FUNC_SIG, EXEC_CODE, GLOBAL, IMPORT
};

struct CompPipes {
  Pipe depend_check;
  Pipe type_check;
  Pipe emit_function;
  Pipe emit_global;
  Pipe emit_import;
  Pipe exec_ir;
  Pipe compile_ir;
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
#if 0
  void add_external_dependency(CompilationUnit* now_waiting);
#endif
};

struct CompilationUnit {
  UnitID id;

  u32 waiting_on_count;
  DependencyListSingle* dependency_list;
  Pipe* insert_to;

  COMPILATION_EMIT_TYPE emit;
  Namespace* available_names;
  AST_LOCAL ast;

  void* extra;
};

struct ImportExtra {
  FileLocation src_loc;
};

struct GlobalExtra {
  Global* global;
};

struct FuncBodyExtra {
  IR::Function* func;
};

struct ExecCodeExtra {
  Type expected_type;
  void* destination;
};

struct CallSignature {
  const InternString* name = nullptr;
  Array<TypeAndFlags> arguments = {};
};

struct UnknownName {
  const InternString* ident = nullptr;
  Namespace* ns = {};
};


struct UnfoundNameHolder {
  UnknownName name = {};
  CompilationUnit* dependency = nullptr;

  //Used if the dependency isnt found
  ErrorMessage as_error = {};
};

struct UnfoundNames {
  Array<UnfoundNameHolder> names = {};
};

struct FileImport {
  FileLocation file_loc = {};
  Namespace* ns = nullptr;
  Span span = {};
};

struct FileLoader {
  FileLocation cwd = {};

  Array<FileImport> unparsed_files = {};
};

struct BuildOptions {
  const InternString* file_name = nullptr;
  const InternString* entry_point = nullptr;

  const InternString* output_file = nullptr;

  const InternString* lib_folder = nullptr;
  const InternString* std_lib_folder = nullptr;

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

  FreelistBlockAllocator<FuncBodyExtra> func_body_extras = {};
  FreelistBlockAllocator<GlobalExtra> global_extras = {};
  FreelistBlockAllocator<ImportExtra> import_extras = {};
  FreelistBlockAllocator<ExecCodeExtra> exec_code_extras = {};
};


struct Services {
  //Must always be acquired in order!
  AtomicPtr<FileLoader> file_loader;
  AtomicPtr<Backend::Program> out_program;
  AtomicPtr<Compilation> compilation;
  AtomicPtr<NameManager> names;

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
  Backend::PlatformInterface platform_interface = {};

  BuiltinTypes* builtin_types = nullptr;

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

  IR::GlobalLabel entry_point_label = { 0 };
  AtomicQueue<const IR::Builder*> finished_irs;

  CompPipes pipelines;

  Namespace* build_file_namespace = {};//needs to be saved for finding main
  Namespace* builtin_namespace = {};

  Array<IR::DynLibraryImport> dyn_lib_imports = {};
  Array<FileAST> parsed_files = {};
  Array<DynamicInitData> dynamic_inits = {};

  SpinLockMutex locals_mutex;
  BucketArray<Local> locals_single_threaded = {};
  SpinLockMutex globals_mutex;
  BucketArray<Global> globals_single_threaded = {};
  SpinLockMutex functions_mutex;
  BucketArray<IR::Function> functions_single_threaded = {};
  SpinLockMutex namespaces_mutex;
  BucketArray<Namespace> namespaces_single_threaded = {};

  SpinLockMutex ir_mutex;
  BucketArray<IR::Builder> ir_builders_single_threaded = {};
  
  SpinLockMutex label_mutex;
  Array<const SignatureStructure*> label_signature_table = {};

  SpinLockMutex constants_mutex;
  ArenaAllocator constants_single_threaded = {};

  IR::GlobalLabel next_function_label(const SignatureStructure* s);
  const SignatureStructure* get_label_signature(IR::GlobalLabel label);

  IR::Builder* new_ir(IR::GlobalLabel label);
  IR::Function* new_function();
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
    if (is_global_panic()) return false;
    if (finished_irs.size > 0) return true;
    if (services.compilation.get()->store.active_units.size > 0) return true;
    if (services.file_loader.get()->unparsed_files.size > 0) return true;

    return false;
  }
};

//Things that cannot be modified by other threads
struct CompilerThread : CompilerConstants {
  bool doing_work = true;

  Array<Token> current_stream = {};
  Errors errors = {};

  UnfoundNames local_unfound_names;
  Array<UnitID> new_depends;
  Array<CompilationUnit*> new_units;

  inline constexpr bool is_panic() const { return errors.is_panic(); }
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

void compile_all(CompilerGlobals* const comp, CompilerThread* const comp_thread);

void init_compiler(const APIOptions& options, CompilerGlobals* comp, CompilerThread* comp_thread);

void add_comp_unit_for_import(CompilerGlobals* const comp, Namespace* ns, const FileLocation& src_loc, ASTImport* imp) noexcept;

void add_comp_unit_for_lambda(CompilerGlobals* const comp, CompilerThread* const comp_thread, Namespace* ns, ASTLambda* lambda) noexcept;

void add_comp_unit_for_global(CompilerGlobals* const comp, CompilerThread* const comp_thread, Namespace* ns, ASTGlobalDecl* global) noexcept;

void add_comp_unit_for_struct(CompilerGlobals* const comp, Namespace* ns, ASTStructBody* struct_body) noexcept;