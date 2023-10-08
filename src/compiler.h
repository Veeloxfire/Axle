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
  VALUE_CATEGORY value_category;
  Type type = {};
};

struct Local {
  Decl decl = {};

  bool is_constant = false;
  const u8* constant = nullptr;

  IR::VariableId variable_id = {};
};

struct Pipe : AtomicQueue<CompilationUnit*> {
  const char* _debug_name;
};

struct ComptimeExec {
  AST_LOCAL ast;
  u8* dest;
  Type type;
};

struct DependencyChecker {
  Namespace* available_names;
  u32 num_locals;
  Array<Local*> locals;

  Local* get_local(const InternString* name);
};

struct Global {
  Decl decl = {};

  bool is_constant = false;
  const u8* constant_value;
  
  bool is_runtime_available = false;
  u32 dynamic_init_index = 0;
};

enum struct COMPILATION_EMIT_TYPE : uint8_t {
  STRUCTURE, LAMBDA_BODY, LAMBDA_SIG, GLOBAL, IMPORT, EXPORT
};

struct CompPipes {
  Pipe depend_check;

  Pipe comp_structure;
  Pipe comp_body;
  Pipe comp_signature;
  Pipe comp_global;
  Pipe comp_import;
  Pipe comp_export;
};

struct DependencyListSingle {
  DependencyListSingle* next;
  CompilationUnit* waiting;
};

struct DependencyManager {
  Pipe* depend_check_pipe;
  FreelistBlockAllocator<DependencyListSingle> dependency_list_entry;

  u64 in_flight_units = 0;//Units that are not waiting somewhere

  void close_dependency(CompilationUnit* ptr, bool print);
  void remove_dependency_from(CompilationUnit* ptr, bool print);
  void add_dependency_to(CompilationUnit* now_waiting, CompilationUnit* waiting_on);
#if 0
  void add_external_dependency(CompilationUnit* now_waiting);
#endif
};

struct CompilationUnit {
  UnitID id;

  u32 waiting_on_count;
  u32 depend_list_size;
  DependencyListSingle* dependency_list;
  Pipe* main_pipe;

  COMPILATION_EMIT_TYPE emit;
  Namespace* available_names;
  AST_LOCAL ast;

  void* detail;
};

struct GlobalCompilation {
  Global* global;
};

struct LambdaBodyCompilation {
  IR::Function* func;
};

struct LambdaSigCompilation {
  IR::Function* func;
  ASTLambda* lambda;
};

struct StructCompilation {};

struct ImportCompilation {
  FileLocation src_loc;
};

struct ExportCompilation {};

struct CallSignature {
  Array<Type> arguments = {};
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
  Directory cwd = {};
  Directory source_diretory = {};
  Directory std_lib_directory = {};

  Array<FileImport> unparsed_files = {};
};

struct BuildOptions {
  bool debug_break_on_entry = false;

  const InternString* file_name = nullptr;
  const InternString* source_folder = nullptr;

  bool is_library = false;
  const InternString* entry_point = nullptr;

  const InternString* output_folder = nullptr;
  const InternString* output_name = nullptr;

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

  CompilationUnitStore store = {};

  FreelistBlockAllocator<StructCompilation> struct_compilation = {};
  FreelistBlockAllocator<LambdaBodyCompilation> lambda_body_compilation = {};
  FreelistBlockAllocator<LambdaSigCompilation> lambda_sig_compilation = {};
  FreelistBlockAllocator<GlobalCompilation> global_compilation = {};
  FreelistBlockAllocator<ExportCompilation> export_compilation = {};
  FreelistBlockAllocator<ImportCompilation> import_compilation = {};
};


struct Services {
  //Must always be acquired in order!
  AtomicPtr<FileLoader> file_loader;
  AtomicPtr<Backend::GenericProgram> out_program;
  AtomicPtr<Compilation> compilation;
  AtomicPtr<NameManager> names;

  AtomicPtr<Structures> structures;
  AtomicPtr<StringInterner> strings;

  void get_multiple(AtomicLock<FileLoader>* files,
                    AtomicLock<Compilation>* comp) {
    file_loader._mutex.acquire();
    compilation._mutex.acquire();

    files->_mutex = &file_loader._mutex;
    files->_ptr = file_loader._ptr;

    comp->_mutex = &compilation._mutex;
    comp->_ptr = compilation._ptr;
  }

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
  std::atomic_uint32_t work_counter = 0;

  Signal global_panic;
  SpinLockMutex global_errors_mutex;
  Array<ErrorMessage> global_errors;

  Services services;

  IR::GlobalLabel entry_point_label = IR::NULL_GLOBAL_LABEL;
  AtomicQueue<const IR::Builder*> finished_irs;

  CompPipes pipelines;

  Namespace* build_file_namespace = {};//needs to be saved for finding main
  Namespace* builtin_namespace = {};

  Array<IR::DynLibraryImport> dyn_lib_imports = {};
  Array<FileAST> parsed_files = {};
  Array<Backend::GlobalData> dynamic_inits = {};

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

  IR::Builder* new_ir(IR::GlobalLabel label, const SignatureStructure* sig);
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
};

//Things that cannot be modified by other threads
struct CompilerThread : CompilerConstants {
  bool doing_work = true;
  u32 thread_id;

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
  void report_error(ERROR_CODE code, const Span& span, const FormatString& f_message, const T& ... ts) {
    return errors.report_error(code, span, f_message, ts...);
  }

  inline void report_error(ERROR_CODE code, const Span& span, OwnedArr<const char>&& msg) {
    return errors.report_error(code, span, std::move(msg));
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

void add_comp_unit_for_export(CompilerGlobals* const comp, Namespace* ns, ASTExport* imp) noexcept;

void add_comp_unit_for_lambda(CompilerGlobals* const comp, CompilerThread* const comp_thread, Namespace* ns, ASTLambda* lambda) noexcept;

void add_comp_unit_for_global(CompilerGlobals* const comp, CompilerThread* const comp_thread, Namespace* ns, ASTDecl* global) noexcept;

void add_comp_unit_for_struct(CompilerGlobals* const comp, Namespace* ns, ASTStructBody* struct_body) noexcept;

const SignatureStructure* find_or_make_lamdba_structure(Structures* const structures,
                                                        StringInterner* strings,
                                                        usize ptr_size,
                                                        const CallingConvention* conv,
                                                        Array<Type>&& params,
                                                        Type ret_type);