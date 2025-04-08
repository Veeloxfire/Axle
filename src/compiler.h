#pragma once
#include <AxleUtil/utility.h>
#include <AxleUtil/strings.h>
#include <AxleUtil/formattable.h>
#include <AxleUtil/files.h>

#include <Axle/comp_utilities.h>
#include <Axle/api.h>

#include "parser.h"
#include "ast.h"
#include "names.h"

#include "type.h"
#include "ir.h"
#include "backends.h"

#include <atomic>

struct Decl {
  Span span = {};

  const Axle::InternString* name = {};
  VALUE_CATEGORY value_category;
  Type type = {};

  const u8* init_value = nullptr;
};

struct Local {
  Decl decl = {};

  IR::VariableId variable_id = {};
};

struct Pipe : Axle::AtomicQueue<CompilationUnit*> {
  Axle::ViewArr<const char> _debug_name;
};

struct ComptimeExec {
  AST_LOCAL ast;
  u8* dest;
  Type type;
};

struct Global {
  Decl decl = {};
  
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
  Axle::FreelistBlockAllocator<DependencyListSingle> dependency_list_entry;

  u64 in_flight_units = 0;//Units that are not waiting somewhere

  void close_dependency(CompilationUnit* ptr, bool print);
  void try_restart(CompilationUnit* ptr, bool print);
  void add_dependency_to(CompilationUnit* now_waiting, CompilationUnit* waiting_on);
};

struct CompilationUnit {
  UnitID id;

  u32 unit_wait_on_count;
  u32 unfound_wait_on_count;

  u32 depend_list_size;
  DependencyListSingle* dependency_list;
  Pipe* main_pipe;

  COMPILATION_EMIT_TYPE emit;
  Namespace* available_names;
  AST_LOCAL ast;

  void* detail;

  constexpr bool waiting() const { return unit_wait_on_count > 0 || unfound_wait_on_count > 0; }
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
  Axle::FileLocation src_loc;
};

struct ExportCompilation {};

struct CallSignature {
  Axle::Array<Type> arguments = {};
};

namespace Axle::Format {
  template<>
  struct FormatArg<CallSignature> {
    template<Formatter F>
    constexpr static void load_string(F& res, const CallSignature& call_sig) {
      auto i = call_sig.arguments.begin();
      const auto end = call_sig.arguments.end();

      res.load_char('(');

      if (i < end) {
        for (; i < (end - 1); i++) {
          FormatArg<const Axle::InternString*>::load_string(res, i->name);
          res.load_string_lit(", ");
        }

        FormatArg<const Axle::InternString*>::load_string(res, i->name);
      }

      res.load_char(')');
    }
  };
}

struct UnknownName {
  const Axle::InternString* ident = nullptr;
  Namespace* ns = {};
};

struct UnfoundNameHolder {
  UnknownName name = {};
  CompilationUnit* dependency = nullptr;

  //Used if the dependency isnt found
  ErrorMessage as_error = {};
};

struct UnfoundNames {
  Axle::Array<UnfoundNameHolder> names = {};
};

struct FileImport {
  Axle::FileLocation file_loc = {};
  Namespace* ns = nullptr;
  Span span = {};
};

struct FileLoader {
  Axle::Directory cwd = {};
  Axle::Directory source_diretory = {};
  Axle::Directory std_lib_directory = {};

  Axle::Array<FileImport> unparsed_files = {};
};

struct BuildOptions {
  bool debug_break_on_entry = false;

  const Axle::InternString* file_name = nullptr;
  const Axle::InternString* source_folder = nullptr;

  bool is_library = false;
  const Axle::InternString* entry_point = nullptr;

  const Axle::InternString* output_folder = nullptr;
  const Axle::InternString* output_name = nullptr;

  const Axle::InternString* lib_folder = nullptr;
  const Axle::InternString* std_lib_folder = nullptr;

  const CallingConvention* default_calling_convention = nullptr;
};

#define IMPORTANT_NAMES_INC \
MOD(ptr) \
MOD(len) \
MOD(axl) \

struct ImportantNames {
#define MOD(n) const Axle::InternString* n;
  IMPORTANT_NAMES_INC;
#undef MOD
};

struct CompilationUnitStore {
  UnitID comp_unit_counter = NULL_ID;

  Axle::FreelistBlockAllocator<CompilationUnit> compilation_units = {};
  Axle::Array<CompilationUnit*> active_units;

  CompilationUnit* allocate_unit();
  void free_unit(CompilationUnit* unit);
  CompilationUnit* get_unit_if_exists(u64 id) const;
};

struct Compilation {
  UnfoundNames unfound_names = {};
  DependencyManager dependencies = {};

  CompilationUnitStore store = {};

  Axle::FreelistBlockAllocator<StructCompilation> struct_compilation = {};
  Axle::FreelistBlockAllocator<LambdaBodyCompilation> lambda_body_compilation = {};
  Axle::FreelistBlockAllocator<LambdaSigCompilation> lambda_sig_compilation = {};
  Axle::FreelistBlockAllocator<GlobalCompilation> global_compilation = {};
  Axle::FreelistBlockAllocator<ExportCompilation> export_compilation = {};
  Axle::FreelistBlockAllocator<ImportCompilation> import_compilation = {};
};


struct Services {
  //Must always be acquired in order!
  Axle::AtomicPtr<FileLoader> file_loader;
  Axle::AtomicPtr<Backend::ProgramData> out_program;
  Axle::AtomicPtr<Compilation> compilation;
  Axle::AtomicPtr<NameManager> names;

  Axle::AtomicPtr<Structures> structures;
  Axle::AtomicPtr<Axle::StringInterner> strings;

  void get_multiple(Axle::AtomicLock<FileLoader>* files,
                    Axle::AtomicLock<Compilation>* comp) {
    file_loader._mutex.acquire();
    compilation._mutex.acquire();

    files->_mutex = &file_loader._mutex;
    files->_ptr = file_loader._ptr;

    comp->_mutex = &compilation._mutex;
    comp->_ptr = compilation._ptr;
  }

  void get_multiple(Axle::AtomicLock<Structures>* structs,
                    Axle::AtomicLock<Axle::StringInterner>* string_int) {
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

struct GlobalLabelInfo {
  const SignatureStructure* signature;
  Span span = {};
};

//Things that may be modified by multiple threads
struct CompilerGlobals : CompilerConstants {
  std::atomic_uint32_t work_counter = 0;

  Axle::Signal global_panic;
  Axle::SpinLockMutex global_errors_mutex;
  Axle::Array<ErrorMessage> global_errors;

  Services services;

  IR::GlobalLabel entry_point_label = IR::NULL_GLOBAL_LABEL;
  Axle::AtomicQueue<const IR::IRStore*> finished_irs;

  CompPipes pipelines;

  Namespace* build_file_namespace = {};//needs to be saved for finding main
  Namespace* builtin_namespace = {};

  Axle::Array<IR::DynLibraryImport> dyn_lib_imports = {};
  Axle::Array<FileAST> parsed_files = {};
  Axle::Array<Backend::GlobalData> dynamic_inits = {};

  Axle::SpinLockMutex locals_mutex;
  Axle::BucketArray<Local> locals_single_threaded = {};
  Axle::SpinLockMutex globals_mutex;
  Axle::BucketArray<Global> globals_single_threaded = {};
  Axle::SpinLockMutex functions_mutex;
  Axle::BucketArray<IR::Function> functions_single_threaded = {};
  Axle::SpinLockMutex namespaces_mutex;
  Axle::BucketArray<Namespace> namespaces_single_threaded = {};

  Axle::SpinLockMutex ir_mutex;
  Axle::BucketArray<IR::IRStore> ir_builders_single_threaded = {};

  Axle::SpinLockMutex label_mutex;
  Axle::Array<GlobalLabelInfo> label_signature_table = {};

  Axle::SpinLockMutex constants_mutex;
  Axle::ArenaAllocator constants_single_threaded = {};

  IR::GlobalLabel next_function_label(const SignatureStructure* s, const Span& span);
  GlobalLabelInfo get_label_info(IR::GlobalLabel label);

  IR::IRStore* new_ir(IR::GlobalLabel label, const SignatureStructure* sig);
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

  Axle::Array<Token> current_stream = {};
  Errors errors = {};

  UnfoundNames local_unfound_names;
  Axle::Array<UnitID> new_depends;
  Axle::Array<CompilationUnit*> new_units;

  inline constexpr bool is_panic() const { return errors.is_panic(); }
  inline bool is_depends() const {
    return new_depends.size > 0 || local_unfound_names.names.size > 0;
  }

  template<typename ... T>
  void report_error(ERROR_CODE code, const Span& span, const Axle::Format::FormatString<T...>& f_message, const T& ... ts) {
    return errors.report_error(code, span, f_message, ts...);
  }

  inline void report_error(ERROR_CODE code, const Span& span, Axle::OwnedArr<const char>&& msg) {
    return errors.report_error(code, span, std::move(msg));
  }
};

inline constexpr void thread_doing_work(CompilerGlobals* comp, CompilerThread* comp_thread) {
  if (!comp_thread->doing_work) {
    comp->work_counter += 1;
    comp_thread->doing_work = true;
  }
}

void compile_all(CompilerGlobals* const comp, CompilerThread* const comp_thread);

void init_compiler(const APIOptions& options, CompilerGlobals* comp, CompilerThread* comp_thread);

void add_comp_unit_for_import(CompilerGlobals* const comp, Namespace* ns, const Axle::FileLocation& src_loc, ASTImport* imp) noexcept;

void add_comp_unit_for_export(CompilerGlobals* const comp, Namespace* ns, ASTExport* imp) noexcept;

void add_comp_unit_for_lambda(CompilerGlobals* const comp, Namespace* ns, ASTLambda* lambda) noexcept;

void add_comp_unit_for_global(CompilerGlobals* const comp, Namespace* ns, ASTDecl* global) noexcept;

void add_comp_unit_for_struct(CompilerGlobals* const comp, Namespace* ns, ASTStructBody* struct_body) noexcept;

