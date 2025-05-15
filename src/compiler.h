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

struct Global {
  Decl decl = {};
  
  bool is_runtime_available = false;
  u32 dynamic_init_index = 0;
};

namespace VM {
  struct EvalPromise {
    u8* data;
    Type type;
  };

  void dispatch_eval_ast(CompilerGlobals* comp, CompilerThread* comp_thread, Namespace* ns, AST_LOCAL root, EvalPromise eval);
}

enum struct COMPILATION_UNIT_TYPE : uint8_t {
  STRUCTURE, LAMBDA_BODY, LAMBDA_SIG, GLOBAL, IMPORT, EXPORT, EVAL,
};

enum struct COMPILATION_UNIT_STAGE : uint8_t {
  DEPEND_CHECK, TYPE_CHECK, EMIT, DONE,
};

struct Pipe : Axle::AtomicQueue<CompilationUnit*> {
  Axle::ViewArr<const char> _debug_name;
};

struct CompPipes {
  Pipe depend_check;
  Pipe type_check;
  Pipe emit;
};

struct DependencySingle {
  COMPILATION_UNIT_STAGE stage_required;
  CompilationUnit* waiting;
};

struct CompilationUnit {
  UnitID id;
  COMPILATION_UNIT_TYPE type;
  COMPILATION_UNIT_STAGE stage;

  // controlled by DependencyManager

  std::atomic_uint32_t unit_wait_on_count;
  std::atomic_uint32_t unfound_wait_on_count;

  Axle::Array<DependencySingle> dependency_list;

  // controlled by unit

  Namespace* available_names;
  AST_LOCAL ast;
  usize next_tc_index;
  Axle::OwnedArr<AstVisit> visit_arr;

  void* detail;

  inline bool waiting() const { return unit_wait_on_count > 0 || unfound_wait_on_count > 0; }
};

struct GlobalCompilation {
  Global* global;
};

struct LambdaBodyCompilation {};

struct LambdaSigCompilation {
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
  bool updated = false;
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
  Axle::Array<FileAST> parsed_files = {};
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
  CompPipes* pipes = nullptr;
  UnfoundNames unfound_names = {};

  CompilationUnitStore store = {};

  Axle::FreelistBlockAllocator<StructCompilation> struct_compilation = {};
  Axle::FreelistBlockAllocator<LambdaBodyCompilation> lambda_body_compilation = {};
  Axle::FreelistBlockAllocator<LambdaSigCompilation> lambda_sig_compilation = {};
  Axle::FreelistBlockAllocator<GlobalCompilation> global_compilation = {};
  Axle::FreelistBlockAllocator<ExportCompilation> export_compilation = {};
  Axle::FreelistBlockAllocator<ImportCompilation> import_compilation = {};
  Axle::FreelistBlockAllocator<VM::EvalPromise> eval_promises = {};

  u64 in_flight_units = 0;//Units that are not waiting somewhere

  void add_dependency_to(CompilationUnit* waiting_on, const DependencySingle& dep);
  static bool maybe_depend(CompilerGlobals* comp, CompilerThread* comp_thread, Compilation* compilation, CompilationUnit* unit);
};


struct Services {
  //Must always be acquired in order!
  Axle::AtomicPtr<FileLoader> file_loader;
  Axle::AtomicPtr<Backend::ProgramData> out_program;
  Axle::AtomicPtr<Compilation> compilation;
  Axle::AtomicPtr<NameManager> names;
  Axle::AtomicPtr<Structures> structures;
  Axle::AtomicPtr<Axle::StringInterner> strings;

  struct ServiceGetter {
    Axle::AtomicLock<FileLoader>* file_loader = nullptr;
    Axle::AtomicLock<Backend::ProgramData>* out_program = nullptr;
    Axle::AtomicLock<Compilation>* compilation = nullptr;
    Axle::AtomicLock<NameManager>* names = nullptr;
    Axle::AtomicLock<Structures>* structures = nullptr;
    Axle::AtomicLock<Axle::StringInterner>* strings = nullptr;
  };

  void get_multiple(const ServiceGetter& getter) {
    if (getter.file_loader) {
      file_loader._mutex.acquire();
      getter.file_loader->_mutex = &file_loader._mutex;
      getter.file_loader->_ptr = file_loader._ptr;
    }

    if (getter.out_program) {
      out_program._mutex.acquire();
      getter.out_program->_mutex = &out_program._mutex;
      getter.out_program->_ptr = out_program._ptr;
    }

    if (getter.compilation) {
      compilation._mutex.acquire();
      getter.compilation->_mutex = &compilation._mutex;
      getter.compilation->_ptr = compilation._ptr;
    }
    
    if (getter.names) {
      names._mutex.acquire();
      getter.names->_mutex = &names._mutex;
      getter.names->_ptr = names._ptr;
    }

    if (getter.structures) {
      structures._mutex.acquire();
      getter.structures->_mutex = &structures._mutex;
      getter.structures->_ptr = structures._ptr;
    }
    
    if (getter.strings) {
      strings._mutex.acquire();
      getter.strings->_mutex = &strings._mutex;
      getter.strings->_ptr = strings._ptr;
    }
  }
};

struct CompilerConstants {
  APIPrintOptions        print_options = {};
  BuildOptions           build_options = {};
  APIOptimizationOptions optimization_options = {};
  Backend::PlatformInterface platform_interface = {};

  BuiltinTypes* builtin_types = nullptr;
  const SignatureStructure* s_global_init_call = nullptr;

  Intrinsics intrinsics = {};
  ImportantNames important_names = {};

  u32 active_threads;
};

constexpr void copy_compiler_constants(const CompilerConstants* from, CompilerConstants* to) {
  *to = *from;
}

struct GlobalLabelInfo {
  const SignatureStructure* signature;
  Span span;
  IR::IRStore* ir;
  UnitID dependency;

  constexpr VALUE_CATEGORY get_call_category() const noexcept {
    if (ir != nullptr && dependency != NULL_ID) {
      return VALUE_CATEGORY::TEMPORARY_CONSTANT;
    }
    else {
      return VALUE_CATEGORY::TEMPORARY_IMMUTABLE;
    }
  }
};

//Things that may be modified by multiple threads
struct CompilerGlobals : CompilerConstants {
  bool names_updated = false;// only available with names
  std::atomic_int32_t available_work_counter = 0;

  Axle::Signal global_panic;
  Axle::SpinLockMutex global_errors_mutex;
  Axle::Array<ErrorMessage> global_errors;

  Services services;

  IR::GlobalLabel entry_point_label = IR::NULL_GLOBAL_LABEL;
  Axle::AtomicQueue<const IR::IRStore*> finished_irs;

  CompPipes pipelines = {};

  Namespace* build_file_namespace = nullptr;//needs to be saved for finding main
  Namespace* builtin_namespace = nullptr;

  Axle::Array<IR::DynLibraryImport> dyn_lib_imports = {};
  Axle::Array<Backend::GlobalData> dynamic_inits = {};

  Axle::SpinLockMutex locals_mutex;
  Axle::BucketArray<Local> locals_single_threaded = {};
  Axle::SpinLockMutex globals_mutex;
  Axle::BucketArray<Global> globals_single_threaded = {};
  Axle::SpinLockMutex namespaces_mutex;
  Axle::BucketArray<Namespace> namespaces_single_threaded = {};

  Axle::SpinLockMutex ir_mutex;
  Axle::BucketArray<IR::IRStore> ir_builders_single_threaded = {};

  Axle::SpinLockMutex label_mutex;
  Axle::Array<GlobalLabelInfo> label_signature_table = {};

  Axle::SpinLockMutex constants_mutex;
  Axle::GrowingMemoryPool<1024> constants_single_threaded = {};

  IR::GlobalLabel new_ir_function(const SignatureStructure* s, const Span& span, UnitID dependency);
  IR::GlobalLabel new_runtime_link_function(const SignatureStructure* s, const Span& span);
  IR::GlobalLabel new_runtime_ir_function(const SignatureStructure* s, const Span& span);
  GlobalLabelInfo get_label_info(IR::GlobalLabel label);

  IR::IRStore* get_ir(IR::GlobalLabel label);
  Local* new_local();
  Global* new_global();
  Namespace* new_namespace();

  template<typename T>
  T* new_global_constant() {
    constants_mutex.acquire();
    T* t = constants_single_threaded.allocate<T>();
    constants_mutex.release();
    return t;
  }

  inline u8* new_global_constant(const Type& t) {
    constants_mutex.acquire();
    u8* v = (u8*)constants_single_threaded.alloc_raw(t.size(), t.structure->alignment);
    constants_mutex.release();
    return v;
  }

  inline bool is_global_panic() const {
    return global_panic.test();
  }
};

struct DeferredDependency {
  COMPILATION_UNIT_STAGE stage;
  UnitID unit;
};

//Things that cannot be modified by other threads
struct CompilerThread : CompilerConstants {
  u32 thread_id;

  Axle::Array<Token> current_stream = {};
  Errors errors = {};

  UnfoundNames local_unfound_names;
  Axle::Array<DeferredDependency> new_depends;

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

void compile_all(CompilerGlobals* const comp, CompilerThread* const comp_thread);

void init_compiler(const APIOptions& options, CompilerGlobals* comp, CompilerThread* comp_thread);

void add_comp_unit_for_import(CompilerGlobals* const comp, Namespace* ns, const Axle::FileLocation& src_loc, ASTImport* imp) noexcept;

void add_comp_unit_for_export(CompilerGlobals* const comp, Namespace* ns, ASTExport* imp) noexcept;

void add_comp_unit_for_lambda(CompilerGlobals* const comp, Namespace* ns, ASTLambda* lambda) noexcept;

void add_comp_unit_for_global(CompilerGlobals* const comp, Namespace* ns, ASTDecl* global) noexcept;

void add_comp_unit_for_struct(CompilerGlobals* const comp, Namespace* ns, ASTStructBody* struct_body) noexcept;

