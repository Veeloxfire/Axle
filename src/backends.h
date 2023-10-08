#pragma once
#include "utility.h"

//Forward decl
namespace IR {
  struct Builder;
  struct DynLibraryImport;
}

enum struct STACK_DIRECTION : uint8_t {
  LEFT_TO_RIGHT, RIGHT_TO_LEFT
};

enum struct STACK_PASS_TYPE : uint8_t {
  VALUE, POINTER
};

enum struct CLEANUP : uint8_t {
  calleE, calleR
};

struct CallingConvention {
  static constexpr size_t OFFSET_TO_SHADOW = 8ull + 8ull;

  const char* name;

  uint8_t return_register = 0;//Usually RAX or 0
  uint8_t stack_pointer_reg = 0;
  uint8_t base_pointer_reg = 0;

  uint8_t num_parameter_registers = 0;
  uint8_t num_volatile_registers = 0;
  uint8_t num_non_volatile_registers = 0;
  uint8_t num_available_registers = 0;

  CLEANUP cleanup = CLEANUP::calleE;
  STACK_PASS_TYPE stack_pass_type = STACK_PASS_TYPE::POINTER;
  STACK_DIRECTION stack_direction = STACK_DIRECTION::RIGHT_TO_LEFT;

  uint8_t shadow_space_size = 0;//bytes

  uint64_t non_volatiles_bit_mask = 0;
  uint64_t volatiles_bit_mask = 0;

  const uint8_t* parameter_registers = nullptr;
  const uint8_t* all_regs_unordered = nullptr;//volatile then non_volatile

  constexpr bool is_non_volatile(uint8_t reg) const {
    return (non_volatiles_bit_mask & ((uint64_t)1 << reg)) != 0;
  }

  constexpr bool is_volatile(uint8_t reg) const {
    return (volatiles_bit_mask & ((uint64_t)1 << reg)) != 0;
  }

  constexpr size_t num_reg_parameters(size_t parameters) const {
    return smaller(parameters, (size_t)num_parameter_registers);
  }
};


namespace Backend {
  inline constexpr usize BUCKET_SIZE = 8 * 1024;

  struct DataBucket {
    u8 arr[BUCKET_SIZE];
    DataBucket* next;
  };

  struct DataBucketIterator {
    DataBucket* bucket;
    usize bucket_counter;
    usize actual_location;

    u8 read_byte();

    void jump_to(usize l);
    void overwrite_arr(const u8* start, const u8* end);
    inline void overwrite_arr(const u8* start, usize count) {
      overwrite_arr(start, start + count);
    }

    constexpr bool operator==(const DataBucketIterator& f) {
      return actual_location == f.actual_location;
    }

    constexpr bool operator<(const DataBucketIterator& f) {
      return actual_location < f.actual_location;
    }

    constexpr bool operator<=(const DataBucketIterator& f) {
      return actual_location <= f.actual_location;
    }
  };

  struct DataBucketStore {
    usize top_fill = 0;
    DataBucket* top = nullptr;
    DataBucket* bottom = nullptr;

    usize total_size = 0;

    DataBucketStore(const DataBucketStore&) = delete;
    DataBucketStore& operator=(const DataBucketStore&) = delete;

    DataBucketStore() = default;

    void free_held();
    inline ~DataBucketStore() noexcept {
      free_held();
    }

    inline DataBucketStore(DataBucketStore&& b) noexcept
      : top_fill(std::exchange(b.top_fill, 0)),
      top(std::exchange(b.top, nullptr)),
      bottom(std::exchange(b.bottom, nullptr)),
      total_size(std::exchange(b.total_size, 0))
    {}


    inline DataBucketStore& operator=(DataBucketStore&& b) noexcept {
      free_held();

      top_fill = std::exchange(b.top_fill, 0);
      top = std::exchange(b.top, nullptr);
      bottom = std::exchange(b.bottom, nullptr);
      total_size = std::exchange(b.total_size, 0);
    }

    void push_zeros(usize count);
    void push_arr(const u8* arr, const u8* end);

    inline void push_arr(const u8* arr, usize count) {
      push_arr(arr, arr + count);
    }


    constexpr DataBucketIterator current_location() const {
      DataBucketIterator f = {};
      f.bucket = top;
      f.bucket_counter = top_fill;
      f.actual_location = total_size;

      return f;
    }

    constexpr DataBucketIterator start() const {
      DataBucketIterator f = {};
      f.bucket = bottom;
      f.bucket_counter = 0;
      f.actual_location = 0;

      return f;
    }
  };

  struct FunctionMetadata {
    usize code_start = 0;
    usize code_size = 0;
  };

  enum struct RelocationType {
    Label,
    LibraryLabel,
    Global,
  };

  struct Relocation {
    RelocationType type = RelocationType::Label;
    union {
      IR::GlobalLabel label = IR::NULL_GLOBAL_LABEL;
      u32 library_call;
      u32 global_index;
    };
    usize location = 0;
  };

  struct DynImport {
    const InternString* function;
    const InternString* library;
  };

  struct DynExport {
    const InternString* name;
    IR::GlobalLabel label;
  };
  
  struct GlobalData {
    bool constant_init = false;

    usize size = 0;
    usize alignment = 0;
    usize data_index = 0;

    union {
      IR::GlobalLabel init_expr_label = IR::NULL_GLOBAL_LABEL;
      const u8* constant_value;
    };
  };

  struct GenericProgram {
    DataBucketStore code_store = {};
    DataBucketStore data_store = {};

    IR::GlobalLabel entry_point = IR::NULL_GLOBAL_LABEL;
    FunctionMetadata start_code;

    Array<FunctionMetadata> functions;
    Array<GlobalData> globals;

    Array<Relocation> relocations;
    Array<DynImport> dyn_imports;
    Array<DynExport> dyn_exports;
  };

  using PROGRAM_INIT = void(*)(CompilerGlobals* comp,
                               CompilerThread* comp_thread,
                               GenericProgram* program);

  using EMIT_FUNCTION = void(*)(CompilerGlobals* comp,
                                CompilerThread* comp_thread,
                                const IR::Builder* ir,
                                const CallingConvention* convention,
                                GenericProgram* program);

  using EMIT_START = void(*)(CompilerGlobals* comp,
                             IR::GlobalLabel entry_point,
                             GenericProgram* program);

  using EMIT_DYNAMIC_LIBRARY_FUNCTION = void(*) (CompilerThread* comp_thread,
                                                 const IR::DynLibraryImport* lib_import,
                                                 const CallingConvention* convention,
                                                 GenericProgram* program);

  struct PlatformInterface {
    CallingConvention const* const* valid_calling_conventions;
    u32 num_calling_conventions;

    System system;
    const char* system_name;

    usize ptr_size;

    PROGRAM_INIT init;
    EMIT_START emit_start;
    EMIT_FUNCTION emit_function;
    EMIT_DYNAMIC_LIBRARY_FUNCTION emit_dyn_library_function;
  };

  using OUTPUT_EXECUTABLE = void(*) (CompilerThread* comp_thread, const GenericProgram* program,
                                     const InternString* out_name, const InternString* out_folder);

  struct ExecutableFormatInterface {
    OutputFileType type;
    OUTPUT_EXECUTABLE output_executable;
    OUTPUT_EXECUTABLE output_dynamic_library;
  };
};