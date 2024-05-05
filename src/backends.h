#pragma once
#include <AxleUtil/utility.h>

#include <Axle/comp_utilities.h>
#include <Axle/api.h>
#include <Axle/calling_convention.h>

//Forward decl
namespace IR {
  struct IRStore;
  struct DynLibraryImport;
}

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

      return *this;
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
    const Axle::InternString* function;
    const Axle::InternString* library;
  };

  struct DynExport {
    const Axle::InternString* name;
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

  struct ProgramData {
    ProgramExtra* extra;

    DataBucketStore code_store = {};
    DataBucketStore data_store = {};

    IR::GlobalLabel entry_point = IR::NULL_GLOBAL_LABEL;
    FunctionMetadata start_code;

    Axle::Array<FunctionMetadata> functions;
    Axle::Array<GlobalData> globals;

    Axle::Array<Relocation> relocations;
    Axle::Array<DynImport> dyn_imports;
    Axle::Array<DynExport> dyn_exports;
  };
}

namespace Axle {
  template<ByteOrder Ord>
  struct Serializer<Backend::DataBucketIterator, Ord> {
    Backend::DataBucketIterator* itr;
    constexpr Serializer(Backend::DataBucketIterator& itr_ref) : itr(&itr_ref) {}

    constexpr bool read_bytes(const ViewArr<u8>& data) {
      FOR_MUT(data, it) {
        *it = itr->read_byte();
      }
      return true;
    }
  };
}
