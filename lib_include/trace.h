#ifndef TRACER_H
#define TRACER_H
#include <stdint.h>

#ifdef TRACING_ENABLE

namespace Tracing {
  using u32 = uint32_t;
  using u64 = uint64_t;

  struct Event {
    const char* name;
    u32 name_size;

    u64 time_start;
    u64 time_end;
  };

  u64 get_time();
  void upload_event(const Event& e);

  template<size_t N>
  Event start_event(const char(&name)[N]) {
    Event e{};
    e.name = name;
    e.name_size = N - 1;
    e.time_start = get_time();

    return e;
  }

  inline void end_event(Event& e) {
    e.time_end = get_time();
    upload_event(e);
  }

  template<size_t N>
  constexpr u32 ARRAY_SIZE(const char(&)[N]) {
    return static_cast<u32>(N);
  }

  void start_default_tracing_thread(const char* output_file_name);
  void end_default_tracing_thread();

  void tracer_thread_proc();
  void create_trace_data(const char* output_file_name);

  void signal_end_trace_thread();

  struct AUTO_SCOPE_EVENT {
    Event e;

    template<size_t N>
    AUTO_SCOPE_EVENT(const char(&name)[N]) : e(start_event(name)) {}

    ~AUTO_SCOPE_EVENT() {
      end_event(e);
    }
  };
}
#define TRACING_JOIN2(a, b) a##b 
#define TRACING_JOIN(a, b) TRACING_JOIN2(a, b)

#define TRACING_SCOPE(name) Tracing::AUTO_SCOPE_EVENT TRACING_JOIN(_tracer_, __LINE__) (name)
#define TRACING_FUNCTION() TRACING_SCOPE(__FUNCTION__)

#else
#define TRACING_SCOPE(name) (void)0
#define TRACING_FUNCTION() (void)0
#endif

#endif