#pragma once
#include <stdint.h>

#ifdef TRACING_ENABLE

namespace Tracing {
  using u64 = uint64_t;

  struct Event {
    const char* name;
    u64 time_start;
    u64 time_end;
  };

  void start_tracer_threaded(const char* output_file_name);
  void end_tracer_threaded();

  u64 get_time();
  void upload_event(const Event& e);

  struct AUTO_SCOPE_EVENT {
    Event e;

    AUTO_SCOPE_EVENT(const char* name) : e() {
      e.name = name;
      e.time_start = get_time();
    }

    ~AUTO_SCOPE_EVENT() {
      e.time_end = get_time();

      upload_event(e);
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
