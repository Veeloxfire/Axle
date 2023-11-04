cmake_minimum_required(VERSION 3.10)
project(AxleCompiler)
set(PROJECT_VERSION 0.1)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

add_library(AxleCompiler STATIC 
  api.cpp
  backends.cpp
  compiler.cpp
  errors.cpp
  "files.cpp"
  format.cpp
  ir.cpp
  memory.cpp
  "names.cpp"
  operators.cpp
  parser.cpp
  PE_file_format.cpp
  "strings.cpp"
  threading.cpp
  "type.cpp"
  type_check.cpp
  utility.cpp
  vm_backend.cpp
  windows_specifics.cpp
  x64_backend.cpp
)

file(COPY api.h DESTINATION "${CMAKE_BINARY_DIR}/include")

if(NOT DEFINED LIB_INCLUDE)
  message(FATAL_ERROR "Missing Option \"LIB_INCLUDE\"")
endif()

if(NOT DEFINED LIB_BIN)
  message(FATAL_ERROR "Missing Option \"LIB_BIN\"")
endif()

if(AxleTRACING)
  target_compile_definitions(AxleCompiler PRIVATE TRACING_ENABLE)
  target_link_libraries(AxleCompiler "${LIB_BIN}/TRACER.lib")
endif()

if(AxleTHROWING_ASSERTS)
  target_compile_definitions(AxleCompiler PRIVATE ASSERT_EXCEPTIONS)
endif()

if(NOT DEFINED LIB_INCLUDE)
  message(FATAL_ERROR "Missing Option \"LIB_INCLUDE\"")
endif()

target_include_directories(AxleCompiler PUBLIC ${PROJECT_SOURCE_DIR})
target_include_directories(AxleCompiler PRIVATE ${LIB_INCLUDE})