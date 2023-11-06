cmake_minimum_required(VERSION 3.10)
project(AxleSandbox)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

add_executable(AxleSandbox "src/sandbox.cpp")

option(AxleTRACING "Enable tracing" OFF)

if(NOT DEFINED COMPILER_DIR)
  set(COMPILER_DIR "${CMAKE_SOURCE_DIR}/../build/src")
  message(WARNING "Missing \"COMPILER_DIR\" option. (Defaulting to \"${COMPILER_DIR}\"")
endif()

set(CompLibDir "${COMPILER_DIR}/$<CONFIG>")

if(NOT DEFINED LIB_INCLUDE)
  set(LIB_INCLUDE "${CMAKE_SOURCE_DIR}/../lib_include")
  message(WARNING "Missing \"LIB_INCLUDE\" option. (Defaulting to \"${LIB_INCLUDE}\")")
endif()

if(NOT DEFINED LIB_BIN)
  set(LIB_BIN "${CMAKE_SOURCE_DIR}/../lib")
  message(WARNING "Missing \"LIB_BIN\" option. (Defaulting to \"${LIB_BIN}\"")
endif()


if(AxleTRACING)
  target_compile_definitions(AxleSandbox PRIVATE TRACING_ENABLE)
  target_link_libraries(AxleCompiler "${LIB_BIN}/TRACER.lib")
endif()

target_include_directories(AxleSandbox PRIVATE src ${LIB_INCLUDE} "${CompLibDir}/include")
target_link_libraries(AxleSandbox "${CompLibDir}/AxleCompiler.lib")