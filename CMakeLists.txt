cmake_minimum_required(VERSION 3.10)
project(AxleCompiler)

option(AxleTRACING "Enable tracing" OFF)
option(AxleTHROWING_ASSERTS, "Enable throwing asserts (for better testing)" OFF)

set(LIB_INCLUDE "${PROJECT_SOURCE_DIR}/lib_include")
set(LIB_BIN "${PROJECT_SOURCE_DIR}/lib")

add_subdirectory(src)