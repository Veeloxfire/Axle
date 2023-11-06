cmake_minimum_required(VERSION 3.10)
project(AxleCompiler)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

option(AxleTRACING "Enable tracing" OFF)
option(AxleTHROWING_ASSERTS, "Enable throwing asserts (for better testing)" OFF)

set(LIB_INCLUDE "${PROJECT_SOURCE_DIR}/lib_include")
set(LIB_BIN "${PROJECT_SOURCE_DIR}/lib")

if(NOT DEFINED AxleUtilINCLUDE)
  message(FATAL_ERROR "Requires AxleUtilINCLUDE to compile")
endif()
if(NOT DEFINED AxleUtilBIN)
  message(FATAL_ERROR "Requires AxleUtilBIN to compile")
endif()
if(NOT DEFINED TracerINCLUDE)
  message(FATAL_ERROR "Requires TracerINCLUDE to compile, even if you're not tracing")
endif()

set(AxleINCLUDE "${PROJECT_SOURCE_DIR}/include")
set(AxleHeaders
  "${AxleINCLUDE}/Axle/comp_utilities.h"
  "${AxleINCLUDE}/Axle/api.h"
  "${AxleINCLUDE}/Axle/calling_convention.h"
  "${AxleINCLUDE}/Axle/backends/x64_backend.h"
  "${AxleINCLUDE}/Axle/backends/PE_file_format.h")

add_library(AxleCompiler STATIC)
target_sources(AxleCompiler PRIVATE 
  FILE_SET HEADERS
  BASE_DIRS ${AxleINCLUDE}
  FILES ${AxleHeaders}
)

target_link_libraries(AxleCompiler PRIVATE "${AxleUtilBIN}/AxleUtil.lib")
target_include_directories(AxleCompiler PUBLIC ${AxleINCLUDE})
target_include_directories(AxleCompiler PUBLIC "${PROJECT_SOURCE_DIR}/src" ${AxleUtilINCLUDE} ${TracerINCLUDE})
add_subdirectory(src)
