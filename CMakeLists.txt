cmake_minimum_required(VERSION 3.10)
project(AxleCompiler)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

foreach(config_type ${CMAKE_CONFIGURATION_TYPES})
  string(TOUPPER ${config_type} config_upper)
  if(NOT DEFINED "CMAKE_ARCHIVE_OUTPUT_DIRECTORY_${config_upper}")
    set("CMAKE_ARCHIVE_OUTPUT_DIRECTORY_${config_upper}" "${PROJECT_BINARY_DIR}/out")
  endif()
  if(NOT DEFINED "CMAKE_LIBRARY_OUTPUT_DIRECTORY_${config_upper}")
    set("CMAKE_LIBRARY_OUTPUT_DIRECTORY_${config_upper}" "${PROJECT_BINARY_DIR}/out")
  endif()
  if(NOT DEFINED "CMAKE_RUNTIME_OUTPUT_DIRECTORY_${config_upper}")
    set("CMAKE_RUNTIME_OUTPUT_DIRECTORY_${config_upper}" "${PROJECT_BINARY_DIR}/out")
  endif()
endforeach()

option(AxleTRACING "Enable tracing" OFF)

if(NOT DEFINED AxleUtilINCLUDE)
  message(FATAL_ERROR "Requires AxleUtilINCLUDE to compile")
endif()
if(NOT DEFINED AxleUtilBIN)
  message(FATAL_ERROR "Requires AxleUtilBIN to compile")
endif()

set(AxleINCLUDE "${PROJECT_SOURCE_DIR}/include")
set(AxleSRC "${PROJECT_SOURCE_DIR}/src")
set(AxleHeaders
  "${AxleINCLUDE}/Axle/comp_utilities.h"
  "${AxleINCLUDE}/Axle/api.h"
  "${AxleINCLUDE}/Axle/calling_convention.h"
  "${AxleINCLUDE}/Axle/backends/x64_backend.h"
  "${AxleINCLUDE}/Axle/backends/PE_file_format.h")

add_library(AxleCompiler STATIC)
set_target_properties(AxleCompiler PROPERTIES OUTPUT_NAME "AxleCompiler$<CONFIG>")
target_sources(AxleCompiler PRIVATE 
  FILE_SET HEADERS
  BASE_DIRS ${AxleINCLUDE}
  FILES ${AxleHeaders}
)

target_link_libraries(AxleCompiler PUBLIC "${AxleUtilBIN}/AxleUtil$<CONFIG>.lib")
target_include_directories(AxleCompiler PUBLIC ${AxleINCLUDE})
target_include_directories(AxleCompiler PUBLIC "${PROJECT_SOURCE_DIR}/src" ${AxleUtilINCLUDE})

if(AxleTRACING)
  if(NOT DEFINED TracerINCLUDE)
    message(FATAL_ERROR "Requires TracerINCLUDE to compile if you're tracing")
  endif()

  target_compile_definitions(AxleCompiler PRIVATE TRACING_ENABLE AXLE_TRAXING)

  if(NOT DEFINED TracerBIN)
    message(FATAL_ERROR "Requires TracerBIN to compile if you're tracing")
  endif()

  target_link_libraries(AxleCompiler "${TracerBIN}/Tracer.lib")
endif()

add_subdirectory(src)

option(BUILD_SANDBOX "Build sandbox" OFF)
if(BUILD_SANDBOX)
  message(NOTICE "Sandbox Enabled")
  add_subdirectory(sandbox)
endif()

option(BUILD_TESTS "Build tests" OFF)
if(BUILD_TESTS)
  message(NOTICE "Tests Enabled")
  set(TesterINCLUDE "${PROJECT_SOURCE_DIR}/lib_include" CACHE STRING "" FORCE)
  set(TesterBIN "${PROJECT_SOURCE_DIR}/lib" CACHE STRING "" FORCE)

  add_subdirectory(unit_tests)
  add_subdirectory(integration_tests)
endif()
