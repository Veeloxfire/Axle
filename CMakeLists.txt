cmake_minimum_required(VERSION 3.10)
project(AxleCompiler)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

if (DEFINED CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT AND CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT AND DEFINED ENV{CMAKE_INSTALL_PREFIX})
  set_property(CACHE CMAKE_INSTALL_PREFIX PROPERTY VALUE $ENV{CMAKE_INSTALL_PREFIX})
endif()

message("Install Prefix: ${CMAKE_INSTALL_PREFIX}")

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

find_package(AxleUtil)

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

if(MSVC)
  set(CxxFlags /diagnostics:caret /Wall /WX /wd4365 /wd4514 /wd4582 /wd4583 /wd4623 /wd4625 /wd4626 /wd4627 /wd4710 /wd4711 /wd4820 /wd5026 /wd5027 /wd5039 /wd5045 /wd5220 /permissive-)
else()
  set(CxxFlags -Wall -Wextra -Wpedantic -Werror)
endif()

target_compile_options(AxleCompiler PUBLIC ${CxxFlags})

target_link_libraries(AxleCompiler PUBLIC AxleUtil::Core)
target_include_directories(AxleCompiler PUBLIC ${AxleINCLUDE})
target_include_directories(AxleCompiler PRIVATE "${PROJECT_SOURCE_DIR}/src")

if(AxleTRACING)
  if(NOT DEFINED TracerINCLUDE)
    message(FATAL_ERROR "Requires TracerINCLUDE to compile if you're tracing")
  endif()

  if(NOT DEFINED TracerBIN)
    message(FATAL_ERROR "Requires TracerBIN to compile if you're tracing")
  endif()

  target_compile_definitions(AxleCompiler PRIVATE AXLE_TRACING_ENABLE)
  target_include_directories(AxleCompiler PRIVATE  ${TracerINCLUDE})
  target_link_libraries(AxleCompiler PRIVATE "${TracerBIN}/Tracer.lib")
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
  add_subdirectory(unit_tests)
  
  set(TesterINCLUDE "${PROJECT_SOURCE_DIR}/lib_include" CACHE STRING "" FORCE)

  add_library(TesterLib STATIC
    "${PROJECT_SOURCE_DIR}/Tester/src/tester.c"
  )
  target_include_directories(TesterLib PUBLIC ${TesterINCLUDE})
  target_compile_options(TesterLib PUBLIC ${CxxFlags})
  
  add_library(TesterDll SHARED
    "${PROJECT_SOURCE_DIR}/Tester/src/tester.c"
  )
  target_include_directories(TesterDll PUBLIC ${TesterINCLUDE})
  target_compile_options(TesterDll PUBLIC ${CxxFlags})

  set_target_properties(TesterDll PROPERTIES OUTPUT_NAME "Tester")

  add_subdirectory(integration_tests)

endif()
