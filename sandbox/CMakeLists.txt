cmake_minimum_required(VERSION 3.10)
project(AxleSandbox)

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

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

add_executable(AxleSandbox "src/sandbox.cpp")

if(NOT DEFINED AxleINCLUDE)
  message(FATAL_ERROR "Require AxleINCLUDE (axle compiler include path) to be provided")
endif()

if(NOT DEFINED AxleUtilINCLUDE)
  message(FATAL_ERROR "Require AxleUtilINCLUDE (axle util include path) to be provided")
endif()

if(NOT DEFINED AxleUtilBIN)
  message(FATAL_ERROR "Require AxleUtilBIN (axle util library path) to be provided")
endif()

option(AxleTRACING "Enable tracing" OFF)
if(AxleTRACING) 
  if(NOT DEFINED TracerINCLUDE)
    message(FATAL_ERROR "Require TracerINCLUDE (tracer include path) to be provided, even if not tracing")
  endif()

  target_include_directories(AxleSandbox PRIVATE  ${TracerINCLUDE})

  if(NOT DEFINED TracerBIN)
    message(FATAL_ERROR "Require TracerBIN (tracer include path) to be provided when tracing")
  endif()

  target_compile_definitions(AxleSandbox PRIVATE TRACING_ENABLE)
  target_link_libraries(AxleSandbox "${TracerBIN}/Tracer.lib")
endif()

target_include_directories(AxleSandbox PRIVATE src ${AxleINCLUDE} ${AxleUtilINCLUDE})
target_link_libraries(AxleSandbox AxleCompiler "${AxleUtilBIN}/AxleUtil$<CONFIG>.lib")
