cmake_minimum_required(VERSION 3.10)
project(AxleSandbox)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

add_executable(AxleSandbox "src/sandbox.cpp")

if(NOT DEFINED AxleINCLUDE)
  message(FATAL_ERROR "Require AxleINCLUDE (axle compiler include path) to be provided")
endif()

if(NOT DEFINED AxleUtilINCLUDE)
  message(FATAL_ERROR "Require AxleUtilINCLUDE (axle util include path) to be provided")
endif()

if(NOT DEFINED TracerINCLUDE)
  message(FATAL_ERROR "Require TracerINCLUDE (tracer include path) to be provided, even if not tracing")
endif()

if(NOT DEFINED AxleBIN)
  message(FATAL_ERROR "Require AxleBIN (axle compiler library path) to be provided")
endif()

if(NOT DEFINED AxleUtilBIN)
  message(FATAL_ERROR "Require AxleUtilBIN (axle util library path) to be provided")
endif()

option(AxleTRACING "Enable tracing" OFF)
if(AxleTRACING)
  if(NOT DEFINED TracerBIN)
    message(FATAL_ERROR "Require TracerBIN (tracer include path) to be provided when tracing")
  endif()

  target_compile_definitions(AxleSandbox PRIVATE TRACING_ENABLE)
  target_link_libraries(AxleSandbox "${TracerBIN}/Tracer.lib")
endif()

target_include_directories(AxleSandbox PRIVATE src ${AxleINCLUDE} ${AxleUtilINCLUDE} ${TracerINCLUDE})
target_link_libraries(AxleSandbox "${AxleBIN}/AxleCompiler.lib" "${AxleUtilBIN}/AxleUtil.lib")