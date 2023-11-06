cmake_minimum_required(VERSION 3.10)
project(AxleUnitTests)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

if(NOT DEFINED AxleINCLUDE)
  message(FATAL_ERROR "Require AxleINCLUDE (axle compiler include path) to be provided")
endif()

if(NOT DEFINED AxleSRC)
  message(FATAL_ERROR "Require AxleSRC (axle compiler src path) to be provided")
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

set(AxleTRACING OFF CACHE BOOL "Enables Tracing (not advised)" FORCE)
set(AxleTHROWING_ASSERTS ON CACHE BOOL "Enables Throwing Asserts" FORCE)

add_executable(AxleUnitTests)

target_include_directories(AxleUnitTests PRIVATE "${PROJECT_SOURCE_DIR}/src" ${AxleINCLUDE} ${AxleSRC} ${AxleUtilINCLUDE})

target_sources(AxleUnitTests PRIVATE
  "${PROJECT_SOURCE_DIR}/src/unit_tests.cpp"
  "${PROJECT_SOURCE_DIR}/src/containers_tests.cpp"
  "${PROJECT_SOURCE_DIR}/src/files_tests.cpp"
  "${PROJECT_SOURCE_DIR}/src/format_tests.cpp"
  "${PROJECT_SOURCE_DIR}/src/math_tests.cpp"
  "${PROJECT_SOURCE_DIR}/src/string_tests.cpp"
  "${PROJECT_SOURCE_DIR}/src/type_tests.cpp"
  "${PROJECT_SOURCE_DIR}/src/vm_tests.cpp"
)
target_sources(AxleUnitTests PRIVATE FILE_SET HEADERS 
  BASE_DIRS ${PROJECT_SOURCE_DIR}
  FILES "${PROJECT_SOURCE_DIR}/src/unit_tests.h"
)

add_subdirectory("${PROJECT_SOURCE_DIR}/.." "${CMAKE_BINARY_DIR}/comp_build")

target_link_libraries(AxleUnitTests AxleCompiler)