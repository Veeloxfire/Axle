cmake_minimum_required(VERSION 3.10)
project(AxleIntegrationTests)

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

if(NOT DEFINED TesterINCLUDE)
  message(FATAL_ERROR "Require TesterINCLUDE (tester include path) to be provided, even if not tracing")
endif()

if(NOT DEFINED TesterBIN)
  message(FATAL_ERROR "Require TesterBIN (tester library path) to be provided, even if not tracing")
endif()

if(NOT DEFINED AxleBIN)
  message(FATAL_ERROR "Require AxleBIN (axle compiler library path) to be provided")
endif()

if(NOT DEFINED AxleUtilBIN)
  message(FATAL_ERROR "Require AxleUtilBIN (axle util library path) to be provided")
endif()

set(AxleTRACING OFF CACHE BOOL "Enables Tracing (not advised)" FORCE)
set(AxleTHROWING_ASSERTS ON CACHE BOOL "Enables Throwing Asserts" FORCE)

add_executable(AxleIntegrationTests "${PROJECT_SOURCE_DIR}/src/tests.cpp")
target_include_directories(AxleIntegrationTests PRIVATE "${PROJECT_SOURCE_DIR}/src" ${AxleINCLUDE} ${AxleUtilINCLUDE})

add_subdirectory("${PROJECT_SOURCE_DIR}/.." "${CMAKE_BINARY_DIR}/comp_build")

target_link_libraries(AxleIntegrationTests AxleCompiler)

target_include_directories(AxleIntegrationTests PUBLIC src ${TesterINCLUDE} "${PROJECT_SOURCE_DIR}/../src")
target_link_libraries(AxleIntegrationTests "${TesterBIN}/Tester.lib")

add_custom_target(CopyDll ALL
      COMMAND ${CMAKE_COMMAND} -E copy_if_different "${TesterBIN}/Tester.dll" "$<TARGET_FILE_DIR:AxleIntegrationTests>"
      DEPENDS AxleIntegrationTests)

target_compile_definitions(AxleIntegrationTests PRIVATE "ROOT_DIR=\"${PROJECT_SOURCE_DIR}\"")