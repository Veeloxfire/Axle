cmake_minimum_required(VERSION 3.10)
project(AxleUnitTests)

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

if(NOT DEFINED AxleUtilINCLUDE)
  message(FATAL_ERROR "Require AxleUtilINCLUDE (axle util include path) to be provided")
endif()

if(NOT DEFINED AxleUtilBIN)
  message(FATAL_ERROR "Require AxleUtilBIN (axle util library path) to be provided")
endif()

if(NOT DEFINED AxleINCLUDE)
  message(FATAL_ERROR "Require AxleINCLUDE (axle include path) to be provided")
endif()

if(NOT DEFINED AxleSRC)
  message(FATAL_ERROR "Require AxleSRC (axle src include path) to be provided")
endif()

if(AxleTRACING)
  message(FATAL_ERROR "Cannot trace the unit tests")
endif()

option(AxleTHROWING_ASSERTS, "Enable throwing asserts (for better testing)" OFF)

add_executable(AxleUnitTests)
set_target_properties(AxleUnitTests PROPERTIES OUTPUT_NAME "AxleUnitTests$<CONFIG>")

if(AxleTHROWING_ASSERTS)
  target_compile_definitions(AxleUnitTests PRIVATE ASSERT_EXCEPTIONS)
endif()

target_include_directories(AxleUnitTests PRIVATE "${PROJECT_SOURCE_DIR}/../src" $"${PROJECT_SOURCE_DIR}/../include" ${AxleUtilINCLUDE})

target_sources(AxleUnitTests PRIVATE
  "${PROJECT_SOURCE_DIR}/tests_main.cpp"

  "${PROJECT_SOURCE_DIR}/errors_tests.cpp"
  "${PROJECT_SOURCE_DIR}/names_tests.cpp"
  "${PROJECT_SOURCE_DIR}/type_tests.cpp"
  "${PROJECT_SOURCE_DIR}/vm_tests.cpp"
)
target_link_libraries(AxleUnitTests AxleCompiler "${AxleUtilBIN}/AxleUtil$<CONFIG>.lib")
