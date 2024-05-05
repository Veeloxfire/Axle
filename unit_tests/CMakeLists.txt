cmake_minimum_required(VERSION 3.10)
project(AxleUnitTests)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

foreach(config_type ${CMAKE_CONFIGURATION_TYPES})
  string(TOUPPER ${config_type} config_upper)
  set("CMAKE_ARCHIVE_OUTPUT_DIRECTORY_${config_upper}" "${PROJECT_BINARY_DIR}/out")
  set("CMAKE_LIBRARY_OUTPUT_DIRECTORY_${config_upper}" "${PROJECT_BINARY_DIR}/out")
  set("CMAKE_RUNTIME_OUTPUT_DIRECTORY_${config_upper}" "${PROJECT_BINARY_DIR}/out")
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

add_executable(UnitTestsServer)
add_executable(UnitTestsClient)
set_target_properties(UnitTestsServer PROPERTIES OUTPUT_NAME "UnitTestsServer$<CONFIG>")
set_target_properties(UnitTestsClient PROPERTIES OUTPUT_NAME "UnitTestsClient$<CONFIG>")

add_dependencies(UnitTestsServer UnitTestsClient)

target_compile_definitions(UnitTestsServer PRIVATE UNIT_TEST_CLIENT_EXE="UnitTestsClient$<CONFIG>.exe")
target_compile_definitions(UnitTestsClient PRIVATE STACKTRACE_ENABLE)

target_include_directories(UnitTestsClient PRIVATE "${PROJECT_SOURCE_DIR}/../src" $"${PROJECT_SOURCE_DIR}/../include" ${AxleUtilINCLUDE})
target_include_directories(UnitTestsServer PRIVATE ${AxleUtilINCLUDE})

target_sources(UnitTestsServer PRIVATE
  "${PROJECT_SOURCE_DIR}/test_server_main.cpp"
)

target_sources(UnitTestsClient PRIVATE
  "${PROJECT_SOURCE_DIR}/test_client_main.cpp"

  "${PROJECT_SOURCE_DIR}/errors_tests.cpp"
  "${PROJECT_SOURCE_DIR}/names_tests.cpp"
  "${PROJECT_SOURCE_DIR}/type_tests.cpp"
  "${PROJECT_SOURCE_DIR}/vm_tests.cpp"
)
target_link_libraries(UnitTestsServer "${AxleUtilBIN}/AxleTestServer$<CONFIG>.lib" "${AxleUtilBIN}/AxleUtil$<CONFIG>.lib")
target_link_libraries(UnitTestsClient AxleCompiler "${AxleUtilBIN}/AxleTestClient$<CONFIG>.lib")
