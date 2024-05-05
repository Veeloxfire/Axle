cmake_minimum_required(VERSION 3.10)
project(AxleIntegrationTests)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

foreach(config_type ${CMAKE_CONFIGURATION_TYPES})
  string(TOUPPER ${config_type} config_upper)
  set("CMAKE_ARCHIVE_OUTPUT_DIRECTORY_${config_upper}" "${PROJECT_BINARY_DIR}/out")
  set("CMAKE_LIBRARY_OUTPUT_DIRECTORY_${config_upper}" "${PROJECT_BINARY_DIR}/out")
  set("CMAKE_RUNTIME_OUTPUT_DIRECTORY_${config_upper}" "${PROJECT_BINARY_DIR}/out")
endforeach()

if(NOT DEFINED AxleINCLUDE)
  message(FATAL_ERROR "Require AxleINCLUDE (axle compiler include path) to be provided")
endif()

if(NOT DEFINED AxleSRC)
  message(FATAL_ERROR "Require AxleSRC (axle compiler src path) to be provided")
endif()

if(NOT DEFINED AxleUtilINCLUDE)
  message(FATAL_ERROR "Require AxleUtilINCLUDE (axle util include path) to be provided")
endif()

if(NOT DEFINED AxleUtilBIN)
  message(FATAL_ERROR "Require AxleUtilBIN (axle util library path) to be provided")
endif()

if(NOT DEFINED TesterINCLUDE)
  message(FATAL_ERROR "Require TesterINCLUDE (tester include path) to be provided, even if not tracing")
endif()

if(NOT DEFINED TesterBIN)
  message(FATAL_ERROR "Require TesterBIN (tester library path) to be provided, even if not tracing")
endif()

if(AxleTRACING)
  message(FATAL_ERROR "Cannot trace the unit tests")
endif()


add_executable(IntegrationTestsServer)
add_executable(IntegrationTestsClient)
set_target_properties(IntegrationTestsServer PROPERTIES OUTPUT_NAME "IntegrationTestsServer$<CONFIG>")
set_target_properties(IntegrationTestsClient PROPERTIES OUTPUT_NAME "IntegrationTestsClient$<CONFIG>")

add_dependencies(IntegrationTestsServer IntegrationTestsClient)

target_compile_definitions(IntegrationTestsServer PRIVATE INTEGRATION_TEST_CLIENT_EXE="IntegrationTestsClient$<CONFIG>.exe")
target_compile_definitions(IntegrationTestsClient PRIVATE INTEGRATION_TEST_CLIENT_DIR=\"${PROJECT_SOURCE_DIR}\")

target_compile_definitions(IntegrationTestsClient PRIVATE STACKTRACE_ENABLE)

target_include_directories(IntegrationTestsClient PRIVATE ${AxleSRC} ${AxleINCLUDE} ${AxleUtilINCLUDE} "${PROJECT_SOURCE_DIR}/src" ${TesterINCLUDE})

target_include_directories(IntegrationTestsServer PRIVATE ${AxleUtilINCLUDE})

target_sources(IntegrationTestsServer PRIVATE
  "${PROJECT_SOURCE_DIR}/src/test_server_main.cpp"
)

target_sources(IntegrationTestsClient PRIVATE
  "${PROJECT_SOURCE_DIR}/src/test_client_main.cpp"
  "${PROJECT_SOURCE_DIR}/src/tests.cpp"
)

target_link_libraries(IntegrationTestsServer "${AxleUtilBIN}/AxleTestServer$<CONFIG>.lib" "${AxleUtilBIN}/AxleUtil$<CONFIG>.lib")
target_link_libraries(IntegrationTestsClient AxleCompiler "${AxleUtilBIN}/AxleTestClient$<CONFIG>.lib" "${TesterBIN}/Tester.lib")

target_include_directories(IntegrationTestsServer PUBLIC src ${TesterINCLUDE} "${PROJECT_SOURCE_DIR}/../src")

add_custom_target(CopyDll ALL
      COMMAND ${CMAKE_COMMAND} -E copy_if_different "${TesterBIN}/Tester.dll" "$<TARGET_FILE_DIR:IntegrationTestsServer>"
      DEPENDS IntegrationTestsServer)
