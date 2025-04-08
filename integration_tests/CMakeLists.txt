if(NOT DEFINED AxleUtilINCLUDE)
  message(FATAL_ERROR "Require AxleUtilINCLUDE (axle util include path) to be provided")
endif()

if(NOT DEFINED AxleUtilBIN)
  message(FATAL_ERROR "Require AxleUtilBIN (axle util library path) to be provided")
endif()

if(NOT DEFINED TesterINCLUDE)
  message(FATAL_ERROR "Require TesterINCLUDE (tester include path) to be provided")
endif()

if(AxleTRACING)
  message(FATAL_ERROR "Cannot trace the unit tests")
endif()


add_executable(IntegrationTestsServer)
add_executable(IntegrationTestsClient)
set_target_properties(IntegrationTestsServer PROPERTIES OUTPUT_NAME "IntegrationTestsServer$<CONFIG>")
set_target_properties(IntegrationTestsClient PROPERTIES OUTPUT_NAME "IntegrationTestsClient$<CONFIG>")

foreach(config_type ${CMAKE_CONFIGURATION_TYPES})
  string(TOUPPER ${config_type} config_upper)
  set_target_properties(IntegrationTestsServer IntegrationTestsClient
      PROPERTIES
      "ARCHIVE_OUTPUT_DIRECTORY_${config_upper}" "${PROJECT_BINARY_DIR}/out/integration_tests"
      "LIBRARY_OUTPUT_DIRECTORY_${config_upper}" "${PROJECT_BINARY_DIR}/out/integration_tests"
      "RUNTIME_OUTPUT_DIRECTORY_${config_upper}" "${PROJECT_BINARY_DIR}/out/integration_tests"
  )
endforeach()

add_dependencies(IntegrationTestsServer IntegrationTestsClient)

target_compile_definitions(IntegrationTestsServer PRIVATE INTEGRATION_TEST_CLIENT_EXE="IntegrationTestsClient$<CONFIG>.exe")
target_compile_definitions(IntegrationTestsClient PRIVATE
  INTEGRATION_TEST_CLIENT_DIR=\"${PROJECT_SOURCE_DIR}\"
  INTEGRATION_TEST_CLIENT_OUT_DIR=\"${PROJECT_SOURCE_DIR}/integration_tests/out/\"
  INTEGRATION_TEST_CLIENT_SRC_DIR=\"${PROJECT_SOURCE_DIR}/integration_tests/src/\"
  INTEGRATION_TEST_CLIENT_STDLIB_DIR=\"${PROJECT_SOURCE_DIR}/stdlib/\"
)

target_compile_definitions(IntegrationTestsClient PRIVATE STACKTRACE_ENABLE)

target_include_directories(IntegrationTestsClient PRIVATE ${AxleUtilINCLUDE} "${PROJECT_SOURCE_DIR}/integration_tests/src" ${TesterINCLUDE})

target_include_directories(IntegrationTestsServer PRIVATE ${AxleUtilINCLUDE})

target_sources(IntegrationTestsServer PRIVATE
  "${PROJECT_SOURCE_DIR}/integration_tests/src/test_server_main.cpp"
)

target_sources(IntegrationTestsClient PRIVATE
  "${PROJECT_SOURCE_DIR}/integration_tests/src/test_client_main.cpp"
  "${PROJECT_SOURCE_DIR}/integration_tests/src/tests.cpp"
)

target_link_libraries(IntegrationTestsServer "${AxleUtilBIN}/AxleTestServer$<CONFIG>.lib" "${AxleUtilBIN}/AxleUtil$<CONFIG>.lib")
target_link_libraries(IntegrationTestsClient AxleCompiler TesterLib "${AxleUtilBIN}/AxleTestClient$<CONFIG>.lib")

add_custom_target(CopyDLL ALL
  COMMAND ${CMAKE_COMMAND} -E copy_if_different "$<TARGET_FILE_DIR:TesterDll>/Tester.dll" "${PROJECT_SOURCE_DIR}/out")
add_custom_target(CopyPDB ALL
  COMMAND ${CMAKE_COMMAND} -E copy_if_different "$<TARGET_FILE_DIR:TesterDll>/Tester.pdb" "${PROJECT_SOURCE_DIR}/out")

add_dependencies(IntegrationTestsClient CopyDLL CopyPDB)
