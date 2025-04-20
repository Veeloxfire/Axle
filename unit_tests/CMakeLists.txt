if(AxleTRACING)
  message(FATAL_ERROR "Cannot trace the unit tests")
endif()

add_executable(UnitTestsServer)
add_executable(UnitTestsClient)

set_target_properties(UnitTestsServer PROPERTIES OUTPUT_NAME "UnitTestsServer$<CONFIG>")
set_target_properties(UnitTestsClient PROPERTIES OUTPUT_NAME "UnitTestsClient$<CONFIG>")

foreach(config_type ${CMAKE_CONFIGURATION_TYPES})
  string(TOUPPER ${config_type} config_upper)
  set_target_properties(UnitTestsServer UnitTestsClient
      PROPERTIES
      "ARCHIVE_OUTPUT_DIRECTORY_${config_upper}" "${PROJECT_BINARY_DIR}/out/unit_tests"
      "LIBRARY_OUTPUT_DIRECTORY_${config_upper}" "${PROJECT_BINARY_DIR}/out/unit_tests"
      "RUNTIME_OUTPUT_DIRECTORY_${config_upper}" "${PROJECT_BINARY_DIR}/out/unit_tests"
  )
endforeach()

add_dependencies(UnitTestsServer UnitTestsClient)

target_compile_definitions(UnitTestsServer PRIVATE UNIT_TEST_CLIENT_EXE="UnitTestsClient$<CONFIG>.exe")
target_compile_definitions(UnitTestsClient PRIVATE STACKTRACE_ENABLE)

target_include_directories(UnitTestsClient PRIVATE "${PROJECT_SOURCE_DIR}/src" $"${PROJECT_SOURCE_DIR}/include")

target_sources(UnitTestsServer PRIVATE
  "${PROJECT_SOURCE_DIR}/unit_tests/test_server_main.cpp"
)

target_sources(UnitTestsClient PRIVATE
  "${PROJECT_SOURCE_DIR}/unit_tests/test_client_main.cpp"

  "${PROJECT_SOURCE_DIR}/unit_tests/errors_tests.cpp"
  "${PROJECT_SOURCE_DIR}/unit_tests/ir_tests.cpp"
  "${PROJECT_SOURCE_DIR}/unit_tests/names_tests.cpp"
  "${PROJECT_SOURCE_DIR}/unit_tests/type_tests.cpp"
  "${PROJECT_SOURCE_DIR}/unit_tests/vm_tests.cpp"
)
target_link_libraries(UnitTestsServer AxleUtil::TestServer AxleUtil::Core)
target_link_libraries(UnitTestsClient AxleCompiler AxleUtil::TestClient)
