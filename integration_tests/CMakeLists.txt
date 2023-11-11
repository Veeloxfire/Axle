cmake_minimum_required(VERSION 3.10)
project(AxleIntegrationTests)

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

if(AxleTHROWING_ASSERTS)
  target_compile_definitions(AxleUnitTests PRIVATE ASSERT_EXCEPTIONS)
endif()

add_executable(AxleIntegrationTests "${PROJECT_SOURCE_DIR}/src/tests.cpp")
set_target_properties(AxleIntegrationTests PROPERTIES OUTPUT_NAME "AxleIntegrationTests$<CONFIG>")
target_include_directories(AxleIntegrationTests PRIVATE "${PROJECT_SOURCE_DIR}/src" ${AxleINCLUDE} ${AxleUtilINCLUDE})

target_link_libraries(AxleIntegrationTests AxleCompiler)

target_include_directories(AxleIntegrationTests PUBLIC src ${TesterINCLUDE} "${PROJECT_SOURCE_DIR}/../src")
target_link_libraries(AxleIntegrationTests "${TesterBIN}/Tester.lib")

add_custom_target(CopyDll ALL
      COMMAND ${CMAKE_COMMAND} -E copy_if_different "${TesterBIN}/Tester.dll" "$<TARGET_FILE_DIR:AxleIntegrationTests>"
      DEPENDS AxleIntegrationTests)

target_compile_definitions(AxleIntegrationTests PRIVATE "ROOT_DIR=\"${PROJECT_SOURCE_DIR}\"")
