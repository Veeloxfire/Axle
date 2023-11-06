cmake_minimum_required(VERSION 3.10)
project(AxleIntegrationTests)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

set(AxleTRACING OFF)
set(AxleTHROWING_ASSERTS ON)

set(LIB_INCLUDE "${PROJECT_SOURCE_DIR}/../lib_include")
set(LIB_BIN "${PROJECT_SOURCE_DIR}/../lib")

add_executable(AxleIntegrationTests "${PROJECT_SOURCE_DIR}/src/tests.cpp")
target_include_directories(AxleIntegrationTests PUBLIC src ${LIB_INCLUDE} "${PROJECT_SOURCE_DIR}/../src")
target_link_libraries(AxleIntegrationTests "${LIB_BIN}/Tester.lib")

add_custom_target(CopyDll ALL
      COMMAND ${CMAKE_COMMAND} -E copy_if_different "${LIB_BIN}/Tester.dll" "$<TARGET_FILE_DIR:AxleIntegrationTests>"
      DEPENDS AxleIntegrationTests)

target_compile_definitions(AxleIntegrationTests PRIVATE "ROOT_DIR=\"${PROJECT_SOURCE_DIR}\"")


add_subdirectory("${PROJECT_SOURCE_DIR}/../src" "${CMAKE_BINARY_DIR}/comp_src_build")

target_link_libraries(AxleIntegrationTests AxleCompiler)