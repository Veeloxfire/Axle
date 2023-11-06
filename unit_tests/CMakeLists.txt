cmake_minimum_required(VERSION 3.10)
project(AxleUnitTests)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

set(AxleTRACING OFF)
set(AxleTHROWING_ASSERTS ON)
set(COPY_HEADERS OFF)

set(LIB_INCLUDE "${PROJECT_SOURCE_DIR}/../lib_include")
set(LIB_BIN "${PROJECT_SOURCE_DIR}/../lib")
set(COMP_INCLUDE "${PROJECT_SOURCE_DIR}/../src")


add_executable(AxleUnitTests)

target_include_directories(AxleUnitTests PRIVATE ${PROJECT_SOURCE_DIR} ${LIB_INCLUDE} ${COMP_INCLUDE})

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

add_subdirectory("${PROJECT_SOURCE_DIR}/../src" "${CMAKE_BINARY_DIR}/comp_src_build")

target_link_libraries(AxleUnitTests AxleCompiler)