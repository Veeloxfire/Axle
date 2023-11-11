cmake_minimum_required(VERSION 3.10)
project(AxleCompiler)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

set(CppFiles
  "${PROJECT_SOURCE_DIR}/api.cpp"
  "${PROJECT_SOURCE_DIR}/backends.cpp"
  "${PROJECT_SOURCE_DIR}/compiler.cpp"
  "${PROJECT_SOURCE_DIR}/errors.cpp"
  "${PROJECT_SOURCE_DIR}/ir.cpp"
  "${PROJECT_SOURCE_DIR}/names.cpp"
  "${PROJECT_SOURCE_DIR}/operators.cpp"
  "${PROJECT_SOURCE_DIR}/parser.cpp"
  "${PROJECT_SOURCE_DIR}/backends/PE_file_format.cpp"
  "${PROJECT_SOURCE_DIR}/type.cpp"
  "${PROJECT_SOURCE_DIR}/type_check.cpp"
  "${PROJECT_SOURCE_DIR}/backends/vm_backend.cpp"
  "${PROJECT_SOURCE_DIR}/backends/x64_backend.cpp")

set(CppHeaders
  "${PROJECT_SOURCE_DIR}/ast.h"
  "${PROJECT_SOURCE_DIR}/compiler.h"
  "${PROJECT_SOURCE_DIR}/errors.h"
  "${PROJECT_SOURCE_DIR}/ir.h"
  "${PROJECT_SOURCE_DIR}/names.h"
  "${PROJECT_SOURCE_DIR}/operators.h"
  "${PROJECT_SOURCE_DIR}/parser.h"
  "${PROJECT_SOURCE_DIR}/type.h"
  "${PROJECT_SOURCE_DIR}/type_check.h")

target_sources(AxleCompiler PRIVATE ${CppFiles})
target_sources(AxleCompiler PRIVATE 
  FILE_SET HEADERS
  BASE_DIRS ${PROJECT_SOURCE_DIR}
  FILES ${CppHeaders}
)

