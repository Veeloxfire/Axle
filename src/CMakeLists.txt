cmake_minimum_required(VERSION 3.10)
project(AxleCompiler)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

add_library(AxleCompiler STATIC)
target_include_directories(AxleCompiler PUBLIC ${PROJECT_SOURCE_DIR})
target_include_directories(AxleCompiler PRIVATE ${LIB_INCLUDE})

set(CppFiles
  "${PROJECT_SOURCE_DIR}/api.cpp"
  "${PROJECT_SOURCE_DIR}/backends.cpp"
  "${PROJECT_SOURCE_DIR}/compiler.cpp"
  "${PROJECT_SOURCE_DIR}/errors.cpp"
  "${PROJECT_SOURCE_DIR}/files.cpp"
  "${PROJECT_SOURCE_DIR}/format.cpp"
  "${PROJECT_SOURCE_DIR}/io.cpp"
  "${PROJECT_SOURCE_DIR}/ir.cpp"
  "${PROJECT_SOURCE_DIR}/memory.cpp"
  "${PROJECT_SOURCE_DIR}/names.cpp"
  "${PROJECT_SOURCE_DIR}/operators.cpp"
  "${PROJECT_SOURCE_DIR}/parser.cpp"
  "${PROJECT_SOURCE_DIR}/PE_file_format.cpp"
  "${PROJECT_SOURCE_DIR}/strings.cpp"
  "${PROJECT_SOURCE_DIR}/threading.cpp"
  "${PROJECT_SOURCE_DIR}/type.cpp"
  "${PROJECT_SOURCE_DIR}/type_check.cpp"
  "${PROJECT_SOURCE_DIR}/utility.cpp"
  "${PROJECT_SOURCE_DIR}/vm_backend.cpp"
  "${PROJECT_SOURCE_DIR}/windows_specifics.cpp"
  "${PROJECT_SOURCE_DIR}/x64_backend.cpp")

set(CppHeaders
  "${PROJECT_SOURCE_DIR}/api.h"
  "${PROJECT_SOURCE_DIR}/args.h"
  "${PROJECT_SOURCE_DIR}/ast.h"
  "${PROJECT_SOURCE_DIR}/backends.h"
  "${PROJECT_SOURCE_DIR}/compiler.h"
  "${PROJECT_SOURCE_DIR}/comp_utilities.h"
  "${PROJECT_SOURCE_DIR}/calling_convention.h"
  "${PROJECT_SOURCE_DIR}/errors.h"
  "${PROJECT_SOURCE_DIR}/files.h"
  "${PROJECT_SOURCE_DIR}/format.h"
  "${PROJECT_SOURCE_DIR}/formattable.h"
  "${PROJECT_SOURCE_DIR}/io.h"
  "${PROJECT_SOURCE_DIR}/ir.h"
  "${PROJECT_SOURCE_DIR}/memory.h"
  "${PROJECT_SOURCE_DIR}/names.h"
  "${PROJECT_SOURCE_DIR}/operators.h"
  "${PROJECT_SOURCE_DIR}/os.h"
  "${PROJECT_SOURCE_DIR}/parser.h"
  "${PROJECT_SOURCE_DIR}/PE_file_format.h"
  "${PROJECT_SOURCE_DIR}/safe_lib.h"
  "${PROJECT_SOURCE_DIR}/strings.h"
  "${PROJECT_SOURCE_DIR}/threading.h"
  "${PROJECT_SOURCE_DIR}/type.h"
  "${PROJECT_SOURCE_DIR}/type_check.h"
  "${PROJECT_SOURCE_DIR}/utility.h"
  "${PROJECT_SOURCE_DIR}/windows_specifics.h"
  "${PROJECT_SOURCE_DIR}/x64_backend.h")

target_sources(AxleCompiler PRIVATE ${CppFiles})
target_sources(AxleCompiler PRIVATE 
  FILE_SET HEADERS
  BASE_DIRS ${PROJECT_SOURCE_DIR}
  FILES ${CppHeaders}
)

option (COPY_HEADERS "Copy Public Headers to the output direcotry" ON)
if(COPY_HEADERS)
  set(PublicHeaders
  "${PROJECT_SOURCE_DIR}/safe_lib.h"
  "${PROJECT_SOURCE_DIR}/comp_utilities.h"
  "${PROJECT_SOURCE_DIR}/api.h"
  "${PROJECT_SOURCE_DIR}/args.h"
  "${PROJECT_SOURCE_DIR}/calling_convention.h"
  "${PROJECT_SOURCE_DIR}/io.h"
  "${PROJECT_SOURCE_DIR}/formattable.h"
  "${PROJECT_SOURCE_DIR}/format.h"
  "${PROJECT_SOURCE_DIR}/x64_backend.h"
  "${PROJECT_SOURCE_DIR}/windows_specifics.h"
  "${PROJECT_SOURCE_DIR}/PE_file_format.h")

  set(IncludesOut "$<TARGET_FILE_DIR:AxleCompiler>/include")

  add_custom_target(MakeHeaderDir ALL
      COMMAND ${CMAKE_COMMAND} -E make_directory ${IncludesOut}
      DEPENDS AxleCompiler)
  add_custom_target(CopyHeaders ALL
      COMMAND ${CMAKE_COMMAND} -E copy_if_different  ${PublicHeaders} ${IncludesOut}
      DEPENDS MakeHeaderDir)
endif()

if(NOT DEFINED LIB_INCLUDE)
  message(FATAL_ERROR "Missing \"LIB_INCLUDE\" option. (should be the `/lib_include` folder)")
endif()

if(NOT DEFINED LIB_BIN)
  message(FATAL_ERROR "Missing \"LIB_BIN\" option. (should be the `/lib` folder)")
endif()

if(AxleTRACING)
  target_compile_definitions(AxleCompiler PRIVATE TRACING_ENABLE)
  target_link_libraries(AxleCompiler "${LIB_BIN}/TRACER.lib")
endif()

if(AxleTHROWING_ASSERTS)
  target_compile_definitions(AxleCompiler PRIVATE ASSERT_EXCEPTIONS)
endif()

if(NOT DEFINED LIB_INCLUDE)
  message(FATAL_ERROR "Missing Option \"LIB_INCLUDE\"")
endif()