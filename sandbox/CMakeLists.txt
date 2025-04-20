
add_executable(AxleSandbox "${PROJECT_SOURCE_DIR}/sandbox/src/sandbox.cpp")

foreach(config_type ${CMAKE_CONFIGURATION_TYPES})
  string(TOUPPER ${config_type} config_upper)
  set_target_properties(AxleSandbox
      PROPERTIES
      "ARCHIVE_OUTPUT_DIRECTORY_${config_upper}" "${PROJECT_BINARY_DIR}/out/sandbox"
      "LIBRARY_OUTPUT_DIRECTORY_${config_upper}" "${PROJECT_BINARY_DIR}/out/sandbox"
      "RUNTIME_OUTPUT_DIRECTORY_${config_upper}" "${PROJECT_BINARY_DIR}/out/sandbox"
  )
endforeach()

option(AxleTRACING "Enable tracing" OFF)
if(AxleTRACING) 
  if(NOT DEFINED TracerINCLUDE)
    message(FATAL_ERROR "Require TracerINCLUDE (tracer include path) to be provided, even if not tracing")
  endif()


  if(NOT DEFINED TracerBIN)
    message(FATAL_ERROR "Require TracerBIN (tracer include path) to be provided when tracing")
  endif()

  target_compile_definitions(AxleSandbox PRIVATE AXLE_TRACING_ENABLE)
  target_include_directories(AxleSandbox PRIVATE  ${TracerINCLUDE})
  target_link_libraries(AxleSandbox "${TracerBIN}/Tracer.lib")
endif()

target_include_directories(AxleSandbox PRIVATE "${PROJECT_SOURCE_DIR}/sandbox/src")
target_link_libraries(AxleSandbox AxleCompiler AxleUtil::Core)
