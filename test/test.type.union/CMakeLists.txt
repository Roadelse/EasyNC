

# SET_SOURCE_FILES_PROPERTIES(m1.F PROPERTIES COMPILE_FLAGS -ffixed-line-length-132)
add_executable(test.type.union main.F90 m1.F)
set_target_properties(test.type.union PROPERTIES OUTPUT_NAME test.type.union.x RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/test)
target_link_libraries(test.type.union EasyNC rdee_fortran netcdff)
target_compile_options(test.type.union
    PUBLIC -g)

add_test(NAME test.type.union
      COMMAND $<TARGET_FILE:test.type.union>)

set_tests_properties(test.type.union PROPERTIES FIXTURES_REQUIRED setup)