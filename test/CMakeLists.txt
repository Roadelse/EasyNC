
add_test(NAME setup
    COMMAND bash -c "rm -f ${CMAKE_BINARY_DIR}/test/*.nc"
)


link_directories(BEFORE ${rdee_fortran_lib} ${netcdff_lib})
include_directories(BEFORE ${rdee_fortran_include} ${netcdff_include})

add_executable(test.numeric test.numeric.F90 )
set_target_properties(test.numeric PROPERTIES OUTPUT_NAME test.numeric.x RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/test)
target_link_libraries(test.numeric EasyNC rdee_fortran netcdff)
#target_include_directories(test.numeric PRIVATE ${rdee_fortran_include})
#target_link_directories(test.numeric PRIVATE ${rdee_fortran_lib})
target_compile_options(test.numeric
    PUBLIC -g)
                
add_executable(test.string test.string.F90 )
set_target_properties(test.string PROPERTIES OUTPUT_NAME test.string.x RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/test)
target_link_libraries(test.string EasyNC rdee_fortran netcdff)
target_compile_options(test.string
    PUBLIC -g)

add_executable(test.logical test.logical.F90 )
set_target_properties(test.logical PROPERTIES OUTPUT_NAME test.logical.x RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/test)
target_link_libraries(test.logical EasyNC rdee_fortran netcdff)
target_compile_options(test.logical
    PUBLIC -g)

add_executable(test.complex test.complex.F90 )
set_target_properties(test.complex PROPERTIES OUTPUT_NAME test.complex.x RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/test)
target_link_libraries(test.complex EasyNC rdee_fortran netcdff)
target_compile_options(test.complex
    PUBLIC -g)


add_executable(test.type.direct test.type.direct.F90 )
set_target_properties(test.type.direct PROPERTIES OUTPUT_NAME test.type.direct.x RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/test)
target_link_libraries(test.type.direct EasyNC rdee_fortran netcdff)
target_compile_options(test.type.direct
    PUBLIC -g)

# add_executable(test.type.union test.type.union.F90 )
# set_target_properties(test.type.union PROPERTIES OUTPUT_NAME test.type.union.x RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/test)
# target_link_libraries(test.type.union EasyNC rdee_fortran netcdff)
# target_compile_options(test.type.union
#     PUBLIC -g)



add_test(NAME test.numeric
    COMMAND $<TARGET_FILE:test.numeric>)
add_test(NAME test.string
   COMMAND $<TARGET_FILE:test.string>)
add_test(NAME test.logical
   COMMAND $<TARGET_FILE:test.logical>)
add_test(NAME test.type.direct
   COMMAND $<TARGET_FILE:test.type.direct>)
# add_test(NAME test.type.union
    #   COMMAND $<TARGET_FILE:test.type.union>)
add_test(NAME test.complex
   COMMAND $<TARGET_FILE:test.complex>)


set_tests_properties(test.numeric test.string test.logical test.complex test.type.direct PROPERTIES FIXTURES_REQUIRED setup)

add_subdirectory(test.type.union)


add_executable(test.unlimited test.unlimited.F90 )
set_target_properties(test.unlimited PROPERTIES OUTPUT_NAME test.unlimited.x RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/test)
target_link_libraries(test.unlimited EasyNC rdee_fortran netcdff)
target_compile_options(test.unlimited
    PUBLIC -g)
add_test(NAME test.unlimited
    COMMAND $<TARGET_FILE:test.unlimited>)
set_tests_properties(test.unlimited PROPERTIES FIXTURES_REQUIRED setup)
