
add_test(NAME setup
    COMMAND bash -c "rm -f ${CMAKE_BINARY_DIR}/test/*.nc"
)

add_executable(test-basic test-basic.F90 )
set_target_properties(test-basic PROPERTIES OUTPUT_NAME test-basic.x)
target_link_libraries(test-basic EasyNC netcdff)

add_executable(test-char test-char.F90 )
set_target_properties(test-char PROPERTIES OUTPUT_NAME test-char.x)
target_link_libraries(test-char EasyNC netcdff)

add_executable(test-logical test-logical.F90 )
set_target_properties(test-logical PROPERTIES OUTPUT_NAME test-logical.x)
target_link_libraries(test-logical EasyNC netcdff)

add_executable(test-type test-type.F90 )
set_target_properties(test-type PROPERTIES OUTPUT_NAME test-type.x)
target_link_libraries(test-type EasyNC netcdff)

add_executable(test-type-allocatable test-type-allocatable.F90 )
set_target_properties(test-type-allocatable PROPERTIES OUTPUT_NAME test-type-allocatable.x)
target_link_libraries(test-type-allocatable EasyNC netcdff)

add_executable(test-allocate test-allocate.F90 )
set_target_properties(test-allocate PROPERTIES OUTPUT_NAME test-allocate.x)
target_link_libraries(test-allocate EasyNC netcdff)

add_executable(test-allocatable-bounds test-allocatable-bounds.F90 )
set_target_properties(test-allocatable-bounds PROPERTIES OUTPUT_NAME test-allocatable-bounds.x)
target_link_libraries(test-allocatable-bounds EasyNC netcdff)

add_executable(test-complex test-complex.F90 )
set_target_properties(test-complex PROPERTIES OUTPUT_NAME test-complex.x)
target_link_libraries(test-complex EasyNC netcdff)


add_test(NAME test-basic
    COMMAND $<TARGET_FILE:test-basic>)
add_test(NAME test-char
    COMMAND $<TARGET_FILE:test-char>)
add_test(NAME test-logical
    COMMAND $<TARGET_FILE:test-logical>)
add_test(NAME test-type
    COMMAND $<TARGET_FILE:test-type>)
add_test(NAME test-type-allocatable
    COMMAND $<TARGET_FILE:test-type-allocatable>)
add_test(NAME test-allocate
        COMMAND $<TARGET_FILE:test-allocate>)
add_test(NAME test-allocatable-bounds
    COMMAND $<TARGET_FILE:test-allocatable-bounds>)
add_test(NAME test-complex
    COMMAND $<TARGET_FILE:test-complex>)


set_tests_properties(test-basic test-char test-logical test-type test-type-allocatable test-allocate test-allocatable-bounds test-complex PROPERTIES FIXTURES_REQUIRED setup)