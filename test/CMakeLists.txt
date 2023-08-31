
add_test(NAME setup
    COMMAND bash -c "rm -f ${CMAKE_BINARY_DIR}/test/*.nc"
)


function(ecsl_test testID)  # encapsulation of test
    add_executable(test.${testID} test.${testID}.F90)
    set_target_properties(test.${testID} PROPERTIES
        OUTPUT_NAME test.${testID}.x
        RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/test)
    target_link_libraries(test.${testID} EasyNC)
    target_compile_options(test.${testID} PUBLIC -g)
    
    if (inSW)
        add_test(
            NAME test.${testID}
            COMMAND bash -c "bsub -b -I -q q_sw_expr -n 1 -cgsp 64 -share_size 7000 -host_stack 512 $<TARGET_FILE:test.${testID}>"
        )
    else()
        add_test(
            NAME test.${testID}
            COMMAND $<TARGET_FILE:test.${testID}>
        )
    endif()

endfunction()

ecsl_test(numeric)
ecsl_test(string)
ecsl_test(logical)
ecsl_test(complex)
ecsl_test(type.direct)
ecsl_test(unlimited)
ecsl_test(dimnames)
ecsl_test(linked_list)


set_tests_properties(
    test.numeric test.string test.logical test.complex test.type.direct test.unlimited test.dimnames
    PROPERTIES FIXTURES_REQUIRED setup)


add_test(NAME post-check.dimnames
    COMMAND bash -c "cd ${CMAKE_BINARY_DIR}/test && bash post-check.dimnames.sh")
add_custom_command(TARGET test.dimnames POST_BUILD
    COMMAND bash -c "cp ${TEST}/post-check.dimnames.sh ${CMAKE_BINARY_DIR}/test/"
)

set_tests_properties(test.dimnames PROPERTIES FIXTURES_CLEANUP post-check.dimnames)
set_tests_properties(post-check.dimnames PROPERTIES ENVIRONMENT "whichCMD=$ENV{ENCT_NC_VIEW_CMD}")

add_subdirectory(test.type.union)
