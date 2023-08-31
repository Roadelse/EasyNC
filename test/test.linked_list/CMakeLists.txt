


add_executable(test.linked_list test.linked_list.F90)
set_target_properties(test.linked_list PROPERTIES OUTPUT_NAME test.linked_list.x RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/test)
target_link_libraries(test.linked_list EasyNC)
target_compile_options(test.linked_list
    PUBLIC -g)


if (inSW)
    add_test(
        NAME test.linked_list
        COMMAND bash -c "bsub -b -I -q q_sw_expr -n 1 -cgsp 64 -share_size 7000 -host_stack 512 $<TARGET_FILE:test.linked_list>"
    )
else()
    add_test(
        NAME test.linked_list
        COMMAND $<TARGET_FILE:test.linked_list>
    )
endif()

set_tests_properties(test.linked_list PROPERTIES FIXTURES_REQUIRED setup)