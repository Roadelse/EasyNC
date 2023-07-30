



message("preparing source EasyNC.F90 ... ...")
execute_process(COMMAND bash ${SRC}/prep/scripts/gen.sh)

file(GLOB SRCS *.F90 *.f90 *.F *.f)
message(STATUS "srcs = ${SRCS}")
add_library(EasyNC STATIC ${SRCS})


target_include_directories(EasyNC PRIVATE ${netcdff_include} ${rdee_fortran_include})

add_custom_command(TARGET EasyNC POST_BUILD
    COMMAND bash -c "mkdir -p ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/render-struct-io"
    COMMAND bash -c "cp ${SRC}/prep/scripts/render_struct_io.py ${SRC}/prep/scripts/jj2_context.py ${SRC}/prep/jj2.post/easync.struct*jj2 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/render-struct-io"
)
