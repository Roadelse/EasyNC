

Message(STATUS "compiler id = ${CMAKE_Fortran_COMPILER_ID}")
if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
    add_compile_options(-g -fPIE -ffree-line-length-none "SHELL:-check all" "SHELL:-warn all" -std08 "SHELL:-warn nounused")
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
    add_compile_options(-g -fPIE -std=f2008 -Wall -fcheck=all -fall-intrinsics -Wno-unused)
endif()

message("preparing source EasyNC.F90 ... ...")
execute_process(COMMAND bash ${SRC}/prep/scripts/gen.sh)

file(GLOB SRCS *.F90 *.f90 *.F *.f)
message(STATUS "srcs = ${SRCS}")
add_library(EasyNC STATIC ${SRCS})

target_include_directories(EasyNC PRIVATE /usr/include)