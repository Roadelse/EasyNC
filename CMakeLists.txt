cmake_minimum_required(VERSION 3.10)

project(EasyNC VERSION 2.0.2 DESCRIPTION "This holds a Fortran module, supporting easily I/O for generic data type and polymorphic via f95")

enable_language(Fortran)


message("compiler=${CMAKE_Fortran_COMPILER}")

list(PREPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/modules)
include(rdee.python)

check_python_lib(jinja2)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)


SET(SRC ${CMAKE_SOURCE_DIR}/src)
SET(TEST ${CMAKE_SOURCE_DIR}/test)


find_package(rdee_fortran)
find_package(netcdff)


enable_testing()

Message("¦ compiler id = ${CMAKE_Fortran_COMPILER_ID}")
if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
    add_compile_options(-O3 -fPIE -extend-source 132 "SHELL:-check all" "SHELL:-warn all" -std08 "SHELL:-warn nounused")
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
    add_compile_options(-O3 -fPIE -ffree-line-length-none -ffixed-line-length-132 -std=f2008 -Wall -fcheck=all -fall-intrinsics -Wno-unused -Wno-maybe-uninitialized)
endif()

add_subdirectory(src)
add_subdirectory(test)


