
cmake_minimum_required(VERSION 3.10)

project(EasyNC VERSION 0.1 DESCRIPTION "This holds a Fortran module, supporting easily I/O for generic data type and polymorphic via f95")

enable_language(Fortran)


message("compiler=${CMAKE_Fortran_COMPILER}")

list(PREPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/modules)
include(rdee.python)
message("python3 is found ? : ${Python3_FOUND}")
message("python3 interpreter : ${Python3_EXECUTABLE}")
message(${Python3_Interpreter_FOUND})

check_python_lib(jinja2)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)


SET(SRC ${CMAKE_SOURCE_DIR}/src)


enable_testing()

add_subdirectory(src)
add_subdirectory(test)

