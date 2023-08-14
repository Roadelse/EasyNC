

if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")

find_path(
    rdee_fortran_lib
    NAMES librdee_fortran.a
    HINTS $ENV{RDEE_FORTRAN}/lib ${CMAKE_SOURCE_DIR}/../rdee_fortran/cbuild.intel/lib
)
else()

find_path(
    rdee_fortran_lib
    NAMES librdee_fortran.a
    HINTS $ENV{RDEE_FORTRAN}/lib ${CMAKE_SOURCE_DIR}/../rdee_fortran/cbuild.sw/lib ${CMAKE_SOURCE_DIR}/../../rdee_fortran/cbuild.gnu/lib
)

endif()


if (rdee_fortran_lib)
    set(rdee_fortran_FOUND TRUE)
    message("find rdee_fortran in ${rdee_fortran_lib}")
    set(rdee_fortran_include ${rdee_fortran_lib}/../include)
else()
    message(FATAL_ERROR "cannot find rdee_fortran!")
endif()
