

find_path(
    netcdff_lib
    NAMES libnetcdff.a
    HINTS $ENV{NETCDFF}/lib /usr/lib/x86_64-linux-gnu
)
find_path(
    netcdff_include
    NAMES netcdf.mod
    HINTS $ENV{NETCDFF}/include /usr/include
)

if (netcdff_lib AND netcdff_include)
    set(netcdff_FOUND true)
    message(STATUS "find netcdf-fortran in ${netcdff_lib}")
    if (DEFINED ENV{NETCDFF_LINK_DIRECTORIES})
        set(NETCDFF_LINK_DIRECTORIES $ENV{NETCDFF_LINK_DIRECTORIES})
        set(NETCDFF_LINK_LIBS $ENV{NETCDFF_LINK_LIBS})
        set(NETCDFF_INC_DIRECTORIES $ENV{NETCDFF_LINK_DIRECTORIES})
        message(STATUS "find NETCDFF compilation information")
    endif()
else()
    message(FATAL_ERROR "cannot find netcdff!")
endif()
