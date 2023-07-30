

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
else()
    message(FATAL_ERROR "cannot find netcdff!")
endif()
