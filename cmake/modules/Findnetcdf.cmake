

find_path(
    netcdf_lib
    NAMES libnetcdf.a
    HINTS ${NETCDF}/lib /usr/lib/x86_64-linux-gnu
)
find_path(
    netcdf_include
    NAMES netcdf.h
    HINTS ${NETCDF}/include /usr/include
)

if (netcdf_lib and netcdf_include)
    set(netcdf_FOUND true)
    message(STATUS "find netcdf-c in ${netcdf_lib}")
else()
    message(FATAL_ERROR "cannot find netcdf!")
endif()