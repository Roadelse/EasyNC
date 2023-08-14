

if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")

find_path(
    EasyNC_lib
    NAMES libEasyNC.a
    HINTS $ENV{EASYNC}/lib /mnt/d/recRoot/GitRepos/EasyNC/cbuild.intel/lib ~/recRoot/GitRepos/EasyNC/cbuild.intel/lib 
)
else()

find_path(
    EasyNC_lib
    NAMES libEasyNC.a
    HINTS $ENV{EASYNC}/lib /mnt/d/recRoot/GitRepos/EasyNC/cbuild.gnu/lib /mnt/d/recRoot/GitRepos/EasyNC/cbuild/lib ~/recRoot/GitRepos/EasyNC/cbuild.gnu/lib ~/recRoot/GitRepos/EasyNC/cbuild/lib 
)

endif()


if (EasyNC_lib)
    set(EasyNC_FOUND TRUE)
    message("find EasyNC in ${EasyNC_lib}")
    set(EasyNC_include ${EasyNC_lib}/../include)
else()
    message(FATAL_ERROR "cannot find EasyNC!")
endif()
