
if(EXISTS /proc/sys/fs/binfmt_misc/WSLInterop)
    set(isWSL TRUE)
    return()
else()
    set(isWSL FALSE)
endif()

if 


find_program(SWGCC NAMES swgcc)
if (SWGCC)
    set(isSW TRUE)
    return()
else()
    set(isSW FALSE)
endif()


