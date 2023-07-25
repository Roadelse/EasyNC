program test_logical
! use netcdf
use EasyNC
    character(*), parameter :: fname = 'test.logical.nc'

contains

    subroutine test_basic()

        implicit none

        logical :: b, q
        logical :: arrB(4), arrC(4)
        logical, allocatable :: la1(:), la1_(:)

        b = .false.
        q = .true.
        arrB = (/.true., .true., .false., .false./)

        allocate(la1, source=arrB)

        call easyO(fname, 'b.d', b)
        call easyI(fname, 'b.d', q)

        ! print *, 'should be F: ', q
        if (q) stop 1

        call easyO(fname, 'arrB', arrB)
        call easyI(fname, 'arrB', arrC)

        ! print *, 'should be TTFF: '
        ! print *, arrC
        if (any(arrC .neqv. arrB)) stop 2


        call easyOA(fname, 'la1', la1)
        call easyIA(fname, 'la1', la1_)
        print *, la1_
        if (any(la1 .neqv. la1_)) stop 3
    end subroutine
end program 
