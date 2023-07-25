program main
    use EasyNC
    implicit none

    character(*), parameter :: fname = 'test.complex.nc'

contains
    subroutine test_basic()
        Complex(kind=4) :: x1 = (1, 2), x1_
        Complex(kind=8) :: xa1(2), xa1_(2)
        Complex(kind=8), allocatable :: xa2(:,:), xa2_(:,:)

        type :: t1
            complex(kind=4) :: x1
            complex(kind=8) :: xa1(2)
            complex(kind=8),allocatable :: xa2(:,:)
        end type

        type(t1) :: tv1, tva1(2)

        xa1(1) = (3,4d0)
        xa1(2) = (4d0, 5)
        allocate(xa2(2,2))
        xa2(1,1) = (1.2, 3.4d0)
        xa2(1,2) = (5.6, 7.8)
        xa2(2,1) = (9.10, 10.11)
        xa2(2,2) = (12.13, 13.14)

        tva1(1)%xa1 = (1.3, 2.4)
        ! allocate(tv1%xa2(2))
        ! allocate(tva1(1)%xa2(2))
        ! allocate(tva1(2)%xa2(2))

        call easyO(fname, 'x1', x1)
        call easyI(fname, 'x1', x1_)

        if (x1 .ne. x1_) stop 1

        call easyO(fname, 'xa1', xa1)
        call easyI(fname, 'xa1', xa1_)
        if (any(xa1 .ne. xa1_)) stop 2

        call easyOA(fname, 'xa2', xa2)
        print *, 'cp1'
        call easyIA(fname, 'xa2', xa2_)
        ! print *, xa2
        ! print *, xa2_
        if (any(xa2 .ne. xa2_)) stop 3

        call easyO(fname, 'tva1(1)%x1', tva1(1)%x1)
        call easyI(fname, 'tva1(1)%x1', x1)
        call assert(x1 .eq. tva1(1)%x1, 'Error in EasyNC for complex, in a struct')
    end subroutine

end program