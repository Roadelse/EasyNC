program writeTest
! use netcdf
use EasyNC

    character(*), parameter :: fname = 'test.string.nc'

    call test_basic
    call test_AP


contains
    subroutine test_basic()
        implicit none

        character(20) :: ss1, sa1(2), sa2(2)
        character(20) :: ss2, ss3

        ss1 = 'hello'
        sa1(1) = 'what?'
        sa1(2) = "how about that"

        call easyO(fname, 'ss1', ss1)
        call easyI(fname, 'ss1', ss2)
        print *, ss2
        call assert(trim(ss1) .eq. trim(ss2), 'Error in rdee.string, easyIO for scalar string')

        call easyO(fname, 'sa1', sa1)
        call easyI(fname, 'sa1', sa2)
        print *, sa2
        call assert(all(sa1 .eq. sa2), 'Error in rdee.string, easyIO for 1-d string')

        call easyI(fname, 'sa1', ss3, position = [2])
        print *, ss3
        call assert(ss3 .eq. 'how about that', 'Error in rdee.string, easyIO for subV')

    end subroutine

    subroutine test_AP()
        implicit none

        character(20), allocatable :: saa1(:)
        character(20), pointer :: sap1(:)

        character(20) :: ss1, sa1(2), sa2(2)
        character(20) :: ss2, ss3

        sap1 => null()

        ss1 = 'hello'
        sa1(1) = 'what?'
        sa1(2) = "how about that"

        call easyO(fname, 'ss1', ss1)
        call easyO(fname, 'sa1', sa1)

        call easyIA(fname, 'sa1', saa1)
        call easyIP(fname, 'sa1', sap1)
        print *, saa1
        print *, sap1
        call assert(all(saa1 .eq. sa1), 'Error in rdee.string, easyIO+A for 1-d string')
        call assert(all(sap1 .eq. sa1), 'Error in rdee.string, easyIO+A for 1-d string')
        deallocate(saa1)
        deallocate(sap1)

    end subroutine


end program


! implicit none
! type :: t1
!     character(10) :: s1, s2(2), s3(2, 3)
! end type

! integer :: i, j, k
! character(10) :: s4, s5(2), s6(3,2)
! character(10) :: s4I, s5I(2), s6I(3,2)
! type(t1) :: t1i, t2i(2)

! s4 = "12345678"
! s5(1) = "92478"
! s5(2) = "814"

! t1i%s1 = "xcd2"
! t1i%s2(1) = "98vchj"
! t1i%s2(2) = "ka9c"

! t2i(1)%s1 = "m,aix"
! t2i(2)%s1 = "vh238"
! t2i(1)%s2(1) = "c82jh"
! t2i(1)%s2(2) = "c82h"
! t2i(2)%s2(1) = "1nc8"
! t2i(2)%s2(2) = ",mdfy"


! do i = 1, 3
! do j = 1, 2
!     s6(i, j) = "5132"
! end do
! end do

! call easyO('testC.nc', 's4', s4)
! call easyI('testC.nc', 's4', s4I)
! ! print *, s4
! ! print *, s4I
! if (s4 .ne. s4I) stop 1

! ! test 1-dimensional character array
! call easyO('testC.nc', 's5', s5)
! call easyI('testC.nc', 's5', s5I)
! ! print *, s5(1)
! ! print *, s5(2)
! if (any(s5 .ne. s5I)) stop 2

! ! test 2-dimensional character array
!  call easyO('testC.nc', 's6', s6)
!  call easyI('testC.nc', 's6', s6I)
! !  print *, s6(1, :)
! !  print *, s6(2, :)
!  if (any(s6 .ne. s6I)) stop 3


! ! >>>>>>>>>>>>>>>>>> test in type
! call easyO('testC.nc', 't1i%s1', t1i%s2)
! call easyI('testC.nc', 't1i%s1', s5I)
! if (any(t1i%s2 .ne. s5I)) stop 4


! call easyO('testC.nc', 't2i%s1', t2i(2)%s1, reshape([2, 1], [2, 1], order=[2,1]))
! call easyI('testC.nc', 't2i%s1', s4I, reshape([2, 1], [2, 1], order=[2,1]))
! ! print *, s4I
! if (t2i(2)%s1 .ne. s4I) stop 1

! call easyO('testC.nc', 't2i%s2', t2i(1)%s2, reshape([2, 1], [2, 1], order=[2,1]))
! call easyI('testC.nc', 't2i%s2', s5I, reshape([2, 1], [2, 1], order=[2,1]))
! ! print *, s5I
! if (any(t2i(1)%s2 .ne. s5I)) stop 4

! end program writeTest


