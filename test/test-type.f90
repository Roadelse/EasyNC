program writeTest
! use netcdf
use EasyNC

implicit none
type :: t1
    integer :: isc1, iarr1(3), iarr2(2,3)
    integer :: isc2, iarr3(3), iarr4(2,3)

    real(kind=4) :: fsc1, farr1(3), farr2(2,3)
    real(kind=4) :: fsc2, farr3(3), farr4(2,3)

    real(kind=8) :: dsc1, darr1(3), darr2(2,3)
    real(kind=8) :: dsc2, darr3(3), darr4(2,3)
end type

integer :: i, j, k ,l, m, n
type(t1) :: i_t1(2), i_t0
type(t1) :: i_t2(2, 2)
integer :: iarrTemp(2,3) = reshape([1,2,3,4,5,6], [2,3])
real(kind=4) :: farrTemp(2,3) = reshape([1.,2.,3.,4.,5.,6.], [2,3])
real(kind=8) :: darrTemp(2,3) = reshape([1.d0,2.d0,3.d0,4.d0,5.d0,6.d0], [2,3])

i_t0%iarr2 = iarrTemp
i_t0%farr2 = farrTemp
i_t0%darr2 = darrTemp
i_t0%iarr1 = [9, 8, 7]
i_t0%farr1 = [9., 8., 7.]
i_t0%darr1 = [9d0, 8d0, 7d0]
i_t0%isc1 = 2
i_t0%fsc1 = 2.
i_t0%dsc1 = 2d0
do i = 1, 2
do j = 1, 2
    i_t2(i, j)%iarr2 = iarrTemp + i + j
    i_t2(i, j)%farr2 = farrTemp + i + j
    i_t2(i, j)%darr2 = darrTemp + i + j
    i_t2(i, j)%iarr1 = [9, 8, 7]
    i_t2(i, j)%farr1 = [9., 8., 7.]
    i_t2(i, j)%darr1 = [9d0, 8d0, 7d0]
    i_t2(i, j)%isc1 = 2 + i + j
    i_t2(i, j)%fsc1 = 2. + i + j
    i_t2(i, j)%dsc1 = 2d0 + i + j
end do
    i_t1(i)%iarr2 = iarrTemp + i
    i_t1(i)%farr2 = farrTemp + i
    i_t1(i)%darr2 = darrTemp + i
    i_t1(i)%iarr1 = [9, 8, 7]
    i_t1(i)%farr1 = [9., 8., 7.]
    i_t1(i)%darr1 = [9d0, 8d0, 7d0]
    i_t1(i)%isc1 = 2 + i
    i_t1(i)%fsc1 = 2. + i
    i_t1(i)%dsc1 = 2d0 + i
end do

! >>>>>>>>>>>>>>>>>>>>>> test type- varaiables in different shapes of type-arr
call easyO('testT.nc', 'i_t0.isc1', i_t0%isc1)
call easyO('testT.nc', 'i_t0.fsc1', i_t0%fsc1)
call easyO('testT.nc', 'i_t0.dsc1', i_t0%dsc1)
call easyO('testT.nc', 'i_t0.iarr1', i_t0%iarr1)
call easyO('testT.nc', 'i_t0.farr1', i_t0%farr1)
call easyO('testT.nc', 'i_t0.darr1', i_t0%darr1)
call easyO('testT.nc', 'i_t0.iarr2', i_t0%iarr2)
call easyO('testT.nc', 'i_t0.farr2', i_t0%farr2)
call easyO('testT.nc', 'i_t0.darr2', i_t0%darr2)
! =============== check easyI anc compare
call easyI('testT.nc', 'i_t0.isc1', i_t0%isc2)
call easyI('testT.nc', 'i_t0.fsc1', i_t0%fsc2)
call easyI('testT.nc', 'i_t0.dsc1', i_t0%dsc2)
! print *, '(test-eastI-scalar) i_t0%isc1=',i_t0%isc1,', i_t0%isc2=',i_t0%isc2
! print *, '(test-eastI-scalar) i_t0%fsc1=',i_t0%fsc1,', i_t0%fsc2=',i_t0%fsc2
! print *, '(test-eastI-scalar) i_t0%dsc1=',i_t0%dsc1,', i_t0%dsc2=',i_t0%dsc2
if (i_t0%isc1 .ne. i_t0%isc2) stop 1
if (i_t0%fsc1 .ne. i_t0%fsc2) stop 2
if (i_t0%dsc1 .ne. i_t0%dsc2) stop 3

call easyI('testT.nc', 'i_t0.iarr1', i_t0%iarr3)
call easyI('testT.nc', 'i_t0.farr1', i_t0%farr3)
call easyI('testT.nc', 'i_t0.darr1', i_t0%darr3)
! print *, '(test-eastI-1darr) all eq? : ', all(i_t0%iarr1 .eq. i_t0%iarr3)
! print *, '(test-eastI-1darr) all eq? : ', all(i_t0%farr1 .eq. i_t0%farr3)
! print *, '(test-eastI-1darr) all eq? : ', all(i_t0%darr1 .eq. i_t0%darr3)
if (any(i_t0%iarr1 .ne. i_t0%iarr3)) stop 4
if (any(i_t0%farr1 .ne. i_t0%farr3)) stop 5
if (any(i_t0%darr1 .ne. i_t0%darr3)) stop 6

call easyI('testT.nc', 'i_t0.iarr2', i_t0%iarr4)
call easyI('testT.nc', 'i_t0.farr2', i_t0%farr4)
call easyI('testT.nc', 'i_t0.darr2', i_t0%darr4)
! print *, '(test-eastI-2darr) all eq? : ', all(i_t0%iarr2 .eq. i_t0%iarr4)
! print *, '(test-eastI-2darr) all eq? : ', all(i_t0%farr2 .eq. i_t0%farr4)
! print *, '(test-eastI-2darr) all eq? : ', all(i_t0%darr2 .eq. i_t0%darr4)
if (any(i_t0%iarr2 .ne. i_t0%iarr4)) stop 7
if (any(i_t0%farr2 .ne. i_t0%farr4)) stop 8
if (any(i_t0%darr2 .ne. i_t0%darr4)) stop 9


print *, 'size(shape(i_t1)) = ', size(shape(i_t1))
do k = 1, 2
    call easyO('testT.nc', 'i_t1%isc1', i_t1(k)%isc1, reshape([2, k], [2, 1], order=[2,1]))
    ! print *, '333'
    call easyO('testT.nc', 'i_t1%fsc1', i_t1(k)%fsc1, reshape([2, k], [2, 1], order=[2,1]))
    call easyO('testT.nc', 'i_t1%dsc1', i_t1(k)%dsc1, reshape([2, k], [2, 1], order=[2,1]))
    ! print *, '22222222222222222222'

    call easyO('testT.nc', 'i_t1%iarr1', i_t1(k)%iarr1, reshape([2,k], [2,size(shape(i_t1))], order=[2,1]))
    call easyO('testT.nc', 'i_t1%farr1', i_t1(k)%farr1, reshape([2,k], [2,size(shape(i_t1))], order=[2,1]))
    call easyO('testT.nc', 'i_t1%darr1', i_t1(k)%darr1, reshape([2,k], [2,size(shape(i_t1))], order=[2,1]))
    call easyO('testT.nc', 'i_t1%iarr2', i_t1(k)%iarr2, reshape([2,k], [2,size(shape(i_t1))], order=[2,1]))
    call easyO('testT.nc', 'i_t1%farr2', i_t1(k)%farr2, reshape([2,k], [2,size(shape(i_t1))], order=[2,1]))
    call easyO('testT.nc', 'i_t1%darr2', i_t1(k)%darr2, reshape([2,k], [2,size(shape(i_t1))], order=[2,1]))
    print *, '11111111111111111111111'
! ================ easy I
    call easyI('testT.nc', 'i_t1%isc1', i_t1(k)%isc2, reshape([2, k], [2, 1], order=[2,1]))
    call easyI('testT.nc', 'i_t1%fsc1', i_t1(k)%fsc2, reshape([2, k], [2, 1], order=[2,1]))
    call easyI('testT.nc', 'i_t1%dsc1', i_t1(k)%dsc2, reshape([2, k], [2, 1], order=[2,1]))
    call easyI('testT.nc', 'i_t1%iarr1', i_t1(k)%iarr3, reshape([2,k], [2,size(shape(i_t1))], order=[2,1]))
    call easyI('testT.nc', 'i_t1%farr1', i_t1(k)%farr3, reshape([2,k], [2,size(shape(i_t1))], order=[2,1]))
    call easyI('testT.nc', 'i_t1%darr1', i_t1(k)%darr3, reshape([2,k], [2,size(shape(i_t1))], order=[2,1]))
    call easyI('testT.nc', 'i_t1%iarr2', i_t1(k)%iarr4, reshape([2,k], [2,size(shape(i_t1))], order=[2,1]))
    call easyI('testT.nc', 'i_t1%farr2', i_t1(k)%farr4, reshape([2,k], [2,size(shape(i_t1))], order=[2,1]))
    call easyI('testT.nc', 'i_t1%darr2', i_t1(k)%darr4, reshape([2,k], [2,size(shape(i_t1))], order=[2,1]))


    if (i_t1(k)%isc1 .ne. i_t1(k)%isc2) stop 998
    if (i_t1(k)%fsc1 .ne. i_t1(k)%fsc2) stop 998
    if (i_t1(k)%dsc1 .ne. i_t1(k)%dsc2) stop 998

    if (any(i_t1(k)%iarr1 .ne. i_t1(k)%iarr3)) stop 998
    if (any(i_t1(k)%farr1 .ne. i_t1(k)%farr3)) stop 998
    if (any(i_t1(k)%darr1 .ne. i_t1(k)%darr3)) stop 998

    if (any(i_t1(k)%iarr2 .ne. i_t1(k)%iarr4)) stop 998
    if (any(i_t1(k)%farr2 .ne. i_t1(k)%farr4)) stop 998
    if (any(i_t1(k)%darr2 .ne. i_t1(k)%darr4)) stop 998


end do


! print *, 'size(shape(i_t2)) = ', size(shape(i_t2))
do i = 1,2
do j = 1,2
    call easyO('testT.nc', 'i_t2%isc1', i_t2(i, j)%isc1, reshape([2, 2, i, j], [2, size(shape(i_t2))], order=[2,1]))
    call easyO('testT.nc', 'i_t2%fsc1', i_t2(i, j)%fsc1, reshape([2, 2, i, j], [2, size(shape(i_t2))], order=[2,1]))
    call easyO('testT.nc', 'i_t2%dsc1', i_t2(i, j)%dsc1, reshape([2, 2, i, j], [2, size(shape(i_t2))], order=[2,1]))
    call easyO('testT.nc', 'i_t2%iarr1', i_t2(i, j)%iarr1, reshape([2, 2, i, j], [2,size(shape(i_t2))], order=[2,1]))
    call easyO('testT.nc', 'i_t2%farr1', i_t2(i, j)%farr1, reshape([2, 2, i, j], [2,size(shape(i_t2))], order=[2,1]))
    call easyO('testT.nc', 'i_t2%darr1', i_t2(i, j)%darr1, reshape([2, 2, i, j], [2,size(shape(i_t2))], order=[2,1]))
    call easyO('testT.nc', 'i_t2%iarr2', i_t2(i, j)%iarr2, reshape([2, 2, i, j], [2,size(shape(i_t2))], order=[2,1]))
    call easyO('testT.nc', 'i_t2%farr2', i_t2(i, j)%farr2, reshape([2, 2, i, j], [2,size(shape(i_t2))], order=[2,1]))
    call easyO('testT.nc', 'i_t2%darr2', i_t2(i, j)%darr2, reshape([2, 2, i, j], [2,size(shape(i_t2))], order=[2,1]))


    call easyI('testT.nc', 'i_t2%isc1', i_t2(i, j)%isc2, reshape([2, 2, i, j], [2, size(shape(i_t2))], order=[2,1]))
    call easyI('testT.nc', 'i_t2%fsc1', i_t2(i, j)%fsc2, reshape([2, 2, i, j], [2, size(shape(i_t2))], order=[2,1]))
    call easyI('testT.nc', 'i_t2%dsc1', i_t2(i, j)%dsc2, reshape([2, 2, i, j], [2, size(shape(i_t2))], order=[2,1]))
    call easyI('testT.nc', 'i_t2%iarr1', i_t2(i, j)%iarr3, reshape([2, 2, i, j], [2,size(shape(i_t2))], order=[2,1]))
    call easyI('testT.nc', 'i_t2%farr1', i_t2(i, j)%farr3, reshape([2, 2, i, j], [2,size(shape(i_t2))], order=[2,1]))
    call easyI('testT.nc', 'i_t2%darr1', i_t2(i, j)%darr3, reshape([2, 2, i, j], [2,size(shape(i_t2))], order=[2,1]))
    call easyI('testT.nc', 'i_t2%iarr2', i_t2(i, j)%iarr4, reshape([2, 2, i, j], [2,size(shape(i_t2))], order=[2,1]))
    call easyI('testT.nc', 'i_t2%farr2', i_t2(i, j)%farr4, reshape([2, 2, i, j], [2,size(shape(i_t2))], order=[2,1]))
    call easyI('testT.nc', 'i_t2%darr2', i_t2(i, j)%darr4, reshape([2, 2, i, j], [2,size(shape(i_t2))], order=[2,1]))


    if (i_t2(i,j)%isc1 .ne. i_t2(i,j)%isc2) stop 997
    if (i_t2(i,j)%fsc1 .ne. i_t2(i,j)%fsc2) stop 997
    if (i_t2(i,j)%dsc1 .ne. i_t2(i,j)%dsc2) stop 997

    if (any(i_t2(i,j)%iarr1 .ne. i_t2(i,j)%iarr3)) stop 997
    if (any(i_t2(i,j)%farr1 .ne. i_t2(i,j)%farr3)) stop 997
    if (any(i_t2(i,j)%darr1 .ne. i_t2(i,j)%darr3)) stop 997

    if (any(i_t2(i,j)%iarr2 .ne. i_t2(i,j)%iarr4)) stop 997
    if (any(i_t2(i,j)%farr2 .ne. i_t2(i,j)%farr4)) stop 997
    if (any(i_t2(i,j)%darr2 .ne. i_t2(i,j)%darr4)) stop 997

end do
end do



end program writeTest
