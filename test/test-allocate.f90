program writeTest
! use netcdf
use EasyNC

implicit none

character(*),parameter :: FNAME = "writeTest.nc"

Integer,Parameter :: NDIMS = 2, NX = 3, NY = 4

Integer :: i, j, k, l
real(4) :: r1
real*8  :: d2
! Integer :: ncid, varid, dimids(NDIMS)
! integer :: xid, yid, vid_int1, vid_real1, vid_dble1

Integer :: valInt1(NY, NX), valInt2(NY, NX)
! Real :: valReal1(NY, NX), valReal2(NY, NX)
! Real*8 :: valDble1(NY, NX), valDble2(NY, NX)
integer :: iarr1(3)
integer,allocatable :: iarr2(:,:)
integer :: kk(1)


! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> gen data
! ====================== int
do j = 1, NY
do i = 1, NX
    valInt1(j, i) = i + j
    ! valReal1(j, i) = i * 1.5 + j * 1.5
    ! valDble1(j, i) = i * 1.5d0 + j * 1.5d0
end do
end do
iarr1 = [1,2,3]


! allocate(iarr2(3))

print *, allocated(iarr2)

call easyO('testA.nc', 'iarr1', iarr1)
call easyO('testA.nc', 'valInt1', valInt1)
! print *, getDims_enc('testA.nc', 'iarr1')
! print *, getDims_enc('testA.nc', 'valInt1')
! allocate(iarr2(4,3))
call easyIA('testA.nc', 'valInt1', iarr2)

if (any(iarr2 .ne. valInt1)) stop 1


end program writeTest
