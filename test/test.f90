program writeTest
use netcdf
use EasyNC

implicit none

character(*),parameter :: FNAME = "writeTest.nc"

Integer,Parameter :: NDIMS = 2, NX = 3, NY = 4

Integer :: i, j, k, l
real(4) :: r1
real*8  :: d2
Integer :: ncid, varid, dimids(NDIMS)
integer :: xid, yid, vid_int1, vid_real1, vid_dble1

Integer :: valInt1(NY, NX), valInt2(NY, NX)
Real :: valReal1(NY, NX), valReal2(NY, NX)
Real*8 :: valDble1(NY, NX), valDble2(NY, NX)


! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> gen data
! ====================== int
do j = 1, NY
do i = 1, NX
    valInt1(j, i) = i + j
    valReal1(j, i) = i * 1.5 + j * 1.5
    valDble1(j, i) = i * 1.5d0 + j * 1.5d0
end do
end do

! print *, valInt1
! call easyO_int(valInt1)
! call easyO_int("test.nc", "valInt1", valInt1, shape(valInt1), (/"abc", "def"/))
! call easyO_real4("test.nc", "valReal1", valReal1, shape(valReal1), (/"abc2", "def2"/))
! call easyO_real8("test.nc", "valDble1", valDble1, shape(valDble1),  [Character(len=10) :: "abc", "def2"])
! call easyO_real8("test_double.nc", "valDble1", valDble1, shape(valDble1), (/"abc", "def"/))
! call easyO_real4_scalar("test.nc", "val1", 2.0)
! call easyO_real8_scalar("test.nc", "val2", 4.0d0)
! call easyO_int4_scalar("test.nc", "val3", 3)

call easyO("test.nc", "arr1", valInt1)
call easyO("test.nc", "arr2", valReal1)
call easyO("test.nc", "arr3", valDble1)
call easyO("test.nc", "val1", 2.0)
call easyO("test.nc", "val2", 3d0)
call easyO("test.nc", "val3", 4)



call easyI('test.nc', 'arr1', valInt2)
call easyI('test.nc', 'arr2', valReal2)
call easyI('test.nc', 'arr3', valDble2)
call easyI('test.nc', 'val1', r1)
call easyI('test.nc', 'val2', d2)
call easyI('test.nc', 'val3', i)
print *, valInt2
print *, valReal2
print *, valDble2
print *, r1
print *, d2
print *, i


end program writeTest