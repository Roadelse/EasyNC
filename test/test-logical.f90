program writeTest
! use netcdf
use EasyNC

implicit none

logical :: b, q
logical :: arrB(4), arrC(4)
logical, allocatable :: la1(:), la1_(:)

b = .false.
q = .true.
arrB = (/.true., .true., .false., .false./)

allocate(la1, source=arrB)

call easyO('testL.nc', 'b.d', b)
call easyI('testL.nc', 'b.d', q)

! print *, 'should be F: ', q
if (q) stop 1

call easyO('testL.nc', 'arrB', arrB)
call easyI('testL.nc', 'arrB', arrC)

! print *, 'should be TTFF: '
! print *, arrC
if (any(arrC .neqv. arrB)) stop 2


call easyOA('testL.nc', 'la1', la1)
call easyIA('testL.nc', 'la1', la1_)
print *, la1_
if (any(la1 .neqv. la1_)) stop 3

end program writeTest
