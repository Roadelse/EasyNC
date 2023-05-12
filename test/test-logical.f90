program writeTest
use netcdf
use EasyNC

implicit none

logical :: b, q
logical :: arrB(4), arrC(4)

b = .false.
q = .true.
arrB = (/.true., .true., .false., .false./)

call easyO('testL.nc', 'b.d', b)
call easyI('testL.nc', 'b.d', q)

print *, q

call easyO('testL.nc', 'arrB', arrB)
call easyI('testL.nc', 'arrB', arrC)

print *, arrC




end program writeTest
