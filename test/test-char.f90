program writeTest
use netcdf
use EasyNC

implicit none
type :: t1
    character(10) :: s1, s2(2), s3(2, 3)
end type

integer :: i, j, k
character(10) :: s4, s5(2), s6(3,2)
character(10) :: s4I, s5I(2), s6I(3,2)
type(t1) :: t1i, t2i(2)

s4 = "12345678"
s5(1) = "92478"
s5(2) = "814"

t1i%s1 = "xcd2"
t1i%s2(1) = "98vchj"
t1i%s2(2) = "ka9c"

t2i(1)%s1 = "m,aix"
t2i(2)%s1 = "vh238"
t2i(1)%s2(1) = "c82jh"
t2i(1)%s2(2) = "c82h"
t2i(2)%s2(1) = "1nc8"
t2i(2)%s2(2) = ",mdfy"






do i = 1, 3
do j = 1, 2
    s6(i, j) = "5132"
end do
end do

! call easyO('testC.nc', 's4', s4)
! call easyI('testC.nc', 's4', s4I)
! print *, s4
! print *, s4I

! test 1-dimensional character array
! call easyO('testC.nc', 's5', s5)
! call easyI('testC.nc', 's5', s5I)
! print *, s5(1)
! print *, s5(2)

! test 2-dimensional character array
! call easyO('testC.nc', 's6', s6)
! call easyI('testC.nc', 's6', s6I)
! print *, s6(1, :)
! print *, s6(2, :)


! >>>>>>>>>>>>>>>>>> test in type
! call easyO('testC.nc', 't1i.s1', t1i%s2)
! call easyI('testC.nc', 't1i.s1', s5I)

! call easyO('testC.nc', 't2i.s1', t2i(2)%s1, reshape([2, 1], [2, 1], order=[2,1]))
! call easyI('testC.nc', 't2i.s1', s4I, reshape([2, 1], [2, 1], order=[2,1]))
! print *, s4I

call easyO('testC.nc', 't2i.s2', t2i(1)%s2, reshape([2, 1], [2, 1], order=[2,1]))
call easyI('testC.nc', 't2i.s2', s5I, reshape([2, 1], [2, 1], order=[2,1]))
print *, s5I

end program writeTest


