program writeTest
use netcdf
use EasyNC

implicit none

integer :: testI = 1, testO = 0

type :: t1
    integer :: ia
    integer :: ib(2,2)
    real*4,allocatable :: kol(:,:)
    character(80) :: ss(2)
    character(80),allocatable :: yvj(:)
end type

integer :: i,j,k
integer :: ia1(3:6, 4:5)
integer,allocatable :: ddd(:)

type(t1),allocatable :: t1s(:)
allocate(t1s(2))
allocate(t1s(1)%kol(2,3)) ; allocate(t1s(2)%kol(3,4))
allocate(t1s(1)%yvj(2))   ; allocate(t1s(2)%yvj(3))


if (testO .eq. 1) then
 t1s(1)%ia = 1
 t1s(2)%ia = 2
 t1s(1)%ib = reshape([1,2,3,4], [2,2])  ;  t1s(2)%ib = reshape([5,6,7,8], [2,2])
 t1s(1)%kol = reshape([1,2,3,4,5,6], [2,3])  ; t1s(2)%kol = reshape([1,2,3,4,5,6,7,8,9,10,11,12], [3,4])
 t1s(1)%yvj(1) = "xcbha" ; t1s(1)%yvj(2) = "qjwdnqj"
 t1s(2)%yvj(1) = "00d8sc2" ; t1s(2)%yvj(2) = "12vd" ; t1s(2)%yvj(3) = "cnjda"
 t1s(1)%ss(1) = "cnakq"  ;  t1s(1)%ss(2) = "812x"  
 t1s(2)%ss(1) = "921yhb"  ;  t1s(2)%ss(2) = "jas556+"  

 do k = 1, 2
     call easyO('testTA.nc', 't1s%ia', t1s(k)%ia, reshape([2, k], [2, 1], order = [2,1]))
     call easyO('testTA.nc', 't1s%ib', t1s(k)%ib, reshape([2, k], [2, 1], order = [2,1]))
     call easyO('testTA.nc', 't1s%kol', t1s(k)%kol, reshape([2, k], [2, 1], order = [2,1]), 1)
     call easyO('testTA.nc', 't1s%ss', t1s(k)%ss, reshape([2, k], [2, 1], order = [2,1]))
     call easyO('testTA.nc', 't1s%yvj', t1s(k)%yvj, reshape([2, k], [2, 1], order = [2,1]), 1)
 end do

 !call easyO('testTA.nc', 'ia1.bounds', reshape([lbound(ia1), ubound(ia1)], [2, size(shape(ia1))], order=[2,1]))
 call easyO('testTA.nc', 'A%b(1,2)%c', reshape([lbound(ia1), ubound(ia1)], [2, size(shape(ia1))], order=[2,1]))

 end if


if (testI .eq. 1) then
call getDims3_enc('testTA.nc', 't1s', ddd)
print *, ddd
do k = 1, 2
    call easyI('testTA.nc', 't1s%ia', t1s(k)%ia, reshape([2, k], [2, 1], order = [2,1]))
    call easyI('testTA.nc', 't1s%ib', t1s(k)%ib, reshape([2, k], [2, 1], order = [2,1]))
    call easyI('testTA.nc', 't1s%kol', t1s(k)%kol, reshape([2, k], [2, 1], order = [2,1]))
    call easyI('testTA.nc', 't1s%ss', t1s(k)%ss, reshape([2, k], [2, 1], order = [2,1]))
    call easyI('testTA.nc', 't1s%yvj', t1s(k)%yvj, reshape([2, k], [2, 1], order = [2,1]))
end do
print *, t1s(1)%ia
print *, t1s(1)%ib
print *, t1s(1)%kol
print *, t1s(2)%ss
print *, t1s(2)%yvj
end if

end program writeTest
