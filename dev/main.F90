program main
  ! use netcdf
  use easync
  implicit none

  real(kind=4) :: faa1(4)
  real(kind=4) :: faa2(2)
  real(kind=4) :: fs1


  faa1 = [1., 2., 3., 4.]
  faa2 = [5.,6.]

  call easyO('test.nc', 'ra1', faa1)

  call easyO('test.nc', 'ra2', faa2, [2,2], [1,1], [1,2])
  call easyO('test.nc', 'ra3', faa2, [3,3,3], [2,2,1], [1,2,1])


  call easyI('test.nc', 'ra3', fs1, [3,3,3], [2,2,1])
  print *, 'fs1 = ', fs1

end program
