program main
use m1
implicit none

character(*),parameter :: fname = 'test.type.union.nc'

type(s1) :: s1i_s_1, s1i_s_2
type(s1), allocatable :: s1i_aa_1(:)
type(s1), pointer :: s1i_ap_1(:) => null()



s1i_s_1%i1 = 2
allocate(s1i_s_1%dap1(2))
s1i_s_1%dap1 = [1.2d0, 3.4d0]
s1i_s_1%c1 = cmplx(1., 2.3)

s1i_s_2%i1 = 3
allocate(s1i_s_2%L1(3))
s1i_s_2%L1 = [.true., .false., .false.]


allocate(s1i_aa_1(2:3))
s1i_aa_1(2) = s1i_s_1
s1i_aa_1(3) = s1i_s_2


call easyO_s1(fname, 's1i_s_1', s1i_s_1)
call easyO_s1(fname, 's1i_s_2', s1i_s_2)
call easyOA_s1(fname, 's1i_aa_1', s1i_aa_1)

print *, allocated(enc_iaaT_1d)

call easyIP_s1(fname, 's1i_aa_1', s1i_ap_1)
print *, lbound(s1i_ap_1)
print *, s1i_ap_1(2)%i1, s1i_ap_1(2)%c1
call assert(s1i_ap_1(2)%i1 .eq. 2, 'Error in easyIP_s1 for int') 
call assert(s1i_ap_1(2)%c1 .eq. cmplx(1., 2.3), 'Error in easyIP_s1 for complex') 


end program

