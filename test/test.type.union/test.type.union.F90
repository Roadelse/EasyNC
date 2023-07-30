program main
    use rdee_fortran
    use EasyNC
    implicit none

#include "easync.struct-io-interface.t1.inc"

    character(*), parameter :: fname = 'test.type.union.nc'

    integer,parameter :: ii = 3
    type :: t1
      integer :: is1, ia1(2)
      integer, allocatable :: iaa1(:)
    end type

    type(t1) :: t1_inst, t1Inst_1d(3)

    t1_inst%is1 = 2
    t1_inst%ia1 = [1,3]
    allocate(t1_inst%iaa1, source=[3,4,5,6])

    t1Inst_1d(1) = t1_inst
    t1Inst_1d(2) = t1_inst
    t1Inst_1d(3) = t1_inst

    call easyO_t1_scalar(fname, 't1_inst', t1_inst)
    call easyO_t1_1d(fname, 't1Inst_1d', t1Inst_1d)
    

contains

#include "easync.struct-io.t1.inc"

end