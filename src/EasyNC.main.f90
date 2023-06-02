Module EasyNC

  use netcdf

  implicit none

  integer,allocatable :: dimsT_enc(:), dimsT2_enc(:,:)
  integer :: enc_i, enc_j, enc_k
  character(80) :: varname_enc, strT_enc

  interface easyO
    module procedure easyO_real4_1d
    module procedure easyO_real4_2d
    module procedure easyO_real4_3d
    module procedure easyO_real4_4d
    module procedure easyO_real4_5d
    module procedure easyO_real4_6d
    module procedure easyO_real4_scalar
    
    module procedure easyO_real8_1d
    module procedure easyO_real8_2d
    module procedure easyO_real8_3d
    module procedure easyO_real8_4d
    module procedure easyO_real8_5d
    module procedure easyO_real8_6d
    module procedure easyO_real8_scalar

    module procedure easyO_int4_1d
    module procedure easyO_int4_2d
    module procedure easyO_int4_3d
    module procedure easyO_int4_4d
    module procedure easyO_int4_5d
    module procedure easyO_int4_6d
    module procedure easyO_int4_scalar

    module procedure easyO_logical_1d
    module procedure easyO_logical_2d
    module procedure easyO_logical_3d
    module procedure easyO_logical_4d
    module procedure easyO_logical_scalar

    module procedure easyO_char
    module procedure easyO_char_1d
    module procedure easyO_char_2d
    module procedure easyO_char_3d
    module procedure easyO_char_4d
  end interface

  interface easyI
    module procedure easyI_real4_1d
    module procedure easyI_real4_2d
    module procedure easyI_real4_3d
    module procedure easyI_real4_4d
    module procedure easyI_real4_5d
    module procedure easyI_real4_6d
    module procedure easyI_real4_scalar
    
    module procedure easyI_real8_1d
    module procedure easyI_real8_2d
    module procedure easyI_real8_3d
    module procedure easyI_real8_4d
    module procedure easyI_real8_5d
    module procedure easyI_real8_6d
    module procedure easyI_real8_scalar

    module procedure easyI_int4_1d
    module procedure easyI_int4_2d
    module procedure easyI_int4_3d
    module procedure easyI_int4_4d
    module procedure easyI_int4_5d
    module procedure easyI_int4_6d
    module procedure easyI_int4_scalar

    module procedure easyI_logical_1d
    module procedure easyI_logical_2d
    module procedure easyI_logical_3d
    module procedure easyI_logical_4d
    module procedure easyI_logical_scalar

    module procedure easyI_char
    module procedure easyI_char_1d
    module procedure easyI_char_2d
    module procedure easyI_char_3d
    module procedure easyI_char_4d
  end interface

  Contains


#include "easync.real4.inc"

#include "easync.real8.inc"

#include "easync.int4.inc"

#include "easync.logical.inc"

#include "easync.char.inc"

#include "easync.funcs.inc"


  subroutine check_enc(status, errInfo)
    integer, intent ( in) :: status
    character(*), intent(in) :: errInfo
    
    if(status /= 0) then 
      print *, trim(nf90_strerror(status))
      print *, errInfo
      stop "Stopped"
    end if
  end subroutine

    
END MODULE