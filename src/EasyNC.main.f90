Module EasyNC

  use netcdf

  implicit none


  interface easyO
    module procedure easyO_real4_1d
    module procedure easyO_real4_2d
    module procedure easyO_real4_3d
    module procedure easyO_real4_4d
    module procedure easyO_real4_scalar
    
    module procedure easyO_real8_1d
    module procedure easyO_real8_2d
    module procedure easyO_real8_3d
    module procedure easyO_real8_4d
    module procedure easyO_real8_scalar

    module procedure easyO_int4_1d
    module procedure easyO_int4_2d
    module procedure easyO_int4_3d
    module procedure easyO_int4_4d
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
    module procedure easyI_real4_scalar
    
    module procedure easyI_real8_1d
    module procedure easyI_real8_2d
    module procedure easyI_real8_3d
    module procedure easyI_real8_4d
    module procedure easyI_real8_scalar

    module procedure easyI_int4_1d
    module procedure easyI_int4_2d
    module procedure easyI_int4_3d
    module procedure easyI_int4_4d
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
#include "easync.getDims.inc"


  subroutine check_enc(status, subfunc)
    integer, intent ( in) :: status
    character(*), intent(in) :: subfunc
    
    if(status /= 0) then 
      print *, trim(nf90_strerror(status))
      print *, "In ", subfunc
      stop "Stopped"
    end if
  end subroutine check_enc  
    
END MODULE