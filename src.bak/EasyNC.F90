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
    module procedure easyO_real4_7d
    module procedure easyO_real4_scalar
  
    module procedure easyO_real8_1d
    module procedure easyO_real8_2d
    module procedure easyO_real8_3d
    module procedure easyO_real8_4d
    module procedure easyO_real8_5d
    module procedure easyO_real8_6d
    module procedure easyO_real8_7d
    module procedure easyO_real8_scalar
    module procedure easyO_int4_1d
    module procedure easyO_int4_2d
    module procedure easyO_int4_3d
    module procedure easyO_int4_4d
    module procedure easyO_int4_5d
    module procedure easyO_int4_6d
    module procedure easyO_int4_7d
    module procedure easyO_int4_scalar
    module procedure easyO_logical_1d
    module procedure easyO_logical_2d
    module procedure easyO_logical_3d
    module procedure easyO_logical_4d
    module procedure easyO_logical_5d
    module procedure easyO_logical_6d
    module procedure easyO_logical_7d
    module procedure easyO_logical_scalar
    module procedure easyO_char_1d
    module procedure easyO_char_2d
    module procedure easyO_char_3d
    module procedure easyO_char_4d
    module procedure easyO_char_5d
    module procedure easyO_char_6d
    module procedure easyO_char
    module procedure easyO_complex4_1d
    module procedure easyO_complex4_2d
    module procedure easyO_complex4_3d
    module procedure easyO_complex4_4d
    module procedure easyO_complex4_5d
    module procedure easyO_complex4_6d
    module procedure easyO_complex4_7d
    module procedure easyO_complex4_scalar
    
    module procedure easyO_complex8_1d
    module procedure easyO_complex8_2d
    module procedure easyO_complex8_3d
    module procedure easyO_complex8_4d
    module procedure easyO_complex8_5d
    module procedure easyO_complex8_6d
    module procedure easyO_complex8_7d
    module procedure easyO_complex8_scalar
  end interface
  interface easyI
    module procedure easyI_real4_1d
    module procedure easyI_real4_2d
    module procedure easyI_real4_3d
    module procedure easyI_real4_4d
    module procedure easyI_real4_5d
    module procedure easyI_real4_6d
    module procedure easyI_real4_7d
    module procedure easyI_real4_scalar
  
    module procedure easyI_real8_1d
    module procedure easyI_real8_2d
    module procedure easyI_real8_3d
    module procedure easyI_real8_4d
    module procedure easyI_real8_5d
    module procedure easyI_real8_6d
    module procedure easyI_real8_7d
    module procedure easyI_real8_scalar
    module procedure easyI_int4_1d
    module procedure easyI_int4_2d
    module procedure easyI_int4_3d
    module procedure easyI_int4_4d
    module procedure easyI_int4_5d
    module procedure easyI_int4_6d
    module procedure easyI_int4_7d
    module procedure easyI_int4_scalar
    module procedure easyI_logical_1d
    module procedure easyI_logical_2d
    module procedure easyI_logical_3d
    module procedure easyI_logical_4d
    module procedure easyI_logical_5d
    module procedure easyI_logical_6d
    module procedure easyI_logical_7d
    module procedure easyI_logical_scalar
    module procedure easyI_char_1d
    module procedure easyI_char_2d
    module procedure easyI_char_3d
    module procedure easyI_char_4d
    module procedure easyI_char_5d
    module procedure easyI_char_6d
    module procedure easyI_char
    module procedure easyI_complex4_1d
    module procedure easyI_complex4_2d
    module procedure easyI_complex4_3d
    module procedure easyI_complex4_4d
    module procedure easyI_complex4_5d
    module procedure easyI_complex4_6d
    module procedure easyI_complex4_7d
    module procedure easyI_complex4_scalar
    
    module procedure easyI_complex8_1d
    module procedure easyI_complex8_2d
    module procedure easyI_complex8_3d
    module procedure easyI_complex8_4d
    module procedure easyI_complex8_5d
    module procedure easyI_complex8_6d
    module procedure easyI_complex8_7d
    module procedure easyI_complex8_scalar
  end interface
  interface easyOA
    module procedure easyOA_real4_1d
    module procedure easyOA_real4_2d
    module procedure easyOA_real4_3d
    module procedure easyOA_real4_4d
    module procedure easyOA_real4_5d
    module procedure easyOA_real4_6d
    module procedure easyOA_real4_7d
    module procedure easyOA_real8_1d
    module procedure easyOA_real8_2d
    module procedure easyOA_real8_3d
    module procedure easyOA_real8_4d
    module procedure easyOA_real8_5d
    module procedure easyOA_real8_6d
    module procedure easyOA_real8_7d
    module procedure easyOA_int4_1d
    module procedure easyOA_int4_2d
    module procedure easyOA_int4_3d
    module procedure easyOA_int4_4d
    module procedure easyOA_int4_5d
    module procedure easyOA_int4_6d
    module procedure easyOA_int4_7d
    module procedure easyOA_logical_1d
    module procedure easyOA_logical_2d
    module procedure easyOA_logical_3d
    module procedure easyOA_logical_4d
    module procedure easyOA_logical_5d
    module procedure easyOA_logical_6d
    module procedure easyOA_logical_7d
    module procedure easyOA_char_1d
    module procedure easyOA_char_2d
    module procedure easyOA_char_3d
    module procedure easyOA_char_4d
    module procedure easyOA_char_5d
    module procedure easyOA_char_6d
    module procedure easyOA_complex4_1d
    module procedure easyOA_complex4_2d
    module procedure easyOA_complex4_3d
    module procedure easyOA_complex4_4d
    module procedure easyOA_complex4_5d
    module procedure easyOA_complex4_6d
    module procedure easyOA_complex4_7d
    module procedure easyOA_complex8_1d
    module procedure easyOA_complex8_2d
    module procedure easyOA_complex8_3d
    module procedure easyOA_complex8_4d
    module procedure easyOA_complex8_5d
    module procedure easyOA_complex8_6d
    module procedure easyOA_complex8_7d
  end interface
  interface easyIA
    module procedure easyIA_real4_1d
    module procedure easyIA_real4_2d
    module procedure easyIA_real4_3d
    module procedure easyIA_real4_4d
    module procedure easyIA_real4_5d
    module procedure easyIA_real4_6d
    module procedure easyIA_real4_7d
    module procedure easyIA_real8_1d
    module procedure easyIA_real8_2d
    module procedure easyIA_real8_3d
    module procedure easyIA_real8_4d
    module procedure easyIA_real8_5d
    module procedure easyIA_real8_6d
    module procedure easyIA_real8_7d
    module procedure easyIA_int4_1d
    module procedure easyIA_int4_2d
    module procedure easyIA_int4_3d
    module procedure easyIA_int4_4d
    module procedure easyIA_int4_5d
    module procedure easyIA_int4_6d
    module procedure easyIA_int4_7d
    module procedure easyIA_logical_1d
    module procedure easyIA_logical_2d
    module procedure easyIA_logical_3d
    module procedure easyIA_logical_4d
    module procedure easyIA_logical_5d
    module procedure easyIA_logical_6d
    module procedure easyIA_logical_7d
    module procedure easyIA_char_1d
    module procedure easyIA_char_2d
    module procedure easyIA_char_3d
    module procedure easyIA_char_4d
    module procedure easyIA_char_5d
    module procedure easyIA_char_6d
    module procedure easyIA_complex4_1d
    module procedure easyIA_complex4_2d
    module procedure easyIA_complex4_3d
    module procedure easyIA_complex4_4d
    module procedure easyIA_complex4_5d
    module procedure easyIA_complex4_6d
    module procedure easyIA_complex4_7d
    module procedure easyIA_complex8_1d
    module procedure easyIA_complex8_2d
    module procedure easyIA_complex8_3d
    module procedure easyIA_complex8_4d
    module procedure easyIA_complex8_5d
    module procedure easyIA_complex8_6d
    module procedure easyIA_complex8_7d
  end interface
  Contains
  Subroutine easyO_real4_1d(fname, vname, data, type_info, type_ele_alone)
    real(kind=4),intent(in) :: data(:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer, intent(in), optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_real4(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_real4(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyOA_real4_1d(fname, vname, data, type_info)
    real(kind=4),allocatable,intent(in) :: data(:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_real4(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_real4(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_real4_1d
  Subroutine easyI_real4_1d(fname, vname, data, type_info)
    real(kind=4),intent(out) :: data(:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_real4(fname, vname, data, shape(data), type_info)
    else
      call easyI_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyIA_real4_1d(fname, vname, data, type_info)
    real(kind=4),allocatable,intent(inout) :: data(:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1)))
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
        call easyI_real4(fname, vname, data, shape(data), type_info)
    else
        call easyI_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_real4_2d(fname, vname, data, type_info, type_ele_alone)
    real(kind=4),intent(in) :: data(:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer, intent(in), optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_real4(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_real4(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyOA_real4_2d(fname, vname, data, type_info)
    real(kind=4),allocatable,intent(in) :: data(:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_real4(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_real4(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_real4_2d
  Subroutine easyI_real4_2d(fname, vname, data, type_info)
    real(kind=4),intent(out) :: data(:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_real4(fname, vname, data, shape(data), type_info)
    else
      call easyI_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyIA_real4_2d(fname, vname, data, type_info)
    real(kind=4),allocatable,intent(inout) :: data(:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2)))
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1),dimsT_enc(2)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
        call easyI_real4(fname, vname, data, shape(data), type_info)
    else
        call easyI_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_real4_3d(fname, vname, data, type_info, type_ele_alone)
    real(kind=4),intent(in) :: data(:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer, intent(in), optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_real4(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_real4(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyOA_real4_3d(fname, vname, data, type_info)
    real(kind=4),allocatable,intent(in) :: data(:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_real4(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_real4(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_real4_3d
  Subroutine easyI_real4_3d(fname, vname, data, type_info)
    real(kind=4),intent(out) :: data(:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_real4(fname, vname, data, shape(data), type_info)
    else
      call easyI_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyIA_real4_3d(fname, vname, data, type_info)
    real(kind=4),allocatable,intent(inout) :: data(:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3)))
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
        call easyI_real4(fname, vname, data, shape(data), type_info)
    else
        call easyI_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_real4_4d(fname, vname, data, type_info, type_ele_alone)
    real(kind=4),intent(in) :: data(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer, intent(in), optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_real4(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_real4(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyOA_real4_4d(fname, vname, data, type_info)
    real(kind=4),allocatable,intent(in) :: data(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_real4(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_real4(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_real4_4d
  Subroutine easyI_real4_4d(fname, vname, data, type_info)
    real(kind=4),intent(out) :: data(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_real4(fname, vname, data, shape(data), type_info)
    else
      call easyI_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyIA_real4_4d(fname, vname, data, type_info)
    real(kind=4),allocatable,intent(inout) :: data(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4))) ! 
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
        call easyI_real4(fname, vname, data, shape(data), type_info)
    else
        call easyI_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_real4_5d(fname, vname, data, type_info, type_ele_alone)
    real(kind=4),intent(in) :: data(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer, intent(in), optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_real4(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_real4(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyOA_real4_5d(fname, vname, data, type_info)
    real(kind=4),allocatable,intent(in) :: data(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_real4(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_real4(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_real4_5d
  Subroutine easyI_real4_5d(fname, vname, data, type_info)
    real(kind=4),intent(out) :: data(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_real4(fname, vname, data, shape(data), type_info)
    else
      call easyI_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyIA_real4_5d(fname, vname, data, type_info)
    real(kind=4),allocatable,intent(inout) :: data(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5))) ! 
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
        call easyI_real4(fname, vname, data, shape(data), type_info)
    else
        call easyI_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_real4_6d(fname, vname, data, type_info, type_ele_alone)
    real(kind=4),intent(in) :: data(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer, intent(in), optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_real4(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_real4(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyOA_real4_6d(fname, vname, data, type_info)
    real(kind=4),allocatable,intent(in) :: data(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_real4(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_real4(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_real4_6d
  Subroutine easyI_real4_6d(fname, vname, data, type_info)
    real(kind=4),intent(out) :: data(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_real4(fname, vname, data, shape(data), type_info)
    else
      call easyI_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyIA_real4_6d(fname, vname, data, type_info)
    real(kind=4),allocatable,intent(inout) :: data(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6))) ! 
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
        call easyI_real4(fname, vname, data, shape(data), type_info)
    else
        call easyI_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_real4_7d(fname, vname, data, type_info, type_ele_alone)
    real(kind=4),intent(in) :: data(:,:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer, intent(in), optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_real4(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_real4(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyOA_real4_7d(fname, vname, data, type_info)
    real(kind=4),allocatable,intent(in) :: data(:,:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_real4(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_real4(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_real4_7d
  Subroutine easyI_real4_7d(fname, vname, data, type_info)
    real(kind=4),intent(out) :: data(:,:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_real4(fname, vname, data, shape(data), type_info)
    else
      call easyI_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyIA_real4_7d(fname, vname, data, type_info)
    real(kind=4),allocatable,intent(inout) :: data(:,:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6),dimsT2_enc(1,7):dimsT2_enc( &
    2,7))) ! 
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6),dimsT_enc(7)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
        call easyI_real4(fname, vname, data, shape(data), type_info)
    else
        call easyI_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_real4(fname, vname, data, dims_data, type_info, type_ele_alone, shape_total, position)
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    real(kind=4),intent(in) :: data(*)
    character(*),intent(in) :: fname, vname
    integer, intent(in) :: dims_data(:)
    integer, intent(in), optional :: type_info(:, :) ! 2 x n, 0 : type array size, 1 : type position
    integer, intent(in), optional :: type_ele_alone
    integer, intent(in), optional :: shape_total    ! total shape, such as (3,4,5)
    integer, intent(in), optional :: position  ! such as (2,2,2)
    character(80) :: ovname, strT, posSuffix
    integer,allocatable :: dims(:)
    integer :: ncid, vid, ierr
    integer :: ndims
    
    integer,allocatable :: dids(:)
    integer :: i, j, k, l, m, n
    integer :: data_size, ndims_data, ndims_type
    
    logical :: isExist
    character(40), allocatable :: dimnames(:)
    character(80) :: tname, tvname
    integer,allocatable :: start_index(:), count_lens(:)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> body
    ! ================== get dims for both base and type-var mode
    ndims_data = size(dims_data)
    if (present(type_info)) then
      if (present(type_ele_alone)) then   ! save type-element, 1 variable for 1 position
        ndims = ndims_data
        allocate(dims(ndims), STAT=ierr)
        call check_enc(ierr, 'allocate')
        dims = dims_data
      else                                ! save type-element in a union n1+n2 dimensional array
        ndims_type = size(type_info(1, :))
        ndims = ndims_data + ndims_type
        allocate(dims(ndims), STAT=ierr)
        call check_enc(ierr, 'allocate')
        dims(1:ndims_data) = dims_data
        do i = ndims_data + 1, ndims
          dims(i) = type_info(1,ndims_type - (i-ndims_data) + 1)
        end do
      end if
    else
      ndims = ndims_data
      allocate(dims(ndims), STAT=ierr)
      call check_enc(ierr, 'allocate')
      dims = dims_data
    end if
    
    ovname = vname
    ! ================== get tname and tvname for type-var
    if (present(type_info)) then
      i = scan(vname, '%')
      if (i .eq. 0) then
        stop 'please use % as the struct connection symbol'
      end if
      tname = vname(:i-1)
      tvname = vname(i+1:)
      if (present(type_ele_alone)) then
        posSuffix = '('
        do j = 1, size(type_info(1,:))
          write(strT, '(I0)') type_info(2,j)
          ! print *, 'strT=',strT
          posSuffix = trim(posSuffix)//trim(strT)
          if (j .lt. size(type_info(1,:))) posSuffix = trim(posSuffix)//','
        end do
        posSuffix = trim(posSuffix)//')'
        ovname = trim(tname)//trim(posSuffix)//'%'//trim(tvname)
        ! print *, 'ovname = ',ovname
      end if
    end if
    allocate(dimnames(ndims), STAT = ierr)
    if (ierr .ne. 0) then
      stop 1
    end if
    
    allocate(dids(ndims), STAT = ierr)
    if (ierr .ne. 0) then
      stop 1
    end if
    
    allocate(start_index(ndims), STAT=ierr)
    call check_enc(ierr, 'allocate start_index')
    start_index = 1
    if (present(type_info)) then
      ! print *, 'type_info=',type_info(1,:), type_info(2,:)
      if (.not. present(type_ele_alone)) then
        do i = ndims_data + 1, ndims
          start_index(i) = type_info(2, ndims_type - (i-ndims_data) + 1)
        end do
      end if
    end if
    allocate(count_lens(ndims), STAT=ierr)
    if (present(type_info)) then
      if (present(type_ele_alone)) then
        count_lens = dims_data
      else
        count_lens(1:ndims_data) = dims_data
        count_lens(ndims_data+1:) = 1
      end if
    else
      count_lens = dims_data
    end if
    data_size = product(dims_data)
    
    ! ================== set dimension names
    if (present(type_info)) then
      if (.not. present(type_ele_alone)) then
        do i = 1, ndims_data
          write(dimnames(i), '(A, ".d", I1)') trim(tvname), ndims_data-i+1
        end do
        do i = ndims_data + 1, ndims
          write(dimnames(i), '(A, ".d", I1)') trim(tname), i-ndims_data
        end do
      else
        do i = 1, ndims
          write(dimnames(i), '(A, A, ".d", I1)') trim(tvname), trim(posSuffix), ndims_data-i+1
        end do
      end if
    else
      do i = 1, ndims
        write(dimnames(i), '(A, ".d", I1)') trim(vname), ndims_data-i+1
      end do
    end if
    ! print *, 'dimnames = ', dimnames
    ! =========================== netcdf-relative
    inquire(file = fname, exist = isExist)
    ! print *, "isExist=",isExist
    ! ~~~~~~~~~~~~ open or create nc
    if (isExist) then
      call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
      call check_enc( nf90_redef(ncid), "nf90_redef")
    else
      call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
    end if
    ! ~~~~~~~~~~~~ get or define dimension
    do i = 1, ndims
      if (isExist) then
        if (nf90_inq_dimid(ncid, dimnames(i), dids(i)) .ne. nf90_noerr) then
          call check_enc(nf90_def_dim(ncid, dimnames(i), dims(i), dids(i)), "nf90_def_dim")
        end if
      else
        call check_enc(nf90_def_dim(ncid, dimnames(i), dims(i), dids(i)), "nf90_def_dim")
      end if
    end do
    ! ~~~~~~~~~~~~ define variable, or error
    if (nf90_inq_varid(ncid, ovname, vid) .eq. nf90_noerr) then
      if (.not. present(type_info)) then
        write(*,*) 'Error! variable already exists!'
        call check_enc( nf90_close(ncid) , "nf90_close")
        stop 1
      end if
    else
      call check_enc( nf90_def_var(ncid, ovname, NF90_FLOAT, dids, vid) , "nf90_def_var")
    end if
    call check_enc( nf90_enddef(ncid) , "nf90_enddef")
    ! ~~~~~~~~~~~~ write data
    ! print *, 'start_index = ',start_index
    ! print *, 'count_lens = ',count_lens
    call check_enc( nf90_put_var(ncid, vid, data(1:data_size), start=start_index, count=count_lens) , & 
      "in easyO_real4, nf90_put_var, "//ovname)
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    return
  end subroutine easyO_real4
  
  Subroutine easyO_real4_scalar(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    real(kind=4),intent(in) :: val
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: type_info(:, :) ! 2 x n, 0 : type array size, 1 : type position
    
    integer,allocatable :: dims(:)
    
    integer :: ncid, vid, ierr
    
    integer :: i, j, k, l, m, n
    
    logical :: isExist
    character(40), allocatable :: dimnames(:)
    integer,allocatable :: dids(:), start_index(:)
    integer :: ndims_type
    character(80) :: tname, tvname
    
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> body
    ! =========================== netcdf-relative
    inquire(file = fname, exist = isExist)
    ! print *, "isExist=",isExist
    
    if (present(type_info)) then
      ndims_type = size(type_info(1, :))
      allocate(dims(ndims_type), STAT=ierr)
      call check_enc(ierr, 'allocate')
      dims = type_info(1, :)
    ! ================== get tname and tvname for type-var
      i = scan(vname, '%')
      if (i .eq. 0) then
        stop 'please use % as the struct connection symbol'
      end if
      tname = vname(:i-1)
      tvname = vname(i+1:)
      allocate(dimnames(ndims_type), STAT = ierr)
      call check_enc(ierr, 'allocate for dimnames')
      ! ================== set dimension names
      do i = 1, ndims_type
        write(dimnames(i), '("'//trim(tname)//'", "_d", I1)') i
      end do
      allocate(dids(ndims_type), STAT = ierr)
      call check_enc(ierr, 'allocate dids')
      allocate(start_index(ndims_type), STAT=ierr)
      call check_enc(ierr, 'allocate start_index')
      ! print *, 'type_info=',type_info(1,:), type_info(2,:)
      do i = 1, ndims_type
        start_index(i) = type_info(2, ndims_type - i + 1)
      end do
    end if
    ! ~~~~~~~~~~~~ open or create nc
    if (isExist) then
      call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "nf90_open")
      call check_enc(nf90_redef(ncid), "nf90_redef")
    else
      call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "nf90_create")
    end if
    ! ~~~~~~~~~~~~ get or define dimension
    if (present(type_info)) then
      do i = 1, ndims_type
        if (isExist) then
          if (nf90_inq_dimid(ncid, dimnames(i), dids(i)) .ne. nf90_noerr) then
            call check_enc(nf90_def_dim(ncid, dimnames(i), dims(i), dids(i)), "nf90_def_dim")
          end if
        else
          call check_enc(nf90_def_dim(ncid, dimnames(i), dims(i), dids(i)), "nf90_def_dim")
        end if
      end do
    end if
    ! ~~~~~~~~~~~~ define variable, or error
    if (nf90_inq_varid(ncid, vname, vid) .eq. nf90_noerr) then
      if (.not. present(type_info)) then
        write(*,*) 'Error! variable already exists!'
        call check_enc( nf90_close(ncid) , "nf90_close")
        stop 1
      end if
    else
      if (present(type_info)) then
        call check_enc( nf90_def_var(ncid, vname, NF90_FLOAT, dids, vid) , "nf90_def_var")
      else
        call check_enc( nf90_def_var(ncid, vname, NF90_FLOAT, varid=vid) , "nf90_def_var")
      end if
    end if
    call check_enc( nf90_enddef(ncid) , "nf90_enddef")
    ! ~~~~~~~~~~~~ write val
    if (present(type_info)) then
      call check_enc( nf90_put_var(ncid, vid, val, start=start_index) , "nf90_put_var")
    else
      call check_enc( nf90_put_var(ncid, vid, val) , "nf90_put_var")
    end if
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    return
  end subroutine easyO_real4_scalar
  Subroutine easyI_real4(fname, vname, data, dims_data, type_info)
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    real(kind=4),intent(out) :: data(*)
    character(*),intent(in) :: fname, vname
    integer, intent(in) :: dims_data(:)
    integer, intent(in), optional :: type_info(:, :) ! 2 x n, 0 : type array size, 1 : type position
    
    character(80) :: ivname, strT, posSuffix, tname, tvname
    integer :: type_ele_alone
    integer,allocatable :: dims(:)
    integer :: ncid, vid, ierr
    integer :: ndims
    
    integer :: i, j, k, l, m, n
    integer :: data_size, ndims_data, ndims_type
    
    logical :: isExist
    integer,allocatable :: start_index(:), count_lens(:)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> body
    ! ================== get dims for both base and type-var mode
    ndims_data = size(dims_data)
    ivname = vname
    if (present(type_info)) then
      i = scan(vname, '%')
      if (i .eq. 0) then
        stop 'please use % as the struct connection symbol'
      end if
      tname = vname(:i-1)
      tvname = vname(i+1:)
      if (hasVar(fname, vname) .eq. 1) then
        type_ele_alone = 0
      else
        type_ele_alone = 1
        ! --- get pos suffix
        posSuffix = '('
        do j = 1, size(type_info(1,:))
          write(strT, '(I0)') type_info(2,j)
          ! print *, 'strT=',strT
          posSuffix = trim(posSuffix)//trim(strT)
          if (j .lt. size(type_info(1,:))) posSuffix = trim(posSuffix)//','
        end do
        posSuffix = trim(posSuffix)//')'
        ivname = trim(tname)//trim(posSuffix)//'%'//trim(tvname)
      end if
      if (type_ele_alone .eq. 1) then
        ndims = ndims_data
        allocate(dims(ndims), STAT=ierr)
        call check_enc(ierr, 'allocate')
        dims = dims_data
      else
        ndims_type = size(type_info(1, :))
        ndims = ndims_data + ndims_type
        allocate(dims(ndims), STAT=ierr)
        call check_enc(ierr, 'allocate')
        dims(1:ndims_data) = dims_data
        do i = ndims_data + 1, ndims
          dims(i) = type_info(1,ndims_type - (i-ndims_data) + 1)
        end do
      end if
    else
      ndims = ndims_data
      allocate(dims(ndims), STAT=ierr)
      call check_enc(ierr, 'allocate')
      dims = dims_data
    end if
    ! ================== get tname and tvname for type-var
    
    allocate(start_index(ndims), STAT=ierr)
    call check_enc(ierr, 'allocate start_index')
    start_index = 1
    if (present(type_info)) then
    ! print *, 'type_info=',type_info(1,:), type_info(2,:)
      if (type_ele_alone .eq. 0) then
        do i = ndims_data + 1, ndims
          start_index(i) = type_info(2, ndims_type - (i-ndims_data) + 1)
        end do
      end if        
    end if
    allocate(count_lens(ndims), STAT=ierr)
    if (present(type_info)) then
      if (type_ele_alone .eq. 0) then
        count_lens(1:ndims_data) = dims_data
        count_lens(ndims_data+1:) = 1
      else
        count_lens = dims_data
      end if
    else
      count_lens = dims_data
    end if
    data_size = product(dims_data)
    
    ! =========================== netcdf-relative
    inquire(file = fname, exist = isExist)
    ! print *, "isExist=",isExist
    ! ~~~~~~~~~~~~ open or create nc
    if (isExist) then
      call check_enc(nf90_open(fname, NF90_NOWRITE, ncid) , "nf90_open")
    else
      print *, "Error, file doesn't exist."
      stop "Stopped"
    end if
    ! ~~~~~~~~~~~~ inquire variable
    call check_enc(nf90_inq_varid(ncid, ivname, vid), &
      'in easyI_real4, nf90_inq_varid for '//trim(ivname))
    ! ~~~~~~~~~~~~ read data
    ! print *, 'start_index = ',start_index
    ! print *, 'count_lens = ',count_lens
    call check_enc( nf90_get_var(ncid, vid, data(1:data_size), start=start_index, count=count_lens), &
      "in easyI_real4, nf90_get_var, "//ivname)
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    return
  end subroutine easyI_real4
  
  Subroutine easyI_real4_scalar(fname, vname, val, type_info)
    ! almost Duplicated with easyI_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    real(kind=4),intent(out) :: val
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: type_info(:, :) ! 2 x n, 0 : type array size, 1 : type position
    
    integer,allocatable :: dims(:)
    
    integer :: ncid, vid, ierr
    
    integer :: i, j, k, l, m, n
    
    logical :: isExist
    integer,allocatable :: start_index(:)
    integer :: ndims_type
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> body
    ! =========================== netcdf-relative
    ! print *, "isExist=",isExist
    
    if (present(type_info)) then
      ndims_type = size(type_info(1, :))
      allocate(dims(ndims_type), STAT=ierr)
      call check_enc(ierr, 'allocate')
      dims = type_info(1, :)
    ! ================== get tname and tvname for type-var
      allocate(start_index(ndims_type), STAT=ierr)
      call check_enc(ierr, 'allocate start_index')
      ! print *, 'type_info=',type_info(1,:), type_info(2,:)
      do i = 1, ndims_type
        start_index(i) = type_info(2, ndims_type - i + 1)
      end do
    end if
    inquire(file = fname, exist = isExist)
    ! ~~~~~~~~~~~~ open or create nc
    if (isExist) then
      call check_enc(nf90_open(fname, NF90_NOWRITE, ncid) , "nf90_open")
    else
      print *, "Error, file doesn't exist."
      stop "Stopped"
    end if
    ! ~~~~~~~~~~~~ inquire variable
    call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
    ! ~~~~~~~~~~~~ write val
    if (present(type_info)) then
      call check_enc( nf90_get_var(ncid, vid, val, start=start_index), "in nf90_get_var, "//vname)
    else
      call check_enc( nf90_get_var(ncid, vid, val), "in nf90_get_var, "//vname)
    end if
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    return
  end subroutine easyI_real4_scalar
  Subroutine easyO_real8_1d(fname, vname, data, type_info, type_ele_alone)
    real(kind=8),intent(in) :: data(:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer, intent(in), optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_real8(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_real8(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_real8(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyOA_real8_1d(fname, vname, data, type_info)
    real(kind=8),allocatable,intent(in) :: data(:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_real8(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_real8(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_real8_1d
  Subroutine easyI_real8_1d(fname, vname, data, type_info)
    real(kind=8),intent(out) :: data(:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_real8(fname, vname, data, shape(data), type_info)
    else
      call easyI_real8(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyIA_real8_1d(fname, vname, data, type_info)
    real(kind=8),allocatable,intent(inout) :: data(:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1)))
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
        call easyI_real8(fname, vname, data, shape(data), type_info)
    else
        call easyI_real8(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_real8_2d(fname, vname, data, type_info, type_ele_alone)
    real(kind=8),intent(in) :: data(:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer, intent(in), optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_real8(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_real8(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_real8(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyOA_real8_2d(fname, vname, data, type_info)
    real(kind=8),allocatable,intent(in) :: data(:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_real8(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_real8(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_real8_2d
  Subroutine easyI_real8_2d(fname, vname, data, type_info)
    real(kind=8),intent(out) :: data(:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_real8(fname, vname, data, shape(data), type_info)
    else
      call easyI_real8(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyIA_real8_2d(fname, vname, data, type_info)
    real(kind=8),allocatable,intent(inout) :: data(:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2)))
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1),dimsT_enc(2)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
        call easyI_real8(fname, vname, data, shape(data), type_info)
    else
        call easyI_real8(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_real8_3d(fname, vname, data, type_info, type_ele_alone)
    real(kind=8),intent(in) :: data(:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer, intent(in), optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_real8(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_real8(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_real8(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyOA_real8_3d(fname, vname, data, type_info)
    real(kind=8),allocatable,intent(in) :: data(:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_real8(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_real8(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_real8_3d
  Subroutine easyI_real8_3d(fname, vname, data, type_info)
    real(kind=8),intent(out) :: data(:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_real8(fname, vname, data, shape(data), type_info)
    else
      call easyI_real8(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyIA_real8_3d(fname, vname, data, type_info)
    real(kind=8),allocatable,intent(inout) :: data(:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3)))
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
        call easyI_real8(fname, vname, data, shape(data), type_info)
    else
        call easyI_real8(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_real8_4d(fname, vname, data, type_info, type_ele_alone)
    real(kind=8),intent(in) :: data(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer, intent(in), optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_real8(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_real8(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_real8(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyOA_real8_4d(fname, vname, data, type_info)
    real(kind=8),allocatable,intent(in) :: data(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_real8(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_real8(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_real8_4d
  Subroutine easyI_real8_4d(fname, vname, data, type_info)
    real(kind=8),intent(out) :: data(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_real8(fname, vname, data, shape(data), type_info)
    else
      call easyI_real8(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyIA_real8_4d(fname, vname, data, type_info)
    real(kind=8),allocatable,intent(inout) :: data(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4))) ! 
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
        call easyI_real8(fname, vname, data, shape(data), type_info)
    else
        call easyI_real8(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_real8_5d(fname, vname, data, type_info, type_ele_alone)
    real(kind=8),intent(in) :: data(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer, intent(in), optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_real8(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_real8(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_real8(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyOA_real8_5d(fname, vname, data, type_info)
    real(kind=8),allocatable,intent(in) :: data(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_real8(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_real8(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_real8_5d
  Subroutine easyI_real8_5d(fname, vname, data, type_info)
    real(kind=8),intent(out) :: data(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_real8(fname, vname, data, shape(data), type_info)
    else
      call easyI_real8(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyIA_real8_5d(fname, vname, data, type_info)
    real(kind=8),allocatable,intent(inout) :: data(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5))) ! 
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
        call easyI_real8(fname, vname, data, shape(data), type_info)
    else
        call easyI_real8(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_real8_6d(fname, vname, data, type_info, type_ele_alone)
    real(kind=8),intent(in) :: data(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer, intent(in), optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_real8(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_real8(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_real8(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyOA_real8_6d(fname, vname, data, type_info)
    real(kind=8),allocatable,intent(in) :: data(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_real8(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_real8(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_real8_6d
  Subroutine easyI_real8_6d(fname, vname, data, type_info)
    real(kind=8),intent(out) :: data(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_real8(fname, vname, data, shape(data), type_info)
    else
      call easyI_real8(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyIA_real8_6d(fname, vname, data, type_info)
    real(kind=8),allocatable,intent(inout) :: data(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6))) ! 
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
        call easyI_real8(fname, vname, data, shape(data), type_info)
    else
        call easyI_real8(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_real8_7d(fname, vname, data, type_info, type_ele_alone)
    real(kind=8),intent(in) :: data(:,:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer, intent(in), optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_real8(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_real8(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_real8(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyOA_real8_7d(fname, vname, data, type_info)
    real(kind=8),allocatable,intent(in) :: data(:,:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_real8(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_real8(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_real8_7d
  Subroutine easyI_real8_7d(fname, vname, data, type_info)
    real(kind=8),intent(out) :: data(:,:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_real8(fname, vname, data, shape(data), type_info)
    else
      call easyI_real8(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyIA_real8_7d(fname, vname, data, type_info)
    real(kind=8),allocatable,intent(inout) :: data(:,:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6),dimsT2_enc(1,7):dimsT2_enc( &
    2,7))) ! 
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6),dimsT_enc(7)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
        call easyI_real8(fname, vname, data, shape(data), type_info)
    else
        call easyI_real8(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_real8(fname, vname, data, dims_data, type_info, type_ele_alone, shape_total, position)
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    real(kind=8),intent(in) :: data(*)
    character(*),intent(in) :: fname, vname
    integer, intent(in) :: dims_data(:)
    integer, intent(in), optional :: type_info(:, :) ! 2 x n, 0 : type array size, 1 : type position
    integer, intent(in), optional :: type_ele_alone
    integer, intent(in), optional :: shape_total    ! total shape, such as (3,4,5)
    integer, intent(in), optional :: position  ! such as (2,2,2)
    character(80) :: ovname, strT, posSuffix
    integer,allocatable :: dims(:)
    integer :: ncid, vid, ierr
    integer :: ndims
    
    integer,allocatable :: dids(:)
    integer :: i, j, k, l, m, n
    integer :: data_size, ndims_data, ndims_type
    
    logical :: isExist
    character(40), allocatable :: dimnames(:)
    character(80) :: tname, tvname
    integer,allocatable :: start_index(:), count_lens(:)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> body
    ! ================== get dims for both base and type-var mode
    ndims_data = size(dims_data)
    if (present(type_info)) then
      if (present(type_ele_alone)) then   ! save type-element, 1 variable for 1 position
        ndims = ndims_data
        allocate(dims(ndims), STAT=ierr)
        call check_enc(ierr, 'allocate')
        dims = dims_data
      else                                ! save type-element in a union n1+n2 dimensional array
        ndims_type = size(type_info(1, :))
        ndims = ndims_data + ndims_type
        allocate(dims(ndims), STAT=ierr)
        call check_enc(ierr, 'allocate')
        dims(1:ndims_data) = dims_data
        do i = ndims_data + 1, ndims
          dims(i) = type_info(1,ndims_type - (i-ndims_data) + 1)
        end do
      end if
    else
      ndims = ndims_data
      allocate(dims(ndims), STAT=ierr)
      call check_enc(ierr, 'allocate')
      dims = dims_data
    end if
    
    ovname = vname
    ! ================== get tname and tvname for type-var
    if (present(type_info)) then
      i = scan(vname, '%')
      if (i .eq. 0) then
        stop 'please use % as the struct connection symbol'
      end if
      tname = vname(:i-1)
      tvname = vname(i+1:)
      if (present(type_ele_alone)) then
        posSuffix = '('
        do j = 1, size(type_info(1,:))
          write(strT, '(I0)') type_info(2,j)
          ! print *, 'strT=',strT
          posSuffix = trim(posSuffix)//trim(strT)
          if (j .lt. size(type_info(1,:))) posSuffix = trim(posSuffix)//','
        end do
        posSuffix = trim(posSuffix)//')'
        ovname = trim(tname)//trim(posSuffix)//'%'//trim(tvname)
        ! print *, 'ovname = ',ovname
      end if
    end if
    allocate(dimnames(ndims), STAT = ierr)
    if (ierr .ne. 0) then
      stop 1
    end if
    
    allocate(dids(ndims), STAT = ierr)
    if (ierr .ne. 0) then
      stop 1
    end if
    
    allocate(start_index(ndims), STAT=ierr)
    call check_enc(ierr, 'allocate start_index')
    start_index = 1
    if (present(type_info)) then
      ! print *, 'type_info=',type_info(1,:), type_info(2,:)
      if (.not. present(type_ele_alone)) then
        do i = ndims_data + 1, ndims
          start_index(i) = type_info(2, ndims_type - (i-ndims_data) + 1)
        end do
      end if
    end if
    allocate(count_lens(ndims), STAT=ierr)
    if (present(type_info)) then
      if (present(type_ele_alone)) then
        count_lens = dims_data
      else
        count_lens(1:ndims_data) = dims_data
        count_lens(ndims_data+1:) = 1
      end if
    else
      count_lens = dims_data
    end if
    data_size = product(dims_data)
    
    ! ================== set dimension names
    if (present(type_info)) then
      if (.not. present(type_ele_alone)) then
        do i = 1, ndims_data
          write(dimnames(i), '(A, ".d", I1)') trim(tvname), ndims_data-i+1
        end do
        do i = ndims_data + 1, ndims
          write(dimnames(i), '(A, ".d", I1)') trim(tname), i-ndims_data
        end do
      else
        do i = 1, ndims
          write(dimnames(i), '(A, A, ".d", I1)') trim(tvname), trim(posSuffix), ndims_data-i+1
        end do
      end if
    else
      do i = 1, ndims
        write(dimnames(i), '(A, ".d", I1)') trim(vname), ndims_data-i+1
      end do
    end if
    ! print *, 'dimnames = ', dimnames
    ! =========================== netcdf-relative
    inquire(file = fname, exist = isExist)
    ! print *, "isExist=",isExist
    ! ~~~~~~~~~~~~ open or create nc
    if (isExist) then
      call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
      call check_enc( nf90_redef(ncid), "nf90_redef")
    else
      call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
    end if
    ! ~~~~~~~~~~~~ get or define dimension
    do i = 1, ndims
      if (isExist) then
        if (nf90_inq_dimid(ncid, dimnames(i), dids(i)) .ne. nf90_noerr) then
          call check_enc(nf90_def_dim(ncid, dimnames(i), dims(i), dids(i)), "nf90_def_dim")
        end if
      else
        call check_enc(nf90_def_dim(ncid, dimnames(i), dims(i), dids(i)), "nf90_def_dim")
      end if
    end do
    ! ~~~~~~~~~~~~ define variable, or error
    if (nf90_inq_varid(ncid, ovname, vid) .eq. nf90_noerr) then
      if (.not. present(type_info)) then
        write(*,*) 'Error! variable already exists!'
        call check_enc( nf90_close(ncid) , "nf90_close")
        stop 1
      end if
    else
      call check_enc( nf90_def_var(ncid, ovname, NF90_DOUBLE, dids, vid) , "nf90_def_var")
    end if
    call check_enc( nf90_enddef(ncid) , "nf90_enddef")
    ! ~~~~~~~~~~~~ write data
    ! print *, 'start_index = ',start_index
    ! print *, 'count_lens = ',count_lens
    call check_enc( nf90_put_var(ncid, vid, data(1:data_size), start=start_index, count=count_lens) , & 
      "in easyO_real8, nf90_put_var, "//ovname)
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    return
  end subroutine easyO_real8
  
  Subroutine easyO_real8_scalar(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    real(kind=8),intent(in) :: val
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: type_info(:, :) ! 2 x n, 0 : type array size, 1 : type position
    
    integer,allocatable :: dims(:)
    
    integer :: ncid, vid, ierr
    
    integer :: i, j, k, l, m, n
    
    logical :: isExist
    character(40), allocatable :: dimnames(:)
    integer,allocatable :: dids(:), start_index(:)
    integer :: ndims_type
    character(80) :: tname, tvname
    
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> body
    ! =========================== netcdf-relative
    inquire(file = fname, exist = isExist)
    ! print *, "isExist=",isExist
    
    if (present(type_info)) then
      ndims_type = size(type_info(1, :))
      allocate(dims(ndims_type), STAT=ierr)
      call check_enc(ierr, 'allocate')
      dims = type_info(1, :)
    ! ================== get tname and tvname for type-var
      i = scan(vname, '%')
      if (i .eq. 0) then
        stop 'please use % as the struct connection symbol'
      end if
      tname = vname(:i-1)
      tvname = vname(i+1:)
      allocate(dimnames(ndims_type), STAT = ierr)
      call check_enc(ierr, 'allocate for dimnames')
      ! ================== set dimension names
      do i = 1, ndims_type
        write(dimnames(i), '("'//trim(tname)//'", "_d", I1)') i
      end do
      allocate(dids(ndims_type), STAT = ierr)
      call check_enc(ierr, 'allocate dids')
      allocate(start_index(ndims_type), STAT=ierr)
      call check_enc(ierr, 'allocate start_index')
      ! print *, 'type_info=',type_info(1,:), type_info(2,:)
      do i = 1, ndims_type
        start_index(i) = type_info(2, ndims_type - i + 1)
      end do
    end if
    ! ~~~~~~~~~~~~ open or create nc
    if (isExist) then
      call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "nf90_open")
      call check_enc(nf90_redef(ncid), "nf90_redef")
    else
      call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "nf90_create")
    end if
    ! ~~~~~~~~~~~~ get or define dimension
    if (present(type_info)) then
      do i = 1, ndims_type
        if (isExist) then
          if (nf90_inq_dimid(ncid, dimnames(i), dids(i)) .ne. nf90_noerr) then
            call check_enc(nf90_def_dim(ncid, dimnames(i), dims(i), dids(i)), "nf90_def_dim")
          end if
        else
          call check_enc(nf90_def_dim(ncid, dimnames(i), dims(i), dids(i)), "nf90_def_dim")
        end if
      end do
    end if
    ! ~~~~~~~~~~~~ define variable, or error
    if (nf90_inq_varid(ncid, vname, vid) .eq. nf90_noerr) then
      if (.not. present(type_info)) then
        write(*,*) 'Error! variable already exists!'
        call check_enc( nf90_close(ncid) , "nf90_close")
        stop 1
      end if
    else
      if (present(type_info)) then
        call check_enc( nf90_def_var(ncid, vname, NF90_DOUBLE, dids, vid) , "nf90_def_var")
      else
        call check_enc( nf90_def_var(ncid, vname, NF90_DOUBLE, varid=vid) , "nf90_def_var")
      end if
    end if
    call check_enc( nf90_enddef(ncid) , "nf90_enddef")
    ! ~~~~~~~~~~~~ write val
    if (present(type_info)) then
      call check_enc( nf90_put_var(ncid, vid, val, start=start_index) , "nf90_put_var")
    else
      call check_enc( nf90_put_var(ncid, vid, val) , "nf90_put_var")
    end if
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    return
  end subroutine easyO_real8_scalar
  Subroutine easyI_real8(fname, vname, data, dims_data, type_info)
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    real(kind=8),intent(out) :: data(*)
    character(*),intent(in) :: fname, vname
    integer, intent(in) :: dims_data(:)
    integer, intent(in), optional :: type_info(:, :) ! 2 x n, 0 : type array size, 1 : type position
    
    character(80) :: ivname, strT, posSuffix, tname, tvname
    integer :: type_ele_alone
    integer,allocatable :: dims(:)
    integer :: ncid, vid, ierr
    integer :: ndims
    
    integer :: i, j, k, l, m, n
    integer :: data_size, ndims_data, ndims_type
    
    logical :: isExist
    integer,allocatable :: start_index(:), count_lens(:)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> body
    ! ================== get dims for both base and type-var mode
    ndims_data = size(dims_data)
    ivname = vname
    if (present(type_info)) then
      i = scan(vname, '%')
      if (i .eq. 0) then
        stop 'please use % as the struct connection symbol'
      end if
      tname = vname(:i-1)
      tvname = vname(i+1:)
      if (hasVar(fname, vname) .eq. 1) then
        type_ele_alone = 0
      else
        type_ele_alone = 1
        ! --- get pos suffix
        posSuffix = '('
        do j = 1, size(type_info(1,:))
          write(strT, '(I0)') type_info(2,j)
          ! print *, 'strT=',strT
          posSuffix = trim(posSuffix)//trim(strT)
          if (j .lt. size(type_info(1,:))) posSuffix = trim(posSuffix)//','
        end do
        posSuffix = trim(posSuffix)//')'
        ivname = trim(tname)//trim(posSuffix)//'%'//trim(tvname)
      end if
      if (type_ele_alone .eq. 1) then
        ndims = ndims_data
        allocate(dims(ndims), STAT=ierr)
        call check_enc(ierr, 'allocate')
        dims = dims_data
      else
        ndims_type = size(type_info(1, :))
        ndims = ndims_data + ndims_type
        allocate(dims(ndims), STAT=ierr)
        call check_enc(ierr, 'allocate')
        dims(1:ndims_data) = dims_data
        do i = ndims_data + 1, ndims
          dims(i) = type_info(1,ndims_type - (i-ndims_data) + 1)
        end do
      end if
    else
      ndims = ndims_data
      allocate(dims(ndims), STAT=ierr)
      call check_enc(ierr, 'allocate')
      dims = dims_data
    end if
    ! ================== get tname and tvname for type-var
    
    allocate(start_index(ndims), STAT=ierr)
    call check_enc(ierr, 'allocate start_index')
    start_index = 1
    if (present(type_info)) then
    ! print *, 'type_info=',type_info(1,:), type_info(2,:)
      if (type_ele_alone .eq. 0) then
        do i = ndims_data + 1, ndims
          start_index(i) = type_info(2, ndims_type - (i-ndims_data) + 1)
        end do
      end if        
    end if
    allocate(count_lens(ndims), STAT=ierr)
    if (present(type_info)) then
      if (type_ele_alone .eq. 0) then
        count_lens(1:ndims_data) = dims_data
        count_lens(ndims_data+1:) = 1
      else
        count_lens = dims_data
      end if
    else
      count_lens = dims_data
    end if
    data_size = product(dims_data)
    
    ! =========================== netcdf-relative
    inquire(file = fname, exist = isExist)
    ! print *, "isExist=",isExist
    ! ~~~~~~~~~~~~ open or create nc
    if (isExist) then
      call check_enc(nf90_open(fname, NF90_NOWRITE, ncid) , "nf90_open")
    else
      print *, "Error, file doesn't exist."
      stop "Stopped"
    end if
    ! ~~~~~~~~~~~~ inquire variable
    call check_enc(nf90_inq_varid(ncid, ivname, vid), &
      'in easyI_real8, nf90_inq_varid for '//trim(ivname))
    ! ~~~~~~~~~~~~ read data
    ! print *, 'start_index = ',start_index
    ! print *, 'count_lens = ',count_lens
    call check_enc( nf90_get_var(ncid, vid, data(1:data_size), start=start_index, count=count_lens), &
      "in easyI_real8, nf90_get_var, "//ivname)
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    return
  end subroutine easyI_real8
  
  Subroutine easyI_real8_scalar(fname, vname, val, type_info)
    ! almost Duplicated with easyI_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    real(kind=8),intent(out) :: val
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: type_info(:, :) ! 2 x n, 0 : type array size, 1 : type position
    
    integer,allocatable :: dims(:)
    
    integer :: ncid, vid, ierr
    
    integer :: i, j, k, l, m, n
    
    logical :: isExist
    integer,allocatable :: start_index(:)
    integer :: ndims_type
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> body
    ! =========================== netcdf-relative
    ! print *, "isExist=",isExist
    
    if (present(type_info)) then
      ndims_type = size(type_info(1, :))
      allocate(dims(ndims_type), STAT=ierr)
      call check_enc(ierr, 'allocate')
      dims = type_info(1, :)
    ! ================== get tname and tvname for type-var
      allocate(start_index(ndims_type), STAT=ierr)
      call check_enc(ierr, 'allocate start_index')
      ! print *, 'type_info=',type_info(1,:), type_info(2,:)
      do i = 1, ndims_type
        start_index(i) = type_info(2, ndims_type - i + 1)
      end do
    end if
    inquire(file = fname, exist = isExist)
    ! ~~~~~~~~~~~~ open or create nc
    if (isExist) then
      call check_enc(nf90_open(fname, NF90_NOWRITE, ncid) , "nf90_open")
    else
      print *, "Error, file doesn't exist."
      stop "Stopped"
    end if
    ! ~~~~~~~~~~~~ inquire variable
    call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
    ! ~~~~~~~~~~~~ write val
    if (present(type_info)) then
      call check_enc( nf90_get_var(ncid, vid, val, start=start_index), "in nf90_get_var, "//vname)
    else
      call check_enc( nf90_get_var(ncid, vid, val), "in nf90_get_var, "//vname)
    end if
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    return
  end subroutine easyI_real8_scalar
Subroutine easyO_complex4_scalar(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    Complex(kind=4),intent(in) :: val
    character(*),intent(in) :: fname, vname
    real(kind=4) :: val_re, val_im
    integer,intent(in),optional :: type_info(:, :)
    val_re = real(val)
    val_im = aimag(val)
    if (present(type_info)) then
      call easyO_real4_scalar(fname, trim(vname)//'.real', val_re, type_info)
      call easyO_real4_scalar(fname, trim(vname)//'.imag', val_im, type_info)
    else
      call easyO_real4_scalar(fname, trim(vname)//'.real', val_re)
      call easyO_real4_scalar(fname, trim(vname)//'.imag', val_im)
    end if
    return
end subroutine easyO_complex4_scalar
Subroutine easyI_complex4_scalar(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    Complex(kind=4),intent(out) :: val
    character(*),intent(in) :: fname, vname
    real(kind=4) :: val_re, val_im
    integer,intent(in),optional :: type_info(:, :)
    val_re = real(val)
    val_im = aimag(val)
    if (present(type_info)) then
      call easyI_real4_scalar(fname, trim(vname)//'.real', val_re, type_info)
      call easyI_real4_scalar(fname, trim(vname)//'.imag', val_im, type_info)
    else
      call easyI_real4_scalar(fname, trim(vname)//'.real', val_re)
      call easyI_real4_scalar(fname, trim(vname)//'.imag', val_im)
    end if
    val = cmplx(val_re, val_im)
    return
end subroutine easyI_complex4_scalar
Subroutine easyO_complex8_scalar(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  Complex(kind=8),intent(in) :: val
  character(*),intent(in) :: fname, vname
  real(kind=8) :: val_re, val_im
  integer,intent(in),optional :: type_info(:, :)
  val_re = real(val)
  val_im = aimag(val)
  if (present(type_info)) then
    call easyO_real8_scalar(fname, trim(vname)//'.real', val_re, type_info)
    call easyO_real8_scalar(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyO_real8_scalar(fname, trim(vname)//'.real', val_re)
    call easyO_real8_scalar(fname, trim(vname)//'.imag', val_im)
  end if
  return
end subroutine easyO_complex8_scalar
Subroutine easyI_complex8_scalar(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  Complex(kind=8),intent(out) :: val
  character(*),intent(in) :: fname, vname
  real(kind=8) :: val_re, val_im
  integer,intent(in),optional :: type_info(:, :)
  val_re = real(val)
  val_im = aimag(val)
  if (present(type_info)) then
    call easyI_real8_scalar(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real8_scalar(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real8_scalar(fname, trim(vname)//'.real', val_re)
    call easyI_real8_scalar(fname, trim(vname)//'.imag', val_im)
  end if
  val = cmplx(val_re, val_im, 8)
  return
end subroutine easyI_complex8_scalar
Subroutine easyO_complex4_1d(fname, vname, val, type_info, type_ele_alone)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),intent(in) :: val(:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  integer,intent(in),optional :: type_ele_alone
  real(kind=4) :: val_re(size(val)), val_im(size(val))
  val_re = real(val)
  val_im = aimag(val)
  if (present(type_info)) then
    if (present(type_ele_alone)) then
      call easyO_real4_1d(fname, trim(vname)//'.real', val_re, type_info, type_ele_alone)
      call easyO_real4_1d(fname, trim(vname)//'.imag', val_re, type_info, type_ele_alone)
    else
      call easyO_real4_1d(fname, trim(vname)//'.real', val_re, type_info)
      call easyO_real4_1d(fname, trim(vname)//'.imag', val_im, type_info)
    end if
  else
    call easyO_real4_1d(fname, trim(vname)//'.real', val_re)
    call easyO_real4_1d(fname, trim(vname)//'.imag', val_im)
  end if
  return
end subroutine easyO_complex4_1d
Subroutine easyOA_complex4_1d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),allocatable,intent(in) :: val(:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=4),allocatable :: val_im(:), val_re(:)
  if (allocated(val) .and. size(val) .gt. 0) then
    allocate(val_re, source=real(val))
    allocate(val_im, source=aimag(val))
    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size( &
    shape(val))],order=[2,1])) ! 
    if (present(type_info)) then
        call easyO_real4_1d(fname, trim(vname)//'.real', val_re, type_info, 1)
        call easyO_real4_1d(fname, trim(vname)//'.imag', val_im, type_info, 1)
    else
      call easyO_real4_1d(fname, trim(vname)//'.real', val_re)
      call easyO_real4_1d(fname, trim(vname)//'.imag', val_im)
    end if
    deallocate(val_re)
    deallocate(val_im)
  end if
  return
end subroutine easyOA_complex4_1d
Subroutine easyI_complex4_1d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),intent(out) :: val(:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=4) :: val_re(size(val)), val_im(size(val))
  if (present(type_info)) then
    call easyI_real4_1d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real4_1d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real4_1d(fname, trim(vname)//'.real', val_re)
    call easyI_real4_1d(fname, trim(vname)//'.imag', val_im)
  end if
  val = cmplx(val_re, val_im)
  return
end subroutine easyI_complex4_1d
Subroutine easyIA_complex4_1d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),allocatable,intent(inout) :: val(:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=4),allocatable :: val_im(:), val_re(:)
  if (.not. allocated(val)) then
    call getDims2_enc(fname, trim(vname)//'.real', dimsT_enc)
    if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
      allocate(dimsT2_enc(2, size(dimsT_enc)))
      call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
      allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1)))
      allocate(val_re(dimsT2_enc(1,1):dimsT2_enc(2,1)))
      allocate(val_im(dimsT2_enc(1,1):dimsT2_enc(2,1)))
      deallocate(dimsT_enc); deallocate(dimsT2_enc)
    else
      allocate(val(dimsT_enc(1)))
      allocate(val_re(dimsT_enc(1)))
      allocate(val_im(dimsT_enc(1)))
      deallocate(dimsT_enc)
    end if
  else
    allocate(val_re, source = real(val))
    allocate(val_im, source = aimag(val))
  end if
  if (present(type_info)) then
    call easyI_real4_1d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real4_1d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real4_1d(fname, trim(vname)//'.real', val_re)
    call easyI_real4_1d(fname, trim(vname)//'.imag', val_im)
  end if
  deallocate(val_re)
  deallocate(val_im)
  return
end subroutine easyIA_complex4_1d
Subroutine easyO_complex4_2d(fname, vname, val, type_info, type_ele_alone)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),intent(in) :: val(:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  integer,intent(in),optional :: type_ele_alone
  real(kind=4) :: val_re(size(val(:,1)), size(val(1,:))), val_im(size(val(:,1)), size(val(1,:)))
  val_re = real(val)
  val_im = aimag(val)
  if (present(type_info)) then
    if (present(type_ele_alone)) then
      call easyO_real4_2d(fname, trim(vname)//'.real', val_re, type_info, type_ele_alone)
      call easyO_real4_2d(fname, trim(vname)//'.imag', val_re, type_info, type_ele_alone)
    else
      call easyO_real4_2d(fname, trim(vname)//'.real', val_re, type_info)
      call easyO_real4_2d(fname, trim(vname)//'.imag', val_im, type_info)
    end if
  else
    call easyO_real4_2d(fname, trim(vname)//'.real', val_re)
    call easyO_real4_2d(fname, trim(vname)//'.imag', val_im)
  end if
  return
end subroutine easyO_complex4_2d
Subroutine easyOA_complex4_2d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),allocatable,intent(in) :: val(:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=4),allocatable :: val_im(:,:), val_re(:,:)
  if (allocated(val) .and. size(val) .gt. 0) then
    allocate(val_re, source=real(val))
    allocate(val_im, source=aimag(val))
    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size( &
    shape(val))],order=[2,1])) ! 
    if (present(type_info)) then
        call easyO_real4_2d(fname, trim(vname)//'.real', val_re, type_info, 1)
        call easyO_real4_2d(fname, trim(vname)//'.imag', val_im, type_info, 1)
    else
      call easyO_real4_2d(fname, trim(vname)//'.real', val_re)
      call easyO_real4_2d(fname, trim(vname)//'.imag', val_im)
    end if
    deallocate(val_re)
    deallocate(val_im)
  end if
  return
end subroutine easyOA_complex4_2d
Subroutine easyI_complex4_2d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),intent(out) :: val(:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=4) :: val_re(size(val(:,1)), size(val(1,:))), val_im(size(val(:,1)), size(val(1,:)))
  if (present(type_info)) then
    call easyI_real4_2d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real4_2d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real4_2d(fname, trim(vname)//'.real', val_re)
    call easyI_real4_2d(fname, trim(vname)//'.imag', val_im)
  end if
  val = cmplx(val_re, val_im)
  return
end subroutine easyI_complex4_2d
Subroutine easyIA_complex4_2d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),allocatable,intent(inout) :: val(:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=4),allocatable :: val_im(:,:), val_re(:,:)
  if (.not. allocated(val)) then
    call getDims2_enc(fname, trim(vname)//'.real', dimsT_enc)
    if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
      allocate(dimsT2_enc(2, size(dimsT_enc)))
      call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
      allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2)))
      allocate(val_re(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2)))
      allocate(val_im(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2)))
      deallocate(dimsT_enc); deallocate(dimsT2_enc)
    else
      allocate(val(dimsT_enc(1),dimsT_enc(2)))
      allocate(val_re(dimsT_enc(1),dimsT_enc(2)))
      allocate(val_im(dimsT_enc(1),dimsT_enc(2)))
      deallocate(dimsT_enc)
    end if
  else
    allocate(val_re, source = real(val))
    allocate(val_im, source = aimag(val))
  end if
  if (present(type_info)) then
    call easyI_real4_2d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real4_2d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real4_2d(fname, trim(vname)//'.real', val_re)
    call easyI_real4_2d(fname, trim(vname)//'.imag', val_im)
  end if
  deallocate(val_re)
  deallocate(val_im)
  return
end subroutine easyIA_complex4_2d
Subroutine easyO_complex4_3d(fname, vname, val, type_info, type_ele_alone)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),intent(in) :: val(:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  integer,intent(in),optional :: type_ele_alone
  real(kind=4) :: val_re(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))), val_im(size(val(:,1,1)), size(val(1,:,1)), size( &
    val(1,1,:))) ! 
  val_re = real(val)
  val_im = aimag(val)
  if (present(type_info)) then
    if (present(type_ele_alone)) then
      call easyO_real4_3d(fname, trim(vname)//'.real', val_re, type_info, type_ele_alone)
      call easyO_real4_3d(fname, trim(vname)//'.imag', val_re, type_info, type_ele_alone)
    else
      call easyO_real4_3d(fname, trim(vname)//'.real', val_re, type_info)
      call easyO_real4_3d(fname, trim(vname)//'.imag', val_im, type_info)
    end if
  else
    call easyO_real4_3d(fname, trim(vname)//'.real', val_re)
    call easyO_real4_3d(fname, trim(vname)//'.imag', val_im)
  end if
  return
end subroutine easyO_complex4_3d
Subroutine easyOA_complex4_3d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),allocatable,intent(in) :: val(:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=4),allocatable :: val_im(:,:,:), val_re(:,:,:)
  if (allocated(val) .and. size(val) .gt. 0) then
    allocate(val_re, source=real(val))
    allocate(val_im, source=aimag(val))
    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size( &
    shape(val))],order=[2,1])) ! 
    if (present(type_info)) then
        call easyO_real4_3d(fname, trim(vname)//'.real', val_re, type_info, 1)
        call easyO_real4_3d(fname, trim(vname)//'.imag', val_im, type_info, 1)
    else
      call easyO_real4_3d(fname, trim(vname)//'.real', val_re)
      call easyO_real4_3d(fname, trim(vname)//'.imag', val_im)
    end if
    deallocate(val_re)
    deallocate(val_im)
  end if
  return
end subroutine easyOA_complex4_3d
Subroutine easyI_complex4_3d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),intent(out) :: val(:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=4) :: val_re(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))), val_im(size(val(:,1,1)), size(val(1,:,1)), size( &
    val(1,1,:))) ! 
  if (present(type_info)) then
    call easyI_real4_3d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real4_3d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real4_3d(fname, trim(vname)//'.real', val_re)
    call easyI_real4_3d(fname, trim(vname)//'.imag', val_im)
  end if
  val = cmplx(val_re, val_im)
  return
end subroutine easyI_complex4_3d
Subroutine easyIA_complex4_3d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),allocatable,intent(inout) :: val(:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=4),allocatable :: val_im(:,:,:), val_re(:,:,:)
  if (.not. allocated(val)) then
    call getDims2_enc(fname, trim(vname)//'.real', dimsT_enc)
    if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
      allocate(dimsT2_enc(2, size(dimsT_enc)))
      call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
      allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3)))
      allocate(val_re(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3)))
      allocate(val_im(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3)))
      deallocate(dimsT_enc); deallocate(dimsT2_enc)
    else
      allocate(val(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3)))
      allocate(val_re(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3)))
      allocate(val_im(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3)))
      deallocate(dimsT_enc)
    end if
  else
    allocate(val_re, source = real(val))
    allocate(val_im, source = aimag(val))
  end if
  if (present(type_info)) then
    call easyI_real4_3d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real4_3d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real4_3d(fname, trim(vname)//'.real', val_re)
    call easyI_real4_3d(fname, trim(vname)//'.imag', val_im)
  end if
  deallocate(val_re)
  deallocate(val_im)
  return
end subroutine easyIA_complex4_3d
Subroutine easyO_complex4_4d(fname, vname, val, type_info, type_ele_alone)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),intent(in) :: val(:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  integer,intent(in),optional :: type_ele_alone
  real(kind=4) :: val_re(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))), val_im(size(val(:,1,1, &
    1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))) ! 
  val_re = real(val)
  val_im = aimag(val)
  if (present(type_info)) then
    if (present(type_ele_alone)) then
      call easyO_real4_4d(fname, trim(vname)//'.real', val_re, type_info, type_ele_alone)
      call easyO_real4_4d(fname, trim(vname)//'.imag', val_re, type_info, type_ele_alone)
    else
      call easyO_real4_4d(fname, trim(vname)//'.real', val_re, type_info)
      call easyO_real4_4d(fname, trim(vname)//'.imag', val_im, type_info)
    end if
  else
    call easyO_real4_4d(fname, trim(vname)//'.real', val_re)
    call easyO_real4_4d(fname, trim(vname)//'.imag', val_im)
  end if
  return
end subroutine easyO_complex4_4d
Subroutine easyOA_complex4_4d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),allocatable,intent(in) :: val(:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=4),allocatable :: val_im(:,:,:,:), val_re(:,:,:,:)
  if (allocated(val) .and. size(val) .gt. 0) then
    allocate(val_re, source=real(val))
    allocate(val_im, source=aimag(val))
    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size( &
    shape(val))],order=[2,1])) ! 
    if (present(type_info)) then
        call easyO_real4_4d(fname, trim(vname)//'.real', val_re, type_info, 1)
        call easyO_real4_4d(fname, trim(vname)//'.imag', val_im, type_info, 1)
    else
      call easyO_real4_4d(fname, trim(vname)//'.real', val_re)
      call easyO_real4_4d(fname, trim(vname)//'.imag', val_im)
    end if
    deallocate(val_re)
    deallocate(val_im)
  end if
  return
end subroutine easyOA_complex4_4d
Subroutine easyI_complex4_4d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),intent(out) :: val(:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=4) :: val_re(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))), val_im(size(val(:,1,1, &
    1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))) ! 
  if (present(type_info)) then
    call easyI_real4_4d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real4_4d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real4_4d(fname, trim(vname)//'.real', val_re)
    call easyI_real4_4d(fname, trim(vname)//'.imag', val_im)
  end if
  val = cmplx(val_re, val_im)
  return
end subroutine easyI_complex4_4d
Subroutine easyIA_complex4_4d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),allocatable,intent(inout) :: val(:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=4),allocatable :: val_im(:,:,:,:), val_re(:,:,:,:)
  if (.not. allocated(val)) then
    call getDims2_enc(fname, trim(vname)//'.real', dimsT_enc)
    if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
      allocate(dimsT2_enc(2, size(dimsT_enc)))
      call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
      allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1, &
    4):dimsT2_enc(2,4))) ! 
      allocate(val_re(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4))) ! 
      allocate(val_im(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4))) ! 
      deallocate(dimsT_enc); deallocate(dimsT2_enc)
    else
      allocate(val(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4)))
      allocate(val_re(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4)))
      allocate(val_im(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4)))
      deallocate(dimsT_enc)
    end if
  else
    allocate(val_re, source = real(val))
    allocate(val_im, source = aimag(val))
  end if
  if (present(type_info)) then
    call easyI_real4_4d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real4_4d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real4_4d(fname, trim(vname)//'.real', val_re)
    call easyI_real4_4d(fname, trim(vname)//'.imag', val_im)
  end if
  deallocate(val_re)
  deallocate(val_im)
  return
end subroutine easyIA_complex4_4d
Subroutine easyO_complex4_5d(fname, vname, val, type_info, type_ele_alone)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),intent(in) :: val(:,:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  integer,intent(in),optional :: type_ele_alone
  real(kind=4) :: val_re(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1, &
    1,:))), val_im(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))) ! 
  val_re = real(val)
  val_im = aimag(val)
  if (present(type_info)) then
    if (present(type_ele_alone)) then
      call easyO_real4_5d(fname, trim(vname)//'.real', val_re, type_info, type_ele_alone)
      call easyO_real4_5d(fname, trim(vname)//'.imag', val_re, type_info, type_ele_alone)
    else
      call easyO_real4_5d(fname, trim(vname)//'.real', val_re, type_info)
      call easyO_real4_5d(fname, trim(vname)//'.imag', val_im, type_info)
    end if
  else
    call easyO_real4_5d(fname, trim(vname)//'.real', val_re)
    call easyO_real4_5d(fname, trim(vname)//'.imag', val_im)
  end if
  return
end subroutine easyO_complex4_5d
Subroutine easyOA_complex4_5d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),allocatable,intent(in) :: val(:,:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=4),allocatable :: val_im(:,:,:,:,:), val_re(:,:,:,:,:)
  if (allocated(val) .and. size(val) .gt. 0) then
    allocate(val_re, source=real(val))
    allocate(val_im, source=aimag(val))
    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size( &
    shape(val))],order=[2,1])) ! 
    if (present(type_info)) then
        call easyO_real4_5d(fname, trim(vname)//'.real', val_re, type_info, 1)
        call easyO_real4_5d(fname, trim(vname)//'.imag', val_im, type_info, 1)
    else
      call easyO_real4_5d(fname, trim(vname)//'.real', val_re)
      call easyO_real4_5d(fname, trim(vname)//'.imag', val_im)
    end if
    deallocate(val_re)
    deallocate(val_im)
  end if
  return
end subroutine easyOA_complex4_5d
Subroutine easyI_complex4_5d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),intent(out) :: val(:,:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=4) :: val_re(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1, &
    1,:))), val_im(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))) ! 
  if (present(type_info)) then
    call easyI_real4_5d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real4_5d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real4_5d(fname, trim(vname)//'.real', val_re)
    call easyI_real4_5d(fname, trim(vname)//'.imag', val_im)
  end if
  val = cmplx(val_re, val_im)
  return
end subroutine easyI_complex4_5d
Subroutine easyIA_complex4_5d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),allocatable,intent(inout) :: val(:,:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=4),allocatable :: val_im(:,:,:,:,:), val_re(:,:,:,:,:)
  if (.not. allocated(val)) then
    call getDims2_enc(fname, trim(vname)//'.real', dimsT_enc)
    if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
      allocate(dimsT2_enc(2, size(dimsT_enc)))
      call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
      allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1, &
    4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5))) ! 
      allocate(val_re(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5))) ! 
      allocate(val_im(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5))) ! 
      deallocate(dimsT_enc); deallocate(dimsT2_enc)
    else
      allocate(val(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5)))
      allocate(val_re(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5)))
      allocate(val_im(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5)))
      deallocate(dimsT_enc)
    end if
  else
    allocate(val_re, source = real(val))
    allocate(val_im, source = aimag(val))
  end if
  if (present(type_info)) then
    call easyI_real4_5d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real4_5d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real4_5d(fname, trim(vname)//'.real', val_re)
    call easyI_real4_5d(fname, trim(vname)//'.imag', val_im)
  end if
  deallocate(val_re)
  deallocate(val_im)
  return
end subroutine easyIA_complex4_5d
Subroutine easyO_complex4_6d(fname, vname, val, type_info, type_ele_alone)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),intent(in) :: val(:,:,:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  integer,intent(in),optional :: type_ele_alone
  real(kind=4) :: val_re(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size( &
    val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))), val_im(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), &
     size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))) ! 
  val_re = real(val)
  val_im = aimag(val)
  if (present(type_info)) then
    if (present(type_ele_alone)) then
      call easyO_real4_6d(fname, trim(vname)//'.real', val_re, type_info, type_ele_alone)
      call easyO_real4_6d(fname, trim(vname)//'.imag', val_re, type_info, type_ele_alone)
    else
      call easyO_real4_6d(fname, trim(vname)//'.real', val_re, type_info)
      call easyO_real4_6d(fname, trim(vname)//'.imag', val_im, type_info)
    end if
  else
    call easyO_real4_6d(fname, trim(vname)//'.real', val_re)
    call easyO_real4_6d(fname, trim(vname)//'.imag', val_im)
  end if
  return
end subroutine easyO_complex4_6d
Subroutine easyOA_complex4_6d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),allocatable,intent(in) :: val(:,:,:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=4),allocatable :: val_im(:,:,:,:,:,:), val_re(:,:,:,:,:,:)
  if (allocated(val) .and. size(val) .gt. 0) then
    allocate(val_re, source=real(val))
    allocate(val_im, source=aimag(val))
    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size( &
    shape(val))],order=[2,1])) ! 
    if (present(type_info)) then
        call easyO_real4_6d(fname, trim(vname)//'.real', val_re, type_info, 1)
        call easyO_real4_6d(fname, trim(vname)//'.imag', val_im, type_info, 1)
    else
      call easyO_real4_6d(fname, trim(vname)//'.real', val_re)
      call easyO_real4_6d(fname, trim(vname)//'.imag', val_im)
    end if
    deallocate(val_re)
    deallocate(val_im)
  end if
  return
end subroutine easyOA_complex4_6d
Subroutine easyI_complex4_6d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),intent(out) :: val(:,:,:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=4) :: val_re(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size( &
    val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))), val_im(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), &
     size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))) ! 
  if (present(type_info)) then
    call easyI_real4_6d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real4_6d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real4_6d(fname, trim(vname)//'.real', val_re)
    call easyI_real4_6d(fname, trim(vname)//'.imag', val_im)
  end if
  val = cmplx(val_re, val_im)
  return
end subroutine easyI_complex4_6d
Subroutine easyIA_complex4_6d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),allocatable,intent(inout) :: val(:,:,:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=4),allocatable :: val_im(:,:,:,:,:,:), val_re(:,:,:,:,:,:)
  if (.not. allocated(val)) then
    call getDims2_enc(fname, trim(vname)//'.real', dimsT_enc)
    if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
      allocate(dimsT2_enc(2, size(dimsT_enc)))
      call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
      allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1, &
    4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6))) ! 
      allocate(val_re(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6))) ! 
      allocate(val_im(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6))) ! 
      deallocate(dimsT_enc); deallocate(dimsT2_enc)
    else
      allocate(val(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6)))
      allocate(val_re(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6)))
      allocate(val_im(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6)))
      deallocate(dimsT_enc)
    end if
  else
    allocate(val_re, source = real(val))
    allocate(val_im, source = aimag(val))
  end if
  if (present(type_info)) then
    call easyI_real4_6d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real4_6d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real4_6d(fname, trim(vname)//'.real', val_re)
    call easyI_real4_6d(fname, trim(vname)//'.imag', val_im)
  end if
  deallocate(val_re)
  deallocate(val_im)
  return
end subroutine easyIA_complex4_6d
Subroutine easyO_complex4_7d(fname, vname, val, type_info, type_ele_alone)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),intent(in) :: val(:,:,:,:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  integer,intent(in),optional :: type_ele_alone
  real(kind=4) :: val_re(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), &
     size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:))), val_im(size(val(:,1,1,1,1,1,1)), size(val(1, &
    :,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size( &
    val(1,1,1,1,1,1,:))) ! 
  val_re = real(val)
  val_im = aimag(val)
  if (present(type_info)) then
    if (present(type_ele_alone)) then
      call easyO_real4_7d(fname, trim(vname)//'.real', val_re, type_info, type_ele_alone)
      call easyO_real4_7d(fname, trim(vname)//'.imag', val_re, type_info, type_ele_alone)
    else
      call easyO_real4_7d(fname, trim(vname)//'.real', val_re, type_info)
      call easyO_real4_7d(fname, trim(vname)//'.imag', val_im, type_info)
    end if
  else
    call easyO_real4_7d(fname, trim(vname)//'.real', val_re)
    call easyO_real4_7d(fname, trim(vname)//'.imag', val_im)
  end if
  return
end subroutine easyO_complex4_7d
Subroutine easyOA_complex4_7d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),allocatable,intent(in) :: val(:,:,:,:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=4),allocatable :: val_im(:,:,:,:,:,:,:), val_re(:,:,:,:,:,:,:)
  if (allocated(val) .and. size(val) .gt. 0) then
    allocate(val_re, source=real(val))
    allocate(val_im, source=aimag(val))
    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size( &
    shape(val))],order=[2,1])) ! 
    if (present(type_info)) then
        call easyO_real4_7d(fname, trim(vname)//'.real', val_re, type_info, 1)
        call easyO_real4_7d(fname, trim(vname)//'.imag', val_im, type_info, 1)
    else
      call easyO_real4_7d(fname, trim(vname)//'.real', val_re)
      call easyO_real4_7d(fname, trim(vname)//'.imag', val_im)
    end if
    deallocate(val_re)
    deallocate(val_im)
  end if
  return
end subroutine easyOA_complex4_7d
Subroutine easyI_complex4_7d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),intent(out) :: val(:,:,:,:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=4) :: val_re(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), &
     size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:))), val_im(size(val(:,1,1,1,1,1,1)), size(val(1, &
    :,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size( &
    val(1,1,1,1,1,1,:))) ! 
  if (present(type_info)) then
    call easyI_real4_7d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real4_7d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real4_7d(fname, trim(vname)//'.real', val_re)
    call easyI_real4_7d(fname, trim(vname)//'.imag', val_im)
  end if
  val = cmplx(val_re, val_im)
  return
end subroutine easyI_complex4_7d
Subroutine easyIA_complex4_7d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=4),allocatable,intent(inout) :: val(:,:,:,:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=4),allocatable :: val_im(:,:,:,:,:,:,:), val_re(:,:,:,:,:,:,:)
  if (.not. allocated(val)) then
    call getDims2_enc(fname, trim(vname)//'.real', dimsT_enc)
    if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
      allocate(dimsT2_enc(2, size(dimsT_enc)))
      call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
      allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1, &
    4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6),dimsT2_enc(1,7):dimsT2_enc(2,7))) ! 
      allocate(val_re(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6),dimsT2_enc(1,7):dimsT2_enc( &
    2,7))) ! 
      allocate(val_im(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6),dimsT2_enc(1,7):dimsT2_enc( &
    2,7))) ! 
      deallocate(dimsT_enc); deallocate(dimsT2_enc)
    else
      allocate(val(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6),dimsT_enc(7)))
      allocate(val_re(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6),dimsT_enc(7)))
      allocate(val_im(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6),dimsT_enc(7)))
      deallocate(dimsT_enc)
    end if
  else
    allocate(val_re, source = real(val))
    allocate(val_im, source = aimag(val))
  end if
  if (present(type_info)) then
    call easyI_real4_7d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real4_7d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real4_7d(fname, trim(vname)//'.real', val_re)
    call easyI_real4_7d(fname, trim(vname)//'.imag', val_im)
  end if
  deallocate(val_re)
  deallocate(val_im)
  return
end subroutine easyIA_complex4_7d
Subroutine easyO_complex8_1d(fname, vname, val, type_info, type_ele_alone)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),intent(in) :: val(:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  integer,intent(in),optional :: type_ele_alone
  real(kind=8) :: val_re(size(val)), val_im(size(val))
  val_re = real(val)
  val_im = aimag(val)
  if (present(type_info)) then
    if (present(type_ele_alone)) then
      call easyO_real8_1d(fname, trim(vname)//'.real', val_re, type_info, type_ele_alone)
      call easyO_real8_1d(fname, trim(vname)//'.imag', val_re, type_info, type_ele_alone)
    else
      call easyO_real8_1d(fname, trim(vname)//'.real', val_re, type_info)
      call easyO_real8_1d(fname, trim(vname)//'.imag', val_im, type_info)
    end if
  else
    call easyO_real8_1d(fname, trim(vname)//'.real', val_re)
    call easyO_real8_1d(fname, trim(vname)//'.imag', val_im)
  end if
  return
end subroutine easyO_complex8_1d
Subroutine easyOA_complex8_1d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),allocatable,intent(in) :: val(:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=8),allocatable :: val_im(:), val_re(:)
  if (allocated(val) .and. size(val) .gt. 0) then
    allocate(val_re, source=real(val))
    allocate(val_im, source=aimag(val))
    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size( &
    shape(val))],order=[2,1])) ! 
    if (present(type_info)) then
        call easyO_real8_1d(fname, trim(vname)//'.real', val_re, type_info, 1)
        call easyO_real8_1d(fname, trim(vname)//'.imag', val_im, type_info, 1)
    else
      call easyO_real8_1d(fname, trim(vname)//'.real', val_re)
      call easyO_real8_1d(fname, trim(vname)//'.imag', val_im)
    end if
    deallocate(val_im)
    deallocate(val_re)
  end if
  return
end subroutine easyOA_complex8_1d
Subroutine easyI_complex8_1d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),intent(out) :: val(:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=8) :: val_re(size(val)), val_im(size(val))
  if (present(type_info)) then
    call easyI_real8_1d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real8_1d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real8_1d(fname, trim(vname)//'.real', val_re)
    call easyI_real8_1d(fname, trim(vname)//'.imag', val_im)
  end if
  val = cmplx(val_re, val_im, 8)
  return
end subroutine easyI_complex8_1d
Subroutine easyIA_complex8_1d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),allocatable,intent(inout) :: val(:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=8),allocatable :: val_im(:), val_re(:)
  if (.not. allocated(val)) then
    call getDims2_enc(fname, trim(vname)//'.real', dimsT_enc)
    if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
      allocate(dimsT2_enc(2, size(dimsT_enc)))
      call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
      allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1)))
      allocate(val_re(dimsT2_enc(1,1):dimsT2_enc(2,1)))
      allocate(val_im(dimsT2_enc(1,1):dimsT2_enc(2,1)))
      deallocate(dimsT_enc); deallocate(dimsT2_enc)
    else
      allocate(val(dimsT_enc(1)))
      allocate(val_re(dimsT_enc(1)))
      allocate(val_im(dimsT_enc(1)))
      deallocate(dimsT_enc)
    end if
  else
    allocate(val_re, source=real(val))
    allocate(val_im, source=aimag(val))
  end if
  if (present(type_info)) then
    call easyI_real8_1d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real8_1d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real8_1d(fname, trim(vname)//'.real', val_re)
    call easyI_real8_1d(fname, trim(vname)//'.imag', val_im)
  end if
  val = cmplx(val_re, val_im, 8)
  deallocate(val_im)
  deallocate(val_re)
  return
end subroutine easyIA_complex8_1d
Subroutine easyO_complex8_2d(fname, vname, val, type_info, type_ele_alone)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),intent(in) :: val(:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  integer,intent(in),optional :: type_ele_alone
  real(kind=8) :: val_re(size(val(:,1)), size(val(1,:))), val_im(size(val(:,1)), size(val(1,:)))
  val_re = real(val)
  val_im = aimag(val)
  if (present(type_info)) then
    if (present(type_ele_alone)) then
      call easyO_real8_2d(fname, trim(vname)//'.real', val_re, type_info, type_ele_alone)
      call easyO_real8_2d(fname, trim(vname)//'.imag', val_re, type_info, type_ele_alone)
    else
      call easyO_real8_2d(fname, trim(vname)//'.real', val_re, type_info)
      call easyO_real8_2d(fname, trim(vname)//'.imag', val_im, type_info)
    end if
  else
    call easyO_real8_2d(fname, trim(vname)//'.real', val_re)
    call easyO_real8_2d(fname, trim(vname)//'.imag', val_im)
  end if
  return
end subroutine easyO_complex8_2d
Subroutine easyOA_complex8_2d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),allocatable,intent(in) :: val(:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=8),allocatable :: val_im(:,:), val_re(:,:)
  if (allocated(val) .and. size(val) .gt. 0) then
    allocate(val_re, source=real(val))
    allocate(val_im, source=aimag(val))
    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size( &
    shape(val))],order=[2,1])) ! 
    if (present(type_info)) then
        call easyO_real8_2d(fname, trim(vname)//'.real', val_re, type_info, 1)
        call easyO_real8_2d(fname, trim(vname)//'.imag', val_im, type_info, 1)
    else
      call easyO_real8_2d(fname, trim(vname)//'.real', val_re)
      call easyO_real8_2d(fname, trim(vname)//'.imag', val_im)
    end if
    deallocate(val_im)
    deallocate(val_re)
  end if
  return
end subroutine easyOA_complex8_2d
Subroutine easyI_complex8_2d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),intent(out) :: val(:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=8) :: val_re(size(val(:,1)), size(val(1,:))), val_im(size(val(:,1)), size(val(1,:)))
  if (present(type_info)) then
    call easyI_real8_2d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real8_2d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real8_2d(fname, trim(vname)//'.real', val_re)
    call easyI_real8_2d(fname, trim(vname)//'.imag', val_im)
  end if
  val = cmplx(val_re, val_im, 8)
  return
end subroutine easyI_complex8_2d
Subroutine easyIA_complex8_2d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),allocatable,intent(inout) :: val(:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=8),allocatable :: val_im(:,:), val_re(:,:)
  if (.not. allocated(val)) then
    call getDims2_enc(fname, trim(vname)//'.real', dimsT_enc)
    if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
      allocate(dimsT2_enc(2, size(dimsT_enc)))
      call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
      allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2)))
      allocate(val_re(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2)))
      allocate(val_im(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2)))
      deallocate(dimsT_enc); deallocate(dimsT2_enc)
    else
      allocate(val(dimsT_enc(1),dimsT_enc(2)))
      allocate(val_re(dimsT_enc(1),dimsT_enc(2)))
      allocate(val_im(dimsT_enc(1),dimsT_enc(2)))
      deallocate(dimsT_enc)
    end if
  else
    allocate(val_re, source=real(val))
    allocate(val_im, source=aimag(val))
  end if
  if (present(type_info)) then
    call easyI_real8_2d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real8_2d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real8_2d(fname, trim(vname)//'.real', val_re)
    call easyI_real8_2d(fname, trim(vname)//'.imag', val_im)
  end if
  val = cmplx(val_re, val_im, 8)
  deallocate(val_im)
  deallocate(val_re)
  return
end subroutine easyIA_complex8_2d
Subroutine easyO_complex8_3d(fname, vname, val, type_info, type_ele_alone)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),intent(in) :: val(:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  integer,intent(in),optional :: type_ele_alone
  real(kind=8) :: val_re(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))), val_im(size(val(:,1,1)), size(val(1,:,1)), size( &
    val(1,1,:))) ! 
  val_re = real(val)
  val_im = aimag(val)
  if (present(type_info)) then
    if (present(type_ele_alone)) then
      call easyO_real8_3d(fname, trim(vname)//'.real', val_re, type_info, type_ele_alone)
      call easyO_real8_3d(fname, trim(vname)//'.imag', val_re, type_info, type_ele_alone)
    else
      call easyO_real8_3d(fname, trim(vname)//'.real', val_re, type_info)
      call easyO_real8_3d(fname, trim(vname)//'.imag', val_im, type_info)
    end if
  else
    call easyO_real8_3d(fname, trim(vname)//'.real', val_re)
    call easyO_real8_3d(fname, trim(vname)//'.imag', val_im)
  end if
  return
end subroutine easyO_complex8_3d
Subroutine easyOA_complex8_3d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),allocatable,intent(in) :: val(:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=8),allocatable :: val_im(:,:,:), val_re(:,:,:)
  if (allocated(val) .and. size(val) .gt. 0) then
    allocate(val_re, source=real(val))
    allocate(val_im, source=aimag(val))
    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size( &
    shape(val))],order=[2,1])) ! 
    if (present(type_info)) then
        call easyO_real8_3d(fname, trim(vname)//'.real', val_re, type_info, 1)
        call easyO_real8_3d(fname, trim(vname)//'.imag', val_im, type_info, 1)
    else
      call easyO_real8_3d(fname, trim(vname)//'.real', val_re)
      call easyO_real8_3d(fname, trim(vname)//'.imag', val_im)
    end if
    deallocate(val_im)
    deallocate(val_re)
  end if
  return
end subroutine easyOA_complex8_3d
Subroutine easyI_complex8_3d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),intent(out) :: val(:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=8) :: val_re(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))), val_im(size(val(:,1,1)), size(val(1,:,1)), size( &
    val(1,1,:))) ! 
  if (present(type_info)) then
    call easyI_real8_3d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real8_3d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real8_3d(fname, trim(vname)//'.real', val_re)
    call easyI_real8_3d(fname, trim(vname)//'.imag', val_im)
  end if
  val = cmplx(val_re, val_im, 8)
  return
end subroutine easyI_complex8_3d
Subroutine easyIA_complex8_3d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),allocatable,intent(inout) :: val(:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=8),allocatable :: val_im(:,:,:), val_re(:,:,:)
  if (.not. allocated(val)) then
    call getDims2_enc(fname, trim(vname)//'.real', dimsT_enc)
    if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
      allocate(dimsT2_enc(2, size(dimsT_enc)))
      call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
      allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3)))
      allocate(val_re(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3)))
      allocate(val_im(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3)))
      deallocate(dimsT_enc); deallocate(dimsT2_enc)
    else
      allocate(val(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3)))
      allocate(val_re(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3)))
      allocate(val_im(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3)))
      deallocate(dimsT_enc)
    end if
  else
    allocate(val_re, source=real(val))
    allocate(val_im, source=aimag(val))
  end if
  if (present(type_info)) then
    call easyI_real8_3d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real8_3d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real8_3d(fname, trim(vname)//'.real', val_re)
    call easyI_real8_3d(fname, trim(vname)//'.imag', val_im)
  end if
  val = cmplx(val_re, val_im, 8)
  deallocate(val_im)
  deallocate(val_re)
  return
end subroutine easyIA_complex8_3d
Subroutine easyO_complex8_4d(fname, vname, val, type_info, type_ele_alone)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),intent(in) :: val(:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  integer,intent(in),optional :: type_ele_alone
  real(kind=8) :: val_re(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))), val_im(size(val(:,1,1, &
    1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))) ! 
  val_re = real(val)
  val_im = aimag(val)
  if (present(type_info)) then
    if (present(type_ele_alone)) then
      call easyO_real8_4d(fname, trim(vname)//'.real', val_re, type_info, type_ele_alone)
      call easyO_real8_4d(fname, trim(vname)//'.imag', val_re, type_info, type_ele_alone)
    else
      call easyO_real8_4d(fname, trim(vname)//'.real', val_re, type_info)
      call easyO_real8_4d(fname, trim(vname)//'.imag', val_im, type_info)
    end if
  else
    call easyO_real8_4d(fname, trim(vname)//'.real', val_re)
    call easyO_real8_4d(fname, trim(vname)//'.imag', val_im)
  end if
  return
end subroutine easyO_complex8_4d
Subroutine easyOA_complex8_4d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),allocatable,intent(in) :: val(:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=8),allocatable :: val_im(:,:,:,:), val_re(:,:,:,:)
  if (allocated(val) .and. size(val) .gt. 0) then
    allocate(val_re, source=real(val))
    allocate(val_im, source=aimag(val))
    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size( &
    shape(val))],order=[2,1])) ! 
    if (present(type_info)) then
        call easyO_real8_4d(fname, trim(vname)//'.real', val_re, type_info, 1)
        call easyO_real8_4d(fname, trim(vname)//'.imag', val_im, type_info, 1)
    else
      call easyO_real8_4d(fname, trim(vname)//'.real', val_re)
      call easyO_real8_4d(fname, trim(vname)//'.imag', val_im)
    end if
    deallocate(val_im)
    deallocate(val_re)
  end if
  return
end subroutine easyOA_complex8_4d
Subroutine easyI_complex8_4d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),intent(out) :: val(:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=8) :: val_re(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))), val_im(size(val(:,1,1, &
    1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))) ! 
  if (present(type_info)) then
    call easyI_real8_4d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real8_4d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real8_4d(fname, trim(vname)//'.real', val_re)
    call easyI_real8_4d(fname, trim(vname)//'.imag', val_im)
  end if
  val = cmplx(val_re, val_im, 8)
  return
end subroutine easyI_complex8_4d
Subroutine easyIA_complex8_4d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),allocatable,intent(inout) :: val(:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=8),allocatable :: val_im(:,:,:,:), val_re(:,:,:,:)
  if (.not. allocated(val)) then
    call getDims2_enc(fname, trim(vname)//'.real', dimsT_enc)
    if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
      allocate(dimsT2_enc(2, size(dimsT_enc)))
      call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
      allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1, &
    4):dimsT2_enc(2,4))) ! 
      allocate(val_re(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4))) ! 
      allocate(val_im(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4))) ! 
      deallocate(dimsT_enc); deallocate(dimsT2_enc)
    else
      allocate(val(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4)))
      allocate(val_re(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4)))
      allocate(val_im(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4)))
      deallocate(dimsT_enc)
    end if
  else
    allocate(val_re, source=real(val))
    allocate(val_im, source=aimag(val))
  end if
  if (present(type_info)) then
    call easyI_real8_4d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real8_4d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real8_4d(fname, trim(vname)//'.real', val_re)
    call easyI_real8_4d(fname, trim(vname)//'.imag', val_im)
  end if
  val = cmplx(val_re, val_im, 8)
  deallocate(val_im)
  deallocate(val_re)
  return
end subroutine easyIA_complex8_4d
Subroutine easyO_complex8_5d(fname, vname, val, type_info, type_ele_alone)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),intent(in) :: val(:,:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  integer,intent(in),optional :: type_ele_alone
  real(kind=8) :: val_re(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1, &
    1,:))), val_im(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))) ! 
  val_re = real(val)
  val_im = aimag(val)
  if (present(type_info)) then
    if (present(type_ele_alone)) then
      call easyO_real8_5d(fname, trim(vname)//'.real', val_re, type_info, type_ele_alone)
      call easyO_real8_5d(fname, trim(vname)//'.imag', val_re, type_info, type_ele_alone)
    else
      call easyO_real8_5d(fname, trim(vname)//'.real', val_re, type_info)
      call easyO_real8_5d(fname, trim(vname)//'.imag', val_im, type_info)
    end if
  else
    call easyO_real8_5d(fname, trim(vname)//'.real', val_re)
    call easyO_real8_5d(fname, trim(vname)//'.imag', val_im)
  end if
  return
end subroutine easyO_complex8_5d
Subroutine easyOA_complex8_5d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),allocatable,intent(in) :: val(:,:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=8),allocatable :: val_im(:,:,:,:,:), val_re(:,:,:,:,:)
  if (allocated(val) .and. size(val) .gt. 0) then
    allocate(val_re, source=real(val))
    allocate(val_im, source=aimag(val))
    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size( &
    shape(val))],order=[2,1])) ! 
    if (present(type_info)) then
        call easyO_real8_5d(fname, trim(vname)//'.real', val_re, type_info, 1)
        call easyO_real8_5d(fname, trim(vname)//'.imag', val_im, type_info, 1)
    else
      call easyO_real8_5d(fname, trim(vname)//'.real', val_re)
      call easyO_real8_5d(fname, trim(vname)//'.imag', val_im)
    end if
    deallocate(val_im)
    deallocate(val_re)
  end if
  return
end subroutine easyOA_complex8_5d
Subroutine easyI_complex8_5d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),intent(out) :: val(:,:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=8) :: val_re(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1, &
    1,:))), val_im(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))) ! 
  if (present(type_info)) then
    call easyI_real8_5d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real8_5d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real8_5d(fname, trim(vname)//'.real', val_re)
    call easyI_real8_5d(fname, trim(vname)//'.imag', val_im)
  end if
  val = cmplx(val_re, val_im, 8)
  return
end subroutine easyI_complex8_5d
Subroutine easyIA_complex8_5d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),allocatable,intent(inout) :: val(:,:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=8),allocatable :: val_im(:,:,:,:,:), val_re(:,:,:,:,:)
  if (.not. allocated(val)) then
    call getDims2_enc(fname, trim(vname)//'.real', dimsT_enc)
    if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
      allocate(dimsT2_enc(2, size(dimsT_enc)))
      call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
      allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1, &
    4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5))) ! 
      allocate(val_re(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5))) ! 
      allocate(val_im(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5))) ! 
      deallocate(dimsT_enc); deallocate(dimsT2_enc)
    else
      allocate(val(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5)))
      allocate(val_re(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5)))
      allocate(val_im(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5)))
      deallocate(dimsT_enc)
    end if
  else
    allocate(val_re, source=real(val))
    allocate(val_im, source=aimag(val))
  end if
  if (present(type_info)) then
    call easyI_real8_5d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real8_5d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real8_5d(fname, trim(vname)//'.real', val_re)
    call easyI_real8_5d(fname, trim(vname)//'.imag', val_im)
  end if
  val = cmplx(val_re, val_im, 8)
  deallocate(val_im)
  deallocate(val_re)
  return
end subroutine easyIA_complex8_5d
Subroutine easyO_complex8_6d(fname, vname, val, type_info, type_ele_alone)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),intent(in) :: val(:,:,:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  integer,intent(in),optional :: type_ele_alone
  real(kind=8) :: val_re(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size( &
    val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))), val_im(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), &
     size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))) ! 
  val_re = real(val)
  val_im = aimag(val)
  if (present(type_info)) then
    if (present(type_ele_alone)) then
      call easyO_real8_6d(fname, trim(vname)//'.real', val_re, type_info, type_ele_alone)
      call easyO_real8_6d(fname, trim(vname)//'.imag', val_re, type_info, type_ele_alone)
    else
      call easyO_real8_6d(fname, trim(vname)//'.real', val_re, type_info)
      call easyO_real8_6d(fname, trim(vname)//'.imag', val_im, type_info)
    end if
  else
    call easyO_real8_6d(fname, trim(vname)//'.real', val_re)
    call easyO_real8_6d(fname, trim(vname)//'.imag', val_im)
  end if
  return
end subroutine easyO_complex8_6d
Subroutine easyOA_complex8_6d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),allocatable,intent(in) :: val(:,:,:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=8),allocatable :: val_im(:,:,:,:,:,:), val_re(:,:,:,:,:,:)
  if (allocated(val) .and. size(val) .gt. 0) then
    allocate(val_re, source=real(val))
    allocate(val_im, source=aimag(val))
    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size( &
    shape(val))],order=[2,1])) ! 
    if (present(type_info)) then
        call easyO_real8_6d(fname, trim(vname)//'.real', val_re, type_info, 1)
        call easyO_real8_6d(fname, trim(vname)//'.imag', val_im, type_info, 1)
    else
      call easyO_real8_6d(fname, trim(vname)//'.real', val_re)
      call easyO_real8_6d(fname, trim(vname)//'.imag', val_im)
    end if
    deallocate(val_im)
    deallocate(val_re)
  end if
  return
end subroutine easyOA_complex8_6d
Subroutine easyI_complex8_6d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),intent(out) :: val(:,:,:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=8) :: val_re(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size( &
    val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))), val_im(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), &
     size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))) ! 
  if (present(type_info)) then
    call easyI_real8_6d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real8_6d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real8_6d(fname, trim(vname)//'.real', val_re)
    call easyI_real8_6d(fname, trim(vname)//'.imag', val_im)
  end if
  val = cmplx(val_re, val_im, 8)
  return
end subroutine easyI_complex8_6d
Subroutine easyIA_complex8_6d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),allocatable,intent(inout) :: val(:,:,:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=8),allocatable :: val_im(:,:,:,:,:,:), val_re(:,:,:,:,:,:)
  if (.not. allocated(val)) then
    call getDims2_enc(fname, trim(vname)//'.real', dimsT_enc)
    if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
      allocate(dimsT2_enc(2, size(dimsT_enc)))
      call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
      allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1, &
    4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6))) ! 
      allocate(val_re(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6))) ! 
      allocate(val_im(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6))) ! 
      deallocate(dimsT_enc); deallocate(dimsT2_enc)
    else
      allocate(val(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6)))
      allocate(val_re(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6)))
      allocate(val_im(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6)))
      deallocate(dimsT_enc)
    end if
  else
    allocate(val_re, source=real(val))
    allocate(val_im, source=aimag(val))
  end if
  if (present(type_info)) then
    call easyI_real8_6d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real8_6d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real8_6d(fname, trim(vname)//'.real', val_re)
    call easyI_real8_6d(fname, trim(vname)//'.imag', val_im)
  end if
  val = cmplx(val_re, val_im, 8)
  deallocate(val_im)
  deallocate(val_re)
  return
end subroutine easyIA_complex8_6d
Subroutine easyO_complex8_7d(fname, vname, val, type_info, type_ele_alone)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),intent(in) :: val(:,:,:,:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  integer,intent(in),optional :: type_ele_alone
  real(kind=8) :: val_re(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), &
     size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:))), val_im(size(val(:,1,1,1,1,1,1)), size(val(1, &
    :,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size( &
    val(1,1,1,1,1,1,:))) ! 
  val_re = real(val)
  val_im = aimag(val)
  if (present(type_info)) then
    if (present(type_ele_alone)) then
      call easyO_real8_7d(fname, trim(vname)//'.real', val_re, type_info, type_ele_alone)
      call easyO_real8_7d(fname, trim(vname)//'.imag', val_re, type_info, type_ele_alone)
    else
      call easyO_real8_7d(fname, trim(vname)//'.real', val_re, type_info)
      call easyO_real8_7d(fname, trim(vname)//'.imag', val_im, type_info)
    end if
  else
    call easyO_real8_7d(fname, trim(vname)//'.real', val_re)
    call easyO_real8_7d(fname, trim(vname)//'.imag', val_im)
  end if
  return
end subroutine easyO_complex8_7d
Subroutine easyOA_complex8_7d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),allocatable,intent(in) :: val(:,:,:,:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=8),allocatable :: val_im(:,:,:,:,:,:,:), val_re(:,:,:,:,:,:,:)
  if (allocated(val) .and. size(val) .gt. 0) then
    allocate(val_re, source=real(val))
    allocate(val_im, source=aimag(val))
    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size( &
    shape(val))],order=[2,1])) ! 
    if (present(type_info)) then
        call easyO_real8_7d(fname, trim(vname)//'.real', val_re, type_info, 1)
        call easyO_real8_7d(fname, trim(vname)//'.imag', val_im, type_info, 1)
    else
      call easyO_real8_7d(fname, trim(vname)//'.real', val_re)
      call easyO_real8_7d(fname, trim(vname)//'.imag', val_im)
    end if
    deallocate(val_im)
    deallocate(val_re)
  end if
  return
end subroutine easyOA_complex8_7d
Subroutine easyI_complex8_7d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),intent(out) :: val(:,:,:,:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=8) :: val_re(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), &
     size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:))), val_im(size(val(:,1,1,1,1,1,1)), size(val(1, &
    :,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size( &
    val(1,1,1,1,1,1,:))) ! 
  if (present(type_info)) then
    call easyI_real8_7d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real8_7d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real8_7d(fname, trim(vname)//'.real', val_re)
    call easyI_real8_7d(fname, trim(vname)//'.imag', val_im)
  end if
  val = cmplx(val_re, val_im, 8)
  return
end subroutine easyI_complex8_7d
Subroutine easyIA_complex8_7d(fname, vname, val, type_info)
  ! almost Duplicated with easyO_int
  implicit none
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
  complex(kind=8),allocatable,intent(inout) :: val(:,:,:,:,:,:,:)
  character(*),intent(in) :: fname, vname
  integer,intent(in),optional :: type_info(:, :)
  real(kind=8),allocatable :: val_im(:,:,:,:,:,:,:), val_re(:,:,:,:,:,:,:)
  if (.not. allocated(val)) then
    call getDims2_enc(fname, trim(vname)//'.real', dimsT_enc)
    if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
      allocate(dimsT2_enc(2, size(dimsT_enc)))
      call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
      allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1, &
    4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6),dimsT2_enc(1,7):dimsT2_enc(2,7))) ! 
      allocate(val_re(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6),dimsT2_enc(1,7):dimsT2_enc( &
    2,7))) ! 
      allocate(val_im(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6),dimsT2_enc(1,7):dimsT2_enc( &
    2,7))) ! 
      deallocate(dimsT_enc); deallocate(dimsT2_enc)
    else
      allocate(val(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6),dimsT_enc(7)))
      allocate(val_re(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6),dimsT_enc(7)))
      allocate(val_im(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6),dimsT_enc(7)))
      deallocate(dimsT_enc)
    end if
  else
    allocate(val_re, source=real(val))
    allocate(val_im, source=aimag(val))
  end if
  if (present(type_info)) then
    call easyI_real8_7d(fname, trim(vname)//'.real', val_re, type_info)
    call easyI_real8_7d(fname, trim(vname)//'.imag', val_im, type_info)
  else
    call easyI_real8_7d(fname, trim(vname)//'.real', val_re)
    call easyI_real8_7d(fname, trim(vname)//'.imag', val_im)
  end if
  val = cmplx(val_re, val_im, 8)
  deallocate(val_im)
  deallocate(val_re)
  return
end subroutine easyIA_complex8_7d
  Subroutine easyO_int4_1d(fname, vname, data, type_info, type_ele_alone)
    integer(kind=4),intent(in) :: data(:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer, intent(in), optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_int4(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_int4(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyOA_int4_1d(fname, vname, data, type_info)
    integer(kind=4),allocatable,intent(in) :: data(:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_int4(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_int4(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_int4_1d
  Subroutine easyI_int4_1d(fname, vname, data, type_info)
    integer(kind=4),intent(out) :: data(:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_int4(fname, vname, data, shape(data), type_info)
    else
      call easyI_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyIA_int4_1d(fname, vname, data, type_info)
    integer(kind=4),allocatable,intent(inout) :: data(:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1)))
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
        call easyI_int4(fname, vname, data, shape(data), type_info)
    else
        call easyI_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_int4_2d(fname, vname, data, type_info, type_ele_alone)
    integer(kind=4),intent(in) :: data(:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer, intent(in), optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_int4(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_int4(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyOA_int4_2d(fname, vname, data, type_info)
    integer(kind=4),allocatable,intent(in) :: data(:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_int4(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_int4(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_int4_2d
  Subroutine easyI_int4_2d(fname, vname, data, type_info)
    integer(kind=4),intent(out) :: data(:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_int4(fname, vname, data, shape(data), type_info)
    else
      call easyI_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyIA_int4_2d(fname, vname, data, type_info)
    integer(kind=4),allocatable,intent(inout) :: data(:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2)))
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1),dimsT_enc(2)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
        call easyI_int4(fname, vname, data, shape(data), type_info)
    else
        call easyI_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_int4_3d(fname, vname, data, type_info, type_ele_alone)
    integer(kind=4),intent(in) :: data(:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer, intent(in), optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_int4(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_int4(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyOA_int4_3d(fname, vname, data, type_info)
    integer(kind=4),allocatable,intent(in) :: data(:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_int4(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_int4(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_int4_3d
  Subroutine easyI_int4_3d(fname, vname, data, type_info)
    integer(kind=4),intent(out) :: data(:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_int4(fname, vname, data, shape(data), type_info)
    else
      call easyI_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyIA_int4_3d(fname, vname, data, type_info)
    integer(kind=4),allocatable,intent(inout) :: data(:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3)))
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
        call easyI_int4(fname, vname, data, shape(data), type_info)
    else
        call easyI_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_int4_4d(fname, vname, data, type_info, type_ele_alone)
    integer(kind=4),intent(in) :: data(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer, intent(in), optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_int4(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_int4(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyOA_int4_4d(fname, vname, data, type_info)
    integer(kind=4),allocatable,intent(in) :: data(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_int4(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_int4(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_int4_4d
  Subroutine easyI_int4_4d(fname, vname, data, type_info)
    integer(kind=4),intent(out) :: data(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_int4(fname, vname, data, shape(data), type_info)
    else
      call easyI_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyIA_int4_4d(fname, vname, data, type_info)
    integer(kind=4),allocatable,intent(inout) :: data(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4))) ! 
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
        call easyI_int4(fname, vname, data, shape(data), type_info)
    else
        call easyI_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_int4_5d(fname, vname, data, type_info, type_ele_alone)
    integer(kind=4),intent(in) :: data(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer, intent(in), optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_int4(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_int4(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyOA_int4_5d(fname, vname, data, type_info)
    integer(kind=4),allocatable,intent(in) :: data(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_int4(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_int4(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_int4_5d
  Subroutine easyI_int4_5d(fname, vname, data, type_info)
    integer(kind=4),intent(out) :: data(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_int4(fname, vname, data, shape(data), type_info)
    else
      call easyI_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyIA_int4_5d(fname, vname, data, type_info)
    integer(kind=4),allocatable,intent(inout) :: data(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5))) ! 
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
        call easyI_int4(fname, vname, data, shape(data), type_info)
    else
        call easyI_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_int4_6d(fname, vname, data, type_info, type_ele_alone)
    integer(kind=4),intent(in) :: data(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer, intent(in), optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_int4(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_int4(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyOA_int4_6d(fname, vname, data, type_info)
    integer(kind=4),allocatable,intent(in) :: data(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_int4(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_int4(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_int4_6d
  Subroutine easyI_int4_6d(fname, vname, data, type_info)
    integer(kind=4),intent(out) :: data(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_int4(fname, vname, data, shape(data), type_info)
    else
      call easyI_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyIA_int4_6d(fname, vname, data, type_info)
    integer(kind=4),allocatable,intent(inout) :: data(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6))) ! 
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
        call easyI_int4(fname, vname, data, shape(data), type_info)
    else
        call easyI_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_int4_7d(fname, vname, data, type_info, type_ele_alone)
    integer(kind=4),intent(in) :: data(:,:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer, intent(in), optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_int4(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_int4(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyOA_int4_7d(fname, vname, data, type_info)
    integer(kind=4),allocatable,intent(in) :: data(:,:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_int4(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_int4(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_int4_7d
  Subroutine easyI_int4_7d(fname, vname, data, type_info)
    integer(kind=4),intent(out) :: data(:,:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_int4(fname, vname, data, shape(data), type_info)
    else
      call easyI_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  
  Subroutine easyIA_int4_7d(fname, vname, data, type_info)
    integer(kind=4),allocatable,intent(inout) :: data(:,:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6),dimsT2_enc(1,7):dimsT2_enc( &
    2,7))) ! 
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6),dimsT_enc(7)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
        call easyI_int4(fname, vname, data, shape(data), type_info)
    else
        call easyI_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_int4(fname, vname, data, dims_data, type_info, type_ele_alone, shape_total, position)
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    integer(kind=4),intent(in) :: data(*)
    character(*),intent(in) :: fname, vname
    integer, intent(in) :: dims_data(:)
    integer, intent(in), optional :: type_info(:, :) ! 2 x n, 0 : type array size, 1 : type position
    integer, intent(in), optional :: type_ele_alone
    integer, intent(in), optional :: shape_total    ! total shape, such as (3,4,5)
    integer, intent(in), optional :: position  ! such as (2,2,2)
    character(80) :: ovname, strT, posSuffix
    integer,allocatable :: dims(:)
    integer :: ncid, vid, ierr
    integer :: ndims
    
    integer,allocatable :: dids(:)
    integer :: i, j, k, l, m, n
    integer :: data_size, ndims_data, ndims_type
    
    logical :: isExist
    character(40), allocatable :: dimnames(:)
    character(80) :: tname, tvname
    integer,allocatable :: start_index(:), count_lens(:)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> body
    ! ================== get dims for both base and type-var mode
    ndims_data = size(dims_data)
    if (present(type_info)) then
      if (present(type_ele_alone)) then   ! save type-element, 1 variable for 1 position
        ndims = ndims_data
        allocate(dims(ndims), STAT=ierr)
        call check_enc(ierr, 'allocate')
        dims = dims_data
      else                                ! save type-element in a union n1+n2 dimensional array
        ndims_type = size(type_info(1, :))
        ndims = ndims_data + ndims_type
        allocate(dims(ndims), STAT=ierr)
        call check_enc(ierr, 'allocate')
        dims(1:ndims_data) = dims_data
        do i = ndims_data + 1, ndims
          dims(i) = type_info(1,ndims_type - (i-ndims_data) + 1)
        end do
      end if
    else
      ndims = ndims_data
      allocate(dims(ndims), STAT=ierr)
      call check_enc(ierr, 'allocate')
      dims = dims_data
    end if
    
    ovname = vname
    ! ================== get tname and tvname for type-var
    if (present(type_info)) then
      i = scan(vname, '%')
      if (i .eq. 0) then
        stop 'please use % as the struct connection symbol'
      end if
      tname = vname(:i-1)
      tvname = vname(i+1:)
      if (present(type_ele_alone)) then
        posSuffix = '('
        do j = 1, size(type_info(1,:))
          write(strT, '(I0)') type_info(2,j)
          ! print *, 'strT=',strT
          posSuffix = trim(posSuffix)//trim(strT)
          if (j .lt. size(type_info(1,:))) posSuffix = trim(posSuffix)//','
        end do
        posSuffix = trim(posSuffix)//')'
        ovname = trim(tname)//trim(posSuffix)//'%'//trim(tvname)
        ! print *, 'ovname = ',ovname
      end if
    end if
    allocate(dimnames(ndims), STAT = ierr)
    if (ierr .ne. 0) then
      stop 1
    end if
    
    allocate(dids(ndims), STAT = ierr)
    if (ierr .ne. 0) then
      stop 1
    end if
    
    allocate(start_index(ndims), STAT=ierr)
    call check_enc(ierr, 'allocate start_index')
    start_index = 1
    if (present(type_info)) then
      ! print *, 'type_info=',type_info(1,:), type_info(2,:)
      if (.not. present(type_ele_alone)) then
        do i = ndims_data + 1, ndims
          start_index(i) = type_info(2, ndims_type - (i-ndims_data) + 1)
        end do
      end if
    end if
    allocate(count_lens(ndims), STAT=ierr)
    if (present(type_info)) then
      if (present(type_ele_alone)) then
        count_lens = dims_data
      else
        count_lens(1:ndims_data) = dims_data
        count_lens(ndims_data+1:) = 1
      end if
    else
      count_lens = dims_data
    end if
    data_size = product(dims_data)
    
    ! ================== set dimension names
    if (present(type_info)) then
      if (.not. present(type_ele_alone)) then
        do i = 1, ndims_data
          write(dimnames(i), '(A, ".d", I1)') trim(tvname), ndims_data-i+1
        end do
        do i = ndims_data + 1, ndims
          write(dimnames(i), '(A, ".d", I1)') trim(tname), i-ndims_data
        end do
      else
        do i = 1, ndims
          write(dimnames(i), '(A, A, ".d", I1)') trim(tvname), trim(posSuffix), ndims_data-i+1
        end do
      end if
    else
      do i = 1, ndims
        write(dimnames(i), '(A, ".d", I1)') trim(vname), ndims_data-i+1
      end do
    end if
    ! print *, 'dimnames = ', dimnames
    ! =========================== netcdf-relative
    inquire(file = fname, exist = isExist)
    ! print *, "isExist=",isExist
    ! ~~~~~~~~~~~~ open or create nc
    if (isExist) then
      call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
      call check_enc( nf90_redef(ncid), "nf90_redef")
    else
      call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
    end if
    ! ~~~~~~~~~~~~ get or define dimension
    do i = 1, ndims
      if (isExist) then
        if (nf90_inq_dimid(ncid, dimnames(i), dids(i)) .ne. nf90_noerr) then
          call check_enc(nf90_def_dim(ncid, dimnames(i), dims(i), dids(i)), "nf90_def_dim")
        end if
      else
        call check_enc(nf90_def_dim(ncid, dimnames(i), dims(i), dids(i)), "nf90_def_dim")
      end if
    end do
    ! ~~~~~~~~~~~~ define variable, or error
    if (nf90_inq_varid(ncid, ovname, vid) .eq. nf90_noerr) then
      if (.not. present(type_info)) then
        write(*,*) 'Error! variable already exists!'
        call check_enc( nf90_close(ncid) , "nf90_close")
        stop 1
      end if
    else
      call check_enc( nf90_def_var(ncid, ovname, NF90_INT, dids, vid) , "nf90_def_var")
    end if
    call check_enc( nf90_enddef(ncid) , "nf90_enddef")
    ! ~~~~~~~~~~~~ write data
    ! print *, 'start_index = ',start_index
    ! print *, 'count_lens = ',count_lens
    call check_enc( nf90_put_var(ncid, vid, data(1:data_size), start=start_index, count=count_lens) , & 
      "in easyO_int4, nf90_put_var, "//ovname)
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    return
  end subroutine easyO_int4
  
  Subroutine easyO_int4_scalar(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    integer(kind=4),intent(in) :: val
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: type_info(:, :) ! 2 x n, 0 : type array size, 1 : type position
    
    integer,allocatable :: dims(:)
    
    integer :: ncid, vid, ierr
    
    integer :: i, j, k, l, m, n
    
    logical :: isExist
    character(40), allocatable :: dimnames(:)
    integer,allocatable :: dids(:), start_index(:)
    integer :: ndims_type
    character(80) :: tname, tvname
    
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> body
    ! =========================== netcdf-relative
    inquire(file = fname, exist = isExist)
    ! print *, "isExist=",isExist
    
    if (present(type_info)) then
      ndims_type = size(type_info(1, :))
      allocate(dims(ndims_type), STAT=ierr)
      call check_enc(ierr, 'allocate')
      dims = type_info(1, :)
    ! ================== get tname and tvname for type-var
      i = scan(vname, '%')
      if (i .eq. 0) then
        stop 'please use % as the struct connection symbol'
      end if
      tname = vname(:i-1)
      tvname = vname(i+1:)
      allocate(dimnames(ndims_type), STAT = ierr)
      call check_enc(ierr, 'allocate for dimnames')
      ! ================== set dimension names
      do i = 1, ndims_type
        write(dimnames(i), '("'//trim(tname)//'", "_d", I1)') i
      end do
      allocate(dids(ndims_type), STAT = ierr)
      call check_enc(ierr, 'allocate dids')
      allocate(start_index(ndims_type), STAT=ierr)
      call check_enc(ierr, 'allocate start_index')
      ! print *, 'type_info=',type_info(1,:), type_info(2,:)
      do i = 1, ndims_type
        start_index(i) = type_info(2, ndims_type - i + 1)
      end do
    end if
    ! ~~~~~~~~~~~~ open or create nc
    if (isExist) then
      call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "nf90_open")
      call check_enc(nf90_redef(ncid), "nf90_redef")
    else
      call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "nf90_create")
    end if
    ! ~~~~~~~~~~~~ get or define dimension
    if (present(type_info)) then
      do i = 1, ndims_type
        if (isExist) then
          if (nf90_inq_dimid(ncid, dimnames(i), dids(i)) .ne. nf90_noerr) then
            call check_enc(nf90_def_dim(ncid, dimnames(i), dims(i), dids(i)), "nf90_def_dim")
          end if
        else
          call check_enc(nf90_def_dim(ncid, dimnames(i), dims(i), dids(i)), "nf90_def_dim")
        end if
      end do
    end if
    ! ~~~~~~~~~~~~ define variable, or error
    if (nf90_inq_varid(ncid, vname, vid) .eq. nf90_noerr) then
      if (.not. present(type_info)) then
        write(*,*) 'Error! variable already exists!'
        call check_enc( nf90_close(ncid) , "nf90_close")
        stop 1
      end if
    else
      if (present(type_info)) then
        call check_enc( nf90_def_var(ncid, vname, NF90_INT, dids, vid) , "nf90_def_var")
      else
        call check_enc( nf90_def_var(ncid, vname, NF90_INT, varid=vid) , "nf90_def_var")
      end if
    end if
    call check_enc( nf90_enddef(ncid) , "nf90_enddef")
    ! ~~~~~~~~~~~~ write val
    if (present(type_info)) then
      call check_enc( nf90_put_var(ncid, vid, val, start=start_index) , "nf90_put_var")
    else
      call check_enc( nf90_put_var(ncid, vid, val) , "nf90_put_var")
    end if
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    return
  end subroutine easyO_int4_scalar
  Subroutine easyI_int4(fname, vname, data, dims_data, type_info)
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    integer(kind=4),intent(out) :: data(*)
    character(*),intent(in) :: fname, vname
    integer, intent(in) :: dims_data(:)
    integer, intent(in), optional :: type_info(:, :) ! 2 x n, 0 : type array size, 1 : type position
    
    character(80) :: ivname, strT, posSuffix, tname, tvname
    integer :: type_ele_alone
    integer,allocatable :: dims(:)
    integer :: ncid, vid, ierr
    integer :: ndims
    
    integer :: i, j, k, l, m, n
    integer :: data_size, ndims_data, ndims_type
    
    logical :: isExist
    integer,allocatable :: start_index(:), count_lens(:)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> body
    ! ================== get dims for both base and type-var mode
    ndims_data = size(dims_data)
    ivname = vname
    if (present(type_info)) then
      i = scan(vname, '%')
      if (i .eq. 0) then
        stop 'please use % as the struct connection symbol'
      end if
      tname = vname(:i-1)
      tvname = vname(i+1:)
      if (hasVar(fname, vname) .eq. 1) then
        type_ele_alone = 0
      else
        type_ele_alone = 1
        ! --- get pos suffix
        posSuffix = '('
        do j = 1, size(type_info(1,:))
          write(strT, '(I0)') type_info(2,j)
          ! print *, 'strT=',strT
          posSuffix = trim(posSuffix)//trim(strT)
          if (j .lt. size(type_info(1,:))) posSuffix = trim(posSuffix)//','
        end do
        posSuffix = trim(posSuffix)//')'
        ivname = trim(tname)//trim(posSuffix)//'%'//trim(tvname)
      end if
      if (type_ele_alone .eq. 1) then
        ndims = ndims_data
        allocate(dims(ndims), STAT=ierr)
        call check_enc(ierr, 'allocate')
        dims = dims_data
      else
        ndims_type = size(type_info(1, :))
        ndims = ndims_data + ndims_type
        allocate(dims(ndims), STAT=ierr)
        call check_enc(ierr, 'allocate')
        dims(1:ndims_data) = dims_data
        do i = ndims_data + 1, ndims
          dims(i) = type_info(1,ndims_type - (i-ndims_data) + 1)
        end do
      end if
    else
      ndims = ndims_data
      allocate(dims(ndims), STAT=ierr)
      call check_enc(ierr, 'allocate')
      dims = dims_data
    end if
    ! ================== get tname and tvname for type-var
    
    allocate(start_index(ndims), STAT=ierr)
    call check_enc(ierr, 'allocate start_index')
    start_index = 1
    if (present(type_info)) then
    ! print *, 'type_info=',type_info(1,:), type_info(2,:)
      if (type_ele_alone .eq. 0) then
        do i = ndims_data + 1, ndims
          start_index(i) = type_info(2, ndims_type - (i-ndims_data) + 1)
        end do
      end if        
    end if
    allocate(count_lens(ndims), STAT=ierr)
    if (present(type_info)) then
      if (type_ele_alone .eq. 0) then
        count_lens(1:ndims_data) = dims_data
        count_lens(ndims_data+1:) = 1
      else
        count_lens = dims_data
      end if
    else
      count_lens = dims_data
    end if
    data_size = product(dims_data)
    
    ! =========================== netcdf-relative
    inquire(file = fname, exist = isExist)
    ! print *, "isExist=",isExist
    ! ~~~~~~~~~~~~ open or create nc
    if (isExist) then
      call check_enc(nf90_open(fname, NF90_NOWRITE, ncid) , "nf90_open")
    else
      print *, "Error, file doesn't exist."
      stop "Stopped"
    end if
    ! ~~~~~~~~~~~~ inquire variable
    call check_enc(nf90_inq_varid(ncid, ivname, vid), &
      'in easyI_int4, nf90_inq_varid for '//trim(ivname))
    ! ~~~~~~~~~~~~ read data
    ! print *, 'start_index = ',start_index
    ! print *, 'count_lens = ',count_lens
    call check_enc( nf90_get_var(ncid, vid, data(1:data_size), start=start_index, count=count_lens), &
      "in easyI_int4, nf90_get_var, "//ivname)
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    return
  end subroutine easyI_int4
  
  Subroutine easyI_int4_scalar(fname, vname, val, type_info)
    ! almost Duplicated with easyI_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    integer(kind=4),intent(out) :: val
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: type_info(:, :) ! 2 x n, 0 : type array size, 1 : type position
    
    integer,allocatable :: dims(:)
    
    integer :: ncid, vid, ierr
    
    integer :: i, j, k, l, m, n
    
    logical :: isExist
    integer,allocatable :: start_index(:)
    integer :: ndims_type
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> body
    ! =========================== netcdf-relative
    ! print *, "isExist=",isExist
    
    if (present(type_info)) then
      ndims_type = size(type_info(1, :))
      allocate(dims(ndims_type), STAT=ierr)
      call check_enc(ierr, 'allocate')
      dims = type_info(1, :)
    ! ================== get tname and tvname for type-var
      allocate(start_index(ndims_type), STAT=ierr)
      call check_enc(ierr, 'allocate start_index')
      ! print *, 'type_info=',type_info(1,:), type_info(2,:)
      do i = 1, ndims_type
        start_index(i) = type_info(2, ndims_type - i + 1)
      end do
    end if
    inquire(file = fname, exist = isExist)
    ! ~~~~~~~~~~~~ open or create nc
    if (isExist) then
      call check_enc(nf90_open(fname, NF90_NOWRITE, ncid) , "nf90_open")
    else
      print *, "Error, file doesn't exist."
      stop "Stopped"
    end if
    ! ~~~~~~~~~~~~ inquire variable
    call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
    ! ~~~~~~~~~~~~ write val
    if (present(type_info)) then
      call check_enc( nf90_get_var(ncid, vid, val, start=start_index), "in nf90_get_var, "//vname)
    else
      call check_enc( nf90_get_var(ncid, vid, val), "in nf90_get_var, "//vname)
    end if
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    return
  end subroutine easyI_int4_scalar
  Subroutine easyO_logical_scalar(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,intent(in) :: val
    character(*),intent(in) :: fname, vname
    integer :: b, i
    integer,intent(in),optional :: type_info(:, :)
    b = 0
    if(val) b = 1
    if (present(type_info)) then
      call easyO_int4_scalar(fname, vname, b, type_info)
    else
      call easyO_int4_scalar(fname, vname, b)
    end if
    return
  end subroutine easyO_logical_scalar
  Subroutine easyI_logical_scalar(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,intent(out) :: val
    character(*),intent(in) :: fname, vname
    integer :: b, i
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then
      call easyI_int4_scalar(fname, vname, b, type_info)
    else
      call easyI_int4_scalar(fname, vname, b)
    end if
    val = .false.
    if(b .eq. 1) val = .true.
    return
  end subroutine easyI_logical_scalar
  Subroutine easyO_logical_1d(fname, vname, val, type_info, type_ele_alone)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,intent(in) :: val(:)
    character(*),intent(in) :: fname, vname
    integer :: b(size(val)), i
    integer,intent(in),optional :: type_info(:, :)
    integer,intent(in),optional :: type_ele_alone
    b = 0
    where(val) b = 1
    if (present(type_info)) then
      if (present(type_ele_alone)) then
        call easyO_int4_1d(fname, vname, b, type_info, type_ele_alone)
      else
        call easyO_int4_1d(fname, vname, b, type_info)
      end if
    else
      call easyO_int4_1d(fname, vname, b)
    end if
    return
  end subroutine easyO_logical_1d
  Subroutine easyOA_logical_1d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,allocatable,intent(in) :: val(:)
    character(*),intent(in) :: fname, vname
    integer,allocatable :: b(:)
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(val) .and. size(val) .gt. 0) then
      allocate(b(size(val)))
      if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size( &
    shape(val))],order=[2,1])) ! 
      b = 0
      where(val) b = 1
      if (present(type_info)) then
          call easyO_int4_1d(fname, vname, b, type_info, 1)
      else
        call easyO_int4_1d(fname, vname, b)
      end if
      deallocate(b)
    end if
    return
  end subroutine easyOA_logical_1d
  Subroutine easyI_logical_1d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,intent(out) :: val(:)
    character(*),intent(in) :: fname, vname
    integer :: b(size(val))
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then
      call easyI_int4_1d(fname, vname, b, type_info)
    else
      call easyI_int4_1d(fname, vname, b)
    end if
    val = .false.
    where(b .eq. 1) val = .true.
    return
  end subroutine easyI_logical_1d
  Subroutine easyIA_logical_1d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,allocatable,intent(inout) :: val(:)
    character(*),intent(in) :: fname, vname
    integer,allocatable :: b(:)
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(val)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1)))
        allocate(b(dimsT2_enc(1,1):dimsT2_enc(2,1)))
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(val(dimsT_enc(1)))
        allocate(b(dimsT_enc(1)))
        deallocate(dimsT_enc)
      end if
    else
      allocate(b(size(val)))
    end if
    if (present(type_info)) then
      call easyI_int4_1d(fname, vname, b, type_info)
    else
      call easyI_int4_1d(fname, vname, b)
    end if
    val = .false.
    where(b .eq. 1) val = .true.
    deallocate(b)
    return
  end subroutine easyIA_logical_1d
  Subroutine easyO_logical_2d(fname, vname, val, type_info, type_ele_alone)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,intent(in) :: val(:,:)
    character(*),intent(in) :: fname, vname
    integer :: b(size(val(:,1)), size(val(1,:))), i
    integer,intent(in),optional :: type_info(:, :)
    integer,intent(in),optional :: type_ele_alone
    b = 0
    where(val) b = 1
    if (present(type_info)) then
      if (present(type_ele_alone)) then
        call easyO_int4_2d(fname, vname, b, type_info, type_ele_alone)
      else
        call easyO_int4_2d(fname, vname, b, type_info)
      end if
    else
      call easyO_int4_2d(fname, vname, b)
    end if
    return
  end subroutine easyO_logical_2d
  Subroutine easyOA_logical_2d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,allocatable,intent(in) :: val(:,:)
    character(*),intent(in) :: fname, vname
    integer,allocatable :: b(:,:)
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(val) .and. size(val) .gt. 0) then
      allocate(b(size(val(:,1)), size(val(1,:))))
      if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size( &
    shape(val))],order=[2,1])) ! 
      b = 0
      where(val) b = 1
      if (present(type_info)) then
          call easyO_int4_2d(fname, vname, b, type_info, 1)
      else
        call easyO_int4_2d(fname, vname, b)
      end if
      deallocate(b)
    end if
    return
  end subroutine easyOA_logical_2d
  Subroutine easyI_logical_2d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,intent(out) :: val(:,:)
    character(*),intent(in) :: fname, vname
    integer :: b(size(val(:,1)), size(val(1,:)))
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then
      call easyI_int4_2d(fname, vname, b, type_info)
    else
      call easyI_int4_2d(fname, vname, b)
    end if
    val = .false.
    where(b .eq. 1) val = .true.
    return
  end subroutine easyI_logical_2d
  Subroutine easyIA_logical_2d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,allocatable,intent(inout) :: val(:,:)
    character(*),intent(in) :: fname, vname
    integer,allocatable :: b(:,:)
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(val)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2)))
        allocate(b(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2)))
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(val(dimsT_enc(1),dimsT_enc(2)))
        allocate(b(dimsT_enc(1),dimsT_enc(2)))
        deallocate(dimsT_enc)
      end if
    else
      allocate(b(size(val(:,1)), size(val(1,:))))
    end if
    if (present(type_info)) then
      call easyI_int4_2d(fname, vname, b, type_info)
    else
      call easyI_int4_2d(fname, vname, b)
    end if
    val = .false.
    where(b .eq. 1) val = .true.
    deallocate(b)
    return
  end subroutine easyIA_logical_2d
  Subroutine easyO_logical_3d(fname, vname, val, type_info, type_ele_alone)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,intent(in) :: val(:,:,:)
    character(*),intent(in) :: fname, vname
    integer :: b(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))), i
    integer,intent(in),optional :: type_info(:, :)
    integer,intent(in),optional :: type_ele_alone
    b = 0
    where(val) b = 1
    if (present(type_info)) then
      if (present(type_ele_alone)) then
        call easyO_int4_3d(fname, vname, b, type_info, type_ele_alone)
      else
        call easyO_int4_3d(fname, vname, b, type_info)
      end if
    else
      call easyO_int4_3d(fname, vname, b)
    end if
    return
  end subroutine easyO_logical_3d
  Subroutine easyOA_logical_3d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,allocatable,intent(in) :: val(:,:,:)
    character(*),intent(in) :: fname, vname
    integer,allocatable :: b(:,:,:)
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(val) .and. size(val) .gt. 0) then
      allocate(b(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))))
      if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size( &
    shape(val))],order=[2,1])) ! 
      b = 0
      where(val) b = 1
      if (present(type_info)) then
          call easyO_int4_3d(fname, vname, b, type_info, 1)
      else
        call easyO_int4_3d(fname, vname, b)
      end if
      deallocate(b)
    end if
    return
  end subroutine easyOA_logical_3d
  Subroutine easyI_logical_3d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,intent(out) :: val(:,:,:)
    character(*),intent(in) :: fname, vname
    integer :: b(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:)))
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then
      call easyI_int4_3d(fname, vname, b, type_info)
    else
      call easyI_int4_3d(fname, vname, b)
    end if
    val = .false.
    where(b .eq. 1) val = .true.
    return
  end subroutine easyI_logical_3d
  Subroutine easyIA_logical_3d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,allocatable,intent(inout) :: val(:,:,:)
    character(*),intent(in) :: fname, vname
    integer,allocatable :: b(:,:,:)
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(val)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3)))
        allocate(b(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3)))
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(val(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3)))
        allocate(b(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3)))
        deallocate(dimsT_enc)
      end if
    else
      allocate(b(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))))
    end if
    if (present(type_info)) then
      call easyI_int4_3d(fname, vname, b, type_info)
    else
      call easyI_int4_3d(fname, vname, b)
    end if
    val = .false.
    where(b .eq. 1) val = .true.
    deallocate(b)
    return
  end subroutine easyIA_logical_3d
  Subroutine easyO_logical_4d(fname, vname, val, type_info, type_ele_alone)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,intent(in) :: val(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer :: b(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))), i
    integer,intent(in),optional :: type_info(:, :)
    integer,intent(in),optional :: type_ele_alone
    b = 0
    where(val) b = 1
    if (present(type_info)) then
      if (present(type_ele_alone)) then
        call easyO_int4_4d(fname, vname, b, type_info, type_ele_alone)
      else
        call easyO_int4_4d(fname, vname, b, type_info)
      end if
    else
      call easyO_int4_4d(fname, vname, b)
    end if
    return
  end subroutine easyO_logical_4d
  Subroutine easyOA_logical_4d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,allocatable,intent(in) :: val(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,allocatable :: b(:,:,:,:)
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(val) .and. size(val) .gt. 0) then
      allocate(b(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))))
      if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size( &
    shape(val))],order=[2,1])) ! 
      b = 0
      where(val) b = 1
      if (present(type_info)) then
          call easyO_int4_4d(fname, vname, b, type_info, 1)
      else
        call easyO_int4_4d(fname, vname, b)
      end if
      deallocate(b)
    end if
    return
  end subroutine easyOA_logical_4d
  Subroutine easyI_logical_4d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,intent(out) :: val(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer :: b(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:)))
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then
      call easyI_int4_4d(fname, vname, b, type_info)
    else
      call easyI_int4_4d(fname, vname, b)
    end if
    val = .false.
    where(b .eq. 1) val = .true.
    return
  end subroutine easyI_logical_4d
  Subroutine easyIA_logical_4d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,allocatable,intent(inout) :: val(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,allocatable :: b(:,:,:,:)
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(val)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc( &
    1,4):dimsT2_enc(2,4))) ! 
        allocate(b(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1, &
    4):dimsT2_enc(2,4))) ! 
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(val(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4)))
        allocate(b(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4)))
        deallocate(dimsT_enc)
      end if
    else
      allocate(b(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))))
    end if
    if (present(type_info)) then
      call easyI_int4_4d(fname, vname, b, type_info)
    else
      call easyI_int4_4d(fname, vname, b)
    end if
    val = .false.
    where(b .eq. 1) val = .true.
    deallocate(b)
    return
  end subroutine easyIA_logical_4d
  Subroutine easyO_logical_5d(fname, vname, val, type_info, type_ele_alone)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,intent(in) :: val(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer :: b(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))), i
    integer,intent(in),optional :: type_info(:, :)
    integer,intent(in),optional :: type_ele_alone
    b = 0
    where(val) b = 1
    if (present(type_info)) then
      if (present(type_ele_alone)) then
        call easyO_int4_5d(fname, vname, b, type_info, type_ele_alone)
      else
        call easyO_int4_5d(fname, vname, b, type_info)
      end if
    else
      call easyO_int4_5d(fname, vname, b)
    end if
    return
  end subroutine easyO_logical_5d
  Subroutine easyOA_logical_5d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,allocatable,intent(in) :: val(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,allocatable :: b(:,:,:,:,:)
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(val) .and. size(val) .gt. 0) then
      allocate(b(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))))
      if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size( &
    shape(val))],order=[2,1])) ! 
      b = 0
      where(val) b = 1
      if (present(type_info)) then
          call easyO_int4_5d(fname, vname, b, type_info, 1)
      else
        call easyO_int4_5d(fname, vname, b)
      end if
      deallocate(b)
    end if
    return
  end subroutine easyOA_logical_5d
  Subroutine easyI_logical_5d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,intent(out) :: val(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer :: b(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:)))
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then
      call easyI_int4_5d(fname, vname, b, type_info)
    else
      call easyI_int4_5d(fname, vname, b)
    end if
    val = .false.
    where(b .eq. 1) val = .true.
    return
  end subroutine easyI_logical_5d
  Subroutine easyIA_logical_5d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,allocatable,intent(inout) :: val(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,allocatable :: b(:,:,:,:,:)
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(val)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc( &
    1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5))) ! 
        allocate(b(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1, &
    4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5))) ! 
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(val(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5)))
        allocate(b(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5)))
        deallocate(dimsT_enc)
      end if
    else
      allocate(b(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))))
    end if
    if (present(type_info)) then
      call easyI_int4_5d(fname, vname, b, type_info)
    else
      call easyI_int4_5d(fname, vname, b)
    end if
    val = .false.
    where(b .eq. 1) val = .true.
    deallocate(b)
    return
  end subroutine easyIA_logical_5d
  Subroutine easyO_logical_6d(fname, vname, val, type_info, type_ele_alone)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,intent(in) :: val(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer :: b(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1, &
    1,:,1)), size(val(1,1,1,1,1,:))), i ! 
    integer,intent(in),optional :: type_info(:, :)
    integer,intent(in),optional :: type_ele_alone
    b = 0
    where(val) b = 1
    if (present(type_info)) then
      if (present(type_ele_alone)) then
        call easyO_int4_6d(fname, vname, b, type_info, type_ele_alone)
      else
        call easyO_int4_6d(fname, vname, b, type_info)
      end if
    else
      call easyO_int4_6d(fname, vname, b)
    end if
    return
  end subroutine easyO_logical_6d
  Subroutine easyOA_logical_6d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,allocatable,intent(in) :: val(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,allocatable :: b(:,:,:,:,:,:)
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(val) .and. size(val) .gt. 0) then
      allocate(b(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1, &
    1,:,1)), size(val(1,1,1,1,1,:)))) ! 
      if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size( &
    shape(val))],order=[2,1])) ! 
      b = 0
      where(val) b = 1
      if (present(type_info)) then
          call easyO_int4_6d(fname, vname, b, type_info, 1)
      else
        call easyO_int4_6d(fname, vname, b)
      end if
      deallocate(b)
    end if
    return
  end subroutine easyOA_logical_6d
  Subroutine easyI_logical_6d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,intent(out) :: val(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer :: b(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1, &
    1,:,1)), size(val(1,1,1,1,1,:))) ! 
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then
      call easyI_int4_6d(fname, vname, b, type_info)
    else
      call easyI_int4_6d(fname, vname, b)
    end if
    val = .false.
    where(b .eq. 1) val = .true.
    return
  end subroutine easyI_logical_6d
  Subroutine easyIA_logical_6d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,allocatable,intent(inout) :: val(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,allocatable :: b(:,:,:,:,:,:)
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(val)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc( &
    1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6))) ! 
        allocate(b(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1, &
    4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6))) ! 
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(val(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6)))
        allocate(b(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6)))
        deallocate(dimsT_enc)
      end if
    else
      allocate(b(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1, &
    1,:,1)), size(val(1,1,1,1,1,:)))) ! 
    end if
    if (present(type_info)) then
      call easyI_int4_6d(fname, vname, b, type_info)
    else
      call easyI_int4_6d(fname, vname, b)
    end if
    val = .false.
    where(b .eq. 1) val = .true.
    deallocate(b)
    return
  end subroutine easyIA_logical_6d
  Subroutine easyO_logical_7d(fname, vname, val, type_info, type_ele_alone)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,intent(in) :: val(:,:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer :: b(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size( &
    val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:))), i ! 
    integer,intent(in),optional :: type_info(:, :)
    integer,intent(in),optional :: type_ele_alone
    b = 0
    where(val) b = 1
    if (present(type_info)) then
      if (present(type_ele_alone)) then
        call easyO_int4_7d(fname, vname, b, type_info, type_ele_alone)
      else
        call easyO_int4_7d(fname, vname, b, type_info)
      end if
    else
      call easyO_int4_7d(fname, vname, b)
    end if
    return
  end subroutine easyO_logical_7d
  Subroutine easyOA_logical_7d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,allocatable,intent(in) :: val(:,:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,allocatable :: b(:,:,:,:,:,:,:)
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(val) .and. size(val) .gt. 0) then
      allocate(b(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size( &
    val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:)))) ! 
      if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size( &
    shape(val))],order=[2,1])) ! 
      b = 0
      where(val) b = 1
      if (present(type_info)) then
          call easyO_int4_7d(fname, vname, b, type_info, 1)
      else
        call easyO_int4_7d(fname, vname, b)
      end if
      deallocate(b)
    end if
    return
  end subroutine easyOA_logical_7d
  Subroutine easyI_logical_7d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,intent(out) :: val(:,:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer :: b(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size( &
    val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:))) ! 
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then
      call easyI_int4_7d(fname, vname, b, type_info)
    else
      call easyI_int4_7d(fname, vname, b)
    end if
    val = .false.
    where(b .eq. 1) val = .true.
    return
  end subroutine easyI_logical_7d
  Subroutine easyIA_logical_7d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,allocatable,intent(inout) :: val(:,:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,allocatable :: b(:,:,:,:,:,:,:)
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(val)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc( &
    1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6),dimsT2_enc(1,7):dimsT2_enc(2,7))) ! 
        allocate(b(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1, &
    4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6),dimsT2_enc(1,7):dimsT2_enc(2,7))) ! 
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(val(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6),dimsT_enc(7)))
        allocate(b(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6),dimsT_enc(7)))
        deallocate(dimsT_enc)
      end if
    else
      allocate(b(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size( &
    val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:)))) ! 
    end if
    if (present(type_info)) then
      call easyI_int4_7d(fname, vname, b, type_info)
    else
      call easyI_int4_7d(fname, vname, b)
    end if
    val = .false.
    where(b .eq. 1) val = .true.
    deallocate(b)
    return
  end subroutine easyIA_logical_7d
  Subroutine easyO_char_1d(fname, vname, data, type_info, type_ele_alone)
    character(*),intent(in) :: data(:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer,intent(in),optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_char_nd(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_char_nd(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine easyO_char_1d
  Subroutine easyOA_char_1d(fname, vname, data, type_info)
    character(*),allocatable,intent(in) :: data(:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_char_nd(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_char_nd(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_char_1d
  
  Subroutine easyI_char_1d(fname, vname, data, type_info)
    character(*),intent(out) :: data(:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_char_nd(fname, vname, data, shape(data), type_info)
    else
      call easyI_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine easyI_char_1d
  Subroutine easyIA_char_1d(fname, vname, data, type_info)
    character(*),allocatable,intent(inout) :: data(:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1)))
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_char_nd(fname, vname, data, shape(data), type_info)
    else
      call easyI_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine easyIA_char_1d
  Subroutine easyO_char_2d(fname, vname, data, type_info, type_ele_alone)
    character(*),intent(in) :: data(:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer,intent(in),optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_char_nd(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_char_nd(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine easyO_char_2d
  Subroutine easyOA_char_2d(fname, vname, data, type_info)
    character(*),allocatable,intent(in) :: data(:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_char_nd(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_char_nd(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_char_2d
  
  Subroutine easyI_char_2d(fname, vname, data, type_info)
    character(*),intent(out) :: data(:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_char_nd(fname, vname, data, shape(data), type_info)
    else
      call easyI_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine easyI_char_2d
  Subroutine easyIA_char_2d(fname, vname, data, type_info)
    character(*),allocatable,intent(inout) :: data(:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2)))
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1),dimsT_enc(2)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_char_nd(fname, vname, data, shape(data), type_info)
    else
      call easyI_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine easyIA_char_2d
  Subroutine easyO_char_3d(fname, vname, data, type_info, type_ele_alone)
    character(*),intent(in) :: data(:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer,intent(in),optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_char_nd(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_char_nd(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine easyO_char_3d
  Subroutine easyOA_char_3d(fname, vname, data, type_info)
    character(*),allocatable,intent(in) :: data(:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_char_nd(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_char_nd(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_char_3d
  
  Subroutine easyI_char_3d(fname, vname, data, type_info)
    character(*),intent(out) :: data(:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_char_nd(fname, vname, data, shape(data), type_info)
    else
      call easyI_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine easyI_char_3d
  Subroutine easyIA_char_3d(fname, vname, data, type_info)
    character(*),allocatable,intent(inout) :: data(:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3)))
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_char_nd(fname, vname, data, shape(data), type_info)
    else
      call easyI_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine easyIA_char_3d
  Subroutine easyO_char_4d(fname, vname, data, type_info, type_ele_alone)
    character(*),intent(in) :: data(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer,intent(in),optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_char_nd(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_char_nd(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine easyO_char_4d
  Subroutine easyOA_char_4d(fname, vname, data, type_info)
    character(*),allocatable,intent(in) :: data(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_char_nd(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_char_nd(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_char_4d
  
  Subroutine easyI_char_4d(fname, vname, data, type_info)
    character(*),intent(out) :: data(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_char_nd(fname, vname, data, shape(data), type_info)
    else
      call easyI_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine easyI_char_4d
  Subroutine easyIA_char_4d(fname, vname, data, type_info)
    character(*),allocatable,intent(inout) :: data(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4))) ! 
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_char_nd(fname, vname, data, shape(data), type_info)
    else
      call easyI_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine easyIA_char_4d
  Subroutine easyO_char_5d(fname, vname, data, type_info, type_ele_alone)
    character(*),intent(in) :: data(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer,intent(in),optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_char_nd(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_char_nd(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine easyO_char_5d
  Subroutine easyOA_char_5d(fname, vname, data, type_info)
    character(*),allocatable,intent(in) :: data(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_char_nd(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_char_nd(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_char_5d
  
  Subroutine easyI_char_5d(fname, vname, data, type_info)
    character(*),intent(out) :: data(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_char_nd(fname, vname, data, shape(data), type_info)
    else
      call easyI_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine easyI_char_5d
  Subroutine easyIA_char_5d(fname, vname, data, type_info)
    character(*),allocatable,intent(inout) :: data(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5))) ! 
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_char_nd(fname, vname, data, shape(data), type_info)
    else
      call easyI_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine easyIA_char_5d
  Subroutine easyO_char_6d(fname, vname, data, type_info, type_ele_alone)
    character(*),intent(in) :: data(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    integer,intent(in),optional :: type_ele_alone
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      if (present(type_ele_alone)) then
        call easyO_char_nd(fname, vname, data, shape(data), type_info, type_ele_alone)
      else
        call easyO_char_nd(fname, vname, data, shape(data), type_info)
      end if
    else
      call easyO_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine easyO_char_6d
  Subroutine easyOA_char_6d(fname, vname, data, type_info)
    character(*),allocatable,intent(in) :: data(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (allocated(data) .and. size(data) .gt. 0) then
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, &
     size(shape(data))],order=[2,1])) ! 
      if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
          call easyO_char_nd(fname, vname, data, shape(data), type_info, 1)
      else
        call easyO_char_nd(fname, vname, data, shape(data))
      end if
    end if
  end subroutine easyOA_char_6d
  
  Subroutine easyI_char_6d(fname, vname, data, type_info)
    character(*),intent(out) :: data(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_char_nd(fname, vname, data, shape(data), type_info)
    else
      call easyI_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine easyI_char_6d
  Subroutine easyIA_char_6d(fname, vname, data, type_info)
    character(*),allocatable,intent(inout) :: data(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (.not. allocated(data)) then
      call getDims2_enc(fname, vname, dimsT_enc)
      if (hasVar(fname, trim(vname) // '.bounds') .eq. 1) then
        allocate(dimsT2_enc(2, size(dimsT_enc)))
        call easyI(fname, trim(vname) // '.bounds', dimsT2_enc)
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3), &
    dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6))) ! 
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(data(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6)))
        deallocate(dimsT_enc)
      end if
    end if
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_char_nd(fname, vname, data, shape(data), type_info)
    else
      call easyI_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine easyIA_char_6d
  Subroutine easyO_char_nd(fname, vname, data, dims_data, type_info, type_ele_alone)
    ! vname : name, or name%name
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    character(*),intent(in) :: data(*)
    character(*),intent(in) :: fname, vname
    integer, intent(in) :: dims_data(:)
    integer, intent(in), optional :: type_info(:, :) ! 2 x n, 0 : type array size, 1 : type position
    integer, intent(in), optional :: type_ele_alone
    
    character(80) :: ovname, strT, posSuffix
    integer,allocatable :: dims(:)
    integer :: ncid, vid, ierr
    integer :: ndims
    
    integer,allocatable :: dids(:)
    integer :: i, j, k, l, m, n
    integer :: data_size, ndims_data, ndims_type
    
    logical :: isExist
    character(40), allocatable :: dimnames(:)
    character(80) :: tname, tvname
    integer,allocatable :: start_index(:), count_lens(:)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> body
    ! ================== get dims for both base and type-var mode
    ndims_data = size(dims_data)
    if (present(type_info)) then
      if (present(type_ele_alone)) then   ! save type-element, 1 variable for 1 position
        ndims = ndims_data
        allocate(dims(ndims+1), STAT=ierr)
        call check_enc(ierr, 'allocate')
        dims(1) = len(data)
        dims(2:) = dims_data
      else
        ndims_type = size(type_info(1, :))
        ndims = ndims_data + ndims_type
        allocate(dims(ndims+1), STAT=ierr)
        call check_enc(ierr, 'allocate')
        dims = len(data)
        dims(2:ndims_data+1) = dims_data
        do i = ndims_data + 2, ndims+1
          dims(i) = type_info(1,ndims_type - (i-1-ndims_data) + 1)
        end do
      end if
    else
      ndims = ndims_data
      allocate(dims(ndims+1), STAT=ierr)  ! add length-dim
      call check_enc(ierr, 'allocate')
      dims(1) = len(data)
      dims(2:) = dims_data
    end if
    ovname = vname
    ! ================== get tname and tvname for type-var
    if (present(type_info)) then
      i = scan(vname, '%')
      if (i .eq. 0) then
        stop "Error in easyO_char_nd! please use % to link type and type-var"
      end if
      tname = vname(:i-1)
      tvname = vname(i+1:)
      if (present(type_ele_alone)) then
        posSuffix = '('
        do j = 1, size(type_info(1,:))
          write(strT, '(I0)') type_info(2,j)
          ! print *, 'strT=',strT
          posSuffix = trim(posSuffix)//trim(strT)
          if (j .lt. size(type_info(1,:))) posSuffix = trim(posSuffix)//','
        end do
        posSuffix = trim(posSuffix)//')'
        ovname = trim(tname)//trim(posSuffix)//'%'//trim(tvname)
        ! print *, 'ovname = ',ovname
      end if
    end if
    allocate(dimnames(ndims+1), STAT = ierr)
    if (ierr .ne. 0) then
      stop 1
    end if
    
    allocate(dids(ndims+1), STAT = ierr)
    if (ierr .ne. 0) then
      stop 1
    end if
    
    allocate(start_index(ndims+1), STAT=ierr)
    call check_enc(ierr, 'allocate start_index')
    start_index = 1
    if (present(type_info)) then
      ! print *, 'type_info=',type_info(1,:), type_info(2,:)
      do i = ndims_data + 2, ndims+1
        start_index(i) = type_info(2, ndims_type - (i-1-ndims_data) + 1)
      end do
    end if
    allocate(count_lens(ndims+1), STAT=ierr)
    if (present(type_info)) then
      count_lens(1) = len(data)
      count_lens(2:ndims_data+1) = dims_data
      count_lens(ndims_data+2:) = 1
    else
      count_lens(1) = len(data)
      count_lens(2:) = dims_data
    end if
    data_size = product(dims_data) * len(data)
    
    ! ================== set dimension names
    if (present(type_info)) then
      dimnames(1)=trim(vname)//"_L"
      if (.not. present(type_ele_alone)) then
        do i = 2, ndims_data+1
          write(dimnames(i), '(A, "_d", I1)') trim(tvname), ndims_data-i+2
        end do
        do i = ndims_data + 2, ndims+1
          write(dimnames(i), '(A, "_d", I1)') trim(tname), i-1-ndims_data
        end do
      else
        do i = 2, ndims+1
          write(dimnames(i), '(A, A, "%", A, "_d", I1)') trim(tname), trim(posSuffix), trim(tvname), ndims_data-i+2
        end do
      end if
    else
      dimnames(1)=trim(vname)//"_L"
      do i = 2, ndims+1
        write(dimnames(i), '(A, "_d", I1)') trim(vname), ndims_data-i+2
      end do
    end if
    ! print *, 'dimnames = ', dimnames
    ! =========================== netcdf-relative
    inquire(file = fname, exist = isExist)
    ! print *, "isExist=",isExist
    ! ~~~~~~~~~~~~ open or create nc
    if (isExist) then
      call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "nf90_open")
      call check_enc( nf90_redef(ncid), "nf90_redef")
    else
      call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "nf90_create")
    end if
    ! ~~~~~~~~~~~~ get or define dimension
    ! print *, 'dimnames = ', dimnames
    do i = 1, ndims+1
      if (isExist) then
        if (nf90_inq_dimid(ncid, dimnames(i), dids(i)) .ne. nf90_noerr) then
          call check_enc(nf90_def_dim(ncid, dimnames(i), dims(i), dids(i)), "nf90_def_dim")
        end if
      else
        call check_enc(nf90_def_dim(ncid, dimnames(i), dims(i), dids(i)), "nf90_def_dim")
      end if
    end do
    ! ~~~~~~~~~~~~ define variable, or error
    if (nf90_inq_varid(ncid, ovname, vid) .eq. nf90_noerr) then
      if (.not. present(type_info)) then
        write(*,*) 'Error! variable already exists!'
        call check_enc( nf90_close(ncid) , "nf90_close")
        stop 1
      end if
    else
      call check_enc( nf90_def_var(ncid, ovname, NF90_CHAR, dids, vid) , "nf90_def_var")
    end if
    call check_enc( nf90_enddef(ncid) , "nf90_enddef")
    ! ~~~~~~~~~~~~ write data
    ! print *, 'start_index = ',start_index
    ! print *, 'count_lens = ',count_lens
    ! print *, 'dims = ', dims
    call check_enc( nf90_put_var(ncid, vid, data(1:data_size), start=start_index, count=count_lens) , &
      "In easyO_char_nd, nf90_put_var, "//ovname)
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    return
  end subroutine easyO_char_nd
  Subroutine easyO_char(fname, vname, val, type_info)
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    character(*),intent(in) :: val
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: type_info(:, :) ! 2 x n, 0 : type array size, 1 : type position
    
    integer,allocatable :: dims(:)
    
    integer :: ncid, vid, ierr
    
    integer :: i, j, k, l, m, n, did
    
    logical :: isExist
    character(40), allocatable :: dimnames(:)
    integer,allocatable :: dids(:), start_index(:)
    integer :: ndims_type
    character(80) :: tname, tvname
    
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> body
    ! =========================== netcdf-relative
    inquire(file = fname, exist = isExist)
    ! print *, "isExist=",isExist
    
    if (present(type_info)) then
      ndims_type = size(type_info(1, :))
      allocate(dims(ndims_type+1), STAT=ierr)
      call check_enc(ierr, 'allocate')
      dims(1) = len(val)
      dims(2:) = type_info(1, :)
    ! ================== get tname and tvname for type-var
      i = scan(vname, '%')
      if (i .eq. 0) then
        stop "Error in easyO_char! please use % to link type and type-var"
      end if
      tname = vname(:i-1)
      tvname = vname(i+1:)
      allocate(dimnames(ndims_type+1), STAT = ierr)
      call check_enc(ierr, 'allocate for dimnames')
      ! ================== set dimension names
      dimnames(1) = trim(tname)//"_L"
      do i = 2, ndims_type+1
        write(dimnames(i), '("'//trim(tname)//'", "_d", I1)') i-1
      end do
      allocate(dids(ndims_type+1), STAT = ierr)
      call check_enc(ierr, 'allocate dids')
      allocate(start_index(ndims_type+1), STAT=ierr)
      call check_enc(ierr, 'allocate start_index')
      ! print *, 'type_info=',type_info(1,:), type_info(2,:)
      start_index = 1
      do i = 2, ndims_type+1
        start_index(i) = type_info(2, ndims_type - (i-1) + 1)
      end do
    end if
    ! ~~~~~~~~~~~~ open or create nc
    if (isExist) then
      call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "nf90_open")
      call check_enc(nf90_redef(ncid), "nf90_redef")
    else
      call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "nf90_create")
    end if
    ! ~~~~~~~~~~~~ get or define dimension
    if (present(type_info)) then
      do i = 1, ndims_type+1
        if (isExist) then
          if (nf90_inq_dimid(ncid, dimnames(i), dids(i)) .ne. nf90_noerr) then
            call check_enc(nf90_def_dim(ncid, dimnames(i), dims(i), dids(i)), "nf90_def_dim")
          end if
        else
          call check_enc(nf90_def_dim(ncid, dimnames(i), dims(i), dids(i)), "nf90_def_dim")
        end if
      end do
    else
      call check_enc(nf90_def_dim(ncid, vname//'_L', len(val), did), 'nf90_def_dim')
    end if
    ! ~~~~~~~~~~~~ define variable, or error
    if (nf90_inq_varid(ncid, vname, vid) .eq. nf90_noerr) then
      if (.not. present(type_info)) then
        write(*,*) 'Error! variable already exists!'
        call check_enc( nf90_close(ncid) , "nf90_close")
        stop 1
      end if
    else
      if (present(type_info)) then
        call check_enc( nf90_def_var(ncid, vname, NF90_CHAR, dids, vid) , "nf90_def_var")
      else
        call check_enc( nf90_def_var(ncid, vname, NF90_CHAR, did, vid) , "nf90_def_var")
      end if
    end if
    call check_enc( nf90_enddef(ncid) , "nf90_enddef")
    ! ~~~~~~~~~~~~ write val
    if (present(type_info)) then
      call check_enc( nf90_put_var(ncid, vid, val, start=start_index) , "nf90_put_var")
    else
      call check_enc( nf90_put_var(ncid, vid, val) , "nf90_put_var")
    end if
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    return
  end subroutine easyO_char
  Subroutine easyI_char_nd(fname, vname, data, dims_data, type_info)
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    character(*),intent(out) :: data(*)
    character(*),intent(in) :: fname, vname
    integer, intent(in) :: dims_data(:)
    integer, intent(in), optional :: type_info(:, :) ! 2 x n, 0 : type array size, 1 : type position
    
    character(80) :: ivname, strT, posSuffix, tname, tvname
    integer :: type_ele_alone
    integer,allocatable :: dims(:)
    integer :: ncid, vid, ierr
    integer :: ndims
    
    integer :: i, j, k, l, m, n
    integer :: data_size, ndims_data, ndims_type
    
    logical :: isExist
    integer,allocatable :: start_index(:), count_lens(:)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> body
    ! ================== get dims for both base and type-var mode
    ndims_data = size(dims_data)
    ivname = vname
    if (present(type_info)) then
      i = scan(vname, '%')
      if (i .eq. 0) then
        stop "Error in easyI_char_nd! please use % to link type and type-var"
      end if
      tname = vname(:i-1)
      tvname = vname(i+1:)
      if (hasVar(fname, vname) .eq. 1) then
        type_ele_alone = 0
      else
        type_ele_alone = 1
        posSuffix = '('
        do j = 1, size(type_info(1,:))
          write(strT, '(I0)') type_info(2,j)
          ! print *, 'strT=',strT
          posSuffix = trim(posSuffix)//trim(strT)
          if (j .lt. size(type_info(1,:))) posSuffix = trim(posSuffix)//','
        end do
        posSuffix = trim(posSuffix)//')'
        ivname = trim(tname)//trim(posSuffix)//'%'//trim(tvname)
        ! print *, 'ovname = ',ovname
      end if
      if (type_ele_alone .eq. 1) then
        ndims = ndims_data
        allocate(dims(ndims+1), STAT=ierr)
        call check_enc(ierr, 'allocate')
        dims = dims_data
      else
        ndims_type = size(type_info(1, :))
        ndims = ndims_data + ndims_type
        allocate(dims(ndims+1), STAT=ierr)
        call check_enc(ierr, 'allocate')
        dims(1) = len(data)
        dims(2:ndims_data+1) = dims_data
        do i = ndims_data + 2, ndims+1
          dims(i) = type_info(1,ndims_type - (i-1-ndims_data) + 1)
        end do
      end if
    else
      ndims = ndims_data
      allocate(dims(ndims+1), STAT=ierr)
      call check_enc(ierr, 'allocate')
      dims(1) = len(data)
      dims(2:) = dims_data
    end if
    ! ================== get tname and tvname for type-var
    allocate(start_index(ndims+1), STAT=ierr)
    call check_enc(ierr, 'allocate start_index')
    start_index = 1
    if (present(type_info)) then
    ! print *, 'type_info=',type_info(1,:), type_info(2,:)
      if (type_ele_alone .eq. 0) then
        do i = ndims_data + 2, ndims+1
          start_index(i) = type_info(2, ndims_type - (i-1-ndims_data) + 1)
        end do
      end if
    end if
    allocate(count_lens(ndims+1), STAT=ierr)
    if (present(type_info)) then
      if (type_ele_alone .eq. 0) then
        count_lens(1) = len(data)
        count_lens(2:ndims_data+1) = dims_data
        count_lens(ndims_data+2:) = 1
      else
        count_lens(1) = len(data)
        count_lens(2:) = dims_data
      end if
    else
      count_lens(1) = len(data)
      count_lens(2:) = dims_data
    end if
    data_size = product(dims_data) * len(data)
    
    ! =========================== netcdf-relative
    inquire(file = fname, exist = isExist)
    ! print *, "isExist=",isExist
    ! ~~~~~~~~~~~~ open or create nc
    if (isExist) then
      call check_enc(nf90_open(fname, NF90_NOWRITE, ncid) , "nf90_open")
    else
      print *, "Error, file doesn't exist."
      stop "Stopped"
    end if
    ! ~~~~~~~~~~~~ inquire variable
    call check_enc(nf90_inq_varid(ncid, ivname, vid), &
      'in easyI_char_nd, nf90_inq_varid for '//trim(ivname))
    ! ~~~~~~~~~~~~ read data
    ! print *, 'start_index = ',start_index
    ! print *, 'count_lens = ',count_lens
    call check_enc( nf90_get_var(ncid, vid, data(1:data_size), start=start_index, count=count_lens), &
      "in easyI_char_nd, nf90_get_var, "//ivname)
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    return
  end subroutine easyI_char_nd
  
  Subroutine easyI_char(fname, vname, val, type_info)
    ! almost Duplicated with easyI_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    character(*),intent(out) :: val
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: type_info(:, :) ! 2 x n, 0 : type array size, 1 : type position
    
    integer,allocatable :: dims(:)
    
    integer :: ncid, vid, ierr, did
    
    integer :: i, j, k, l, m, n
    
    logical :: isExist
    integer,allocatable :: start_index(:)
    integer :: ndims_type
    
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> body
    ! =========================== netcdf-relative
    ! print *, "isExist=",isExist
    
    if (present(type_info)) then
      ndims_type = size(type_info(1, :))
      allocate(dims(ndims_type+1), STAT=ierr)
      call check_enc(ierr, 'allocate')
      dims(1) = len(val)
      dims(2:) = type_info(1, :)
    ! ================== get tname and tvname for type-var
      allocate(start_index(ndims_type+1), STAT=ierr)
      call check_enc(ierr, 'allocate start_index')
      ! print *, 'type_info=',type_info(1,:), type_info(2,:)
      start_index = 1
      do i = 2, ndims_type+1
        start_index(i) = type_info(2, ndims_type - (i-1) + 1)
      end do
    end if
    inquire(file = fname, exist = isExist)
    ! ~~~~~~~~~~~~ open or create nc
    if (isExist) then
      call check_enc(nf90_open(fname, NF90_NOWRITE, ncid) , "nf90_open")
    else
      print *, "Error, file doesn't exist."
      stop "Stopped"
    end if
    ! ~~~~~~~~~~~~ inquire variable
    call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
    ! ~~~~~~~~~~~~ get val
    if (present(type_info)) then
      call check_enc( nf90_get_var(ncid, vid, val, start=start_index) , "in nf90_get_var, "//vname)
    else
      call check_enc( nf90_get_var(ncid, vid, val) , "in nf90_get_var, "//vname)
    end if
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    return
  end subroutine easyI_char
  function hasVar(fname, vname, pos)  ! postion
    implicit none
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: pos(:)
    character(80) :: vname2, strT, posSuffix
    integer :: i,j,k
    integer :: hasVar, ncid, status, vid
    logical :: isExist
    inquire(file = fname, exist = isExist)
    if (isExist) then
      call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "nf90_open")
    else
      write(*,*) 'Error! file not exist : '//trim(fname)
      stop 1
    end if
    vname2 = vname
    if (present(pos)) then
      posSuffix = ''
      do i = 1, size(pos)
        write(strT, '(I0)') pos(i)
        posSuffix = trim(posSuffix)//trim(strT)
        if (i .lt. size(pos)) posSuffix = trim(posSuffix)//'_'
      end do
      vname2 = trim(vname2)//'.'//trim(posSuffix)
    end if
    status = nf90_inq_varid(ncid, vname2, vid)
    if (status /= 0) then
      hasVar = 0
    else
      varname_enc = vname2
      hasVar = 1
    end if
    call check_enc(nf90_close(ncid) , "in hasVar, nf90_close")
    return
  end function
  function hasDim(fname, dname)  ! postion
    implicit none
    character(*),intent(in) :: fname, dname
    integer :: i,j,k
    integer :: hasDim, ncid, status
    logical :: isExist
    inquire(file = fname, exist = isExist)
    if (isExist) then
      call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "nf90_open")
    else
      write(*,*) 'Error! file not exist : '//trim(fname)
      stop 1
    end if
    status = nf90_inq_dimid(ncid, trim(dname)//'_d1', k)
    if (status /= 0) then
      hasDim = 0
    else
      hasDim = 1
    end if
    call check_enc(nf90_close(ncid) , "in hasDim, nf90_close")
    return
  end function
  function getDims_enc(fname, vname) result(dims)  ! return dynamic dims 
    implicit none
    character(*),intent(in) :: fname, vname
    integer :: i, j, k, l, m, n
    integer :: ncid, vid, ierr, ndims
    logical :: isExist
    integer :: xt  ! xtype
    integer :: dids(10)
    integer, allocatable :: dims(:)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ================== open netcdf file
    inquire(file = fname, exist = isExist)
    if (isExist) then
      call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "nf90_open")
    else
      write(*,*) 'Error! file not exist : '//trim(fname)
      stop 1
    end if
    ! ================== get variable and its dimensions
    call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid')
    call check_enc(nf90_inquire_variable(ncid, vid, ndims=ndims), 'nf90_inquire_variable')
    call check_enc(nf90_inquire_variable(ncid, vid, dimids=dids(:ndims)), 'nf90_inquire_variable')
    call check_enc(nf90_inquire_variable(ncid, vid, xtype=xt), 'in nf90_inquire_variable for xtype')
    if (xt .eq. NF90_CHAR) then
      allocate(dims(ndims-1), stat=i)
      call check_enc(i, 'allocate : dims(ndims)')
      do i = 1, ndims - 1
        call check_enc(nf90_inquire_dimension(ncid, dids(i+1), len=dims(i)), 'nf90_inquire_dimension')
      end do
    else
      allocate(dims(ndims), stat=i)
      call check_enc(i, 'allocate : dims(ndims)')
      do i = 1, ndims
        call check_enc(nf90_inquire_dimension(ncid, dids(i), len=dims(i)), 'nf90_inquire_dimension')
      end do
    endif
    call check_enc(nf90_close(ncid) , "in getDims_enc, nf90_close")
  end function
    
  subroutine getDims2_enc(fname, vname, dims)  ! pass allocatable dims in 
    implicit none
    character(*),intent(in) :: fname, vname
    integer, allocatable :: dims(:)
    integer :: i, j, k, l, m, n
    integer :: ncid, vid, ierr, ndims
    logical :: isExist
    integer :: xt  ! xtype
    integer :: dids(10)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ================== open netcdf file
    inquire(file = fname, exist = isExist)
    if (isExist) then
      call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "nf90_open")
    else
      write(*,*) 'Error! file not exist : '//trim(fname)
      stop 1
    end if
    ! ================== get variable and its dimensions
    call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid')
    call check_enc(nf90_inquire_variable(ncid, vid, ndims=ndims), 'nf90_inquire_variable')
    call check_enc(nf90_inquire_variable(ncid, vid, dimids=dids(:ndims)), 'nf90_inquire_variable')
    call check_enc(nf90_inquire_variable(ncid, vid, xtype=xt), 'in nf90_inquire_variable for xtype')
    if (xt .eq. NF90_CHAR) then
      allocate(dims(ndims-1), stat=i)
      call check_enc(i, 'allocate : dims(ndims)')
      do i = 1, ndims - 1
        call check_enc(nf90_inquire_dimension(ncid, dids(i+1), len=dims(i)), 'nf90_inquire_dimension')
      end do
    else
      allocate(dims(ndims), stat=i)
      call check_enc(i, 'allocate : dims(ndims)')
      do i = 1, ndims
        call check_enc(nf90_inquire_dimension(ncid, dids(i), len=dims(i)), 'nf90_inquire_dimension')
      end do
    endif
    call check_enc(nf90_close(ncid) , "in getDims2_enc, nf90_close")
  end subroutine
  subroutine getDims3_enc(fname, tname, dims)  ! pass allocatable dims in, get struct variable shape for allocate
    implicit none
    character(*),intent(in) :: fname, tname
    integer, allocatable :: dims(:)
    integer :: i, j, k, l, m, n
    integer :: ncid, vid, ierr, ndims
    logical :: isExist
    character(1) :: c
    integer :: xt  ! xtype
    integer :: dids(10)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ================== open netcdf file
    inquire(file = fname, exist = isExist)
    if (isExist) then
      call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "nf90_open")
    else
      write(*,*) 'Error! file not exist : '//trim(fname)
      stop 1
    end if
    ! ================== get variable and its dimensions
    do i = 1, 6
      write(c, '(I1)') i
      ierr = nf90_inq_dimid(ncid, trim(tname)//'_d'//c, dids(i))
      if (ierr .ne. 0) exit
    end do
    ! print *, 'sss = ',i
    allocate(dims(i-1), stat=ierr)
    call check_enc(ierr, 'in allocate')
    do j = 1, i-1
      call check_enc(nf90_inquire_dimension(ncid, dids(j), len=dims(i-j)), 'nf90_inquire_dimension')
    end do
    call check_enc(nf90_close(ncid) , "in getDims3_enc, nf90_close")
    return
  end subroutine
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
