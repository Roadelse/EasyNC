
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

    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size(shape(val))],order=[2,1]))
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

    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size(shape(val))],order=[2,1]))
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

  real(kind=4) :: val_re(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))), val_im(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:)))

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

    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size(shape(val))],order=[2,1]))
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

  real(kind=4) :: val_re(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))), val_im(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:)))


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

  real(kind=4) :: val_re(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))), val_im(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:)))

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

    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size(shape(val))],order=[2,1]))
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

  real(kind=4) :: val_re(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))), val_im(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:)))


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
      allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4)))
      allocate(val_re(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4)))
      allocate(val_im(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4)))
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

  real(kind=4) :: val_re(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))), val_im(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:)))

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

    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size(shape(val))],order=[2,1]))
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

  real(kind=4) :: val_re(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))), val_im(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:)))


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
      allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5)))
      allocate(val_re(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5)))
      allocate(val_im(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5)))
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

  real(kind=4) :: val_re(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))), val_im(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:)))

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

    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size(shape(val))],order=[2,1]))
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

  real(kind=4) :: val_re(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))), val_im(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:)))


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
      allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6)))
      allocate(val_re(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6)))
      allocate(val_im(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6)))
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

  real(kind=4) :: val_re(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:))), val_im(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:)))

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

    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size(shape(val))],order=[2,1]))
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

  real(kind=4) :: val_re(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:))), val_im(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:)))


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
      allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6),dimsT2_enc(1,7):dimsT2_enc(2,7)))
      allocate(val_re(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6),dimsT2_enc(1,7):dimsT2_enc(2,7)))
      allocate(val_im(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6),dimsT2_enc(1,7):dimsT2_enc(2,7)))
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

    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size(shape(val))],order=[2,1]))
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

    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size(shape(val))],order=[2,1]))
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

  real(kind=8) :: val_re(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))), val_im(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:)))

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

    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size(shape(val))],order=[2,1]))
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

  real(kind=8) :: val_re(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))), val_im(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:)))


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

  real(kind=8) :: val_re(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))), val_im(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:)))

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

    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size(shape(val))],order=[2,1]))
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

  real(kind=8) :: val_re(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))), val_im(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:)))


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
      allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4)))
      allocate(val_re(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4)))
      allocate(val_im(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4)))
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

  real(kind=8) :: val_re(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))), val_im(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:)))

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

    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size(shape(val))],order=[2,1]))
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

  real(kind=8) :: val_re(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))), val_im(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:)))


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
      allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5)))
      allocate(val_re(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5)))
      allocate(val_im(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5)))
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

  real(kind=8) :: val_re(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))), val_im(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:)))

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

    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size(shape(val))],order=[2,1]))
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

  real(kind=8) :: val_re(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))), val_im(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:)))


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
      allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6)))
      allocate(val_re(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6)))
      allocate(val_im(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6)))
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

  real(kind=8) :: val_re(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:))), val_im(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:)))

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

    if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size(shape(val))],order=[2,1]))
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

  real(kind=8) :: val_re(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:))), val_im(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:)))


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
      allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6),dimsT2_enc(1,7):dimsT2_enc(2,7)))
      allocate(val_re(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6),dimsT2_enc(1,7):dimsT2_enc(2,7)))
      allocate(val_im(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6),dimsT2_enc(1,7):dimsT2_enc(2,7)))
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
