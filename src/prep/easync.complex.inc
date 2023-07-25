
Subroutine easyO_complex4_scalar(fname, vname, val, shape_total, position)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    Complex(kind=4),intent(in) :: val
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)      ! total shape
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=4) :: val_re, val_im

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    val_re = real(val)
    val_im = aimag(val)

    if (present(shape_total)) then
      call easyO_real4_scalar(fname, trim(vname)//'.real', val_re, shape_total, position)
      call easyO_real4_scalar(fname, trim(vname)//'.imag', val_im, shape_total, position)
    else
      call easyO_real4_scalar(fname, trim(vname)//'.real', val_re)
      call easyO_real4_scalar(fname, trim(vname)//'.imag', val_im)
    end if

    return
end subroutine easyO_complex4_scalar

Subroutine easyI_complex4_scalar(fname, vname, val, position)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    Complex(kind=4),intent(out) :: val
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: position(:)         ! `start` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
    real(kind=4) :: val_re, val_im

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    val_re = real(val)
    val_im = aimag(val)

    if (present(position)) then
      call easyI_real4_scalar(fname, trim(vname)//'.real', val_re, position)
      call easyI_real4_scalar(fname, trim(vname)//'.imag', val_im, position)
    else
      call easyI_real4_scalar(fname, trim(vname)//'.real', val_re)
      call easyI_real4_scalar(fname, trim(vname)//'.imag', val_im)
    end if

    val = cmplx(val_re, val_im)

    return
end subroutine easyI_complex4_scalar

Subroutine easyO_complex8_scalar(fname, vname, val, shape_total, position)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    Complex(kind=8),intent(in) :: val
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)      ! total shape
    integer, intent(in), optional :: position(:)         ! `start` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=8) :: val_re, val_im

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body

    val_re = real(val)
    val_im = aimag(val)

    if (present(shape_total)) then
        call easyO_real8_scalar(fname, trim(vname)//'.real', val_re, shape_total, position)
        call easyO_real8_scalar(fname, trim(vname)//'.imag', val_im, shape_total, position)
    else
        call easyO_real8_scalar(fname, trim(vname)//'.real', val_re)
        call easyO_real8_scalar(fname, trim(vname)//'.imag', val_im)
    end if

    return
end subroutine easyO_complex8_scalar

Subroutine easyI_complex8_scalar(fname, vname, val, position)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    Complex(kind=8),intent(out) :: val
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: position(:)         ! `start` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=8) :: val_re, val_im

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    val_re = real(val)
    val_im = aimag(val)

    if (present(position)) then
        call easyI_real8_scalar(fname, trim(vname)//'.real', val_re, position)
        call easyI_real8_scalar(fname, trim(vname)//'.imag', val_im, position)
    else
        call easyI_real8_scalar(fname, trim(vname)//'.real', val_re)
        call easyI_real8_scalar(fname, trim(vname)//'.imag', val_im)
    end if

    val = cmplx(val_re, val_im, 8)

    return
end subroutine easyI_complex8_scalar



Subroutine easyO_complex4_1d(fname, vname, val, shape_total, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=4),intent(in) :: val(:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)      ! total shape
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=4) ,allocatable :: val_re(:), val_im(:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val)))
    allocate(val_im(size(val)))

    val_re = real(val)
    val_im = aimag(val)

    if (present(shape_total)) then
        call easyO_real4_1d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
        call easyO_real4_1d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
    else
        call easyO_real4_1d(fname, trim(vname)//'.real', val_re)
        call easyO_real4_1d(fname, trim(vname)//'.imag', val_im)
    end if

    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyO_complex4_1d

Subroutine easyI_complex4_1d(fname, vname, val, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=4),intent(out) :: val(:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=4) ,allocatable :: val_re(:), val_im(:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val)))
    allocate(val_im(size(val)))

    if (present(count_lens)) then
        call easyI_real4_1d(fname, trim(vname)//'.real', val_re, position, count_lens)
        call easyI_real4_1d(fname, trim(vname)//'.imag', val_im, position, count_lens)
    else
        call easyI_real4_1d(fname, trim(vname)//'.real', val_re)
        call easyI_real4_1d(fname, trim(vname)//'.imag', val_im)
    end if

    val = cmplx(val_re, val_im)
    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyI_complex4_1d

Subroutine easyO_complex8_1d(fname, vname, val, shape_total, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=8),intent(in) :: val(:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)      ! total shape
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=8) ,allocatable :: val_re(:), val_im(:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val)))
    allocate(val_im(size(val)))

    val_re = real(val)
    val_im = aimag(val)

    if (present(shape_total)) then
        call easyO_real8_1d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
        call easyO_real8_1d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
    else
        call easyO_real8_1d(fname, trim(vname)//'.real', val_re)
        call easyO_real8_1d(fname, trim(vname)//'.imag', val_im)
    end if

    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyO_complex8_1d

Subroutine easyI_complex8_1d(fname, vname, val, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=8),intent(out) :: val(:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=8) ,allocatable :: val_re(:), val_im(:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val)))
    allocate(val_im(size(val)))

    if (present(count_lens)) then
        call easyI_real8_1d(fname, trim(vname)//'.real', val_re, position, count_lens)
        call easyI_real8_1d(fname, trim(vname)//'.imag', val_im, position, count_lens)
    else
        call easyI_real8_1d(fname, trim(vname)//'.real', val_re)
        call easyI_real8_1d(fname, trim(vname)//'.imag', val_im)
    end if

    val = cmplx(val_re, val_im)
    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyI_complex8_1d
Subroutine easyO_complex4_2d(fname, vname, val, shape_total, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=4),intent(in) :: val(:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)      ! total shape
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=4) ,allocatable :: val_re(:,:), val_im(:,:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val(:,1)), size(val(1,:))))
    allocate(val_im(size(val(:,1)), size(val(1,:))))

    val_re = real(val)
    val_im = aimag(val)

    if (present(shape_total)) then
        call easyO_real4_2d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
        call easyO_real4_2d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
    else
        call easyO_real4_2d(fname, trim(vname)//'.real', val_re)
        call easyO_real4_2d(fname, trim(vname)//'.imag', val_im)
    end if

    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyO_complex4_2d

Subroutine easyI_complex4_2d(fname, vname, val, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=4),intent(out) :: val(:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=4) ,allocatable :: val_re(:,:), val_im(:,:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val(:,1)), size(val(1,:))))
    allocate(val_im(size(val(:,1)), size(val(1,:))))

    if (present(count_lens)) then
        call easyI_real4_2d(fname, trim(vname)//'.real', val_re, position, count_lens)
        call easyI_real4_2d(fname, trim(vname)//'.imag', val_im, position, count_lens)
    else
        call easyI_real4_2d(fname, trim(vname)//'.real', val_re)
        call easyI_real4_2d(fname, trim(vname)//'.imag', val_im)
    end if

    val = cmplx(val_re, val_im)
    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyI_complex4_2d

Subroutine easyO_complex8_2d(fname, vname, val, shape_total, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=8),intent(in) :: val(:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)      ! total shape
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=8) ,allocatable :: val_re(:,:), val_im(:,:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val(:,1)), size(val(1,:))))
    allocate(val_im(size(val(:,1)), size(val(1,:))))

    val_re = real(val)
    val_im = aimag(val)

    if (present(shape_total)) then
        call easyO_real8_2d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
        call easyO_real8_2d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
    else
        call easyO_real8_2d(fname, trim(vname)//'.real', val_re)
        call easyO_real8_2d(fname, trim(vname)//'.imag', val_im)
    end if

    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyO_complex8_2d

Subroutine easyI_complex8_2d(fname, vname, val, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=8),intent(out) :: val(:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=8) ,allocatable :: val_re(:,:), val_im(:,:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val(:,1)), size(val(1,:))))
    allocate(val_im(size(val(:,1)), size(val(1,:))))

    if (present(count_lens)) then
        call easyI_real8_2d(fname, trim(vname)//'.real', val_re, position, count_lens)
        call easyI_real8_2d(fname, trim(vname)//'.imag', val_im, position, count_lens)
    else
        call easyI_real8_2d(fname, trim(vname)//'.real', val_re)
        call easyI_real8_2d(fname, trim(vname)//'.imag', val_im)
    end if

    val = cmplx(val_re, val_im)
    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyI_complex8_2d
Subroutine easyO_complex4_3d(fname, vname, val, shape_total, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=4),intent(in) :: val(:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)      ! total shape
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=4) ,allocatable :: val_re(:,:,:), val_im(:,:,:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))))
    allocate(val_im(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))))

    val_re = real(val)
    val_im = aimag(val)

    if (present(shape_total)) then
        call easyO_real4_3d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
        call easyO_real4_3d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
    else
        call easyO_real4_3d(fname, trim(vname)//'.real', val_re)
        call easyO_real4_3d(fname, trim(vname)//'.imag', val_im)
    end if

    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyO_complex4_3d

Subroutine easyI_complex4_3d(fname, vname, val, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=4),intent(out) :: val(:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=4) ,allocatable :: val_re(:,:,:), val_im(:,:,:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))))
    allocate(val_im(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))))

    if (present(count_lens)) then
        call easyI_real4_3d(fname, trim(vname)//'.real', val_re, position, count_lens)
        call easyI_real4_3d(fname, trim(vname)//'.imag', val_im, position, count_lens)
    else
        call easyI_real4_3d(fname, trim(vname)//'.real', val_re)
        call easyI_real4_3d(fname, trim(vname)//'.imag', val_im)
    end if

    val = cmplx(val_re, val_im)
    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyI_complex4_3d

Subroutine easyO_complex8_3d(fname, vname, val, shape_total, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=8),intent(in) :: val(:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)      ! total shape
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=8) ,allocatable :: val_re(:,:,:), val_im(:,:,:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))))
    allocate(val_im(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))))

    val_re = real(val)
    val_im = aimag(val)

    if (present(shape_total)) then
        call easyO_real8_3d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
        call easyO_real8_3d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
    else
        call easyO_real8_3d(fname, trim(vname)//'.real', val_re)
        call easyO_real8_3d(fname, trim(vname)//'.imag', val_im)
    end if

    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyO_complex8_3d

Subroutine easyI_complex8_3d(fname, vname, val, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=8),intent(out) :: val(:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=8) ,allocatable :: val_re(:,:,:), val_im(:,:,:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))))
    allocate(val_im(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))))

    if (present(count_lens)) then
        call easyI_real8_3d(fname, trim(vname)//'.real', val_re, position, count_lens)
        call easyI_real8_3d(fname, trim(vname)//'.imag', val_im, position, count_lens)
    else
        call easyI_real8_3d(fname, trim(vname)//'.real', val_re)
        call easyI_real8_3d(fname, trim(vname)//'.imag', val_im)
    end if

    val = cmplx(val_re, val_im)
    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyI_complex8_3d
Subroutine easyO_complex4_4d(fname, vname, val, shape_total, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=4),intent(in) :: val(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)      ! total shape
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=4) ,allocatable :: val_re(:,:,:,:), val_im(:,:,:,:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))))
    allocate(val_im(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))))

    val_re = real(val)
    val_im = aimag(val)

    if (present(shape_total)) then
        call easyO_real4_4d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
        call easyO_real4_4d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
    else
        call easyO_real4_4d(fname, trim(vname)//'.real', val_re)
        call easyO_real4_4d(fname, trim(vname)//'.imag', val_im)
    end if

    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyO_complex4_4d

Subroutine easyI_complex4_4d(fname, vname, val, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=4),intent(out) :: val(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=4) ,allocatable :: val_re(:,:,:,:), val_im(:,:,:,:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))))
    allocate(val_im(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))))

    if (present(count_lens)) then
        call easyI_real4_4d(fname, trim(vname)//'.real', val_re, position, count_lens)
        call easyI_real4_4d(fname, trim(vname)//'.imag', val_im, position, count_lens)
    else
        call easyI_real4_4d(fname, trim(vname)//'.real', val_re)
        call easyI_real4_4d(fname, trim(vname)//'.imag', val_im)
    end if

    val = cmplx(val_re, val_im)
    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyI_complex4_4d

Subroutine easyO_complex8_4d(fname, vname, val, shape_total, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=8),intent(in) :: val(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)      ! total shape
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=8) ,allocatable :: val_re(:,:,:,:), val_im(:,:,:,:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))))
    allocate(val_im(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))))

    val_re = real(val)
    val_im = aimag(val)

    if (present(shape_total)) then
        call easyO_real8_4d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
        call easyO_real8_4d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
    else
        call easyO_real8_4d(fname, trim(vname)//'.real', val_re)
        call easyO_real8_4d(fname, trim(vname)//'.imag', val_im)
    end if

    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyO_complex8_4d

Subroutine easyI_complex8_4d(fname, vname, val, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=8),intent(out) :: val(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=8) ,allocatable :: val_re(:,:,:,:), val_im(:,:,:,:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))))
    allocate(val_im(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))))

    if (present(count_lens)) then
        call easyI_real8_4d(fname, trim(vname)//'.real', val_re, position, count_lens)
        call easyI_real8_4d(fname, trim(vname)//'.imag', val_im, position, count_lens)
    else
        call easyI_real8_4d(fname, trim(vname)//'.real', val_re)
        call easyI_real8_4d(fname, trim(vname)//'.imag', val_im)
    end if

    val = cmplx(val_re, val_im)
    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyI_complex8_4d
Subroutine easyO_complex4_5d(fname, vname, val, shape_total, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=4),intent(in) :: val(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)      ! total shape
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=4) ,allocatable :: val_re(:,:,:,:,:), val_im(:,:,:,:,:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))))
    allocate(val_im(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))))

    val_re = real(val)
    val_im = aimag(val)

    if (present(shape_total)) then
        call easyO_real4_5d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
        call easyO_real4_5d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
    else
        call easyO_real4_5d(fname, trim(vname)//'.real', val_re)
        call easyO_real4_5d(fname, trim(vname)//'.imag', val_im)
    end if

    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyO_complex4_5d

Subroutine easyI_complex4_5d(fname, vname, val, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=4),intent(out) :: val(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=4) ,allocatable :: val_re(:,:,:,:,:), val_im(:,:,:,:,:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))))
    allocate(val_im(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))))

    if (present(count_lens)) then
        call easyI_real4_5d(fname, trim(vname)//'.real', val_re, position, count_lens)
        call easyI_real4_5d(fname, trim(vname)//'.imag', val_im, position, count_lens)
    else
        call easyI_real4_5d(fname, trim(vname)//'.real', val_re)
        call easyI_real4_5d(fname, trim(vname)//'.imag', val_im)
    end if

    val = cmplx(val_re, val_im)
    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyI_complex4_5d

Subroutine easyO_complex8_5d(fname, vname, val, shape_total, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=8),intent(in) :: val(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)      ! total shape
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=8) ,allocatable :: val_re(:,:,:,:,:), val_im(:,:,:,:,:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))))
    allocate(val_im(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))))

    val_re = real(val)
    val_im = aimag(val)

    if (present(shape_total)) then
        call easyO_real8_5d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
        call easyO_real8_5d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
    else
        call easyO_real8_5d(fname, trim(vname)//'.real', val_re)
        call easyO_real8_5d(fname, trim(vname)//'.imag', val_im)
    end if

    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyO_complex8_5d

Subroutine easyI_complex8_5d(fname, vname, val, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=8),intent(out) :: val(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=8) ,allocatable :: val_re(:,:,:,:,:), val_im(:,:,:,:,:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))))
    allocate(val_im(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))))

    if (present(count_lens)) then
        call easyI_real8_5d(fname, trim(vname)//'.real', val_re, position, count_lens)
        call easyI_real8_5d(fname, trim(vname)//'.imag', val_im, position, count_lens)
    else
        call easyI_real8_5d(fname, trim(vname)//'.real', val_re)
        call easyI_real8_5d(fname, trim(vname)//'.imag', val_im)
    end if

    val = cmplx(val_re, val_im)
    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyI_complex8_5d
Subroutine easyO_complex4_6d(fname, vname, val, shape_total, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=4),intent(in) :: val(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)      ! total shape
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=4) ,allocatable :: val_re(:,:,:,:,:,:), val_im(:,:,:,:,:,:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))))
    allocate(val_im(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))))

    val_re = real(val)
    val_im = aimag(val)

    if (present(shape_total)) then
        call easyO_real4_6d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
        call easyO_real4_6d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
    else
        call easyO_real4_6d(fname, trim(vname)//'.real', val_re)
        call easyO_real4_6d(fname, trim(vname)//'.imag', val_im)
    end if

    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyO_complex4_6d

Subroutine easyI_complex4_6d(fname, vname, val, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=4),intent(out) :: val(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=4) ,allocatable :: val_re(:,:,:,:,:,:), val_im(:,:,:,:,:,:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))))
    allocate(val_im(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))))

    if (present(count_lens)) then
        call easyI_real4_6d(fname, trim(vname)//'.real', val_re, position, count_lens)
        call easyI_real4_6d(fname, trim(vname)//'.imag', val_im, position, count_lens)
    else
        call easyI_real4_6d(fname, trim(vname)//'.real', val_re)
        call easyI_real4_6d(fname, trim(vname)//'.imag', val_im)
    end if

    val = cmplx(val_re, val_im)
    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyI_complex4_6d

Subroutine easyO_complex8_6d(fname, vname, val, shape_total, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=8),intent(in) :: val(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)      ! total shape
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=8) ,allocatable :: val_re(:,:,:,:,:,:), val_im(:,:,:,:,:,:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))))
    allocate(val_im(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))))

    val_re = real(val)
    val_im = aimag(val)

    if (present(shape_total)) then
        call easyO_real8_6d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
        call easyO_real8_6d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
    else
        call easyO_real8_6d(fname, trim(vname)//'.real', val_re)
        call easyO_real8_6d(fname, trim(vname)//'.imag', val_im)
    end if

    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyO_complex8_6d

Subroutine easyI_complex8_6d(fname, vname, val, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=8),intent(out) :: val(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=8) ,allocatable :: val_re(:,:,:,:,:,:), val_im(:,:,:,:,:,:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))))
    allocate(val_im(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))))

    if (present(count_lens)) then
        call easyI_real8_6d(fname, trim(vname)//'.real', val_re, position, count_lens)
        call easyI_real8_6d(fname, trim(vname)//'.imag', val_im, position, count_lens)
    else
        call easyI_real8_6d(fname, trim(vname)//'.real', val_re)
        call easyI_real8_6d(fname, trim(vname)//'.imag', val_im)
    end if

    val = cmplx(val_re, val_im)
    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyI_complex8_6d
Subroutine easyO_complex4_7d(fname, vname, val, shape_total, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=4),intent(in) :: val(:,:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)      ! total shape
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=4) ,allocatable :: val_re(:,:,:,:,:,:,:), val_im(:,:,:,:,:,:,:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:))))
    allocate(val_im(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:))))

    val_re = real(val)
    val_im = aimag(val)

    if (present(shape_total)) then
        call easyO_real4_7d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
        call easyO_real4_7d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
    else
        call easyO_real4_7d(fname, trim(vname)//'.real', val_re)
        call easyO_real4_7d(fname, trim(vname)//'.imag', val_im)
    end if

    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyO_complex4_7d

Subroutine easyI_complex4_7d(fname, vname, val, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=4),intent(out) :: val(:,:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=4) ,allocatable :: val_re(:,:,:,:,:,:,:), val_im(:,:,:,:,:,:,:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:))))
    allocate(val_im(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:))))

    if (present(count_lens)) then
        call easyI_real4_7d(fname, trim(vname)//'.real', val_re, position, count_lens)
        call easyI_real4_7d(fname, trim(vname)//'.imag', val_im, position, count_lens)
    else
        call easyI_real4_7d(fname, trim(vname)//'.real', val_re)
        call easyI_real4_7d(fname, trim(vname)//'.imag', val_im)
    end if

    val = cmplx(val_re, val_im)
    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyI_complex4_7d

Subroutine easyO_complex8_7d(fname, vname, val, shape_total, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=8),intent(in) :: val(:,:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)      ! total shape
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=8) ,allocatable :: val_re(:,:,:,:,:,:,:), val_im(:,:,:,:,:,:,:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:))))
    allocate(val_im(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:))))

    val_re = real(val)
    val_im = aimag(val)

    if (present(shape_total)) then
        call easyO_real8_7d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
        call easyO_real8_7d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
    else
        call easyO_real8_7d(fname, trim(vname)//'.real', val_re)
        call easyO_real8_7d(fname, trim(vname)//'.imag', val_im)
    end if

    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyO_complex8_7d

Subroutine easyI_complex8_7d(fname, vname, val, position, count_lens)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=8),intent(out) :: val(:,:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=8) ,allocatable :: val_re(:,:,:,:,:,:,:), val_im(:,:,:,:,:,:,:)

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    allocate(val_re(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:))))
    allocate(val_im(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:))))

    if (present(count_lens)) then
        call easyI_real8_7d(fname, trim(vname)//'.real', val_re, position, count_lens)
        call easyI_real8_7d(fname, trim(vname)//'.imag', val_im, position, count_lens)
    else
        call easyI_real8_7d(fname, trim(vname)//'.real', val_re)
        call easyI_real8_7d(fname, trim(vname)//'.imag', val_im)
    end if

    val = cmplx(val_re, val_im)
    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyI_complex8_7d
