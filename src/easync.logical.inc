
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



  Subroutine easyO_logical_1d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,intent(in) :: val(:)
    character(*),intent(in) :: fname, vname
    integer :: b(size(val)), i
    integer,intent(in),optional :: type_info(:, :)

    b = 0
    where(val) b = 1
    if (present(type_info)) then
      call easyO_int4_1d(fname, vname, b, type_info)
    else
      call easyO_int4_1d(fname, vname, b)
    end if

    return
  end subroutine easyO_logical_1d

  Subroutine easyI_logical_1d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,intent(out) :: val(:)
    character(*),intent(in) :: fname, vname
    integer :: b(size(val)), i
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



  Subroutine easyO_logical_2d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,intent(in) :: val(:, :)
    character(*),intent(in) :: fname, vname
    integer :: b(size(val(:, 1)), size(val(1, :))), i
    integer,intent(in),optional :: type_info(:, :)

    b = 0
    where(val) b = 1
    if (present(type_info)) then
      call easyO_int4_2d(fname, vname, b, type_info)
    else
      call easyO_int4_2d(fname, vname, b)
    end if

    return
  end subroutine easyO_logical_2d

  Subroutine easyI_logical_2d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,intent(out) :: val(:, :)
    character(*),intent(in) :: fname, vname
    integer :: b(size(val(:, 1)), size(val(1, :))), i
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



  Subroutine easyO_logical_3d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,intent(in) :: val(:, :, :)
    character(*),intent(in) :: fname, vname
    integer :: b(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))), i
    integer,intent(in),optional :: type_info(:, :)

    b = 0
    where(val) b = 1
    if (present(type_info)) then
      call easyO_int4_3d(fname, vname, b, type_info)
    else
      call easyO_int4_3d(fname, vname, b)
    end if

    return
  end subroutine easyO_logical_3d

  Subroutine easyI_logical_3d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,intent(out) :: val(:, :, :)
    character(*),intent(in) :: fname, vname
    integer :: b(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))), i
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



  Subroutine easyO_logical_4d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,intent(in) :: val(:, :, :, :)
    character(*),intent(in) :: fname, vname
    integer :: b(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))), i
    integer,intent(in),optional :: type_info(:, :)

    b = 0
    where(val) b = 1
    if (present(type_info)) then
      call easyO_int4_4d(fname, vname, b, type_info)
    else
      call easyO_int4_4d(fname, vname, b)
    end if

    return
  end subroutine easyO_logical_4d

  Subroutine easyI_logical_4d(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    logical,intent(out) :: val(:, :, :, :)
    character(*),intent(in) :: fname, vname
    integer :: b(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))), i
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
