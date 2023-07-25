
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
      if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size(shape(val))],order=[2,1]))
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
      if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size(shape(val))],order=[2,1]))
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
      if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size(shape(val))],order=[2,1]))
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
      if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size(shape(val))],order=[2,1]))
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
        allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4)))
        allocate(b(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4)))
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
      if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size(shape(val))],order=[2,1]))
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
        allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5)))
        allocate(b(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5)))
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
    integer :: b(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))), i
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
      allocate(b(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))))
      if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size(shape(val))],order=[2,1]))
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
    integer :: b(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:)))
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
        allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6)))
        allocate(b(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6)))
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(val(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6)))
        allocate(b(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6)))
        deallocate(dimsT_enc)
      end if
    else
      allocate(b(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))))
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
    integer :: b(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:))), i
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
      allocate(b(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:))))
      if (any(lbound(val) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(val), ubound(val)], [2, size(shape(val))],order=[2,1]))
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
    integer :: b(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:)))
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
        allocate(val(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6),dimsT2_enc(1,7):dimsT2_enc(2,7)))
        allocate(b(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6),dimsT2_enc(1,7):dimsT2_enc(2,7)))
        deallocate(dimsT_enc); deallocate(dimsT2_enc)
      else
        allocate(val(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6),dimsT_enc(7)))
        allocate(b(dimsT_enc(1),dimsT_enc(2),dimsT_enc(3),dimsT_enc(4),dimsT_enc(5),dimsT_enc(6),dimsT_enc(7)))
        deallocate(dimsT_enc)
      end if
    else
      allocate(b(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:))))
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
