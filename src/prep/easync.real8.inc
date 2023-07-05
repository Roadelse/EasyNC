
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
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
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
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
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
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
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
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
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
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4)))
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
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
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
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5)))
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
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
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
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6)))
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
      if (any(lbound(data) .ne. 1)) call easyO(trim(fname), trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
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
        allocate(data(dimsT2_enc(1,1):dimsT2_enc(2,1),dimsT2_enc(1,2):dimsT2_enc(2,2),dimsT2_enc(1,3):dimsT2_enc(2,3),dimsT2_enc(1,4):dimsT2_enc(2,4),dimsT2_enc(1,5):dimsT2_enc(2,5),dimsT2_enc(1,6):dimsT2_enc(2,6),dimsT2_enc(1,7):dimsT2_enc(2,7)))
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
  Subroutine easyO_real8(fname, vname, data, dims_data, type_info, type_ele_alone)
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    real(kind=8),intent(in) :: data(*)
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