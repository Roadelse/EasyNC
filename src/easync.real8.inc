
  Subroutine easyO_real8_1d(fname, vname, data, type_info)
    real*8,intent(in) :: data(:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)

    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyO_real8(fname, vname, data, shape(data), type_info)
    else
      call easyO_real8(fname, vname, data, shape(data))
    end if
  end subroutine

  Subroutine easyO_real8_2d(fname, vname, data, type_info)
    real*8,intent(in) :: data(:, :)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)

    if (present(type_info)) then
      call easyO_real8(fname, vname, data, shape(data), type_info)
    else
      call easyO_real8(fname, vname, data, shape(data))
    end if
  end subroutine

  Subroutine easyO_real8_3d(fname, vname, data, type_info)
    real*8,intent(in) :: data(:, :, :)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)

    if (present(type_info)) then
      call easyO_real8(fname, vname, data, shape(data), type_info)
    else
      call easyO_real8(fname, vname, data, shape(data))
    end if
  end subroutine

  Subroutine easyO_real8_4d(fname, vname, data, type_info)
    real*8,intent(in) :: data(:, :, :, :)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)

    if (present(type_info)) then
      call easyO_real8(fname, vname, data, shape(data), type_info)
    else
      call easyO_real8(fname, vname, data, shape(data))
    end if
  end subroutine

  Subroutine easyO_real8(fname, vname, data, dims_data, type_info)
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    real*8,intent(in) :: data(*)
    character(*),intent(in) :: fname, vname
    integer, intent(in) :: dims_data(:)
    integer, intent(in), optional :: type_info(:, :) ! 2 x n, 0 : type array size, 1 : type position
    
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
      ndims_type = size(type_info(1, :))
      ndims = ndims_data + ndims_type
      allocate(dims(ndims), STAT=ierr)
      call check_enc(ierr, 'allocate')
      dims(1:ndims_data) = dims_data
      do i = ndims_data + 1, ndims
        dims(i) = type_info(1,ndims_type - (i-ndims_data) + 1)
      end do
    else
      ndims = ndims_data
      allocate(dims(ndims), STAT=ierr)
      call check_enc(ierr, 'allocate')
      dims = dims_data

    end if

    ! ================== get tname and tvname for type-var
    if (present(type_info)) then
      i = scan(vname, '.')
      if (i .eq. 0) then
        stop 1
      end if
      tname = vname(:i-1)
      tvname = vname(i+1:)
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
      do i = ndims_data + 1, ndims
        start_index(i) = type_info(2, ndims_type - (i-ndims_data) + 1)
      end do
    end if

    allocate(count_lens(ndims), STAT=ierr)
    if (present(type_info)) then
      count_lens(1:ndims_data) = dims_data
      count_lens(ndims_data+1:) = 1
    else
      count_lens = dims_data
    end if

    data_size = product(dims_data)
    
    ! ================== set dimension names
    if (present(type_info)) then
      do i = 1, ndims_data
        write(dimnames(i), '("'//trim(tvname)//'", "_d", I1)') ndims_data-i+1
      end do
      do i = ndims_data + 1, ndims
        write(dimnames(i), '("'//trim(tname)//'", "_d", I1)') (i - ndims_data)
      end do
    else
      do i = 1, ndims
        write(dimnames(i), '("'//trim(vname)//'", "_d", I1)') ndims_data-i+1
      end do
    end if

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
    if (nf90_inq_varid(ncid, vname, vid) .eq. nf90_noerr) then
      if (.not. present(type_info)) then
        write(*,*) 'Error! variable already exists!'
        call check_enc( nf90_close(ncid) , "nf90_close")
        stop 1
      end if
    else
      call check_enc( nf90_def_var(ncid, vname, NF90_DOUBLE, dids, vid) , "nf90_def_var")
    end if

    call check_enc( nf90_enddef(ncid) , "nf90_enddef")
    ! ~~~~~~~~~~~~ write data
    ! print *, 'start_index = ',start_index
    ! print *, 'count_lens = ',count_lens
    call check_enc( nf90_put_var(ncid, vid, data(1:data_size), start=start_index, count=count_lens) , "nf90_put_var")

    call check_enc( nf90_close(ncid) , "nf90_close")
    
    return
  end subroutine easyO_real8
  


  Subroutine easyO_real8_scalar(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    real*8,intent(in) :: val
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
      i = scan(vname, '.')
      if (i .eq. 0) then
        stop 1
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







  Subroutine easyI_real8_1d(fname, vname, data, type_info)
    real*8,intent(out) :: data(:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)

    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_real8(fname, vname, data, shape(data), type_info)
    else
      call easyI_real8(fname, vname, data, shape(data))
    end if
  end subroutine

  Subroutine easyI_real8_2d(fname, vname, data, type_info)
    real*8,intent(out) :: data(:, :)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)

    if (present(type_info)) then
      call easyI_real8(fname, vname, data, shape(data), type_info)
    else
      call easyI_real8(fname, vname, data, shape(data))
    end if
  end subroutine

  Subroutine easyI_real8_3d(fname, vname, data, type_info)
    real*8,intent(out) :: data(:, :, :)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)

    if (present(type_info)) then
      call easyI_real8(fname, vname, data, shape(data), type_info)
    else
      call easyI_real8(fname, vname, data, shape(data))
    end if
  end subroutine

  Subroutine easyI_real8_4d(fname, vname, data, type_info)
    real*8,intent(out) :: data(:, :, :, :)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)

    if (present(type_info)) then
      call easyI_real8(fname, vname, data, shape(data), type_info)
    else
      call easyI_real8(fname, vname, data, shape(data))
    end if
  end subroutine

  Subroutine easyI_real8(fname, vname, data, dims_data, type_info)
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    real*8,intent(out) :: data(*)
    character(*),intent(in) :: fname, vname
    integer, intent(in) :: dims_data(:)
    integer, intent(in), optional :: type_info(:, :) ! 2 x n, 0 : type array size, 1 : type position
    
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
    if (present(type_info)) then
      ndims_type = size(type_info(1, :))
      ndims = ndims_data + ndims_type
      allocate(dims(ndims), STAT=ierr)
      call check_enc(ierr, 'allocate')
      dims(1:ndims_data) = dims_data
      do i = ndims_data + 1, ndims
        dims(i) = type_info(1,ndims_type - (i-ndims_data) + 1)
      end do
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

      do i = ndims_data + 1, ndims
        start_index(i) = type_info(2, ndims_type - (i-ndims_data) + 1)
      end do
    end if

    allocate(count_lens(ndims), STAT=ierr)
    if (present(type_info)) then
      count_lens(1:ndims_data) = dims_data
      count_lens(ndims_data+1:) = 1
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
    call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))

    ! ~~~~~~~~~~~~ read data
    ! print *, 'start_index = ',start_index
    ! print *, 'count_lens = ',count_lens
    call check_enc( nf90_get_var(ncid, vid, data(1:data_size), start=start_index, count=count_lens) , "nf90_get_var")

    call check_enc( nf90_close(ncid) , "nf90_close")
    
    return
  end subroutine easyI_real8
  

  Subroutine easyI_real8_scalar(fname, vname, val, type_info)
    ! almost Duplicated with easyI_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    real*8,intent(out) :: val
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
      call check_enc( nf90_get_var(ncid, vid, val, start=start_index) , "nf90_put_var")
    else
      call check_enc( nf90_get_var(ncid, vid, val) , "nf90_put_var")
    end if

    call check_enc( nf90_close(ncid) , "nf90_close")
    
    return
  end subroutine easyI_real8_scalar
