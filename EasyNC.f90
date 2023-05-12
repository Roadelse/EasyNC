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
  Subroutine easyO_real4_1d(fname, vname, data, type_info)
    real*4,intent(in) :: data(:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyO_real4(fname, vname, data, shape(data), type_info)
    else
      call easyO_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_real4_2d(fname, vname, data, type_info)
    real*4,intent(in) :: data(:, :)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then
      call easyO_real4(fname, vname, data, shape(data), type_info)
    else
      call easyO_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_real4_3d(fname, vname, data, type_info)
    real*4,intent(in) :: data(:, :, :)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then
      call easyO_real4(fname, vname, data, shape(data), type_info)
    else
      call easyO_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_real4_4d(fname, vname, data, type_info)
    real*4,intent(in) :: data(:, :, :, :)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then
      call easyO_real4(fname, vname, data, shape(data), type_info)
    else
      call easyO_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_real4(fname, vname, data, dims_data, type_info)
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    real*4,intent(in) :: data(*)
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
      call check_enc( nf90_def_var(ncid, vname, NF90_FLOAT, dids, vid) , "nf90_def_var")
    end if
    call check_enc( nf90_enddef(ncid) , "nf90_enddef")
    ! ~~~~~~~~~~~~ write data
    ! print *, 'start_index = ',start_index
    ! print *, 'count_lens = ',count_lens
    call check_enc( nf90_put_var(ncid, vid, data(1:data_size), start=start_index, count=count_lens) , "nf90_put_var")
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    return
  end subroutine easyO_real4
  
  Subroutine easyO_real4_scalar(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    real*4,intent(in) :: val
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
  Subroutine easyI_real4_1d(fname, vname, data, type_info)
    real*4,intent(out) :: data(:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_real4(fname, vname, data, shape(data), type_info)
    else
      call easyI_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyI_real4_2d(fname, vname, data, type_info)
    real*4,intent(out) :: data(:, :)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then
      call easyI_real4(fname, vname, data, shape(data), type_info)
    else
      call easyI_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyI_real4_3d(fname, vname, data, type_info)
    real*4,intent(out) :: data(:, :, :)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then
      call easyI_real4(fname, vname, data, shape(data), type_info)
    else
      call easyI_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyI_real4_4d(fname, vname, data, type_info)
    real*4,intent(out) :: data(:, :, :, :)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then
      call easyI_real4(fname, vname, data, shape(data), type_info)
    else
      call easyI_real4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyI_real4(fname, vname, data, dims_data, type_info)
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    real*4,intent(out) :: data(*)
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
  end subroutine easyI_real4
  
  Subroutine easyI_real4_scalar(fname, vname, val, type_info)
    ! almost Duplicated with easyI_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    real*4,intent(out) :: val
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
  end subroutine easyI_real4_scalar
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
  Subroutine easyO_int4_1d(fname, vname, data, type_info)
    integer,intent(in) :: data(:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyO_int4(fname, vname, data, shape(data), type_info)
    else
      call easyO_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_int4_2d(fname, vname, data, type_info)
    integer,intent(in) :: data(:, :)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then
      call easyO_int4(fname, vname, data, shape(data), type_info)
    else
      call easyO_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_int4_3d(fname, vname, data, type_info)
    integer,intent(in) :: data(:, :, :)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then
      call easyO_int4(fname, vname, data, shape(data), type_info)
    else
      call easyO_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_int4_4d(fname, vname, data, type_info)
    integer,intent(in) :: data(:, :, :, :)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then
      call easyO_int4(fname, vname, data, shape(data), type_info)
    else
      call easyO_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_int4(fname, vname, data, dims_data, type_info)
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    integer,intent(in) :: data(*)
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
      call check_enc( nf90_def_var(ncid, vname, NF90_INT, dids, vid) , "nf90_def_var")
    end if
    call check_enc( nf90_enddef(ncid) , "nf90_enddef")
    ! ~~~~~~~~~~~~ write data
    ! print *, 'start_index = ',start_index
    ! print *, 'count_lens = ',count_lens
    call check_enc( nf90_put_var(ncid, vid, data(1:data_size), start=start_index, count=count_lens) , "nf90_put_var")
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    return
  end subroutine easyO_int4
  
  Subroutine easyO_int4_scalar(fname, vname, val, type_info)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    integer,intent(in) :: val
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
  Subroutine easyI_int4_1d(fname, vname, data, type_info)
    integer,intent(out) :: data(:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_int4(fname, vname, data, shape(data), type_info)
    else
      call easyI_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyI_int4_2d(fname, vname, data, type_info)
    integer,intent(out) :: data(:, :)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then
      call easyI_int4(fname, vname, data, shape(data), type_info)
    else
      call easyI_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyI_int4_3d(fname, vname, data, type_info)
    integer,intent(out) :: data(:, :, :)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then
      call easyI_int4(fname, vname, data, shape(data), type_info)
    else
      call easyI_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyI_int4_4d(fname, vname, data, type_info)
    integer,intent(out) :: data(:, :, :, :)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then
      call easyI_int4(fname, vname, data, shape(data), type_info)
    else
      call easyI_int4(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyI_int4(fname, vname, data, dims_data, type_info)
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    integer,intent(out) :: data(*)
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
  end subroutine easyI_int4
  
  Subroutine easyI_int4_scalar(fname, vname, val, type_info)
    ! almost Duplicated with easyI_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    integer,intent(out) :: val
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
  
  Subroutine easyO_char_1d(fname, vname, data, type_info)
    character(*),intent(in) :: data(:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyO_char_nd(fname, vname, data, shape(data), type_info)
    else
      call easyO_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_char_2d(fname, vname, data, type_info)
    character(*),intent(in) :: data(:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyO_char_nd(fname, vname, data, shape(data), type_info)
    else
      call easyO_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_char_3d(fname, vname, data, type_info)
    character(*),intent(in) :: data(:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyO_char_nd(fname, vname, data, shape(data), type_info)
    else
      call easyO_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_char_4d(fname, vname, data, type_info)
    character(*),intent(in) :: data(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyO_char_nd(fname, vname, data, shape(data), type_info)
    else
      call easyO_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyO_char_nd(fname, vname, data, dims_data, type_info)
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    character(*),intent(in) :: data(*)
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
      dims = len(data)
      dims(2:ndims_data+1) = dims_data
      do i = ndims_data + 2, ndims+1
        dims(i) = type_info(1,ndims_type - (i-1-ndims_data) + 1)
      end do
    else
      ndims = ndims_data
      allocate(dims(ndims+1), STAT=ierr)  ! add length-dim
      call check_enc(ierr, 'allocate')
      dims(1) = len(data)
      dims(2:) = dims_data
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
      do i = 2, ndims_data+1
        write(dimnames(i), '("'//trim(tvname)//'", "_d", I1)') ndims_data-i-1+1
      end do
      do i = ndims_data + 2, ndims+1
        write(dimnames(i), '("'//trim(tname)//'", "_d", I1)') (i-1 - ndims_data)
      end do
    else
      dimnames(1)=trim(vname)//"_L"
      do i = 2, ndims+1
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
    print *, 'dimnames = ', dimnames
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
    if (nf90_inq_varid(ncid, vname, vid) .eq. nf90_noerr) then
      if (.not. present(type_info)) then
        write(*,*) 'Error! variable already exists!'
        call check_enc( nf90_close(ncid) , "nf90_close")
        stop 1
      end if
    else
      call check_enc( nf90_def_var(ncid, vname, NF90_CHAR, dids, vid) , "nf90_def_var")
    end if
    call check_enc( nf90_enddef(ncid) , "nf90_enddef")
    ! ~~~~~~~~~~~~ write data
    print *, 'start_index = ',start_index
    print *, 'count_lens = ',count_lens
    print *, 'dims = ', dims
    call check_enc( nf90_put_var(ncid, vid, data(1:data_size), start=start_index, count=count_lens) , "nf90_put_var")
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
      i = scan(vname, '.')
      if (i .eq. 0) then
        stop 1
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
    print *, size(start_index)
    print *, ndims_type
    if (present(type_info)) then
      call check_enc( nf90_put_var(ncid, vid, val, start=start_index) , "nf90_put_var")
    else
      call check_enc( nf90_put_var(ncid, vid, val) , "nf90_put_var")
    end if
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    return
  end subroutine easyO_char
  Subroutine easyI_char_1d(fname, vname, data, type_info)
    character(*),intent(out) :: data(:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_char_nd(fname, vname, data, shape(data), type_info)
    else
      call easyI_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyI_char_2d(fname, vname, data, type_info)
    character(*),intent(out) :: data(:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_char_nd(fname, vname, data, shape(data), type_info)
    else
      call easyI_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyI_char_3d(fname, vname, data, type_info)
    character(*),intent(out) :: data(:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_char_nd(fname, vname, data, shape(data), type_info)
    else
      call easyI_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyI_char_4d(fname, vname, data, type_info)
    character(*),intent(out) :: data(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer,intent(in),optional :: type_info(:, :)
    if (present(type_info)) then  ! shape(data) is necessary due to shape cannot be used in assumed-size array
      call easyI_char_nd(fname, vname, data, shape(data), type_info)
    else
      call easyI_char_nd(fname, vname, data, shape(data))
    end if
  end subroutine
  Subroutine easyI_char_nd(fname, vname, data, dims_data, type_info)
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> header
    character(*),intent(out) :: data(*)
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
      allocate(dims(ndims+1), STAT=ierr)
      call check_enc(ierr, 'allocate')
      dims(1) = len(data)
      dims(2:ndims_data+1) = dims_data
      do i = ndims_data + 2, ndims+1
        dims(i) = type_info(1,ndims_type - (i-1-ndims_data) + 1)
      end do
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
      call check_enc( nf90_get_var(ncid, vid, val, start=start_index) , "nf90_put_var")
    else
      call check_enc( nf90_get_var(ncid, vid, val) , "nf90_put_var")
    end if
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    return
  end subroutine easyI_char
  function getDims_enc(fname, vname) result(dims)
    implicit none
    character(*),intent(in) :: fname, vname
    integer :: i, j, k, l, m, n
    integer :: ncid, vid, ierr, ndims
    logical :: isExist
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
    allocate(dims(ndims), stat=i)
    call check_enc(i, 'allocate : dims(ndims)')
    do i = 1, ndims
      call check_enc(nf90_inquire_dimension(ncid, dids(i), len=dims(i)), 'nf90_inquire_dimension')
    end do
  end function
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
