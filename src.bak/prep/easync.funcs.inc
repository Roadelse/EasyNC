
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


