
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

