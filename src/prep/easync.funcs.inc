
    function enc_var_exist(fname, vname) result(rst)
        !!! #########################################
        ! This function aims to check if a variable is in netcdf file or not
        !!! #########################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Argument
        character(*),intent(in) :: fname, vname
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Return variable
        logical :: rst
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: i,j,k
        integer :: ncid, status, vid
        logical :: isExist
        character(len=80) :: verbose_suffix


        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ======================== check file existance
        verbose_suffix = 'nc_var_exist('//trim(fname)//','//trim(vname)//')'
        inquire(file = fname, exist = isExist)

        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , 'nf90_open in '//trim(verbose_suffix))
        else  !# netcdf file doesn't exist!
            rst = .false.
            return
        end if
        ! ======================== check var existance
        status = nf90_inq_varid(ncid, vname, vid)
        if (status .ne. 0) then
            rst = .false.
        else
            ! varname_enc = vname2
            rst = .true.
        end if

        call check_enc(nf90_close(ncid) , 'nf90_close in '//trim(verbose_suffix))
        return

    end function


    function enc_dim_exist(fname, dname) result(rst) ! postion
        !!! #########################################
        ! This function aims to check if a dimension is in netcdf file or not
        !!! #########################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Argument
        character(*),intent(in) :: fname, dname
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Return variable
        logical :: rst
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: i,j,k
        integer :: ncid, status
        logical :: isExist

        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ======================== check file existance
        inquire(file = fname, exist = isExist)
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "nf90_open")
        else
            rst = .false.
            return
        end if
        ! ======================== check dim existance
        status = nf90_inq_dimid(ncid, dname, k)

        if (status .ne. 0) then
            rst = .false.
        else
            rst = .true.
        end if

        call check_enc(nf90_close(ncid) , "in hasDim, nf90_close")

        return

    end function


    subroutine enc_get_dims(fname, vname, dims)
        !!! #########################################
        ! This function aims to obtain dimensions for a variable
        !!! #########################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),intent(in) :: fname, vname
        integer, allocatable :: dims(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: i, j, k, l, m, n
        integer :: ncid, vid, ierr, ndims
        logical :: isExist

        integer :: xt  ! xtype
        integer :: dids(10)

        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main Body
        ! ================== open netcdf file
        inquire(file = fname, exist = isExist)

        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "nf90_open in enc_get_dims")
        else
            write(*,*) 'Error! file not exist : '//trim(fname)
            stop 1
        end if

        ! ================== get variable and its dimensions
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid in enc_get_dims')
        call check_enc(nf90_inquire_variable(ncid, vid, ndims=ndims), 'nf90_inquire_variable in enc_get_dims')
        call check_enc(nf90_inquire_variable(ncid, vid, dimids=dids(:ndims)), 'nf90_inquire_variable in enc_get_dims')
        call check_enc(nf90_inquire_variable(ncid, vid, xtype=xt), 'in nf90_inquire_variable for xtype in enc_get_dims')

        if (xt .eq. NF90_CHAR) then
            allocate(dims(ndims-1))
            do i = 1, ndims - 1
                call check_enc(nf90_inquire_dimension(ncid, dids(i+1), len=dims(i)), 'nf90_inquire_dimension')
            end do
        else
            allocate(dims(ndims))
            do i = 1, ndims
                call check_enc(nf90_inquire_dimension(ncid, dids(i), len=dims(i)), 'nf90_inquire_dimension')  !# order issue?
            end do
        endif

        call check_enc(nf90_close(ncid) , "nf90_close in getDims2_enc")

    end subroutine

    function enc_func_get_dims(fname, vname) result(dims)  ! return dynamic dims 
        !!! #########################################
        ! This function wraps the subroutine "enc_get_dims", except for being a function
        !!! #########################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),intent(in) :: fname, vname
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Return variable
        integer, allocatable :: dims(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        call enc_get_dims(fname, vname, dims)

        return
    end function
    



  ! subroutine getDims3_enc(fname, tname, dims)  ! pass allocatable dims in, get struct variable shape for allocate
  !   implicit none
  !   character(*),intent(in) :: fname, tname
  !   integer, allocatable :: dims(:)

  !   integer :: i, j, k, l, m, n
  !   integer :: ncid, vid, ierr, ndims
  !   logical :: isExist

  !   character(1) :: c


  !   integer :: xt  ! xtype
  !   integer :: dids(10)

  !   ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
  !   ! ================== open netcdf file
  !   inquire(file = fname, exist = isExist)

  !   if (isExist) then
  !     call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "nf90_open")
  !   else
  !     write(*,*) 'Error! file not exist : '//trim(fname)
  !     stop 1
  !   end if

  !   ! ================== get variable and its dimensions
  !   do i = 1, 6
  !     write(c, '(I1)') i
  !     ierr = nf90_inq_dimid(ncid, trim(tname)//'_d'//c, dids(i))
  !     if (ierr .ne. 0) exit
  !   end do
  !   ! print *, 'sss = ',i
  !   allocate(dims(i-1), stat=ierr)
  !   call check_enc(ierr, 'in allocate')

  !   do j = 1, i-1
  !     call check_enc(nf90_inquire_dimension(ncid, dids(j), len=dims(i-j)), 'nf90_inquire_dimension')
  !   end do

  !   call check_enc(nf90_close(ncid) , "in getDims3_enc, nf90_close")

  !   return

  ! end subroutine


