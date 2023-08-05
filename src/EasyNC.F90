Module EasyNC
    use netcdf
    use rdee_fortran
    
    implicit none
    integer,allocatable :: enc_iaaT_1d(:), enc_iaaT2_1d(:), enc_iaaT_2d(:,:)
    integer :: enc_i, enc_j, enc_k
    character(80) :: varname_enc, strT_enc
    integer :: enc_vea = 0  ! var-exist-action, 0 - ignore it; 1 - just return; -1 : Abort
    logical :: enc_use_nc4 = .false.  ! determine if easyO* will write data to netcdf-4 format file
    !! Doesn't support allocatable/pointer scalar variable @2023-07-20
    interface easyO
        module procedure easyO_int4_1d
        module procedure easyO_int4_2d
        module procedure easyO_int4_3d
        module procedure easyO_int4_4d
        module procedure easyO_int4_5d
        module procedure easyO_int4_6d
        module procedure easyO_int4_7d
        module procedure easyO_int4_scalar
        module procedure easyO_int8_1d
        module procedure easyO_int8_2d
        module procedure easyO_int8_3d
        module procedure easyO_int8_4d
        module procedure easyO_int8_5d
        module procedure easyO_int8_6d
        module procedure easyO_int8_7d
        module procedure easyO_int8_scalar
        module procedure easyO_real4_1d
        module procedure easyO_real4_2d
        module procedure easyO_real4_3d
        module procedure easyO_real4_4d
        module procedure easyO_real4_5d
        module procedure easyO_real4_6d
        module procedure easyO_real4_7d
        module procedure easyO_real4_scalar
        module procedure easyO_real8_1d
        module procedure easyO_real8_2d
        module procedure easyO_real8_3d
        module procedure easyO_real8_4d
        module procedure easyO_real8_5d
        module procedure easyO_real8_6d
        module procedure easyO_real8_7d
        module procedure easyO_real8_scalar
        module procedure easyO_string_1d
        module procedure easyO_string_2d
        module procedure easyO_string_3d
        module procedure easyO_string_4d
        module procedure easyO_string_5d
        module procedure easyO_string_6d
        module procedure easyO_string_scalar
        module procedure easyO_logical_1d
        module procedure easyO_logical_2d
        module procedure easyO_logical_3d
        module procedure easyO_logical_4d
        module procedure easyO_logical_5d
        module procedure easyO_logical_6d
        module procedure easyO_logical_7d
        module procedure easyO_logical_scalar
        module procedure easyO_complex4_1d
        module procedure easyO_complex4_2d
        module procedure easyO_complex4_3d
        module procedure easyO_complex4_4d
        module procedure easyO_complex4_5d
        module procedure easyO_complex4_6d
        module procedure easyO_complex4_7d
        module procedure easyO_complex4_scalar
        module procedure easyO_complex8_1d
        module procedure easyO_complex8_2d
        module procedure easyO_complex8_3d
        module procedure easyO_complex8_4d
        module procedure easyO_complex8_5d
        module procedure easyO_complex8_6d
        module procedure easyO_complex8_7d
        module procedure easyO_complex8_scalar
    end interface
    interface easyI
        module procedure easyI_int4_1d
        module procedure easyI_int4_2d
        module procedure easyI_int4_3d
        module procedure easyI_int4_4d
        module procedure easyI_int4_5d
        module procedure easyI_int4_6d
        module procedure easyI_int4_7d
        module procedure easyI_int4_scalar
        module procedure easyI_int8_1d
        module procedure easyI_int8_2d
        module procedure easyI_int8_3d
        module procedure easyI_int8_4d
        module procedure easyI_int8_5d
        module procedure easyI_int8_6d
        module procedure easyI_int8_7d
        module procedure easyI_int8_scalar
        module procedure easyI_real4_1d
        module procedure easyI_real4_2d
        module procedure easyI_real4_3d
        module procedure easyI_real4_4d
        module procedure easyI_real4_5d
        module procedure easyI_real4_6d
        module procedure easyI_real4_7d
        module procedure easyI_real4_scalar
        module procedure easyI_real8_1d
        module procedure easyI_real8_2d
        module procedure easyI_real8_3d
        module procedure easyI_real8_4d
        module procedure easyI_real8_5d
        module procedure easyI_real8_6d
        module procedure easyI_real8_7d
        module procedure easyI_real8_scalar
        module procedure easyI_string_1d
        module procedure easyI_string_2d
        module procedure easyI_string_3d
        module procedure easyI_string_4d
        module procedure easyI_string_5d
        module procedure easyI_string_6d
        module procedure easyI_string_scalar
        module procedure easyI_logical_1d
        module procedure easyI_logical_2d
        module procedure easyI_logical_3d
        module procedure easyI_logical_4d
        module procedure easyI_logical_5d
        module procedure easyI_logical_6d
        module procedure easyI_logical_7d
        module procedure easyI_logical_scalar
        module procedure easyI_complex4_1d
        module procedure easyI_complex4_2d
        module procedure easyI_complex4_3d
        module procedure easyI_complex4_4d
        module procedure easyI_complex4_5d
        module procedure easyI_complex4_6d
        module procedure easyI_complex4_7d
        module procedure easyI_complex4_scalar
        module procedure easyI_complex8_1d
        module procedure easyI_complex8_2d
        module procedure easyI_complex8_3d
        module procedure easyI_complex8_4d
        module procedure easyI_complex8_5d
        module procedure easyI_complex8_6d
        module procedure easyI_complex8_7d
        module procedure easyI_complex8_scalar
    end interface
    interface easyOA
        module procedure easyOA_int4_1d
        module procedure easyOA_int4_2d
        module procedure easyOA_int4_3d
        module procedure easyOA_int4_4d
        module procedure easyOA_int4_5d
        module procedure easyOA_int4_6d
        module procedure easyOA_int4_7d
        ! module procedure easyOA_int4_scalar
        module procedure easyOA_int8_1d
        module procedure easyOA_int8_2d
        module procedure easyOA_int8_3d
        module procedure easyOA_int8_4d
        module procedure easyOA_int8_5d
        module procedure easyOA_int8_6d
        module procedure easyOA_int8_7d
        ! module procedure easyOA_int8_scalar
        module procedure easyOA_real4_1d
        module procedure easyOA_real4_2d
        module procedure easyOA_real4_3d
        module procedure easyOA_real4_4d
        module procedure easyOA_real4_5d
        module procedure easyOA_real4_6d
        module procedure easyOA_real4_7d
        ! module procedure easyOA_real4_scalar
        module procedure easyOA_real8_1d
        module procedure easyOA_real8_2d
        module procedure easyOA_real8_3d
        module procedure easyOA_real8_4d
        module procedure easyOA_real8_5d
        module procedure easyOA_real8_6d
        module procedure easyOA_real8_7d
        ! module procedure easyOA_real8_scalar
        module procedure easyOA_string_1d
        module procedure easyOA_string_2d
        module procedure easyOA_string_3d
        module procedure easyOA_string_4d
        module procedure easyOA_string_5d
        module procedure easyOA_string_6d
        ! module procedure easyOA_string_scalar
        module procedure easyOA_logical_1d
        module procedure easyOA_logical_2d
        module procedure easyOA_logical_3d
        module procedure easyOA_logical_4d
        module procedure easyOA_logical_5d
        module procedure easyOA_logical_6d
        module procedure easyOA_logical_7d
        ! module procedure easyOA_logical_scalar
        module procedure easyOA_complex4_1d
        module procedure easyOA_complex4_2d
        module procedure easyOA_complex4_3d
        module procedure easyOA_complex4_4d
        module procedure easyOA_complex4_5d
        module procedure easyOA_complex4_6d
        module procedure easyOA_complex4_7d
        ! module procedure easyOA_complex4_scalar
        module procedure easyOA_complex8_1d
        module procedure easyOA_complex8_2d
        module procedure easyOA_complex8_3d
        module procedure easyOA_complex8_4d
        module procedure easyOA_complex8_5d
        module procedure easyOA_complex8_6d
        module procedure easyOA_complex8_7d
        ! module procedure easyOA_complex8_scalar
    end interface
    interface easyIA
        module procedure easyIA_int4_1d
        module procedure easyIA_int4_2d
        module procedure easyIA_int4_3d
        module procedure easyIA_int4_4d
        module procedure easyIA_int4_5d
        module procedure easyIA_int4_6d
        module procedure easyIA_int4_7d
        ! module procedure easyIA_int4_scalar
        module procedure easyIA_int8_1d
        module procedure easyIA_int8_2d
        module procedure easyIA_int8_3d
        module procedure easyIA_int8_4d
        module procedure easyIA_int8_5d
        module procedure easyIA_int8_6d
        module procedure easyIA_int8_7d
        ! module procedure easyIA_int8_scalar
        module procedure easyIA_real4_1d
        module procedure easyIA_real4_2d
        module procedure easyIA_real4_3d
        module procedure easyIA_real4_4d
        module procedure easyIA_real4_5d
        module procedure easyIA_real4_6d
        module procedure easyIA_real4_7d
        ! module procedure easyIA_real4_scalar
        module procedure easyIA_real8_1d
        module procedure easyIA_real8_2d
        module procedure easyIA_real8_3d
        module procedure easyIA_real8_4d
        module procedure easyIA_real8_5d
        module procedure easyIA_real8_6d
        module procedure easyIA_real8_7d
        ! module procedure easyIA_real8_scalar
        module procedure easyIA_string_1d
        module procedure easyIA_string_2d
        module procedure easyIA_string_3d
        module procedure easyIA_string_4d
        module procedure easyIA_string_5d
        module procedure easyIA_string_6d
        ! module procedure easyIA_string_scalar
        module procedure easyIA_logical_1d
        module procedure easyIA_logical_2d
        module procedure easyIA_logical_3d
        module procedure easyIA_logical_4d
        module procedure easyIA_logical_5d
        module procedure easyIA_logical_6d
        module procedure easyIA_logical_7d
        ! module procedure easyIA_logical_scalar
        module procedure easyIA_complex4_1d
        module procedure easyIA_complex4_2d
        module procedure easyIA_complex4_3d
        module procedure easyIA_complex4_4d
        module procedure easyIA_complex4_5d
        module procedure easyIA_complex4_6d
        module procedure easyIA_complex4_7d
        ! module procedure easyIA_complex4_scalar
        module procedure easyIA_complex8_1d
        module procedure easyIA_complex8_2d
        module procedure easyIA_complex8_3d
        module procedure easyIA_complex8_4d
        module procedure easyIA_complex8_5d
        module procedure easyIA_complex8_6d
        module procedure easyIA_complex8_7d
        ! module procedure easyIA_complex8_scalar
    end interface
    interface easyOP
        module procedure easyOP_int4_1d
        module procedure easyOP_int4_2d
        module procedure easyOP_int4_3d
        module procedure easyOP_int4_4d
        module procedure easyOP_int4_5d
        module procedure easyOP_int4_6d
        module procedure easyOP_int4_7d
        ! module procedure easyOP_int4_scalar
        module procedure easyOP_int8_1d
        module procedure easyOP_int8_2d
        module procedure easyOP_int8_3d
        module procedure easyOP_int8_4d
        module procedure easyOP_int8_5d
        module procedure easyOP_int8_6d
        module procedure easyOP_int8_7d
        ! module procedure easyOP_int8_scalar
        module procedure easyOP_real4_1d
        module procedure easyOP_real4_2d
        module procedure easyOP_real4_3d
        module procedure easyOP_real4_4d
        module procedure easyOP_real4_5d
        module procedure easyOP_real4_6d
        module procedure easyOP_real4_7d
        ! module procedure easyOP_real4_scalar
        module procedure easyOP_real8_1d
        module procedure easyOP_real8_2d
        module procedure easyOP_real8_3d
        module procedure easyOP_real8_4d
        module procedure easyOP_real8_5d
        module procedure easyOP_real8_6d
        module procedure easyOP_real8_7d
        ! module procedure easyOP_real8_scalar
        module procedure easyOP_string_1d
        module procedure easyOP_string_2d
        module procedure easyOP_string_3d
        module procedure easyOP_string_4d
        module procedure easyOP_string_5d
        module procedure easyOP_string_6d
        ! module procedure easyOP_string_scalar
        module procedure easyOP_logical_1d
        module procedure easyOP_logical_2d
        module procedure easyOP_logical_3d
        module procedure easyOP_logical_4d
        module procedure easyOP_logical_5d
        module procedure easyOP_logical_6d
        module procedure easyOP_logical_7d
        ! module procedure easyOP_logical_scalar
        module procedure easyOP_complex4_1d
        module procedure easyOP_complex4_2d
        module procedure easyOP_complex4_3d
        module procedure easyOP_complex4_4d
        module procedure easyOP_complex4_5d
        module procedure easyOP_complex4_6d
        module procedure easyOP_complex4_7d
        ! module procedure easyOP_complex4_scalar
        module procedure easyOP_complex8_1d
        module procedure easyOP_complex8_2d
        module procedure easyOP_complex8_3d
        module procedure easyOP_complex8_4d
        module procedure easyOP_complex8_5d
        module procedure easyOP_complex8_6d
        module procedure easyOP_complex8_7d
        ! module procedure easyOP_complex8_scalar
    end interface
    interface easyIP
        module procedure easyIP_int4_1d
        module procedure easyIP_int4_2d
        module procedure easyIP_int4_3d
        module procedure easyIP_int4_4d
        module procedure easyIP_int4_5d
        module procedure easyIP_int4_6d
        module procedure easyIP_int4_7d
        ! module procedure easyIP_int4_scalar
        module procedure easyIP_int8_1d
        module procedure easyIP_int8_2d
        module procedure easyIP_int8_3d
        module procedure easyIP_int8_4d
        module procedure easyIP_int8_5d
        module procedure easyIP_int8_6d
        module procedure easyIP_int8_7d
        ! module procedure easyIP_int8_scalar
        module procedure easyIP_real4_1d
        module procedure easyIP_real4_2d
        module procedure easyIP_real4_3d
        module procedure easyIP_real4_4d
        module procedure easyIP_real4_5d
        module procedure easyIP_real4_6d
        module procedure easyIP_real4_7d
        ! module procedure easyIP_real4_scalar
        module procedure easyIP_real8_1d
        module procedure easyIP_real8_2d
        module procedure easyIP_real8_3d
        module procedure easyIP_real8_4d
        module procedure easyIP_real8_5d
        module procedure easyIP_real8_6d
        module procedure easyIP_real8_7d
        ! module procedure easyIP_real8_scalar
        module procedure easyIP_string_1d
        module procedure easyIP_string_2d
        module procedure easyIP_string_3d
        module procedure easyIP_string_4d
        module procedure easyIP_string_5d
        module procedure easyIP_string_6d
        ! module procedure easyIP_string_scalar
        module procedure easyIP_logical_1d
        module procedure easyIP_logical_2d
        module procedure easyIP_logical_3d
        module procedure easyIP_logical_4d
        module procedure easyIP_logical_5d
        module procedure easyIP_logical_6d
        module procedure easyIP_logical_7d
        ! module procedure easyIP_logical_scalar
        module procedure easyIP_complex4_1d
        module procedure easyIP_complex4_2d
        module procedure easyIP_complex4_3d
        module procedure easyIP_complex4_4d
        module procedure easyIP_complex4_5d
        module procedure easyIP_complex4_6d
        module procedure easyIP_complex4_7d
        ! module procedure easyIP_complex4_scalar
        module procedure easyIP_complex8_1d
        module procedure easyIP_complex8_2d
        module procedure easyIP_complex8_3d
        module procedure easyIP_complex8_4d
        module procedure easyIP_complex8_5d
        module procedure easyIP_complex8_6d
        module procedure easyIP_complex8_7d
        ! module procedure easyIP_complex8_scalar
    end interface
Contains
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
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid in enc_get_dims: '//trim(vname))
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
    
! *********************************************************
! easyIO for scalar generic numeric data type :
!   int4. int8, real4, real8
! *********************************************************
    Subroutine easyO_int4_scalar(fname, vname, data, shape_total, position, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total" and "position"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),intent(in) :: data
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= prepare for netcdf IO
        if (.not. present(shape_total)) then  !# sub-array IO
            rank_ncv = 1
            allocate(shape_ncv(1))
            shape_ncv = 1
        else !# single element  io
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
            ! ========================= handle dimensions, e.g., dimnames_ & dimdis
            allocate(dimnames_(rank_ncv))
            allocate(dimids(rank_ncv))
            do i = 1, rank_ncv
                if (present(dimnames)) then
                    call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_int4_scalar, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                    if (len_trim(dimnames(i)) .gt. 0) then
                        dimnames_(i) = trim(dimnames(i))
                        cycle
                    end if
                end if
                write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
            end do
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        if (present(shape_total)) then
            do i = 1, rank_ncv
                if (isExist) then
                    if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                        call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
                    end if
                else
                    call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
                end if
            end do
        end if
        ! ~~~~~~~~~~~~~~ define variable
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            if (present(shape_total)) then
                call check_enc( nf90_def_var(ncid, vname, NF90_INT, dimids, vid) , "nf90_def_var")
            else
                call check_enc( nf90_def_var(ncid, vname, NF90_INT, varid=vid) , "nf90_def_var")
            end if
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        if (present(shape_total)) then
            call check_enc( nf90_put_var(ncid, vid, data, start=position) , & 
            "in easyO_int4_scalar, nf90_put_var, "//trim(vname))
            
        else
            call check_enc( nf90_put_var(ncid, vid, data) , & 
            "in easyO_int4_scalar, nf90_put_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        if (present(shape_total)) then
            deallocate(dimids)
            deallocate(dimnames_)
        end if
        deallocate(shape_ncv)
        return
    end subroutine easyO_int4_scalar
    Subroutine easyI_int4_scalar(fname, vname, data, position)
        !!! #####################################
        ! This Subroutine aims to read a scalar int4 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total" and "position_"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),intent(out) :: data
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: ncid, vid
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ read data
        if (present(position)) then
            call check_enc( nf90_get_var(ncid, vid, data, start=position) , & 
            "in easyI_int4_scalar, nf90_get_var, "//trim(vname))
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_int4_scalar, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        return
    end subroutine easyI_int4_scalar
    Subroutine easyO_int8_scalar(fname, vname, data, shape_total, position, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a scalar int8 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total" and "position"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),intent(in) :: data
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= netcdf-4 compatability
        if (.not. enc_use_nc4) then
            print *, 'Error, cannot write INT64 to netcdf classic file, or set the enc_use_nc4 to .true.'
            stop 1
        end if
        ! ============================= prepare for netcdf IO
        if (.not. present(shape_total)) then  !# sub-array IO
            rank_ncv = 1
            allocate(shape_ncv(1))
            shape_ncv = 1
        else !# single element  io
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
            ! ========================= handle dimensions, e.g., dimnames_ & dimdis
            allocate(dimnames_(rank_ncv))
            allocate(dimids(rank_ncv))
            do i = 1, rank_ncv
                if (present(dimnames)) then
                    call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_int8_scalar, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                    if (len_trim(dimnames(i)) .gt. 0) then
                        dimnames_(i) = trim(dimnames(i))
                        cycle
                    end if
                end if
                write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
            end do
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        if (present(shape_total)) then
            do i = 1, rank_ncv
                if (isExist) then
                    if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                        call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
                    end if
                else
                    call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
                end if
            end do
        end if
        ! ~~~~~~~~~~~~~~ define variable
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            if (present(shape_total)) then
                call check_enc( nf90_def_var(ncid, vname, NF90_INT64, dimids, vid) , "nf90_def_var")
            else
                call check_enc( nf90_def_var(ncid, vname, NF90_INT64, varid=vid) , "nf90_def_var")
            end if
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        if (present(shape_total)) then
            call check_enc( nf90_put_var(ncid, vid, data, start=position) , & 
            "in easyO_int8_scalar, nf90_put_var, "//trim(vname))
            
        else
            call check_enc( nf90_put_var(ncid, vid, data) , & 
            "in easyO_int8_scalar, nf90_put_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        if (present(shape_total)) then
            deallocate(dimids)
            deallocate(dimnames_)
        end if
        deallocate(shape_ncv)
        return
    end subroutine easyO_int8_scalar
    Subroutine easyI_int8_scalar(fname, vname, data, position)
        !!! #####################################
        ! This Subroutine aims to read a scalar int8 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total" and "position_"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),intent(out) :: data
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: ncid, vid
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ read data
        if (present(position)) then
            call check_enc( nf90_get_var(ncid, vid, data, start=position) , & 
            "in easyI_int8_scalar, nf90_get_var, "//trim(vname))
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_int8_scalar, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        return
    end subroutine easyI_int8_scalar
    Subroutine easyO_real4_scalar(fname, vname, data, shape_total, position, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a scalar real4 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total" and "position"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),intent(in) :: data
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= prepare for netcdf IO
        if (.not. present(shape_total)) then  !# sub-array IO
            rank_ncv = 1
            allocate(shape_ncv(1))
            shape_ncv = 1
        else !# single element  io
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
            ! ========================= handle dimensions, e.g., dimnames_ & dimdis
            allocate(dimnames_(rank_ncv))
            allocate(dimids(rank_ncv))
            do i = 1, rank_ncv
                if (present(dimnames)) then
                    call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_real4_scalar, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                    if (len_trim(dimnames(i)) .gt. 0) then
                        dimnames_(i) = trim(dimnames(i))
                        cycle
                    end if
                end if
                write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
            end do
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        if (present(shape_total)) then
            do i = 1, rank_ncv
                if (isExist) then
                    if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                        call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
                    end if
                else
                    call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
                end if
            end do
        end if
        ! ~~~~~~~~~~~~~~ define variable
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            if (present(shape_total)) then
                call check_enc( nf90_def_var(ncid, vname, NF90_FLOAT, dimids, vid) , "nf90_def_var")
            else
                call check_enc( nf90_def_var(ncid, vname, NF90_FLOAT, varid=vid) , "nf90_def_var")
            end if
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        if (present(shape_total)) then
            call check_enc( nf90_put_var(ncid, vid, data, start=position) , & 
            "in easyO_real4_scalar, nf90_put_var, "//trim(vname))
            
        else
            call check_enc( nf90_put_var(ncid, vid, data) , & 
            "in easyO_real4_scalar, nf90_put_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        if (present(shape_total)) then
            deallocate(dimids)
            deallocate(dimnames_)
        end if
        deallocate(shape_ncv)
        return
    end subroutine easyO_real4_scalar
    Subroutine easyI_real4_scalar(fname, vname, data, position)
        !!! #####################################
        ! This Subroutine aims to read a scalar real4 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total" and "position_"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),intent(out) :: data
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: ncid, vid
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ read data
        if (present(position)) then
            call check_enc( nf90_get_var(ncid, vid, data, start=position) , & 
            "in easyI_real4_scalar, nf90_get_var, "//trim(vname))
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_scalar, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        return
    end subroutine easyI_real4_scalar
    Subroutine easyO_real8_scalar(fname, vname, data, shape_total, position, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a scalar real8 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total" and "position"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),intent(in) :: data
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= prepare for netcdf IO
        if (.not. present(shape_total)) then  !# sub-array IO
            rank_ncv = 1
            allocate(shape_ncv(1))
            shape_ncv = 1
        else !# single element  io
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
            ! ========================= handle dimensions, e.g., dimnames_ & dimdis
            allocate(dimnames_(rank_ncv))
            allocate(dimids(rank_ncv))
            do i = 1, rank_ncv
                if (present(dimnames)) then
                    call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_real8_scalar, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                    if (len_trim(dimnames(i)) .gt. 0) then
                        dimnames_(i) = trim(dimnames(i))
                        cycle
                    end if
                end if
                write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
            end do
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        if (present(shape_total)) then
            do i = 1, rank_ncv
                if (isExist) then
                    if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                        call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
                    end if
                else
                    call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
                end if
            end do
        end if
        ! ~~~~~~~~~~~~~~ define variable
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            if (present(shape_total)) then
                call check_enc( nf90_def_var(ncid, vname, NF90_DOUBLE, dimids, vid) , "nf90_def_var")
            else
                call check_enc( nf90_def_var(ncid, vname, NF90_DOUBLE, varid=vid) , "nf90_def_var")
            end if
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        if (present(shape_total)) then
            call check_enc( nf90_put_var(ncid, vid, data, start=position) , & 
            "in easyO_real8_scalar, nf90_put_var, "//trim(vname))
            
        else
            call check_enc( nf90_put_var(ncid, vid, data) , & 
            "in easyO_real8_scalar, nf90_put_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        if (present(shape_total)) then
            deallocate(dimids)
            deallocate(dimnames_)
        end if
        deallocate(shape_ncv)
        return
    end subroutine easyO_real8_scalar
    Subroutine easyI_real8_scalar(fname, vname, data, position)
        !!! #####################################
        ! This Subroutine aims to read a scalar real8 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total" and "position_"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),intent(out) :: data
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: ncid, vid
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ read data
        if (present(position)) then
            call check_enc( nf90_get_var(ncid, vid, data, start=position) , & 
            "in easyI_real8_scalar, nf90_get_var, "//trim(vname))
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real8_scalar, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        return
    end subroutine easyI_real8_scalar
! *********************************************************
! easyIO for n-dimensional generic numeric data type :
!   int4. int8, real4, real8
! *********************************************************
    Subroutine easyO_int4_1d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 1-dimensional int4 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),intent(in) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_int4_1d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_INT, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_1d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_int4_1d
    Subroutine easyI_int4_1d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 1-dimensional int4 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),intent(out) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_1d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_1d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_int4_1d
    Subroutine easyO_int8_1d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 1-dimensional int8 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),intent(in) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= netcdf-4 compatability
        if (.not. enc_use_nc4) then
            print *, 'Error, cannot write INT64 to netcdf classic file, or set the enc_use_nc4 to .true.'
            stop 1
        end if
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_int8_1d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_INT64, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_1d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_int8_1d
    Subroutine easyI_int8_1d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 1-dimensional int8 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),intent(out) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_1d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_1d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_int8_1d
    Subroutine easyO_real4_1d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 1-dimensional real4 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),intent(in) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_real4_1d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_FLOAT, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_1d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_real4_1d
    Subroutine easyI_real4_1d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 1-dimensional real4 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),intent(out) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_1d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_1d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_real4_1d
    Subroutine easyO_real8_1d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 1-dimensional real8 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),intent(in) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_real8_1d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_DOUBLE, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_1d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_real8_1d
    Subroutine easyI_real8_1d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 1-dimensional real8 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),intent(out) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_1d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_1d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_real8_1d
    Subroutine easyO_int4_2d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 2-dimensional int4 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),intent(in) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_int4_2d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_INT, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_2d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_int4_2d
    Subroutine easyI_int4_2d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 2-dimensional int4 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),intent(out) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_2d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_2d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_int4_2d
    Subroutine easyO_int8_2d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 2-dimensional int8 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),intent(in) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= netcdf-4 compatability
        if (.not. enc_use_nc4) then
            print *, 'Error, cannot write INT64 to netcdf classic file, or set the enc_use_nc4 to .true.'
            stop 1
        end if
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_int8_2d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_INT64, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_2d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_int8_2d
    Subroutine easyI_int8_2d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 2-dimensional int8 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),intent(out) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_2d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_2d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_int8_2d
    Subroutine easyO_real4_2d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 2-dimensional real4 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),intent(in) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_real4_2d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_FLOAT, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_2d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_real4_2d
    Subroutine easyI_real4_2d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 2-dimensional real4 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),intent(out) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_2d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_2d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_real4_2d
    Subroutine easyO_real8_2d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 2-dimensional real8 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),intent(in) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_real8_2d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_DOUBLE, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_2d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_real8_2d
    Subroutine easyI_real8_2d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 2-dimensional real8 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),intent(out) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_2d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_2d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_real8_2d
    Subroutine easyO_int4_3d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 3-dimensional int4 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),intent(in) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_int4_3d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_INT, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_3d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_int4_3d
    Subroutine easyI_int4_3d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 3-dimensional int4 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),intent(out) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_3d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_3d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_int4_3d
    Subroutine easyO_int8_3d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 3-dimensional int8 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),intent(in) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= netcdf-4 compatability
        if (.not. enc_use_nc4) then
            print *, 'Error, cannot write INT64 to netcdf classic file, or set the enc_use_nc4 to .true.'
            stop 1
        end if
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_int8_3d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_INT64, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_3d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_int8_3d
    Subroutine easyI_int8_3d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 3-dimensional int8 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),intent(out) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_3d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_3d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_int8_3d
    Subroutine easyO_real4_3d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 3-dimensional real4 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),intent(in) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_real4_3d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_FLOAT, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_3d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_real4_3d
    Subroutine easyI_real4_3d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 3-dimensional real4 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),intent(out) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_3d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_3d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_real4_3d
    Subroutine easyO_real8_3d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 3-dimensional real8 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),intent(in) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_real8_3d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_DOUBLE, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_3d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_real8_3d
    Subroutine easyI_real8_3d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 3-dimensional real8 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),intent(out) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_3d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_3d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_real8_3d
    Subroutine easyO_int4_4d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 4-dimensional int4 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),intent(in) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_int4_4d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_INT, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_4d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_int4_4d
    Subroutine easyI_int4_4d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 4-dimensional int4 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),intent(out) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_4d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_4d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_int4_4d
    Subroutine easyO_int8_4d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 4-dimensional int8 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),intent(in) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= netcdf-4 compatability
        if (.not. enc_use_nc4) then
            print *, 'Error, cannot write INT64 to netcdf classic file, or set the enc_use_nc4 to .true.'
            stop 1
        end if
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_int8_4d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_INT64, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_4d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_int8_4d
    Subroutine easyI_int8_4d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 4-dimensional int8 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),intent(out) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_4d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_4d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_int8_4d
    Subroutine easyO_real4_4d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 4-dimensional real4 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),intent(in) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_real4_4d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_FLOAT, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_4d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_real4_4d
    Subroutine easyI_real4_4d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 4-dimensional real4 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),intent(out) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_4d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_4d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_real4_4d
    Subroutine easyO_real8_4d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 4-dimensional real8 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),intent(in) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_real8_4d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_DOUBLE, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_4d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_real8_4d
    Subroutine easyI_real8_4d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 4-dimensional real8 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),intent(out) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_4d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_4d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_real8_4d
    Subroutine easyO_int4_5d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 5-dimensional int4 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),intent(in) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_int4_5d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_INT, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_5d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_int4_5d
    Subroutine easyI_int4_5d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 5-dimensional int4 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),intent(out) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_5d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_5d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_int4_5d
    Subroutine easyO_int8_5d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 5-dimensional int8 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),intent(in) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= netcdf-4 compatability
        if (.not. enc_use_nc4) then
            print *, 'Error, cannot write INT64 to netcdf classic file, or set the enc_use_nc4 to .true.'
            stop 1
        end if
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_int8_5d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_INT64, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_5d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_int8_5d
    Subroutine easyI_int8_5d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 5-dimensional int8 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),intent(out) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_5d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_5d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_int8_5d
    Subroutine easyO_real4_5d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 5-dimensional real4 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),intent(in) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_real4_5d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_FLOAT, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_5d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_real4_5d
    Subroutine easyI_real4_5d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 5-dimensional real4 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),intent(out) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_5d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_5d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_real4_5d
    Subroutine easyO_real8_5d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 5-dimensional real8 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),intent(in) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_real8_5d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_DOUBLE, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_5d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_real8_5d
    Subroutine easyI_real8_5d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 5-dimensional real8 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),intent(out) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_5d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_5d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_real8_5d
    Subroutine easyO_int4_6d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 6-dimensional int4 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),intent(in) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_int4_6d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_INT, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_6d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_int4_6d
    Subroutine easyI_int4_6d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 6-dimensional int4 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),intent(out) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_6d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_6d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_int4_6d
    Subroutine easyO_int8_6d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 6-dimensional int8 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),intent(in) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= netcdf-4 compatability
        if (.not. enc_use_nc4) then
            print *, 'Error, cannot write INT64 to netcdf classic file, or set the enc_use_nc4 to .true.'
            stop 1
        end if
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_int8_6d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_INT64, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_6d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_int8_6d
    Subroutine easyI_int8_6d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 6-dimensional int8 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),intent(out) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_6d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_6d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_int8_6d
    Subroutine easyO_real4_6d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 6-dimensional real4 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),intent(in) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_real4_6d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_FLOAT, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_6d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_real4_6d
    Subroutine easyI_real4_6d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 6-dimensional real4 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),intent(out) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_6d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_6d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_real4_6d
    Subroutine easyO_real8_6d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 6-dimensional real8 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),intent(in) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_real8_6d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_DOUBLE, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_6d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_real8_6d
    Subroutine easyI_real8_6d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 6-dimensional real8 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),intent(out) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_6d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_6d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_real8_6d
    Subroutine easyO_int4_7d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 7-dimensional int4 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),intent(in) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_int4_7d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_INT, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_7d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_int4_7d
    Subroutine easyI_int4_7d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 7-dimensional int4 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),intent(out) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_7d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_7d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_int4_7d
    Subroutine easyO_int8_7d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 7-dimensional int8 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),intent(in) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= netcdf-4 compatability
        if (.not. enc_use_nc4) then
            print *, 'Error, cannot write INT64 to netcdf classic file, or set the enc_use_nc4 to .true.'
            stop 1
        end if
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_int8_7d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_INT64, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_7d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_int8_7d
    Subroutine easyI_int8_7d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 7-dimensional int8 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),intent(out) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_7d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_7d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_int8_7d
    Subroutine easyO_real4_7d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 7-dimensional real4 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),intent(in) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_real4_7d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_FLOAT, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_7d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_real4_7d
    Subroutine easyI_real4_7d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 7-dimensional real4 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),intent(out) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_7d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_7d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_real4_7d
    Subroutine easyO_real8_7d(fname, vname, data, shape_total, position, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a 7-dimensional real8 variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),intent(in) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_data(:)
        integer :: rank_data                    ! ranks of data, e.g., size of shape_data
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer, allocatable :: position_(:), count_lens_(:)
        
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer :: ncid, vid
            
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
            
        ! ============================= prepare for netcdf IO
        ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
        rank_data = size(shape(data))
        ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
        allocate(shape_data(size(shape(data))), source=shape(data))
        if (present(shape_total)) then
            rank_ncv = size(shape_total)
            allocate(shape_ncv, source=shape_total)
        else
            rank_ncv = rank_data
            allocate(shape_ncv, source=shape_data)
        end if
        ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
        allocate(dimnames_(rank_ncv))
        allocate(dimids(rank_ncv))
        do i = 1, rank_ncv
            if (present(dimnames)) then
                call assert(size(dimnames) .eq. rank_ncv, 'Error in easyO_real8_7d, &
     argument<dimnames> should have the same size as rank_ncv') ! 
                if (len_trim(dimnames(i)) .gt. 0) then
                    dimnames_(i) = trim(dimnames(i))
                    cycle
                end if
            end if
            write(dimnames_(i), '(A, ".d", I1)') trim(vname), i
        end do
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        allocate(position_(rank_ncv))
        allocate(count_lens_(rank_ncv))
        if (present(shape_total)) then
            position_ = position
            count_lens_ = count_lens
        else
            position_ = 1
            count_lens_ = shape_ncv
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
            if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
            else
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable, or error
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_DOUBLE, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyO_real4_7d, nf90_put_var, "//trim(vname))
        
        call check_enc( nf90_close(ncid) , "nf90_close")
        
        deallocate(shape_data)
        deallocate(shape_ncv)
        deallocate(dimids)
        deallocate(dimnames_)
        deallocate(position_)
        deallocate(count_lens_)
        return
    end subroutine easyO_real8_7d
    Subroutine easyI_real8_7d(fname, vname, data, position, count_lens)
        !!! #####################################
        ! This Subroutine aims to read a 7-dimensional real8 variable from a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total", "position" and "count_lens"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),intent(out) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        integer :: ncid, vid
            
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        else
            print *, "Error, file doesn't exist."
            stop 1
        end if
        ! ~~~~~~~~~~~~~~ get variable
        call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
        ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
        if (present(position)) then
            ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
            ! ~~~~~~~~~~~~~~ write data
            call check_enc( nf90_get_var(ncid, vid, data, start=position, count=count_lens) , & 
                "in easyI_real4_7d, nf90_get_var, "//trim(vname))
            
        else
            call check_enc( nf90_get_var(ncid, vid, data) , & 
            "in easyI_real4_7d, nf90_get_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        return
    end subroutine easyI_real8_7d
! *********************************************************
! easyIO for scalar generic numeric data type :
!   character()
! *********************************************************
    Subroutine easyO_string_scalar(fname, vname, data, shape_total, position, dimnames)
        !!! #####################################
        ! This Subroutine aims to write a scalar string variable into a netcdf dataset
        ! Also, the scalar variable can also be put in an array record in netcdf, 
        ! via setting optional argument "shape_total" and "position"
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),intent(in) :: data
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: rank_ncv                     ! ranks of output netcdf variable
        integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
        character(80), allocatable :: dimnames_(:)
        integer, allocatable :: dimids(:)
        integer, allocatable :: position_(:)  !  , count_lens_(:)  ! should be unnecessary
        integer :: ncid, vid
        integer :: i
        logical :: isExist
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        ! ============================= prepare for netcdf IO
        if (.not. present(shape_total)) then  !# sub-array IO
            rank_ncv = 1
            allocate(shape_ncv(1))
            shape_ncv = len(data)
            allocate(dimnames_(rank_ncv))
            allocate(dimids(rank_ncv))
            dimnames_(1) = toString(vname, '.L')
        else !# single element  io
            rank_ncv = size(shape_total) + 1  !# add one dim for character length
            shape_ncv = union_arr1d([len(data)], shape_total)
            ! allocate(shape_ncv(rank_ncv))
            ! shape_ncv(1) = len(data)
            ! shape_ncv(2:) = shape_total
            ! ========================= handle dimensions, e.g., dimnames_ & dimdis
            allocate(dimnames_(rank_ncv))
            allocate(dimids(rank_ncv))
            dimnames_(1) = toString(vname, '.L')
            do i = 2, rank_ncv
                if (present(dimnames)) then
                    call assert(size(dimnames)+1 .eq. rank_ncv, 'Error in easyO_string_scalar, &
     argument<dimnames> should have the same size as rank_ncv (for string, needs +1)') ! 
                    if (len_trim(dimnames(i+1)) .gt. 0) then
                        dimnames_(i) = trim(dimnames(i+1))
                        cycle
                    end if
                end if
                write(dimnames_(i), '(A, ".d", I1)') trim(vname), i-1
            end do
            position_ = union_arr1d([1], position)  !# imply allocate
        end if
        ! ============================= netcdf core part
        inquire(file = fname, exist = isExist)
        ! ~~~~~~~~~~~~~~ open or create nc
        if (isExist) then
            call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
            call check_enc( nf90_redef(ncid), "nf90_redef")
        else
            if (enc_use_nc4) then
                call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
            else
                call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
            end if
        end if
        ! ~~~~~~~~~~~~~~ get or define dimension
        do i = 1, rank_ncv
            if (isExist) then
                if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
                    call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
                end if
            else
                call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
            end if
        end do
        ! ~~~~~~~~~~~~~~ define variable
        if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
            call check_enc( nf90_def_var(ncid, vname, NF90_CHAR, dimids, vid) , "nf90_def_var")
        end if
        call check_enc( nf90_enddef(ncid) , "nf90_enddef")
        ! ~~~~~~~~~~~~~~ write data
        ! print *, 'start_index = ',start_index
        ! print *, 'count_lens_ = ',count_lens_
        if (present(shape_total)) then
            call check_enc( nf90_put_var(ncid, vid, data, start=position_) , & 
            "in easyO__scalar, nf90_put_var, "//trim(vname))
            deallocate(position_)
            
        else
            call check_enc( nf90_put_var(ncid, vid, data) , & 
            "in easyO__scalar, nf90_put_var, "//trim(vname))
        end if
        call check_enc( nf90_close(ncid) , "nf90_close")
        deallocate(shape_ncv)
        deallocate(dimnames_)
        deallocate(dimids)
        
        return
    end subroutine easyO_string_scalar
  
Subroutine easyI_string_scalar(fname, vname, data, position)
    !!! #####################################
    ! This Subroutine aims to read a scalar  variable from a netcdf dataset
    ! Also, the scalar variable can also be put in an array record in netcdf, 
    ! via setting optional argument "shape_total" and "position_"
    !!! #####################################
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    character(*),intent(inout) :: data
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    integer :: ncid, vid
    logical :: isExist
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= netcdf core part
    inquire(file = fname, exist = isExist)
    ! ~~~~~~~~~~~~~~ open or create nc
    if (isExist) then
        call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
    else
        print *, "Error, file doesn't exist."
        stop 1
    end if
    ! ~~~~~~~~~~~~~~ get variable
    call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
    ! ~~~~~~~~~~~~~~ read data
    if (present(position)) then
        call check_enc( nf90_get_var(ncid, vid, data, start=union_arr1d([1], position)) , & 
        "in easyI__scalar, nf90_get_var, "//trim(vname))
    else
        call check_enc( nf90_get_var(ncid, vid, data) , & 
        "in easyI__scalar, nf90_get_var, "//trim(vname))
    end if
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    return
end subroutine easyI_string_scalar
Subroutine easyO_string_1d(fname, vname, data, shape_total, position, count_lens, dimnames)
    !!! #####################################
    ! This Subroutine aims to write a 1-dimensional  variable into a netcdf dataset
    ! Also, the scalar variable can also be put in an array record in netcdf, 
    ! via setting optional argument "shape_total", "position" and "count_lens"
    !!! #####################################
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    character(*),intent(in) :: data(:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)         ! total shape
    integer, intent(in), optional :: position(:)            ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
    character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    integer, allocatable :: shape_data(:)
    integer :: rank_data                    ! ranks of data, e.g., size of shape_data
    integer :: rank_ncv                     ! ranks of output netcdf variable
    integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
    integer, allocatable :: position_(:), count_lens_(:)
    
    character(80), allocatable :: dimnames_(:)
    integer, allocatable :: dimids(:)
    integer :: ncid, vid
        
    integer :: i
    logical :: isExist
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
    if (enc_var_exist(fname, vname)) then
        if (enc_vea .eq. 1) then
            return
        elseif (enc_vea .eq. -1) then
            print *, 'Error in easyO with enc_vea = -1, variable exist!'
            stop 1
        end if
    end if
    ! ============================= prepare for netcdf IO
    ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
    rank_data = size(shape(data))
    ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
    allocate(shape_data(size(shape(data))), source=shape(data))
    if (present(shape_total)) then
        rank_ncv = size(shape_total) + 1
        ! allocate(shape_ncv, source=shape_total)
        shape_ncv = union_arr1d([len(data)], shape_total)
    else
        rank_ncv = rank_data + 1
        ! allocate(shape_ncv, source=shape_data)
        shape_ncv = union_arr1d([len(data)], shape_data)
    end if
    ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
    allocate(dimnames_(rank_ncv))
    allocate(dimids(rank_ncv))
    dimnames_(1) = toString(vname, '.L')
    do i = 2, rank_ncv
        if (present(dimnames)) then
            call assert(size(dimnames)+1 .eq. rank_ncv, 'Error in easyO_string_1d, &
     argument<dimnames> should have the same size as rank_ncv (for string, needs +1)') ! 
            if (len_trim(dimnames(i+1)) .gt. 0) then
                dimnames_(i) = trim(dimnames(i+1))
                cycle
            end if
        end if
        write(dimnames_(i), '(A, ".d", I1)') trim(vname), i-1
    end do
    ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
    if (present(shape_total)) then
        position_ = union_arr1d([1], position)  !# imply allocate
        count_lens_ = union_arr1d([len(data)], count_lens)
    else
        allocate(position_(rank_ncv))
        position_ = 1
        allocate(count_lens_, source=shape_ncv)
    end if
    ! ============================= netcdf core part
    inquire(file = fname, exist = isExist)
    ! ~~~~~~~~~~~~~~ open or create nc
    if (isExist) then
        call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        call check_enc( nf90_redef(ncid), "nf90_redef")
    else
        if (enc_use_nc4) then
            call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
        else
            call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
        end if
    end if
    ! ~~~~~~~~~~~~~~ get or define dimension
    do i = 1, rank_ncv
        if (isExist) then
        if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
        end if
        else
        call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
        end if
    end do
    ! ~~~~~~~~~~~~~~ define variable, or error
    if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
        call check_enc( nf90_def_var(ncid, vname, NF90_CHAR, dimids, vid) , "nf90_def_var")
    end if
    call check_enc( nf90_enddef(ncid) , "nf90_enddef")
    ! ~~~~~~~~~~~~~~ write data
    ! print *, 'start_index = ',start_index
    ! print *, 'count_lens_ = ',count_lens_
    call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
        "in easyO_string_1d, nf90_put_var, "//trim(vname))
    
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    deallocate(shape_data)
    deallocate(shape_ncv)
    deallocate(dimids)
    deallocate(dimnames_)
    deallocate(count_lens_)
    deallocate(position_)
    return
end subroutine easyO_string_1d
Subroutine easyI_string_1d(fname, vname, data, position, count_lens)
    !!! #####################################
    ! This Subroutine aims to read a 1-dimensional  variable from a netcdf dataset
    ! Also, the scalar variable can also be put in an array record in netcdf, 
    ! via setting optional argument "shape_total", "position" and "count_lens"
    !!! #####################################
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    character(*),intent(inout) :: data(:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    integer :: rank_ncv                     ! ranks of output netcdf variable
    integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
    integer, allocatable :: position_(:), count_lens_(:)
    
    integer :: ncid, vid
        
    logical :: isExist
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= netcdf core part
    inquire(file = fname, exist = isExist)
    ! ~~~~~~~~~~~~~~ open or create nc
    if (isExist) then
        call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
    else
        print *, "Error, file doesn't exist."
        stop 1
    end if
    ! ~~~~~~~~~~~~~~ get variable
    call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
    ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
    if (present(position)) then
        ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
        position_ = union_arr1d([1], position)
        count_lens_ = union_arr1d([len(data)], count_lens)
        ! ~~~~~~~~~~~~~~ write data
        call check_enc( nf90_get_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyI_string_1d, nf90_get_var, "//trim(vname))
        
        deallocate(position_)
        deallocate(count_lens_)
    else
        call check_enc( nf90_get_var(ncid, vid, data) , & 
        "in easyI_string_1d, nf90_get_var, "//trim(vname))
    end if
    call check_enc( nf90_close(ncid) , "nf90_close")
    return
end subroutine easyI_string_1d
Subroutine easyO_string_2d(fname, vname, data, shape_total, position, count_lens, dimnames)
    !!! #####################################
    ! This Subroutine aims to write a 2-dimensional  variable into a netcdf dataset
    ! Also, the scalar variable can also be put in an array record in netcdf, 
    ! via setting optional argument "shape_total", "position" and "count_lens"
    !!! #####################################
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    character(*),intent(in) :: data(:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)         ! total shape
    integer, intent(in), optional :: position(:)            ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
    character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    integer, allocatable :: shape_data(:)
    integer :: rank_data                    ! ranks of data, e.g., size of shape_data
    integer :: rank_ncv                     ! ranks of output netcdf variable
    integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
    integer, allocatable :: position_(:), count_lens_(:)
    
    character(80), allocatable :: dimnames_(:)
    integer, allocatable :: dimids(:)
    integer :: ncid, vid
        
    integer :: i
    logical :: isExist
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
    if (enc_var_exist(fname, vname)) then
        if (enc_vea .eq. 1) then
            return
        elseif (enc_vea .eq. -1) then
            print *, 'Error in easyO with enc_vea = -1, variable exist!'
            stop 1
        end if
    end if
    ! ============================= prepare for netcdf IO
    ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
    rank_data = size(shape(data))
    ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
    allocate(shape_data(size(shape(data))), source=shape(data))
    if (present(shape_total)) then
        rank_ncv = size(shape_total) + 1
        ! allocate(shape_ncv, source=shape_total)
        shape_ncv = union_arr1d([len(data)], shape_total)
    else
        rank_ncv = rank_data + 1
        ! allocate(shape_ncv, source=shape_data)
        shape_ncv = union_arr1d([len(data)], shape_data)
    end if
    ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
    allocate(dimnames_(rank_ncv))
    allocate(dimids(rank_ncv))
    dimnames_(1) = toString(vname, '.L')
    do i = 2, rank_ncv
        if (present(dimnames)) then
            call assert(size(dimnames)+1 .eq. rank_ncv, 'Error in easyO_string_2d, &
     argument<dimnames> should have the same size as rank_ncv (for string, needs +1)') ! 
            if (len_trim(dimnames(i+1)) .gt. 0) then
                dimnames_(i) = trim(dimnames(i+1))
                cycle
            end if
        end if
        write(dimnames_(i), '(A, ".d", I1)') trim(vname), i-1
    end do
    ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
    if (present(shape_total)) then
        position_ = union_arr1d([1], position)  !# imply allocate
        count_lens_ = union_arr1d([len(data)], count_lens)
    else
        allocate(position_(rank_ncv))
        position_ = 1
        allocate(count_lens_, source=shape_ncv)
    end if
    ! ============================= netcdf core part
    inquire(file = fname, exist = isExist)
    ! ~~~~~~~~~~~~~~ open or create nc
    if (isExist) then
        call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        call check_enc( nf90_redef(ncid), "nf90_redef")
    else
        if (enc_use_nc4) then
            call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
        else
            call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
        end if
    end if
    ! ~~~~~~~~~~~~~~ get or define dimension
    do i = 1, rank_ncv
        if (isExist) then
        if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
        end if
        else
        call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
        end if
    end do
    ! ~~~~~~~~~~~~~~ define variable, or error
    if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
        call check_enc( nf90_def_var(ncid, vname, NF90_CHAR, dimids, vid) , "nf90_def_var")
    end if
    call check_enc( nf90_enddef(ncid) , "nf90_enddef")
    ! ~~~~~~~~~~~~~~ write data
    ! print *, 'start_index = ',start_index
    ! print *, 'count_lens_ = ',count_lens_
    call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
        "in easyO_string_2d, nf90_put_var, "//trim(vname))
    
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    deallocate(shape_data)
    deallocate(shape_ncv)
    deallocate(dimids)
    deallocate(dimnames_)
    deallocate(count_lens_)
    deallocate(position_)
    return
end subroutine easyO_string_2d
Subroutine easyI_string_2d(fname, vname, data, position, count_lens)
    !!! #####################################
    ! This Subroutine aims to read a 2-dimensional  variable from a netcdf dataset
    ! Also, the scalar variable can also be put in an array record in netcdf, 
    ! via setting optional argument "shape_total", "position" and "count_lens"
    !!! #####################################
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    character(*),intent(inout) :: data(:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    integer :: rank_ncv                     ! ranks of output netcdf variable
    integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
    integer, allocatable :: position_(:), count_lens_(:)
    
    integer :: ncid, vid
        
    logical :: isExist
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= netcdf core part
    inquire(file = fname, exist = isExist)
    ! ~~~~~~~~~~~~~~ open or create nc
    if (isExist) then
        call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
    else
        print *, "Error, file doesn't exist."
        stop 1
    end if
    ! ~~~~~~~~~~~~~~ get variable
    call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
    ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
    if (present(position)) then
        ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
        position_ = union_arr1d([1], position)
        count_lens_ = union_arr1d([len(data)], count_lens)
        ! ~~~~~~~~~~~~~~ write data
        call check_enc( nf90_get_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyI_string_2d, nf90_get_var, "//trim(vname))
        
        deallocate(position_)
        deallocate(count_lens_)
    else
        call check_enc( nf90_get_var(ncid, vid, data) , & 
        "in easyI_string_2d, nf90_get_var, "//trim(vname))
    end if
    call check_enc( nf90_close(ncid) , "nf90_close")
    return
end subroutine easyI_string_2d
Subroutine easyO_string_3d(fname, vname, data, shape_total, position, count_lens, dimnames)
    !!! #####################################
    ! This Subroutine aims to write a 3-dimensional  variable into a netcdf dataset
    ! Also, the scalar variable can also be put in an array record in netcdf, 
    ! via setting optional argument "shape_total", "position" and "count_lens"
    !!! #####################################
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    character(*),intent(in) :: data(:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)         ! total shape
    integer, intent(in), optional :: position(:)            ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
    character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    integer, allocatable :: shape_data(:)
    integer :: rank_data                    ! ranks of data, e.g., size of shape_data
    integer :: rank_ncv                     ! ranks of output netcdf variable
    integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
    integer, allocatable :: position_(:), count_lens_(:)
    
    character(80), allocatable :: dimnames_(:)
    integer, allocatable :: dimids(:)
    integer :: ncid, vid
        
    integer :: i
    logical :: isExist
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
    if (enc_var_exist(fname, vname)) then
        if (enc_vea .eq. 1) then
            return
        elseif (enc_vea .eq. -1) then
            print *, 'Error in easyO with enc_vea = -1, variable exist!'
            stop 1
        end if
    end if
    ! ============================= prepare for netcdf IO
    ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
    rank_data = size(shape(data))
    ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
    allocate(shape_data(size(shape(data))), source=shape(data))
    if (present(shape_total)) then
        rank_ncv = size(shape_total) + 1
        ! allocate(shape_ncv, source=shape_total)
        shape_ncv = union_arr1d([len(data)], shape_total)
    else
        rank_ncv = rank_data + 1
        ! allocate(shape_ncv, source=shape_data)
        shape_ncv = union_arr1d([len(data)], shape_data)
    end if
    ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
    allocate(dimnames_(rank_ncv))
    allocate(dimids(rank_ncv))
    dimnames_(1) = toString(vname, '.L')
    do i = 2, rank_ncv
        if (present(dimnames)) then
            call assert(size(dimnames)+1 .eq. rank_ncv, 'Error in easyO_string_3d, &
     argument<dimnames> should have the same size as rank_ncv (for string, needs +1)') ! 
            if (len_trim(dimnames(i+1)) .gt. 0) then
                dimnames_(i) = trim(dimnames(i+1))
                cycle
            end if
        end if
        write(dimnames_(i), '(A, ".d", I1)') trim(vname), i-1
    end do
    ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
    if (present(shape_total)) then
        position_ = union_arr1d([1], position)  !# imply allocate
        count_lens_ = union_arr1d([len(data)], count_lens)
    else
        allocate(position_(rank_ncv))
        position_ = 1
        allocate(count_lens_, source=shape_ncv)
    end if
    ! ============================= netcdf core part
    inquire(file = fname, exist = isExist)
    ! ~~~~~~~~~~~~~~ open or create nc
    if (isExist) then
        call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        call check_enc( nf90_redef(ncid), "nf90_redef")
    else
        if (enc_use_nc4) then
            call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
        else
            call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
        end if
    end if
    ! ~~~~~~~~~~~~~~ get or define dimension
    do i = 1, rank_ncv
        if (isExist) then
        if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
        end if
        else
        call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
        end if
    end do
    ! ~~~~~~~~~~~~~~ define variable, or error
    if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
        call check_enc( nf90_def_var(ncid, vname, NF90_CHAR, dimids, vid) , "nf90_def_var")
    end if
    call check_enc( nf90_enddef(ncid) , "nf90_enddef")
    ! ~~~~~~~~~~~~~~ write data
    ! print *, 'start_index = ',start_index
    ! print *, 'count_lens_ = ',count_lens_
    call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
        "in easyO_string_3d, nf90_put_var, "//trim(vname))
    
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    deallocate(shape_data)
    deallocate(shape_ncv)
    deallocate(dimids)
    deallocate(dimnames_)
    deallocate(count_lens_)
    deallocate(position_)
    return
end subroutine easyO_string_3d
Subroutine easyI_string_3d(fname, vname, data, position, count_lens)
    !!! #####################################
    ! This Subroutine aims to read a 3-dimensional  variable from a netcdf dataset
    ! Also, the scalar variable can also be put in an array record in netcdf, 
    ! via setting optional argument "shape_total", "position" and "count_lens"
    !!! #####################################
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    character(*),intent(inout) :: data(:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    integer :: rank_ncv                     ! ranks of output netcdf variable
    integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
    integer, allocatable :: position_(:), count_lens_(:)
    
    integer :: ncid, vid
        
    logical :: isExist
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= netcdf core part
    inquire(file = fname, exist = isExist)
    ! ~~~~~~~~~~~~~~ open or create nc
    if (isExist) then
        call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
    else
        print *, "Error, file doesn't exist."
        stop 1
    end if
    ! ~~~~~~~~~~~~~~ get variable
    call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
    ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
    if (present(position)) then
        ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
        position_ = union_arr1d([1], position)
        count_lens_ = union_arr1d([len(data)], count_lens)
        ! ~~~~~~~~~~~~~~ write data
        call check_enc( nf90_get_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyI_string_3d, nf90_get_var, "//trim(vname))
        
        deallocate(position_)
        deallocate(count_lens_)
    else
        call check_enc( nf90_get_var(ncid, vid, data) , & 
        "in easyI_string_3d, nf90_get_var, "//trim(vname))
    end if
    call check_enc( nf90_close(ncid) , "nf90_close")
    return
end subroutine easyI_string_3d
Subroutine easyO_string_4d(fname, vname, data, shape_total, position, count_lens, dimnames)
    !!! #####################################
    ! This Subroutine aims to write a 4-dimensional  variable into a netcdf dataset
    ! Also, the scalar variable can also be put in an array record in netcdf, 
    ! via setting optional argument "shape_total", "position" and "count_lens"
    !!! #####################################
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    character(*),intent(in) :: data(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)         ! total shape
    integer, intent(in), optional :: position(:)            ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
    character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    integer, allocatable :: shape_data(:)
    integer :: rank_data                    ! ranks of data, e.g., size of shape_data
    integer :: rank_ncv                     ! ranks of output netcdf variable
    integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
    integer, allocatable :: position_(:), count_lens_(:)
    
    character(80), allocatable :: dimnames_(:)
    integer, allocatable :: dimids(:)
    integer :: ncid, vid
        
    integer :: i
    logical :: isExist
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
    if (enc_var_exist(fname, vname)) then
        if (enc_vea .eq. 1) then
            return
        elseif (enc_vea .eq. -1) then
            print *, 'Error in easyO with enc_vea = -1, variable exist!'
            stop 1
        end if
    end if
    ! ============================= prepare for netcdf IO
    ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
    rank_data = size(shape(data))
    ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
    allocate(shape_data(size(shape(data))), source=shape(data))
    if (present(shape_total)) then
        rank_ncv = size(shape_total) + 1
        ! allocate(shape_ncv, source=shape_total)
        shape_ncv = union_arr1d([len(data)], shape_total)
    else
        rank_ncv = rank_data + 1
        ! allocate(shape_ncv, source=shape_data)
        shape_ncv = union_arr1d([len(data)], shape_data)
    end if
    ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
    allocate(dimnames_(rank_ncv))
    allocate(dimids(rank_ncv))
    dimnames_(1) = toString(vname, '.L')
    do i = 2, rank_ncv
        if (present(dimnames)) then
            call assert(size(dimnames)+1 .eq. rank_ncv, 'Error in easyO_string_4d, &
     argument<dimnames> should have the same size as rank_ncv (for string, needs +1)') ! 
            if (len_trim(dimnames(i+1)) .gt. 0) then
                dimnames_(i) = trim(dimnames(i+1))
                cycle
            end if
        end if
        write(dimnames_(i), '(A, ".d", I1)') trim(vname), i-1
    end do
    ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
    if (present(shape_total)) then
        position_ = union_arr1d([1], position)  !# imply allocate
        count_lens_ = union_arr1d([len(data)], count_lens)
    else
        allocate(position_(rank_ncv))
        position_ = 1
        allocate(count_lens_, source=shape_ncv)
    end if
    ! ============================= netcdf core part
    inquire(file = fname, exist = isExist)
    ! ~~~~~~~~~~~~~~ open or create nc
    if (isExist) then
        call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        call check_enc( nf90_redef(ncid), "nf90_redef")
    else
        if (enc_use_nc4) then
            call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
        else
            call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
        end if
    end if
    ! ~~~~~~~~~~~~~~ get or define dimension
    do i = 1, rank_ncv
        if (isExist) then
        if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
        end if
        else
        call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
        end if
    end do
    ! ~~~~~~~~~~~~~~ define variable, or error
    if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
        call check_enc( nf90_def_var(ncid, vname, NF90_CHAR, dimids, vid) , "nf90_def_var")
    end if
    call check_enc( nf90_enddef(ncid) , "nf90_enddef")
    ! ~~~~~~~~~~~~~~ write data
    ! print *, 'start_index = ',start_index
    ! print *, 'count_lens_ = ',count_lens_
    call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
        "in easyO_string_4d, nf90_put_var, "//trim(vname))
    
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    deallocate(shape_data)
    deallocate(shape_ncv)
    deallocate(dimids)
    deallocate(dimnames_)
    deallocate(count_lens_)
    deallocate(position_)
    return
end subroutine easyO_string_4d
Subroutine easyI_string_4d(fname, vname, data, position, count_lens)
    !!! #####################################
    ! This Subroutine aims to read a 4-dimensional  variable from a netcdf dataset
    ! Also, the scalar variable can also be put in an array record in netcdf, 
    ! via setting optional argument "shape_total", "position" and "count_lens"
    !!! #####################################
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    character(*),intent(inout) :: data(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    integer :: rank_ncv                     ! ranks of output netcdf variable
    integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
    integer, allocatable :: position_(:), count_lens_(:)
    
    integer :: ncid, vid
        
    logical :: isExist
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= netcdf core part
    inquire(file = fname, exist = isExist)
    ! ~~~~~~~~~~~~~~ open or create nc
    if (isExist) then
        call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
    else
        print *, "Error, file doesn't exist."
        stop 1
    end if
    ! ~~~~~~~~~~~~~~ get variable
    call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
    ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
    if (present(position)) then
        ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
        position_ = union_arr1d([1], position)
        count_lens_ = union_arr1d([len(data)], count_lens)
        ! ~~~~~~~~~~~~~~ write data
        call check_enc( nf90_get_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyI_string_4d, nf90_get_var, "//trim(vname))
        
        deallocate(position_)
        deallocate(count_lens_)
    else
        call check_enc( nf90_get_var(ncid, vid, data) , & 
        "in easyI_string_4d, nf90_get_var, "//trim(vname))
    end if
    call check_enc( nf90_close(ncid) , "nf90_close")
    return
end subroutine easyI_string_4d
Subroutine easyO_string_5d(fname, vname, data, shape_total, position, count_lens, dimnames)
    !!! #####################################
    ! This Subroutine aims to write a 5-dimensional  variable into a netcdf dataset
    ! Also, the scalar variable can also be put in an array record in netcdf, 
    ! via setting optional argument "shape_total", "position" and "count_lens"
    !!! #####################################
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    character(*),intent(in) :: data(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)         ! total shape
    integer, intent(in), optional :: position(:)            ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
    character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    integer, allocatable :: shape_data(:)
    integer :: rank_data                    ! ranks of data, e.g., size of shape_data
    integer :: rank_ncv                     ! ranks of output netcdf variable
    integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
    integer, allocatable :: position_(:), count_lens_(:)
    
    character(80), allocatable :: dimnames_(:)
    integer, allocatable :: dimids(:)
    integer :: ncid, vid
        
    integer :: i
    logical :: isExist
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
    if (enc_var_exist(fname, vname)) then
        if (enc_vea .eq. 1) then
            return
        elseif (enc_vea .eq. -1) then
            print *, 'Error in easyO with enc_vea = -1, variable exist!'
            stop 1
        end if
    end if
    ! ============================= prepare for netcdf IO
    ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
    rank_data = size(shape(data))
    ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
    allocate(shape_data(size(shape(data))), source=shape(data))
    if (present(shape_total)) then
        rank_ncv = size(shape_total) + 1
        ! allocate(shape_ncv, source=shape_total)
        shape_ncv = union_arr1d([len(data)], shape_total)
    else
        rank_ncv = rank_data + 1
        ! allocate(shape_ncv, source=shape_data)
        shape_ncv = union_arr1d([len(data)], shape_data)
    end if
    ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
    allocate(dimnames_(rank_ncv))
    allocate(dimids(rank_ncv))
    dimnames_(1) = toString(vname, '.L')
    do i = 2, rank_ncv
        if (present(dimnames)) then
            call assert(size(dimnames)+1 .eq. rank_ncv, 'Error in easyO_string_5d, &
     argument<dimnames> should have the same size as rank_ncv (for string, needs +1)') ! 
            if (len_trim(dimnames(i+1)) .gt. 0) then
                dimnames_(i) = trim(dimnames(i+1))
                cycle
            end if
        end if
        write(dimnames_(i), '(A, ".d", I1)') trim(vname), i-1
    end do
    ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
    if (present(shape_total)) then
        position_ = union_arr1d([1], position)  !# imply allocate
        count_lens_ = union_arr1d([len(data)], count_lens)
    else
        allocate(position_(rank_ncv))
        position_ = 1
        allocate(count_lens_, source=shape_ncv)
    end if
    ! ============================= netcdf core part
    inquire(file = fname, exist = isExist)
    ! ~~~~~~~~~~~~~~ open or create nc
    if (isExist) then
        call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        call check_enc( nf90_redef(ncid), "nf90_redef")
    else
        if (enc_use_nc4) then
            call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
        else
            call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
        end if
    end if
    ! ~~~~~~~~~~~~~~ get or define dimension
    do i = 1, rank_ncv
        if (isExist) then
        if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
        end if
        else
        call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
        end if
    end do
    ! ~~~~~~~~~~~~~~ define variable, or error
    if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
        call check_enc( nf90_def_var(ncid, vname, NF90_CHAR, dimids, vid) , "nf90_def_var")
    end if
    call check_enc( nf90_enddef(ncid) , "nf90_enddef")
    ! ~~~~~~~~~~~~~~ write data
    ! print *, 'start_index = ',start_index
    ! print *, 'count_lens_ = ',count_lens_
    call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
        "in easyO_string_5d, nf90_put_var, "//trim(vname))
    
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    deallocate(shape_data)
    deallocate(shape_ncv)
    deallocate(dimids)
    deallocate(dimnames_)
    deallocate(count_lens_)
    deallocate(position_)
    return
end subroutine easyO_string_5d
Subroutine easyI_string_5d(fname, vname, data, position, count_lens)
    !!! #####################################
    ! This Subroutine aims to read a 5-dimensional  variable from a netcdf dataset
    ! Also, the scalar variable can also be put in an array record in netcdf, 
    ! via setting optional argument "shape_total", "position" and "count_lens"
    !!! #####################################
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    character(*),intent(inout) :: data(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    integer :: rank_ncv                     ! ranks of output netcdf variable
    integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
    integer, allocatable :: position_(:), count_lens_(:)
    
    integer :: ncid, vid
        
    logical :: isExist
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= netcdf core part
    inquire(file = fname, exist = isExist)
    ! ~~~~~~~~~~~~~~ open or create nc
    if (isExist) then
        call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
    else
        print *, "Error, file doesn't exist."
        stop 1
    end if
    ! ~~~~~~~~~~~~~~ get variable
    call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
    ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
    if (present(position)) then
        ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
        position_ = union_arr1d([1], position)
        count_lens_ = union_arr1d([len(data)], count_lens)
        ! ~~~~~~~~~~~~~~ write data
        call check_enc( nf90_get_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyI_string_5d, nf90_get_var, "//trim(vname))
        
        deallocate(position_)
        deallocate(count_lens_)
    else
        call check_enc( nf90_get_var(ncid, vid, data) , & 
        "in easyI_string_5d, nf90_get_var, "//trim(vname))
    end if
    call check_enc( nf90_close(ncid) , "nf90_close")
    return
end subroutine easyI_string_5d
Subroutine easyO_string_6d(fname, vname, data, shape_total, position, count_lens, dimnames)
    !!! #####################################
    ! This Subroutine aims to write a 6-dimensional  variable into a netcdf dataset
    ! Also, the scalar variable can also be put in an array record in netcdf, 
    ! via setting optional argument "shape_total", "position" and "count_lens"
    !!! #####################################
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    character(*),intent(in) :: data(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)         ! total shape
    integer, intent(in), optional :: position(:)            ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
    character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    integer, allocatable :: shape_data(:)
    integer :: rank_data                    ! ranks of data, e.g., size of shape_data
    integer :: rank_ncv                     ! ranks of output netcdf variable
    integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
    integer, allocatable :: position_(:), count_lens_(:)
    
    character(80), allocatable :: dimnames_(:)
    integer, allocatable :: dimids(:)
    integer :: ncid, vid
        
    integer :: i
    logical :: isExist
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
    if (enc_var_exist(fname, vname)) then
        if (enc_vea .eq. 1) then
            return
        elseif (enc_vea .eq. -1) then
            print *, 'Error in easyO with enc_vea = -1, variable exist!'
            stop 1
        end if
    end if
    ! ============================= prepare for netcdf IO
    ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
    rank_data = size(shape(data))
    ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
    allocate(shape_data(size(shape(data))), source=shape(data))
    if (present(shape_total)) then
        rank_ncv = size(shape_total) + 1
        ! allocate(shape_ncv, source=shape_total)
        shape_ncv = union_arr1d([len(data)], shape_total)
    else
        rank_ncv = rank_data + 1
        ! allocate(shape_ncv, source=shape_data)
        shape_ncv = union_arr1d([len(data)], shape_data)
    end if
    ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
    allocate(dimnames_(rank_ncv))
    allocate(dimids(rank_ncv))
    dimnames_(1) = toString(vname, '.L')
    do i = 2, rank_ncv
        if (present(dimnames)) then
            call assert(size(dimnames)+1 .eq. rank_ncv, 'Error in easyO_string_6d, &
     argument<dimnames> should have the same size as rank_ncv (for string, needs +1)') ! 
            if (len_trim(dimnames(i+1)) .gt. 0) then
                dimnames_(i) = trim(dimnames(i+1))
                cycle
            end if
        end if
        write(dimnames_(i), '(A, ".d", I1)') trim(vname), i-1
    end do
    ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
    if (present(shape_total)) then
        position_ = union_arr1d([1], position)  !# imply allocate
        count_lens_ = union_arr1d([len(data)], count_lens)
    else
        allocate(position_(rank_ncv))
        position_ = 1
        allocate(count_lens_, source=shape_ncv)
    end if
    ! ============================= netcdf core part
    inquire(file = fname, exist = isExist)
    ! ~~~~~~~~~~~~~~ open or create nc
    if (isExist) then
        call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        call check_enc( nf90_redef(ncid), "nf90_redef")
    else
        if (enc_use_nc4) then
            call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
        else
            call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
        end if
    end if
    ! ~~~~~~~~~~~~~~ get or define dimension
    do i = 1, rank_ncv
        if (isExist) then
        if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
        end if
        else
        call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
        end if
    end do
    ! ~~~~~~~~~~~~~~ define variable, or error
    if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
        call check_enc( nf90_def_var(ncid, vname, NF90_CHAR, dimids, vid) , "nf90_def_var")
    end if
    call check_enc( nf90_enddef(ncid) , "nf90_enddef")
    ! ~~~~~~~~~~~~~~ write data
    ! print *, 'start_index = ',start_index
    ! print *, 'count_lens_ = ',count_lens_
    call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
        "in easyO_string_6d, nf90_put_var, "//trim(vname))
    
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    deallocate(shape_data)
    deallocate(shape_ncv)
    deallocate(dimids)
    deallocate(dimnames_)
    deallocate(count_lens_)
    deallocate(position_)
    return
end subroutine easyO_string_6d
Subroutine easyI_string_6d(fname, vname, data, position, count_lens)
    !!! #####################################
    ! This Subroutine aims to read a 6-dimensional  variable from a netcdf dataset
    ! Also, the scalar variable can also be put in an array record in netcdf, 
    ! via setting optional argument "shape_total", "position" and "count_lens"
    !!! #####################################
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    character(*),intent(inout) :: data(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    integer :: rank_ncv                     ! ranks of output netcdf variable
    integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
    integer, allocatable :: position_(:), count_lens_(:)
    
    integer :: ncid, vid
        
    logical :: isExist
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= netcdf core part
    inquire(file = fname, exist = isExist)
    ! ~~~~~~~~~~~~~~ open or create nc
    if (isExist) then
        call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
    else
        print *, "Error, file doesn't exist."
        stop 1
    end if
    ! ~~~~~~~~~~~~~~ get variable
    call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
    ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
    if (present(position)) then
        ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
        position_ = union_arr1d([1], position)
        count_lens_ = union_arr1d([len(data)], count_lens)
        ! ~~~~~~~~~~~~~~ write data
        call check_enc( nf90_get_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyI_string_6d, nf90_get_var, "//trim(vname))
        
        deallocate(position_)
        deallocate(count_lens_)
    else
        call check_enc( nf90_get_var(ncid, vid, data) , & 
        "in easyI_string_6d, nf90_get_var, "//trim(vname))
    end if
    call check_enc( nf90_close(ncid) , "nf90_close")
    return
end subroutine easyI_string_6d
Subroutine easyO_string_7d(fname, vname, data, shape_total, position, count_lens, dimnames)
    !!! #####################################
    ! This Subroutine aims to write a 7-dimensional  variable into a netcdf dataset
    ! Also, the scalar variable can also be put in an array record in netcdf, 
    ! via setting optional argument "shape_total", "position" and "count_lens"
    !!! #####################################
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    character(*),intent(in) :: data(:,:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)         ! total shape
    integer, intent(in), optional :: position(:)            ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
    character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    integer, allocatable :: shape_data(:)
    integer :: rank_data                    ! ranks of data, e.g., size of shape_data
    integer :: rank_ncv                     ! ranks of output netcdf variable
    integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
    integer, allocatable :: position_(:), count_lens_(:)
    
    character(80), allocatable :: dimnames_(:)
    integer, allocatable :: dimids(:)
    integer :: ncid, vid
        
    integer :: i
    logical :: isExist
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
    if (enc_var_exist(fname, vname)) then
        if (enc_vea .eq. 1) then
            return
        elseif (enc_vea .eq. -1) then
            print *, 'Error in easyO with enc_vea = -1, variable exist!'
            stop 1
        end if
    end if
    ! ============================= prepare for netcdf IO
    ! ~~~~~~~~~~~~~~ handle rank & shape for netcdf variable
    rank_data = size(shape(data))
    ! allocate(shape_data(rank_data), source=shape(data))  ! will fail in gfortran
    allocate(shape_data(size(shape(data))), source=shape(data))
    if (present(shape_total)) then
        rank_ncv = size(shape_total) + 1
        ! allocate(shape_ncv, source=shape_total)
        shape_ncv = union_arr1d([len(data)], shape_total)
    else
        rank_ncv = rank_data + 1
        ! allocate(shape_ncv, source=shape_data)
        shape_ncv = union_arr1d([len(data)], shape_data)
    end if
    ! ~~~~~~~~~~~~~~ handle dimensions, e.g., dimnames_ & dimids
    allocate(dimnames_(rank_ncv))
    allocate(dimids(rank_ncv))
    dimnames_(1) = toString(vname, '.L')
    do i = 2, rank_ncv
        if (present(dimnames)) then
            call assert(size(dimnames)+1 .eq. rank_ncv, 'Error in easyO_string_7d, &
     argument<dimnames> should have the same size as rank_ncv (for string, needs +1)') ! 
            if (len_trim(dimnames(i+1)) .gt. 0) then
                dimnames_(i) = trim(dimnames(i+1))
                cycle
            end if
        end if
        write(dimnames_(i), '(A, ".d", I1)') trim(vname), i-1
    end do
    ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
    if (present(shape_total)) then
        position_ = union_arr1d([1], position)  !# imply allocate
        count_lens_ = union_arr1d([len(data)], count_lens)
    else
        allocate(position_(rank_ncv))
        position_ = 1
        allocate(count_lens_, source=shape_ncv)
    end if
    ! ============================= netcdf core part
    inquire(file = fname, exist = isExist)
    ! ~~~~~~~~~~~~~~ open or create nc
    if (isExist) then
        call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
        call check_enc( nf90_redef(ncid), "nf90_redef")
    else
        if (enc_use_nc4) then
            call check_enc( nf90_create(fname, NF90_NETCDF4, ncid) , "in nf90_create nc4, "//fname)
        else
            call check_enc( nf90_create(fname, NF90_CLOBBER, ncid) , "in nf90_create, "//fname)
        end if
    end if
    ! ~~~~~~~~~~~~~~ get or define dimension
    do i = 1, rank_ncv
        if (isExist) then
        if (nf90_inq_dimid(ncid, dimnames_(i), dimids(i)) .ne. nf90_noerr) then
            call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
        end if
        else
        call check_enc(nf90_def_dim(ncid, dimnames_(i), shape_ncv(i), dimids(i)), "nf90_def_dim")
        end if
    end do
    ! ~~~~~~~~~~~~~~ define variable, or error
    if (nf90_inq_varid(ncid, vname, vid) .ne. nf90_noerr) then
        call check_enc( nf90_def_var(ncid, vname, NF90_CHAR, dimids, vid) , "nf90_def_var")
    end if
    call check_enc( nf90_enddef(ncid) , "nf90_enddef")
    ! ~~~~~~~~~~~~~~ write data
    ! print *, 'start_index = ',start_index
    ! print *, 'count_lens_ = ',count_lens_
    call check_enc( nf90_put_var(ncid, vid, data, start=position_, count=count_lens_) , & 
        "in easyO_string_7d, nf90_put_var, "//trim(vname))
    
    call check_enc( nf90_close(ncid) , "nf90_close")
    
    deallocate(shape_data)
    deallocate(shape_ncv)
    deallocate(dimids)
    deallocate(dimnames_)
    deallocate(count_lens_)
    deallocate(position_)
    return
end subroutine easyO_string_7d
Subroutine easyI_string_7d(fname, vname, data, position, count_lens)
    !!! #####################################
    ! This Subroutine aims to read a 7-dimensional  variable from a netcdf dataset
    ! Also, the scalar variable can also be put in an array record in netcdf, 
    ! via setting optional argument "shape_total", "position" and "count_lens"
    !!! #####################################
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    character(*),intent(inout) :: data(:,:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    integer :: rank_ncv                     ! ranks of output netcdf variable
    integer, allocatable :: shape_ncv(:)    ! shape if output netcdf variable
    integer, allocatable :: position_(:), count_lens_(:)
    
    integer :: ncid, vid
        
    logical :: isExist
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= netcdf core part
    inquire(file = fname, exist = isExist)
    ! ~~~~~~~~~~~~~~ open or create nc
    if (isExist) then
        call check_enc( nf90_open(fname, NF90_WRITE, ncid) , "in nf90_open, "//fname)
    else
        print *, "Error, file doesn't exist."
        stop 1
    end if
    ! ~~~~~~~~~~~~~~ get variable
    call check_enc(nf90_inq_varid(ncid, vname, vid), 'nf90_inq_varid for '//trim(vname))
    ! ~~~~~~~~~~~~~~ handle position_ & count_lens_
    if (present(position)) then
        ! call check_enc(nf90_inquire_variable(ncid, vid, ndims=rank_ncv), 'nf90_inquire_variable for '//trim(vname))
        position_ = union_arr1d([1], position)
        count_lens_ = union_arr1d([len(data)], count_lens)
        ! ~~~~~~~~~~~~~~ write data
        call check_enc( nf90_get_var(ncid, vid, data, start=position_, count=count_lens_) , & 
            "in easyI_string_7d, nf90_get_var, "//trim(vname))
        
        deallocate(position_)
        deallocate(count_lens_)
    else
        call check_enc( nf90_get_var(ncid, vid, data) , & 
        "in easyI_string_7d, nf90_get_var, "//trim(vname))
    end if
    call check_enc( nf90_close(ncid) , "nf90_close")
    return
end subroutine easyI_string_7d
    Subroutine easyO_logical_scalar(fname, vname, val, shape_total, position, dimnames)
        ! almost Duplicated with easyO_int
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,intent(in) :: val
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
        integer :: b, i
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        b = 0
        if(val) b = 1
        if (present(shape_total)) then
            if (present(dimnames)) then
                call easyO_int4_scalar(fname, vname, b, shape_total, position, dimnames)
            else
                call easyO_int4_scalar(fname, vname, b, shape_total, position)
            end if
        else
            call easyO_int4_scalar(fname, vname, b)
        end if
        return
    end subroutine easyO_logical_scalar
    Subroutine easyI_logical_scalar(fname, vname, val, position)
        ! almost Duplicated with easyO_int
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,intent(out) :: val
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
        integer :: b, i
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        if (present(position)) then
            call easyI_int4_scalar(fname, vname, b, position)
        else
            call easyI_int4_scalar(fname, vname, b)
        end if
        val = .false.
        if(b .eq. 1) val = .true.
        return
    end subroutine easyI_logical_scalar
    Subroutine easyO_logical_1d(fname, vname, val, shape_total, position, count_lens, dimnames)
        ! almost Duplicated with easyO_int
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,intent(in) :: val(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
        integer, allocatable :: b(:)
        integer :: i
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        allocate(b(size(val)))
        b = 0
        where(val) b = 1
        if (present(shape_total)) then
            if (present(dimnames)) then
                call easyO_int4_1d(fname, vname, b, shape_total, position, count_lens, dimnames)
            else
                call easyO_int4_1d(fname, vname, b, shape_total, position, count_lens)
            end if
        else
            if (present(dimnames)) then
                call easyO_int4_1d(fname, vname, b, dimnames=dimnames)
            else
                call easyO_int4_1d(fname, vname, b)
            end if
        end if
        
        deallocate(b)
        return
    end subroutine easyO_logical_1d
    Subroutine easyI_logical_1d(fname, vname, val, position, count_lens)
        ! almost Duplicated with easyO_int
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,intent(out) :: val(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
        integer, allocatable :: b(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        allocate(b(size(val)))
        if (present(position)) then
            call easyI_int4_1d(fname, vname, b, position, count_lens)
        else
            call easyI_int4_1d(fname, vname, b)
        end if
        val = .false.
        where(b .eq. 1) val = .true.
        deallocate(b)
        return
    end subroutine easyI_logical_1d
    Subroutine easyO_logical_2d(fname, vname, val, shape_total, position, count_lens, dimnames)
        ! almost Duplicated with easyO_int
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,intent(in) :: val(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
        integer, allocatable :: b(:,:)
        integer :: i
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        allocate(b(size(val(:,1)), size(val(1,:))))
        b = 0
        where(val) b = 1
        if (present(shape_total)) then
            if (present(dimnames)) then
                call easyO_int4_2d(fname, vname, b, shape_total, position, count_lens, dimnames)
            else
                call easyO_int4_2d(fname, vname, b, shape_total, position, count_lens)
            end if
        else
            if (present(dimnames)) then
                call easyO_int4_2d(fname, vname, b, dimnames=dimnames)
            else
                call easyO_int4_2d(fname, vname, b)
            end if
        end if
        
        deallocate(b)
        return
    end subroutine easyO_logical_2d
    Subroutine easyI_logical_2d(fname, vname, val, position, count_lens)
        ! almost Duplicated with easyO_int
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,intent(out) :: val(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
        integer, allocatable :: b(:,:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        allocate(b(size(val(:,1)), size(val(1,:))))
        if (present(position)) then
            call easyI_int4_2d(fname, vname, b, position, count_lens)
        else
            call easyI_int4_2d(fname, vname, b)
        end if
        val = .false.
        where(b .eq. 1) val = .true.
        deallocate(b)
        return
    end subroutine easyI_logical_2d
    Subroutine easyO_logical_3d(fname, vname, val, shape_total, position, count_lens, dimnames)
        ! almost Duplicated with easyO_int
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,intent(in) :: val(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
        integer, allocatable :: b(:,:,:)
        integer :: i
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        allocate(b(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))))
        b = 0
        where(val) b = 1
        if (present(shape_total)) then
            if (present(dimnames)) then
                call easyO_int4_3d(fname, vname, b, shape_total, position, count_lens, dimnames)
            else
                call easyO_int4_3d(fname, vname, b, shape_total, position, count_lens)
            end if
        else
            if (present(dimnames)) then
                call easyO_int4_3d(fname, vname, b, dimnames=dimnames)
            else
                call easyO_int4_3d(fname, vname, b)
            end if
        end if
        
        deallocate(b)
        return
    end subroutine easyO_logical_3d
    Subroutine easyI_logical_3d(fname, vname, val, position, count_lens)
        ! almost Duplicated with easyO_int
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,intent(out) :: val(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
        integer, allocatable :: b(:,:,:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        allocate(b(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))))
        if (present(position)) then
            call easyI_int4_3d(fname, vname, b, position, count_lens)
        else
            call easyI_int4_3d(fname, vname, b)
        end if
        val = .false.
        where(b .eq. 1) val = .true.
        deallocate(b)
        return
    end subroutine easyI_logical_3d
    Subroutine easyO_logical_4d(fname, vname, val, shape_total, position, count_lens, dimnames)
        ! almost Duplicated with easyO_int
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,intent(in) :: val(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
        integer, allocatable :: b(:,:,:,:)
        integer :: i
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        allocate(b(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))))
        b = 0
        where(val) b = 1
        if (present(shape_total)) then
            if (present(dimnames)) then
                call easyO_int4_4d(fname, vname, b, shape_total, position, count_lens, dimnames)
            else
                call easyO_int4_4d(fname, vname, b, shape_total, position, count_lens)
            end if
        else
            if (present(dimnames)) then
                call easyO_int4_4d(fname, vname, b, dimnames=dimnames)
            else
                call easyO_int4_4d(fname, vname, b)
            end if
        end if
        
        deallocate(b)
        return
    end subroutine easyO_logical_4d
    Subroutine easyI_logical_4d(fname, vname, val, position, count_lens)
        ! almost Duplicated with easyO_int
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,intent(out) :: val(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
        integer, allocatable :: b(:,:,:,:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        allocate(b(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))))
        if (present(position)) then
            call easyI_int4_4d(fname, vname, b, position, count_lens)
        else
            call easyI_int4_4d(fname, vname, b)
        end if
        val = .false.
        where(b .eq. 1) val = .true.
        deallocate(b)
        return
    end subroutine easyI_logical_4d
    Subroutine easyO_logical_5d(fname, vname, val, shape_total, position, count_lens, dimnames)
        ! almost Duplicated with easyO_int
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,intent(in) :: val(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
        integer, allocatable :: b(:,:,:,:,:)
        integer :: i
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        allocate(b(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))))
        b = 0
        where(val) b = 1
        if (present(shape_total)) then
            if (present(dimnames)) then
                call easyO_int4_5d(fname, vname, b, shape_total, position, count_lens, dimnames)
            else
                call easyO_int4_5d(fname, vname, b, shape_total, position, count_lens)
            end if
        else
            if (present(dimnames)) then
                call easyO_int4_5d(fname, vname, b, dimnames=dimnames)
            else
                call easyO_int4_5d(fname, vname, b)
            end if
        end if
        
        deallocate(b)
        return
    end subroutine easyO_logical_5d
    Subroutine easyI_logical_5d(fname, vname, val, position, count_lens)
        ! almost Duplicated with easyO_int
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,intent(out) :: val(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
        integer, allocatable :: b(:,:,:,:,:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        allocate(b(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))))
        if (present(position)) then
            call easyI_int4_5d(fname, vname, b, position, count_lens)
        else
            call easyI_int4_5d(fname, vname, b)
        end if
        val = .false.
        where(b .eq. 1) val = .true.
        deallocate(b)
        return
    end subroutine easyI_logical_5d
    Subroutine easyO_logical_6d(fname, vname, val, shape_total, position, count_lens, dimnames)
        ! almost Duplicated with easyO_int
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,intent(in) :: val(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
        integer, allocatable :: b(:,:,:,:,:,:)
        integer :: i
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        allocate(b(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1, &
    1,1,:,1)), size(val(1,1,1,1,1,:)))) ! 
        b = 0
        where(val) b = 1
        if (present(shape_total)) then
            if (present(dimnames)) then
                call easyO_int4_6d(fname, vname, b, shape_total, position, count_lens, dimnames)
            else
                call easyO_int4_6d(fname, vname, b, shape_total, position, count_lens)
            end if
        else
            if (present(dimnames)) then
                call easyO_int4_6d(fname, vname, b, dimnames=dimnames)
            else
                call easyO_int4_6d(fname, vname, b)
            end if
        end if
        
        deallocate(b)
        return
    end subroutine easyO_logical_6d
    Subroutine easyI_logical_6d(fname, vname, val, position, count_lens)
        ! almost Duplicated with easyO_int
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,intent(out) :: val(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
        integer, allocatable :: b(:,:,:,:,:,:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        allocate(b(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1, &
    1,1,:,1)), size(val(1,1,1,1,1,:)))) ! 
        if (present(position)) then
            call easyI_int4_6d(fname, vname, b, position, count_lens)
        else
            call easyI_int4_6d(fname, vname, b)
        end if
        val = .false.
        where(b .eq. 1) val = .true.
        deallocate(b)
        return
    end subroutine easyI_logical_6d
    Subroutine easyO_logical_7d(fname, vname, val, shape_total, position, count_lens, dimnames)
        ! almost Duplicated with easyO_int
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,intent(in) :: val(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: position(:)            ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
        integer, allocatable :: b(:,:,:,:,:,:,:)
        integer :: i
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        allocate(b(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size( &
    val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:)))) ! 
        b = 0
        where(val) b = 1
        if (present(shape_total)) then
            if (present(dimnames)) then
                call easyO_int4_7d(fname, vname, b, shape_total, position, count_lens, dimnames)
            else
                call easyO_int4_7d(fname, vname, b, shape_total, position, count_lens)
            end if
        else
            if (present(dimnames)) then
                call easyO_int4_7d(fname, vname, b, dimnames=dimnames)
            else
                call easyO_int4_7d(fname, vname, b)
            end if
        end if
        
        deallocate(b)
        return
    end subroutine easyO_logical_7d
    Subroutine easyI_logical_7d(fname, vname, val, position, count_lens)
        ! almost Duplicated with easyO_int
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,intent(out) :: val(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
        integer, allocatable :: b(:,:,:,:,:,:,:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        allocate(b(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size( &
    val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:)))) ! 
        if (present(position)) then
            call easyI_int4_7d(fname, vname, b, position, count_lens)
        else
            call easyI_int4_7d(fname, vname, b)
        end if
        val = .false.
        where(b .eq. 1) val = .true.
        deallocate(b)
        return
    end subroutine easyI_logical_7d
Subroutine easyO_complex4_scalar(fname, vname, val, shape_total, position, dimnames)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    Complex(kind=4),intent(in) :: val
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)         ! total shape
    integer, intent(in), optional :: position(:)            ! `start` in netcdf
    character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=4) :: val_re, val_im
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
    if (enc_var_exist(fname, vname)) then
        if (enc_vea .eq. 1) then
            return
        elseif (enc_vea .eq. -1) then
            print *, 'Error in easyO with enc_vea = -1, variable exist!'
            stop 1
        end if
    end if
    val_re = real(val)
    val_im = aimag(val)
    if (present(shape_total)) then
        if (present(dimnames)) then
            call easyO_real4_scalar(fname, trim(vname)//'.real', val_re, shape_total, position, dimnames)
            call easyO_real4_scalar(fname, trim(vname)//'.imag', val_im, shape_total, position, dimnames)
        else
            call easyO_real4_scalar(fname, trim(vname)//'.real', val_re, shape_total, position)
            call easyO_real4_scalar(fname, trim(vname)//'.imag', val_im, shape_total, position)
        end if
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
Subroutine easyO_complex8_scalar(fname, vname, val, shape_total, position, dimnames)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    Complex(kind=8),intent(in) :: val
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)      ! total shape
    integer, intent(in), optional :: position(:)         ! `start` in netcdf
    character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=8) :: val_re, val_im
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
    if (enc_var_exist(fname, vname)) then
        if (enc_vea .eq. 1) then
            return
        elseif (enc_vea .eq. -1) then
            print *, 'Error in easyO with enc_vea = -1, variable exist!'
            stop 1
        end if
    end if
    val_re = real(val)
    val_im = aimag(val)
    if (present(shape_total)) then
        if (present(dimnames)) then
            call easyO_real8_scalar(fname, trim(vname)//'.real', val_re, shape_total, position, dimnames)
            call easyO_real8_scalar(fname, trim(vname)//'.imag', val_im, shape_total, position, dimnames)
        else
            call easyO_real8_scalar(fname, trim(vname)//'.real', val_re, shape_total, position)
            call easyO_real8_scalar(fname, trim(vname)//'.imag', val_im, shape_total, position)
        end if
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
Subroutine easyO_complex4_1d(fname, vname, val, shape_total, position, count_lens, dimnames)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=4),intent(in) :: val(:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)         ! total shape
    integer, intent(in), optional :: position(:)            ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
    character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=4) ,allocatable :: val_re(:), val_im(:)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
    if (enc_var_exist(fname, vname)) then
        if (enc_vea .eq. 1) then
            return
        elseif (enc_vea .eq. -1) then
            print *, 'Error in easyO with enc_vea = -1, variable exist!'
            stop 1
        end if
    end if
    allocate(val_re(size(val)))
    allocate(val_im(size(val)))
    val_re = real(val)
    val_im = aimag(val)
    if (present(shape_total)) then
        if (present(dimnames)) then
            call easyO_real4_1d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens, dimnames)
            call easyO_real4_1d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens, dimnames)
        else
            call easyO_real4_1d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
            call easyO_real4_1d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
        end if       
    else
        if (present(dimnames)) then
            call easyO_real4_1d(fname, trim(vname)//'.real', val_re, dimnames=dimnames)
            call easyO_real4_1d(fname, trim(vname)//'.imag', val_im, dimnames=dimnames)
        else
            call easyO_real4_1d(fname, trim(vname)//'.real', val_re)
            call easyO_real4_1d(fname, trim(vname)//'.imag', val_im)
        end if
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
Subroutine easyO_complex8_1d(fname, vname, val, shape_total, position, count_lens, dimnames)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=8),intent(in) :: val(:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)         ! total shape
    integer, intent(in), optional :: position(:)            ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
    character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=8) ,allocatable :: val_re(:), val_im(:)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
    if (enc_var_exist(fname, vname)) then
        if (enc_vea .eq. 1) then
            return
        elseif (enc_vea .eq. -1) then
            print *, 'Error in easyO with enc_vea = -1, variable exist!'
            stop 1
        end if
    end if
    allocate(val_re(size(val)))
    allocate(val_im(size(val)))
    val_re = real(val)
    val_im = aimag(val)
    if (present(shape_total)) then
        if (present(dimnames)) then
            call easyO_real8_1d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens, dimnames)
            call easyO_real8_1d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens, dimnames)
        else
            call easyO_real8_1d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
            call easyO_real8_1d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
        end if       
    else
        if (present(dimnames)) then
            call easyO_real8_1d(fname, trim(vname)//'.real', val_re, dimnames=dimnames)
            call easyO_real8_1d(fname, trim(vname)//'.imag', val_im, dimnames=dimnames)
        else
            call easyO_real8_1d(fname, trim(vname)//'.real', val_re)
            call easyO_real8_1d(fname, trim(vname)//'.imag', val_im)
        end if
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
    val = cmplx(val_re, val_im, 8)
    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyI_complex8_1d
Subroutine easyO_complex4_2d(fname, vname, val, shape_total, position, count_lens, dimnames)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=4),intent(in) :: val(:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)         ! total shape
    integer, intent(in), optional :: position(:)            ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
    character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=4) ,allocatable :: val_re(:,:), val_im(:,:)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
    if (enc_var_exist(fname, vname)) then
        if (enc_vea .eq. 1) then
            return
        elseif (enc_vea .eq. -1) then
            print *, 'Error in easyO with enc_vea = -1, variable exist!'
            stop 1
        end if
    end if
    allocate(val_re(size(val(:,1)), size(val(1,:))))
    allocate(val_im(size(val(:,1)), size(val(1,:))))
    val_re = real(val)
    val_im = aimag(val)
    if (present(shape_total)) then
        if (present(dimnames)) then
            call easyO_real4_2d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens, dimnames)
            call easyO_real4_2d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens, dimnames)
        else
            call easyO_real4_2d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
            call easyO_real4_2d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
        end if       
    else
        if (present(dimnames)) then
            call easyO_real4_2d(fname, trim(vname)//'.real', val_re, dimnames=dimnames)
            call easyO_real4_2d(fname, trim(vname)//'.imag', val_im, dimnames=dimnames)
        else
            call easyO_real4_2d(fname, trim(vname)//'.real', val_re)
            call easyO_real4_2d(fname, trim(vname)//'.imag', val_im)
        end if
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
Subroutine easyO_complex8_2d(fname, vname, val, shape_total, position, count_lens, dimnames)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=8),intent(in) :: val(:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)         ! total shape
    integer, intent(in), optional :: position(:)            ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
    character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=8) ,allocatable :: val_re(:,:), val_im(:,:)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
    if (enc_var_exist(fname, vname)) then
        if (enc_vea .eq. 1) then
            return
        elseif (enc_vea .eq. -1) then
            print *, 'Error in easyO with enc_vea = -1, variable exist!'
            stop 1
        end if
    end if
    allocate(val_re(size(val(:,1)), size(val(1,:))))
    allocate(val_im(size(val(:,1)), size(val(1,:))))
    val_re = real(val)
    val_im = aimag(val)
    if (present(shape_total)) then
        if (present(dimnames)) then
            call easyO_real8_2d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens, dimnames)
            call easyO_real8_2d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens, dimnames)
        else
            call easyO_real8_2d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
            call easyO_real8_2d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
        end if       
    else
        if (present(dimnames)) then
            call easyO_real8_2d(fname, trim(vname)//'.real', val_re, dimnames=dimnames)
            call easyO_real8_2d(fname, trim(vname)//'.imag', val_im, dimnames=dimnames)
        else
            call easyO_real8_2d(fname, trim(vname)//'.real', val_re)
            call easyO_real8_2d(fname, trim(vname)//'.imag', val_im)
        end if
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
    val = cmplx(val_re, val_im, 8)
    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyI_complex8_2d
Subroutine easyO_complex4_3d(fname, vname, val, shape_total, position, count_lens, dimnames)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=4),intent(in) :: val(:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)         ! total shape
    integer, intent(in), optional :: position(:)            ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
    character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=4) ,allocatable :: val_re(:,:,:), val_im(:,:,:)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
    if (enc_var_exist(fname, vname)) then
        if (enc_vea .eq. 1) then
            return
        elseif (enc_vea .eq. -1) then
            print *, 'Error in easyO with enc_vea = -1, variable exist!'
            stop 1
        end if
    end if
    allocate(val_re(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))))
    allocate(val_im(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))))
    val_re = real(val)
    val_im = aimag(val)
    if (present(shape_total)) then
        if (present(dimnames)) then
            call easyO_real4_3d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens, dimnames)
            call easyO_real4_3d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens, dimnames)
        else
            call easyO_real4_3d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
            call easyO_real4_3d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
        end if       
    else
        if (present(dimnames)) then
            call easyO_real4_3d(fname, trim(vname)//'.real', val_re, dimnames=dimnames)
            call easyO_real4_3d(fname, trim(vname)//'.imag', val_im, dimnames=dimnames)
        else
            call easyO_real4_3d(fname, trim(vname)//'.real', val_re)
            call easyO_real4_3d(fname, trim(vname)//'.imag', val_im)
        end if
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
Subroutine easyO_complex8_3d(fname, vname, val, shape_total, position, count_lens, dimnames)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=8),intent(in) :: val(:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)         ! total shape
    integer, intent(in), optional :: position(:)            ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
    character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=8) ,allocatable :: val_re(:,:,:), val_im(:,:,:)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
    if (enc_var_exist(fname, vname)) then
        if (enc_vea .eq. 1) then
            return
        elseif (enc_vea .eq. -1) then
            print *, 'Error in easyO with enc_vea = -1, variable exist!'
            stop 1
        end if
    end if
    allocate(val_re(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))))
    allocate(val_im(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))))
    val_re = real(val)
    val_im = aimag(val)
    if (present(shape_total)) then
        if (present(dimnames)) then
            call easyO_real8_3d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens, dimnames)
            call easyO_real8_3d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens, dimnames)
        else
            call easyO_real8_3d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
            call easyO_real8_3d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
        end if       
    else
        if (present(dimnames)) then
            call easyO_real8_3d(fname, trim(vname)//'.real', val_re, dimnames=dimnames)
            call easyO_real8_3d(fname, trim(vname)//'.imag', val_im, dimnames=dimnames)
        else
            call easyO_real8_3d(fname, trim(vname)//'.real', val_re)
            call easyO_real8_3d(fname, trim(vname)//'.imag', val_im)
        end if
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
    val = cmplx(val_re, val_im, 8)
    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyI_complex8_3d
Subroutine easyO_complex4_4d(fname, vname, val, shape_total, position, count_lens, dimnames)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=4),intent(in) :: val(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)         ! total shape
    integer, intent(in), optional :: position(:)            ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
    character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=4) ,allocatable :: val_re(:,:,:,:), val_im(:,:,:,:)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
    if (enc_var_exist(fname, vname)) then
        if (enc_vea .eq. 1) then
            return
        elseif (enc_vea .eq. -1) then
            print *, 'Error in easyO with enc_vea = -1, variable exist!'
            stop 1
        end if
    end if
    allocate(val_re(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))))
    allocate(val_im(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))))
    val_re = real(val)
    val_im = aimag(val)
    if (present(shape_total)) then
        if (present(dimnames)) then
            call easyO_real4_4d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens, dimnames)
            call easyO_real4_4d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens, dimnames)
        else
            call easyO_real4_4d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
            call easyO_real4_4d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
        end if       
    else
        if (present(dimnames)) then
            call easyO_real4_4d(fname, trim(vname)//'.real', val_re, dimnames=dimnames)
            call easyO_real4_4d(fname, trim(vname)//'.imag', val_im, dimnames=dimnames)
        else
            call easyO_real4_4d(fname, trim(vname)//'.real', val_re)
            call easyO_real4_4d(fname, trim(vname)//'.imag', val_im)
        end if
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
Subroutine easyO_complex8_4d(fname, vname, val, shape_total, position, count_lens, dimnames)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=8),intent(in) :: val(:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)         ! total shape
    integer, intent(in), optional :: position(:)            ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
    character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=8) ,allocatable :: val_re(:,:,:,:), val_im(:,:,:,:)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
    if (enc_var_exist(fname, vname)) then
        if (enc_vea .eq. 1) then
            return
        elseif (enc_vea .eq. -1) then
            print *, 'Error in easyO with enc_vea = -1, variable exist!'
            stop 1
        end if
    end if
    allocate(val_re(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))))
    allocate(val_im(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))))
    val_re = real(val)
    val_im = aimag(val)
    if (present(shape_total)) then
        if (present(dimnames)) then
            call easyO_real8_4d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens, dimnames)
            call easyO_real8_4d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens, dimnames)
        else
            call easyO_real8_4d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
            call easyO_real8_4d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
        end if       
    else
        if (present(dimnames)) then
            call easyO_real8_4d(fname, trim(vname)//'.real', val_re, dimnames=dimnames)
            call easyO_real8_4d(fname, trim(vname)//'.imag', val_im, dimnames=dimnames)
        else
            call easyO_real8_4d(fname, trim(vname)//'.real', val_re)
            call easyO_real8_4d(fname, trim(vname)//'.imag', val_im)
        end if
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
    val = cmplx(val_re, val_im, 8)
    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyI_complex8_4d
Subroutine easyO_complex4_5d(fname, vname, val, shape_total, position, count_lens, dimnames)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=4),intent(in) :: val(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)         ! total shape
    integer, intent(in), optional :: position(:)            ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
    character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=4) ,allocatable :: val_re(:,:,:,:,:), val_im(:,:,:,:,:)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
    if (enc_var_exist(fname, vname)) then
        if (enc_vea .eq. 1) then
            return
        elseif (enc_vea .eq. -1) then
            print *, 'Error in easyO with enc_vea = -1, variable exist!'
            stop 1
        end if
    end if
    allocate(val_re(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))))
    allocate(val_im(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))))
    val_re = real(val)
    val_im = aimag(val)
    if (present(shape_total)) then
        if (present(dimnames)) then
            call easyO_real4_5d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens, dimnames)
            call easyO_real4_5d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens, dimnames)
        else
            call easyO_real4_5d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
            call easyO_real4_5d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
        end if       
    else
        if (present(dimnames)) then
            call easyO_real4_5d(fname, trim(vname)//'.real', val_re, dimnames=dimnames)
            call easyO_real4_5d(fname, trim(vname)//'.imag', val_im, dimnames=dimnames)
        else
            call easyO_real4_5d(fname, trim(vname)//'.real', val_re)
            call easyO_real4_5d(fname, trim(vname)//'.imag', val_im)
        end if
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
Subroutine easyO_complex8_5d(fname, vname, val, shape_total, position, count_lens, dimnames)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=8),intent(in) :: val(:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)         ! total shape
    integer, intent(in), optional :: position(:)            ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
    character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=8) ,allocatable :: val_re(:,:,:,:,:), val_im(:,:,:,:,:)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
    if (enc_var_exist(fname, vname)) then
        if (enc_vea .eq. 1) then
            return
        elseif (enc_vea .eq. -1) then
            print *, 'Error in easyO with enc_vea = -1, variable exist!'
            stop 1
        end if
    end if
    allocate(val_re(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))))
    allocate(val_im(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))))
    val_re = real(val)
    val_im = aimag(val)
    if (present(shape_total)) then
        if (present(dimnames)) then
            call easyO_real8_5d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens, dimnames)
            call easyO_real8_5d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens, dimnames)
        else
            call easyO_real8_5d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
            call easyO_real8_5d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
        end if       
    else
        if (present(dimnames)) then
            call easyO_real8_5d(fname, trim(vname)//'.real', val_re, dimnames=dimnames)
            call easyO_real8_5d(fname, trim(vname)//'.imag', val_im, dimnames=dimnames)
        else
            call easyO_real8_5d(fname, trim(vname)//'.real', val_re)
            call easyO_real8_5d(fname, trim(vname)//'.imag', val_im)
        end if
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
    val = cmplx(val_re, val_im, 8)
    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyI_complex8_5d
Subroutine easyO_complex4_6d(fname, vname, val, shape_total, position, count_lens, dimnames)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=4),intent(in) :: val(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)         ! total shape
    integer, intent(in), optional :: position(:)            ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
    character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=4) ,allocatable :: val_re(:,:,:,:,:,:), val_im(:,:,:,:,:,:)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
    if (enc_var_exist(fname, vname)) then
        if (enc_vea .eq. 1) then
            return
        elseif (enc_vea .eq. -1) then
            print *, 'Error in easyO with enc_vea = -1, variable exist!'
            stop 1
        end if
    end if
    allocate(val_re(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1, &
    1,1,1,:,1)), size(val(1,1,1,1,1,:)))) ! 
    allocate(val_im(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1, &
    1,1,1,:,1)), size(val(1,1,1,1,1,:)))) ! 
    val_re = real(val)
    val_im = aimag(val)
    if (present(shape_total)) then
        if (present(dimnames)) then
            call easyO_real4_6d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens, dimnames)
            call easyO_real4_6d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens, dimnames)
        else
            call easyO_real4_6d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
            call easyO_real4_6d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
        end if       
    else
        if (present(dimnames)) then
            call easyO_real4_6d(fname, trim(vname)//'.real', val_re, dimnames=dimnames)
            call easyO_real4_6d(fname, trim(vname)//'.imag', val_im, dimnames=dimnames)
        else
            call easyO_real4_6d(fname, trim(vname)//'.real', val_re)
            call easyO_real4_6d(fname, trim(vname)//'.imag', val_im)
        end if
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
    allocate(val_re(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1, &
    1,1,1,:,1)), size(val(1,1,1,1,1,:)))) ! 
    allocate(val_im(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1, &
    1,1,1,:,1)), size(val(1,1,1,1,1,:)))) ! 
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
Subroutine easyO_complex8_6d(fname, vname, val, shape_total, position, count_lens, dimnames)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=8),intent(in) :: val(:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)         ! total shape
    integer, intent(in), optional :: position(:)            ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
    character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=8) ,allocatable :: val_re(:,:,:,:,:,:), val_im(:,:,:,:,:,:)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
    if (enc_var_exist(fname, vname)) then
        if (enc_vea .eq. 1) then
            return
        elseif (enc_vea .eq. -1) then
            print *, 'Error in easyO with enc_vea = -1, variable exist!'
            stop 1
        end if
    end if
    allocate(val_re(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1, &
    1,1,1,:,1)), size(val(1,1,1,1,1,:)))) ! 
    allocate(val_im(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1, &
    1,1,1,:,1)), size(val(1,1,1,1,1,:)))) ! 
    val_re = real(val)
    val_im = aimag(val)
    if (present(shape_total)) then
        if (present(dimnames)) then
            call easyO_real8_6d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens, dimnames)
            call easyO_real8_6d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens, dimnames)
        else
            call easyO_real8_6d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
            call easyO_real8_6d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
        end if       
    else
        if (present(dimnames)) then
            call easyO_real8_6d(fname, trim(vname)//'.real', val_re, dimnames=dimnames)
            call easyO_real8_6d(fname, trim(vname)//'.imag', val_im, dimnames=dimnames)
        else
            call easyO_real8_6d(fname, trim(vname)//'.real', val_re)
            call easyO_real8_6d(fname, trim(vname)//'.imag', val_im)
        end if
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
    allocate(val_re(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1, &
    1,1,1,:,1)), size(val(1,1,1,1,1,:)))) ! 
    allocate(val_im(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1, &
    1,1,1,:,1)), size(val(1,1,1,1,1,:)))) ! 
    if (present(count_lens)) then
        call easyI_real8_6d(fname, trim(vname)//'.real', val_re, position, count_lens)
        call easyI_real8_6d(fname, trim(vname)//'.imag', val_im, position, count_lens)
    else
        call easyI_real8_6d(fname, trim(vname)//'.real', val_re)
        call easyI_real8_6d(fname, trim(vname)//'.imag', val_im)
    end if
    val = cmplx(val_re, val_im, 8)
    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyI_complex8_6d
Subroutine easyO_complex4_7d(fname, vname, val, shape_total, position, count_lens, dimnames)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=4),intent(in) :: val(:,:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)         ! total shape
    integer, intent(in), optional :: position(:)            ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
    character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=4) ,allocatable :: val_re(:,:,:,:,:,:,:), val_im(:,:,:,:,:,:,:)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
    if (enc_var_exist(fname, vname)) then
        if (enc_vea .eq. 1) then
            return
        elseif (enc_vea .eq. -1) then
            print *, 'Error in easyO with enc_vea = -1, variable exist!'
            stop 1
        end if
    end if
    allocate(val_re(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), &
     size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:)))) ! 
    allocate(val_im(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), &
     size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:)))) ! 
    val_re = real(val)
    val_im = aimag(val)
    if (present(shape_total)) then
        if (present(dimnames)) then
            call easyO_real4_7d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens, dimnames)
            call easyO_real4_7d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens, dimnames)
        else
            call easyO_real4_7d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
            call easyO_real4_7d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
        end if       
    else
        if (present(dimnames)) then
            call easyO_real4_7d(fname, trim(vname)//'.real', val_re, dimnames=dimnames)
            call easyO_real4_7d(fname, trim(vname)//'.imag', val_im, dimnames=dimnames)
        else
            call easyO_real4_7d(fname, trim(vname)//'.real', val_re)
            call easyO_real4_7d(fname, trim(vname)//'.imag', val_im)
        end if
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
    allocate(val_re(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), &
     size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:)))) ! 
    allocate(val_im(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), &
     size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:)))) ! 
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
Subroutine easyO_complex8_7d(fname, vname, val, shape_total, position, count_lens, dimnames)
    ! almost Duplicated with easyO_int
    implicit none
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    complex(kind=8),intent(in) :: val(:,:,:,:,:,:,:)
    character(*),intent(in) :: fname, vname
    integer, intent(in), optional :: shape_total(:)         ! total shape
    integer, intent(in), optional :: position(:)            ! `start` in netcdf
    integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
    character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    real(kind=8) ,allocatable :: val_re(:,:,:,:,:,:,:), val_im(:,:,:,:,:,:,:)
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
    if (enc_var_exist(fname, vname)) then
        if (enc_vea .eq. 1) then
            return
        elseif (enc_vea .eq. -1) then
            print *, 'Error in easyO with enc_vea = -1, variable exist!'
            stop 1
        end if
    end if
    allocate(val_re(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), &
     size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:)))) ! 
    allocate(val_im(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), &
     size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:)))) ! 
    val_re = real(val)
    val_im = aimag(val)
    if (present(shape_total)) then
        if (present(dimnames)) then
            call easyO_real8_7d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens, dimnames)
            call easyO_real8_7d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens, dimnames)
        else
            call easyO_real8_7d(fname, trim(vname)//'.real', val_re, shape_total, position, count_lens)
            call easyO_real8_7d(fname, trim(vname)//'.imag', val_im, shape_total, position, count_lens)
        end if       
    else
        if (present(dimnames)) then
            call easyO_real8_7d(fname, trim(vname)//'.real', val_re, dimnames=dimnames)
            call easyO_real8_7d(fname, trim(vname)//'.imag', val_im, dimnames=dimnames)
        else
            call easyO_real8_7d(fname, trim(vname)//'.real', val_re)
            call easyO_real8_7d(fname, trim(vname)//'.imag', val_im)
        end if
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
    allocate(val_re(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), &
     size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:)))) ! 
    allocate(val_im(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), &
     size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:)))) ! 
    if (present(count_lens)) then
        call easyI_real8_7d(fname, trim(vname)//'.real', val_re, position, count_lens)
        call easyI_real8_7d(fname, trim(vname)//'.imag', val_im, position, count_lens)
    else
        call easyI_real8_7d(fname, trim(vname)//'.real', val_re)
        call easyI_real8_7d(fname, trim(vname)//'.imag', val_im)
    end if
    val = cmplx(val_re, val_im, 8)
    deallocate(val_re)
    deallocate(val_im)
    return
end subroutine easyI_complex8_7d
! *********************************************************
! easyIO for n-dimensional generic numeric data type :
!   int4. int8, real4, real8
! *********************************************************
    Subroutine easyOA_int4_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),allocatable,intent(in) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int4_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int4_1d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int4_1d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int4_1d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_int4_1d
    Subroutine easyIA_int4_1d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),allocatable,intent(inout) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int4_1d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int4_1d(fname, vname, data)
        end if
    End Subroutine easyIA_int4_1d
    Subroutine easyOA_int8_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),allocatable,intent(in) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int8_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int8_1d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int8_1d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int8_1d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_int8_1d
    Subroutine easyIA_int8_1d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),allocatable,intent(inout) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int8_1d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int8_1d(fname, vname, data)
        end if
    End Subroutine easyIA_int8_1d
    Subroutine easyOA_real4_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),allocatable,intent(in) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real4_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real4_1d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real4_1d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real4_1d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_real4_1d
    Subroutine easyIA_real4_1d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),allocatable,intent(inout) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real4_1d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real4_1d(fname, vname, data)
        end if
    End Subroutine easyIA_real4_1d
    Subroutine easyOA_real8_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),allocatable,intent(in) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real8_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real8_1d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real8_1d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real8_1d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_real8_1d
    Subroutine easyIA_real8_1d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),allocatable,intent(inout) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real8_1d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real8_1d(fname, vname, data)
        end if
    End Subroutine easyIA_real8_1d
    Subroutine easyOA_string_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),allocatable,intent(in) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_string_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_string_1d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_string_1d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_string_1d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_string_1d
    Subroutine easyIA_string_1d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),allocatable,intent(inout) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_string_1d(fname, vname, data, positioin, count_lens)
        else
            call easyI_string_1d(fname, vname, data)
        end if
    End Subroutine easyIA_string_1d
    Subroutine easyOA_logical_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,allocatable,intent(in) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_logical_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_logical_1d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_logical_1d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_logical_1d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_logical_1d
    Subroutine easyIA_logical_1d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,allocatable,intent(inout) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_logical_1d(fname, vname, data, positioin, count_lens)
        else
            call easyI_logical_1d(fname, vname, data)
        end if
    End Subroutine easyIA_logical_1d
    Subroutine easyOA_complex4_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),allocatable,intent(in) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex4_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex4_1d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex4_1d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex4_1d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_complex4_1d
    Subroutine easyIA_complex4_1d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),allocatable,intent(inout) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex4_1d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex4_1d(fname, vname, data)
        end if
    End Subroutine easyIA_complex4_1d
    Subroutine easyOA_complex8_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),allocatable,intent(in) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex8_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex8_1d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex8_1d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex8_1d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_complex8_1d
    Subroutine easyIA_complex8_1d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),allocatable,intent(inout) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex8_1d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex8_1d(fname, vname, data)
        end if
    End Subroutine easyIA_complex8_1d
    Subroutine easyOA_int4_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),allocatable,intent(in) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int4_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int4_2d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int4_2d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int4_2d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_int4_2d
    Subroutine easyIA_int4_2d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),allocatable,intent(inout) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int4_2d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int4_2d(fname, vname, data)
        end if
    End Subroutine easyIA_int4_2d
    Subroutine easyOA_int8_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),allocatable,intent(in) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int8_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int8_2d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int8_2d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int8_2d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_int8_2d
    Subroutine easyIA_int8_2d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),allocatable,intent(inout) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int8_2d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int8_2d(fname, vname, data)
        end if
    End Subroutine easyIA_int8_2d
    Subroutine easyOA_real4_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),allocatable,intent(in) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real4_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real4_2d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real4_2d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real4_2d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_real4_2d
    Subroutine easyIA_real4_2d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),allocatable,intent(inout) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real4_2d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real4_2d(fname, vname, data)
        end if
    End Subroutine easyIA_real4_2d
    Subroutine easyOA_real8_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),allocatable,intent(in) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real8_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real8_2d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real8_2d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real8_2d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_real8_2d
    Subroutine easyIA_real8_2d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),allocatable,intent(inout) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real8_2d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real8_2d(fname, vname, data)
        end if
    End Subroutine easyIA_real8_2d
    Subroutine easyOA_string_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),allocatable,intent(in) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_string_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_string_2d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_string_2d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_string_2d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_string_2d
    Subroutine easyIA_string_2d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),allocatable,intent(inout) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_string_2d(fname, vname, data, positioin, count_lens)
        else
            call easyI_string_2d(fname, vname, data)
        end if
    End Subroutine easyIA_string_2d
    Subroutine easyOA_logical_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,allocatable,intent(in) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_logical_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_logical_2d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_logical_2d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_logical_2d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_logical_2d
    Subroutine easyIA_logical_2d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,allocatable,intent(inout) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_logical_2d(fname, vname, data, positioin, count_lens)
        else
            call easyI_logical_2d(fname, vname, data)
        end if
    End Subroutine easyIA_logical_2d
    Subroutine easyOA_complex4_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),allocatable,intent(in) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex4_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex4_2d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex4_2d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex4_2d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_complex4_2d
    Subroutine easyIA_complex4_2d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),allocatable,intent(inout) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex4_2d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex4_2d(fname, vname, data)
        end if
    End Subroutine easyIA_complex4_2d
    Subroutine easyOA_complex8_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),allocatable,intent(in) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex8_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex8_2d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex8_2d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex8_2d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_complex8_2d
    Subroutine easyIA_complex8_2d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),allocatable,intent(inout) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex8_2d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex8_2d(fname, vname, data)
        end if
    End Subroutine easyIA_complex8_2d
    Subroutine easyOA_int4_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),allocatable,intent(in) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int4_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int4_3d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int4_3d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int4_3d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_int4_3d
    Subroutine easyIA_int4_3d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),allocatable,intent(inout) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int4_3d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int4_3d(fname, vname, data)
        end if
    End Subroutine easyIA_int4_3d
    Subroutine easyOA_int8_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),allocatable,intent(in) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int8_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int8_3d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int8_3d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int8_3d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_int8_3d
    Subroutine easyIA_int8_3d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),allocatable,intent(inout) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int8_3d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int8_3d(fname, vname, data)
        end if
    End Subroutine easyIA_int8_3d
    Subroutine easyOA_real4_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),allocatable,intent(in) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real4_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real4_3d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real4_3d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real4_3d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_real4_3d
    Subroutine easyIA_real4_3d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),allocatable,intent(inout) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real4_3d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real4_3d(fname, vname, data)
        end if
    End Subroutine easyIA_real4_3d
    Subroutine easyOA_real8_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),allocatable,intent(in) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real8_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real8_3d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real8_3d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real8_3d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_real8_3d
    Subroutine easyIA_real8_3d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),allocatable,intent(inout) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real8_3d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real8_3d(fname, vname, data)
        end if
    End Subroutine easyIA_real8_3d
    Subroutine easyOA_string_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),allocatable,intent(in) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_string_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_string_3d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_string_3d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_string_3d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_string_3d
    Subroutine easyIA_string_3d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),allocatable,intent(inout) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_string_3d(fname, vname, data, positioin, count_lens)
        else
            call easyI_string_3d(fname, vname, data)
        end if
    End Subroutine easyIA_string_3d
    Subroutine easyOA_logical_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,allocatable,intent(in) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_logical_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_logical_3d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_logical_3d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_logical_3d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_logical_3d
    Subroutine easyIA_logical_3d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,allocatable,intent(inout) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_logical_3d(fname, vname, data, positioin, count_lens)
        else
            call easyI_logical_3d(fname, vname, data)
        end if
    End Subroutine easyIA_logical_3d
    Subroutine easyOA_complex4_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),allocatable,intent(in) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex4_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex4_3d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex4_3d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex4_3d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_complex4_3d
    Subroutine easyIA_complex4_3d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),allocatable,intent(inout) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex4_3d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex4_3d(fname, vname, data)
        end if
    End Subroutine easyIA_complex4_3d
    Subroutine easyOA_complex8_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),allocatable,intent(in) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex8_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex8_3d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex8_3d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex8_3d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_complex8_3d
    Subroutine easyIA_complex8_3d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),allocatable,intent(inout) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex8_3d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex8_3d(fname, vname, data)
        end if
    End Subroutine easyIA_complex8_3d
    Subroutine easyOA_int4_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),allocatable,intent(in) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int4_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int4_4d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int4_4d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int4_4d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_int4_4d
    Subroutine easyIA_int4_4d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),allocatable,intent(inout) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int4_4d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int4_4d(fname, vname, data)
        end if
    End Subroutine easyIA_int4_4d
    Subroutine easyOA_int8_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),allocatable,intent(in) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int8_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int8_4d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int8_4d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int8_4d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_int8_4d
    Subroutine easyIA_int8_4d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),allocatable,intent(inout) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int8_4d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int8_4d(fname, vname, data)
        end if
    End Subroutine easyIA_int8_4d
    Subroutine easyOA_real4_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),allocatable,intent(in) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real4_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real4_4d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real4_4d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real4_4d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_real4_4d
    Subroutine easyIA_real4_4d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),allocatable,intent(inout) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real4_4d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real4_4d(fname, vname, data)
        end if
    End Subroutine easyIA_real4_4d
    Subroutine easyOA_real8_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),allocatable,intent(in) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real8_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real8_4d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real8_4d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real8_4d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_real8_4d
    Subroutine easyIA_real8_4d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),allocatable,intent(inout) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real8_4d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real8_4d(fname, vname, data)
        end if
    End Subroutine easyIA_real8_4d
    Subroutine easyOA_string_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),allocatable,intent(in) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_string_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_string_4d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_string_4d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_string_4d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_string_4d
    Subroutine easyIA_string_4d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),allocatable,intent(inout) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_string_4d(fname, vname, data, positioin, count_lens)
        else
            call easyI_string_4d(fname, vname, data)
        end if
    End Subroutine easyIA_string_4d
    Subroutine easyOA_logical_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,allocatable,intent(in) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_logical_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_logical_4d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_logical_4d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_logical_4d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_logical_4d
    Subroutine easyIA_logical_4d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,allocatable,intent(inout) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_logical_4d(fname, vname, data, positioin, count_lens)
        else
            call easyI_logical_4d(fname, vname, data)
        end if
    End Subroutine easyIA_logical_4d
    Subroutine easyOA_complex4_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),allocatable,intent(in) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex4_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex4_4d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex4_4d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex4_4d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_complex4_4d
    Subroutine easyIA_complex4_4d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),allocatable,intent(inout) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex4_4d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex4_4d(fname, vname, data)
        end if
    End Subroutine easyIA_complex4_4d
    Subroutine easyOA_complex8_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),allocatable,intent(in) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex8_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex8_4d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex8_4d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex8_4d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_complex8_4d
    Subroutine easyIA_complex8_4d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),allocatable,intent(inout) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex8_4d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex8_4d(fname, vname, data)
        end if
    End Subroutine easyIA_complex8_4d
    Subroutine easyOA_int4_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),allocatable,intent(in) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int4_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int4_5d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int4_5d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int4_5d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_int4_5d
    Subroutine easyIA_int4_5d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),allocatable,intent(inout) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int4_5d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int4_5d(fname, vname, data)
        end if
    End Subroutine easyIA_int4_5d
    Subroutine easyOA_int8_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),allocatable,intent(in) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int8_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int8_5d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int8_5d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int8_5d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_int8_5d
    Subroutine easyIA_int8_5d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),allocatable,intent(inout) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int8_5d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int8_5d(fname, vname, data)
        end if
    End Subroutine easyIA_int8_5d
    Subroutine easyOA_real4_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),allocatable,intent(in) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real4_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real4_5d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real4_5d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real4_5d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_real4_5d
    Subroutine easyIA_real4_5d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),allocatable,intent(inout) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real4_5d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real4_5d(fname, vname, data)
        end if
    End Subroutine easyIA_real4_5d
    Subroutine easyOA_real8_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),allocatable,intent(in) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real8_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real8_5d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real8_5d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real8_5d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_real8_5d
    Subroutine easyIA_real8_5d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),allocatable,intent(inout) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real8_5d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real8_5d(fname, vname, data)
        end if
    End Subroutine easyIA_real8_5d
    Subroutine easyOA_string_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),allocatable,intent(in) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_string_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_string_5d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_string_5d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_string_5d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_string_5d
    Subroutine easyIA_string_5d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),allocatable,intent(inout) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_string_5d(fname, vname, data, positioin, count_lens)
        else
            call easyI_string_5d(fname, vname, data)
        end if
    End Subroutine easyIA_string_5d
    Subroutine easyOA_logical_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,allocatable,intent(in) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_logical_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_logical_5d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_logical_5d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_logical_5d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_logical_5d
    Subroutine easyIA_logical_5d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,allocatable,intent(inout) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_logical_5d(fname, vname, data, positioin, count_lens)
        else
            call easyI_logical_5d(fname, vname, data)
        end if
    End Subroutine easyIA_logical_5d
    Subroutine easyOA_complex4_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),allocatable,intent(in) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex4_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex4_5d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex4_5d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex4_5d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_complex4_5d
    Subroutine easyIA_complex4_5d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),allocatable,intent(inout) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex4_5d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex4_5d(fname, vname, data)
        end if
    End Subroutine easyIA_complex4_5d
    Subroutine easyOA_complex8_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),allocatable,intent(in) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex8_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex8_5d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex8_5d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex8_5d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_complex8_5d
    Subroutine easyIA_complex8_5d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),allocatable,intent(inout) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex8_5d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex8_5d(fname, vname, data)
        end if
    End Subroutine easyIA_complex8_5d
    Subroutine easyOA_int4_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),allocatable,intent(in) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int4_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int4_6d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int4_6d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int4_6d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_int4_6d
    Subroutine easyIA_int4_6d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),allocatable,intent(inout) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int4_6d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int4_6d(fname, vname, data)
        end if
    End Subroutine easyIA_int4_6d
    Subroutine easyOA_int8_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),allocatable,intent(in) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int8_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int8_6d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int8_6d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int8_6d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_int8_6d
    Subroutine easyIA_int8_6d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),allocatable,intent(inout) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int8_6d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int8_6d(fname, vname, data)
        end if
    End Subroutine easyIA_int8_6d
    Subroutine easyOA_real4_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),allocatable,intent(in) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real4_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real4_6d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real4_6d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real4_6d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_real4_6d
    Subroutine easyIA_real4_6d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),allocatable,intent(inout) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real4_6d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real4_6d(fname, vname, data)
        end if
    End Subroutine easyIA_real4_6d
    Subroutine easyOA_real8_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),allocatable,intent(in) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real8_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real8_6d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real8_6d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real8_6d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_real8_6d
    Subroutine easyIA_real8_6d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),allocatable,intent(inout) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real8_6d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real8_6d(fname, vname, data)
        end if
    End Subroutine easyIA_real8_6d
    Subroutine easyOA_string_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),allocatable,intent(in) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_string_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_string_6d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_string_6d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_string_6d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_string_6d
    Subroutine easyIA_string_6d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),allocatable,intent(inout) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_string_6d(fname, vname, data, positioin, count_lens)
        else
            call easyI_string_6d(fname, vname, data)
        end if
    End Subroutine easyIA_string_6d
    Subroutine easyOA_logical_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,allocatable,intent(in) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_logical_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_logical_6d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_logical_6d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_logical_6d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_logical_6d
    Subroutine easyIA_logical_6d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,allocatable,intent(inout) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_logical_6d(fname, vname, data, positioin, count_lens)
        else
            call easyI_logical_6d(fname, vname, data)
        end if
    End Subroutine easyIA_logical_6d
    Subroutine easyOA_complex4_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),allocatable,intent(in) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex4_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex4_6d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex4_6d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex4_6d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_complex4_6d
    Subroutine easyIA_complex4_6d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),allocatable,intent(inout) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex4_6d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex4_6d(fname, vname, data)
        end if
    End Subroutine easyIA_complex4_6d
    Subroutine easyOA_complex8_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),allocatable,intent(in) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex8_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex8_6d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex8_6d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex8_6d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_complex8_6d
    Subroutine easyIA_complex8_6d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),allocatable,intent(inout) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex8_6d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex8_6d(fname, vname, data)
        end if
    End Subroutine easyIA_complex8_6d
    Subroutine easyOA_int4_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),allocatable,intent(in) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int4_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int4_7d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int4_7d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int4_7d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_int4_7d
    Subroutine easyIA_int4_7d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),allocatable,intent(inout) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6), &
    shape_manual(7))) ! 
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6), &
    enc_iaaT_2d(1,7):enc_iaaT_2d(2,7))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6), &
    enc_iaaT_1d(7))) ! 
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int4_7d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int4_7d(fname, vname, data)
        end if
    End Subroutine easyIA_int4_7d
    Subroutine easyOA_int8_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),allocatable,intent(in) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int8_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int8_7d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int8_7d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int8_7d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_int8_7d
    Subroutine easyIA_int8_7d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),allocatable,intent(inout) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6), &
    shape_manual(7))) ! 
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6), &
    enc_iaaT_2d(1,7):enc_iaaT_2d(2,7))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6), &
    enc_iaaT_1d(7))) ! 
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int8_7d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int8_7d(fname, vname, data)
        end if
    End Subroutine easyIA_int8_7d
    Subroutine easyOA_real4_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),allocatable,intent(in) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real4_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real4_7d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real4_7d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real4_7d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_real4_7d
    Subroutine easyIA_real4_7d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),allocatable,intent(inout) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6), &
    shape_manual(7))) ! 
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6), &
    enc_iaaT_2d(1,7):enc_iaaT_2d(2,7))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6), &
    enc_iaaT_1d(7))) ! 
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real4_7d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real4_7d(fname, vname, data)
        end if
    End Subroutine easyIA_real4_7d
    Subroutine easyOA_real8_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),allocatable,intent(in) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real8_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real8_7d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real8_7d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real8_7d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_real8_7d
    Subroutine easyIA_real8_7d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),allocatable,intent(inout) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6), &
    shape_manual(7))) ! 
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6), &
    enc_iaaT_2d(1,7):enc_iaaT_2d(2,7))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6), &
    enc_iaaT_1d(7))) ! 
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real8_7d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real8_7d(fname, vname, data)
        end if
    End Subroutine easyIA_real8_7d
    Subroutine easyOA_logical_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,allocatable,intent(in) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_logical_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_logical_7d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_logical_7d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_logical_7d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_logical_7d
    Subroutine easyIA_logical_7d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,allocatable,intent(inout) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6), &
    shape_manual(7))) ! 
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6), &
    enc_iaaT_2d(1,7):enc_iaaT_2d(2,7))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6), &
    enc_iaaT_1d(7))) ! 
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_logical_7d(fname, vname, data, positioin, count_lens)
        else
            call easyI_logical_7d(fname, vname, data)
        end if
    End Subroutine easyIA_logical_7d
    Subroutine easyOA_complex4_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),allocatable,intent(in) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex4_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex4_7d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex4_7d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex4_7d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_complex4_7d
    Subroutine easyIA_complex4_7d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),allocatable,intent(inout) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6), &
    shape_manual(7))) ! 
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6), &
    enc_iaaT_2d(1,7):enc_iaaT_2d(2,7))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6), &
    enc_iaaT_1d(7))) ! 
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex4_7d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex4_7d(fname, vname, data)
        end if
    End Subroutine easyIA_complex4_7d
    Subroutine easyOA_complex8_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),allocatable,intent(in) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (allocated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex8_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex8_7d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex8_7d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex8_7d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOA_complex8_7d
    Subroutine easyIA_complex8_7d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),allocatable,intent(inout) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. allocated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6), &
    shape_manual(7))) ! 
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6), &
    enc_iaaT_2d(1,7):enc_iaaT_2d(2,7))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6), &
    enc_iaaT_1d(7))) ! 
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex8_7d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex8_7d(fname, vname, data)
        end if
    End Subroutine easyIA_complex8_7d
! *********************************************************
! easyIO for n-dimensional generic numeric data type :
!   int4. int8, real4, real8
! *********************************************************
    Subroutine easyOP_int4_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),pointer,intent(in) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int4_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int4_1d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int4_1d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int4_1d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_int4_1d
    Subroutine easyIP_int4_1d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),pointer,intent(inout) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int4_1d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int4_1d(fname, vname, data)
        end if
    End Subroutine easyIP_int4_1d
    Subroutine easyOP_int8_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),pointer,intent(in) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int8_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int8_1d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int8_1d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int8_1d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_int8_1d
    Subroutine easyIP_int8_1d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),pointer,intent(inout) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int8_1d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int8_1d(fname, vname, data)
        end if
    End Subroutine easyIP_int8_1d
    Subroutine easyOP_real4_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),pointer,intent(in) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real4_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real4_1d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real4_1d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real4_1d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_real4_1d
    Subroutine easyIP_real4_1d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),pointer,intent(inout) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real4_1d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real4_1d(fname, vname, data)
        end if
    End Subroutine easyIP_real4_1d
    Subroutine easyOP_real8_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),pointer,intent(in) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real8_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real8_1d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real8_1d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real8_1d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_real8_1d
    Subroutine easyIP_real8_1d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),pointer,intent(inout) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real8_1d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real8_1d(fname, vname, data)
        end if
    End Subroutine easyIP_real8_1d
    Subroutine easyOP_string_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),pointer,intent(in) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_string_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_string_1d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_string_1d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_string_1d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_string_1d
    Subroutine easyIP_string_1d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),pointer,intent(inout) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_string_1d(fname, vname, data, positioin, count_lens)
        else
            call easyI_string_1d(fname, vname, data)
        end if
    End Subroutine easyIP_string_1d
    Subroutine easyOP_logical_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,pointer,intent(in) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_logical_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_logical_1d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_logical_1d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_logical_1d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_logical_1d
    Subroutine easyIP_logical_1d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,pointer,intent(inout) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_logical_1d(fname, vname, data, positioin, count_lens)
        else
            call easyI_logical_1d(fname, vname, data)
        end if
    End Subroutine easyIP_logical_1d
    Subroutine easyOP_complex4_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),pointer,intent(in) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex4_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex4_1d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex4_1d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex4_1d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_complex4_1d
    Subroutine easyIP_complex4_1d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),pointer,intent(inout) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex4_1d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex4_1d(fname, vname, data)
        end if
    End Subroutine easyIP_complex4_1d
    Subroutine easyOP_complex8_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),pointer,intent(in) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex8_1d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex8_1d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex8_1d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex8_1d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_complex8_1d
    Subroutine easyIP_complex8_1d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),pointer,intent(inout) :: data(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex8_1d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex8_1d(fname, vname, data)
        end if
    End Subroutine easyIP_complex8_1d
    Subroutine easyOP_int4_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),pointer,intent(in) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int4_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int4_2d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int4_2d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int4_2d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_int4_2d
    Subroutine easyIP_int4_2d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),pointer,intent(inout) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int4_2d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int4_2d(fname, vname, data)
        end if
    End Subroutine easyIP_int4_2d
    Subroutine easyOP_int8_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),pointer,intent(in) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int8_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int8_2d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int8_2d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int8_2d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_int8_2d
    Subroutine easyIP_int8_2d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),pointer,intent(inout) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int8_2d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int8_2d(fname, vname, data)
        end if
    End Subroutine easyIP_int8_2d
    Subroutine easyOP_real4_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),pointer,intent(in) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real4_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real4_2d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real4_2d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real4_2d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_real4_2d
    Subroutine easyIP_real4_2d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),pointer,intent(inout) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real4_2d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real4_2d(fname, vname, data)
        end if
    End Subroutine easyIP_real4_2d
    Subroutine easyOP_real8_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),pointer,intent(in) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real8_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real8_2d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real8_2d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real8_2d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_real8_2d
    Subroutine easyIP_real8_2d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),pointer,intent(inout) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real8_2d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real8_2d(fname, vname, data)
        end if
    End Subroutine easyIP_real8_2d
    Subroutine easyOP_string_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),pointer,intent(in) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_string_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_string_2d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_string_2d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_string_2d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_string_2d
    Subroutine easyIP_string_2d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),pointer,intent(inout) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_string_2d(fname, vname, data, positioin, count_lens)
        else
            call easyI_string_2d(fname, vname, data)
        end if
    End Subroutine easyIP_string_2d
    Subroutine easyOP_logical_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,pointer,intent(in) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_logical_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_logical_2d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_logical_2d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_logical_2d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_logical_2d
    Subroutine easyIP_logical_2d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,pointer,intent(inout) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_logical_2d(fname, vname, data, positioin, count_lens)
        else
            call easyI_logical_2d(fname, vname, data)
        end if
    End Subroutine easyIP_logical_2d
    Subroutine easyOP_complex4_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),pointer,intent(in) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex4_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex4_2d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex4_2d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex4_2d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_complex4_2d
    Subroutine easyIP_complex4_2d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),pointer,intent(inout) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex4_2d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex4_2d(fname, vname, data)
        end if
    End Subroutine easyIP_complex4_2d
    Subroutine easyOP_complex8_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),pointer,intent(in) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex8_2d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex8_2d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex8_2d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex8_2d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_complex8_2d
    Subroutine easyIP_complex8_2d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),pointer,intent(inout) :: data(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex8_2d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex8_2d(fname, vname, data)
        end if
    End Subroutine easyIP_complex8_2d
    Subroutine easyOP_int4_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),pointer,intent(in) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int4_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int4_3d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int4_3d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int4_3d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_int4_3d
    Subroutine easyIP_int4_3d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),pointer,intent(inout) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int4_3d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int4_3d(fname, vname, data)
        end if
    End Subroutine easyIP_int4_3d
    Subroutine easyOP_int8_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),pointer,intent(in) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int8_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int8_3d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int8_3d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int8_3d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_int8_3d
    Subroutine easyIP_int8_3d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),pointer,intent(inout) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int8_3d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int8_3d(fname, vname, data)
        end if
    End Subroutine easyIP_int8_3d
    Subroutine easyOP_real4_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),pointer,intent(in) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real4_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real4_3d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real4_3d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real4_3d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_real4_3d
    Subroutine easyIP_real4_3d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),pointer,intent(inout) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real4_3d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real4_3d(fname, vname, data)
        end if
    End Subroutine easyIP_real4_3d
    Subroutine easyOP_real8_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),pointer,intent(in) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real8_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real8_3d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real8_3d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real8_3d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_real8_3d
    Subroutine easyIP_real8_3d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),pointer,intent(inout) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real8_3d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real8_3d(fname, vname, data)
        end if
    End Subroutine easyIP_real8_3d
    Subroutine easyOP_string_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),pointer,intent(in) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_string_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_string_3d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_string_3d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_string_3d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_string_3d
    Subroutine easyIP_string_3d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),pointer,intent(inout) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_string_3d(fname, vname, data, positioin, count_lens)
        else
            call easyI_string_3d(fname, vname, data)
        end if
    End Subroutine easyIP_string_3d
    Subroutine easyOP_logical_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,pointer,intent(in) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_logical_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_logical_3d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_logical_3d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_logical_3d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_logical_3d
    Subroutine easyIP_logical_3d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,pointer,intent(inout) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_logical_3d(fname, vname, data, positioin, count_lens)
        else
            call easyI_logical_3d(fname, vname, data)
        end if
    End Subroutine easyIP_logical_3d
    Subroutine easyOP_complex4_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),pointer,intent(in) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex4_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex4_3d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex4_3d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex4_3d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_complex4_3d
    Subroutine easyIP_complex4_3d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),pointer,intent(inout) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex4_3d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex4_3d(fname, vname, data)
        end if
    End Subroutine easyIP_complex4_3d
    Subroutine easyOP_complex8_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),pointer,intent(in) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex8_3d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex8_3d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex8_3d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex8_3d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_complex8_3d
    Subroutine easyIP_complex8_3d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),pointer,intent(inout) :: data(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex8_3d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex8_3d(fname, vname, data)
        end if
    End Subroutine easyIP_complex8_3d
    Subroutine easyOP_int4_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),pointer,intent(in) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int4_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int4_4d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int4_4d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int4_4d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_int4_4d
    Subroutine easyIP_int4_4d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),pointer,intent(inout) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int4_4d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int4_4d(fname, vname, data)
        end if
    End Subroutine easyIP_int4_4d
    Subroutine easyOP_int8_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),pointer,intent(in) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int8_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int8_4d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int8_4d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int8_4d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_int8_4d
    Subroutine easyIP_int8_4d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),pointer,intent(inout) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int8_4d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int8_4d(fname, vname, data)
        end if
    End Subroutine easyIP_int8_4d
    Subroutine easyOP_real4_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),pointer,intent(in) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real4_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real4_4d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real4_4d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real4_4d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_real4_4d
    Subroutine easyIP_real4_4d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),pointer,intent(inout) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real4_4d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real4_4d(fname, vname, data)
        end if
    End Subroutine easyIP_real4_4d
    Subroutine easyOP_real8_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),pointer,intent(in) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real8_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real8_4d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real8_4d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real8_4d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_real8_4d
    Subroutine easyIP_real8_4d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),pointer,intent(inout) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real8_4d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real8_4d(fname, vname, data)
        end if
    End Subroutine easyIP_real8_4d
    Subroutine easyOP_string_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),pointer,intent(in) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_string_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_string_4d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_string_4d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_string_4d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_string_4d
    Subroutine easyIP_string_4d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),pointer,intent(inout) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_string_4d(fname, vname, data, positioin, count_lens)
        else
            call easyI_string_4d(fname, vname, data)
        end if
    End Subroutine easyIP_string_4d
    Subroutine easyOP_logical_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,pointer,intent(in) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_logical_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_logical_4d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_logical_4d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_logical_4d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_logical_4d
    Subroutine easyIP_logical_4d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,pointer,intent(inout) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_logical_4d(fname, vname, data, positioin, count_lens)
        else
            call easyI_logical_4d(fname, vname, data)
        end if
    End Subroutine easyIP_logical_4d
    Subroutine easyOP_complex4_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),pointer,intent(in) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex4_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex4_4d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex4_4d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex4_4d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_complex4_4d
    Subroutine easyIP_complex4_4d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),pointer,intent(inout) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex4_4d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex4_4d(fname, vname, data)
        end if
    End Subroutine easyIP_complex4_4d
    Subroutine easyOP_complex8_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),pointer,intent(in) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex8_4d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex8_4d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex8_4d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex8_4d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_complex8_4d
    Subroutine easyIP_complex8_4d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),pointer,intent(inout) :: data(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex8_4d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex8_4d(fname, vname, data)
        end if
    End Subroutine easyIP_complex8_4d
    Subroutine easyOP_int4_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),pointer,intent(in) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int4_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int4_5d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int4_5d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int4_5d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_int4_5d
    Subroutine easyIP_int4_5d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),pointer,intent(inout) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int4_5d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int4_5d(fname, vname, data)
        end if
    End Subroutine easyIP_int4_5d
    Subroutine easyOP_int8_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),pointer,intent(in) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int8_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int8_5d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int8_5d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int8_5d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_int8_5d
    Subroutine easyIP_int8_5d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),pointer,intent(inout) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int8_5d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int8_5d(fname, vname, data)
        end if
    End Subroutine easyIP_int8_5d
    Subroutine easyOP_real4_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),pointer,intent(in) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real4_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real4_5d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real4_5d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real4_5d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_real4_5d
    Subroutine easyIP_real4_5d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),pointer,intent(inout) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real4_5d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real4_5d(fname, vname, data)
        end if
    End Subroutine easyIP_real4_5d
    Subroutine easyOP_real8_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),pointer,intent(in) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real8_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real8_5d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real8_5d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real8_5d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_real8_5d
    Subroutine easyIP_real8_5d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),pointer,intent(inout) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real8_5d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real8_5d(fname, vname, data)
        end if
    End Subroutine easyIP_real8_5d
    Subroutine easyOP_string_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),pointer,intent(in) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_string_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_string_5d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_string_5d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_string_5d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_string_5d
    Subroutine easyIP_string_5d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),pointer,intent(inout) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_string_5d(fname, vname, data, positioin, count_lens)
        else
            call easyI_string_5d(fname, vname, data)
        end if
    End Subroutine easyIP_string_5d
    Subroutine easyOP_logical_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,pointer,intent(in) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_logical_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_logical_5d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_logical_5d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_logical_5d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_logical_5d
    Subroutine easyIP_logical_5d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,pointer,intent(inout) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_logical_5d(fname, vname, data, positioin, count_lens)
        else
            call easyI_logical_5d(fname, vname, data)
        end if
    End Subroutine easyIP_logical_5d
    Subroutine easyOP_complex4_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),pointer,intent(in) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex4_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex4_5d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex4_5d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex4_5d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_complex4_5d
    Subroutine easyIP_complex4_5d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),pointer,intent(inout) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex4_5d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex4_5d(fname, vname, data)
        end if
    End Subroutine easyIP_complex4_5d
    Subroutine easyOP_complex8_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),pointer,intent(in) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex8_5d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex8_5d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex8_5d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex8_5d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_complex8_5d
    Subroutine easyIP_complex8_5d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),pointer,intent(inout) :: data(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex8_5d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex8_5d(fname, vname, data)
        end if
    End Subroutine easyIP_complex8_5d
    Subroutine easyOP_int4_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),pointer,intent(in) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int4_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int4_6d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int4_6d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int4_6d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_int4_6d
    Subroutine easyIP_int4_6d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),pointer,intent(inout) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int4_6d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int4_6d(fname, vname, data)
        end if
    End Subroutine easyIP_int4_6d
    Subroutine easyOP_int8_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),pointer,intent(in) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int8_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int8_6d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int8_6d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int8_6d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_int8_6d
    Subroutine easyIP_int8_6d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),pointer,intent(inout) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int8_6d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int8_6d(fname, vname, data)
        end if
    End Subroutine easyIP_int8_6d
    Subroutine easyOP_real4_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),pointer,intent(in) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real4_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real4_6d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real4_6d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real4_6d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_real4_6d
    Subroutine easyIP_real4_6d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),pointer,intent(inout) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real4_6d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real4_6d(fname, vname, data)
        end if
    End Subroutine easyIP_real4_6d
    Subroutine easyOP_real8_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),pointer,intent(in) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real8_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real8_6d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real8_6d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real8_6d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_real8_6d
    Subroutine easyIP_real8_6d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),pointer,intent(inout) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real8_6d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real8_6d(fname, vname, data)
        end if
    End Subroutine easyIP_real8_6d
    Subroutine easyOP_string_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),pointer,intent(in) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_string_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_string_6d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_string_6d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_string_6d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_string_6d
    Subroutine easyIP_string_6d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*),pointer,intent(inout) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_string_6d(fname, vname, data, positioin, count_lens)
        else
            call easyI_string_6d(fname, vname, data)
        end if
    End Subroutine easyIP_string_6d
    Subroutine easyOP_logical_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,pointer,intent(in) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_logical_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_logical_6d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_logical_6d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_logical_6d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_logical_6d
    Subroutine easyIP_logical_6d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,pointer,intent(inout) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_logical_6d(fname, vname, data, positioin, count_lens)
        else
            call easyI_logical_6d(fname, vname, data)
        end if
    End Subroutine easyIP_logical_6d
    Subroutine easyOP_complex4_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),pointer,intent(in) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex4_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex4_6d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex4_6d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex4_6d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_complex4_6d
    Subroutine easyIP_complex4_6d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),pointer,intent(inout) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex4_6d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex4_6d(fname, vname, data)
        end if
    End Subroutine easyIP_complex4_6d
    Subroutine easyOP_complex8_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),pointer,intent(in) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex8_6d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex8_6d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex8_6d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex8_6d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_complex8_6d
    Subroutine easyIP_complex8_6d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),pointer,intent(inout) :: data(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6)))
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6)))
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex8_6d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex8_6d(fname, vname, data)
        end if
    End Subroutine easyIP_complex8_6d
    Subroutine easyOP_int4_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),pointer,intent(in) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int4_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int4_7d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int4_7d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int4_7d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_int4_7d
    Subroutine easyIP_int4_7d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4),pointer,intent(inout) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6), &
    shape_manual(7))) ! 
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6), &
    enc_iaaT_2d(1,7):enc_iaaT_2d(2,7))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6), &
    enc_iaaT_1d(7))) ! 
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int4_7d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int4_7d(fname, vname, data)
        end if
    End Subroutine easyIP_int4_7d
    Subroutine easyOP_int8_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),pointer,intent(in) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_int8_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_int8_7d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_int8_7d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_int8_7d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_int8_7d
    Subroutine easyIP_int8_7d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8),pointer,intent(inout) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6), &
    shape_manual(7))) ! 
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6), &
    enc_iaaT_2d(1,7):enc_iaaT_2d(2,7))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6), &
    enc_iaaT_1d(7))) ! 
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_int8_7d(fname, vname, data, positioin, count_lens)
        else
            call easyI_int8_7d(fname, vname, data)
        end if
    End Subroutine easyIP_int8_7d
    Subroutine easyOP_real4_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),pointer,intent(in) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real4_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real4_7d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real4_7d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real4_7d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_real4_7d
    Subroutine easyIP_real4_7d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4),pointer,intent(inout) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6), &
    shape_manual(7))) ! 
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6), &
    enc_iaaT_2d(1,7):enc_iaaT_2d(2,7))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6), &
    enc_iaaT_1d(7))) ! 
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real4_7d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real4_7d(fname, vname, data)
        end if
    End Subroutine easyIP_real4_7d
    Subroutine easyOP_real8_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),pointer,intent(in) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_real8_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_real8_7d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_real8_7d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_real8_7d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_real8_7d
    Subroutine easyIP_real8_7d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8),pointer,intent(inout) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6), &
    shape_manual(7))) ! 
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6), &
    enc_iaaT_2d(1,7):enc_iaaT_2d(2,7))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6), &
    enc_iaaT_1d(7))) ! 
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_real8_7d(fname, vname, data, positioin, count_lens)
        else
            call easyI_real8_7d(fname, vname, data)
        end if
    End Subroutine easyIP_real8_7d
    Subroutine easyOP_logical_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,pointer,intent(in) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_logical_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_logical_7d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_logical_7d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_logical_7d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_logical_7d
    Subroutine easyIP_logical_7d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,pointer,intent(inout) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6), &
    shape_manual(7))) ! 
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6), &
    enc_iaaT_2d(1,7):enc_iaaT_2d(2,7))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6), &
    enc_iaaT_1d(7))) ! 
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_logical_7d(fname, vname, data, positioin, count_lens)
        else
            call easyI_logical_7d(fname, vname, data)
        end if
    End Subroutine easyIP_logical_7d
    Subroutine easyOP_complex4_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),pointer,intent(in) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex4_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex4_7d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex4_7d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex4_7d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_complex4_7d
    Subroutine easyIP_complex4_7d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=4),pointer,intent(inout) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6), &
    shape_manual(7))) ! 
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6), &
    enc_iaaT_2d(1,7):enc_iaaT_2d(2,7))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6), &
    enc_iaaT_1d(7))) ! 
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex4_7d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex4_7d(fname, vname, data)
        end if
    End Subroutine easyIP_complex4_7d
    Subroutine easyOP_complex8_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
        !!! #####################################
        ! This Subroutine wraps the easyO_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),pointer,intent(in) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)         ! total shape
        integer, intent(in), optional :: positioin(:)           ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)          ! `count` in netcdf
        character(*), intent(in), optional :: dimnames(:)       ! control dimnames_ for specific dimensions
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ============================= handle var-exist actions (not very strict, may need to check size or some what?)
        if (enc_var_exist(fname, vname)) then
            if (enc_vea .eq. 1) then
                return
            elseif (enc_vea .eq. -1) then
                print *, 'Error in easyO with enc_vea = -1, variable exist!'
                stop 1
            end if
        end if
        if (associated(data) .and. size(data) .gt. 0) then
            if (any(lbound(data) .ne. 1)) then
                call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],order=[2,1]))
            end if
            if (present(shape_total)) then
                if (present(dimnames)) then
                    call easyO_complex8_7d(fname, vname, data, shape_total, positioin, count_lens, dimnames)
                else
                    call easyO_complex8_7d(fname, vname, data, shape_total, positioin, count_lens)
                end if
            else
                if (present(dimnames)) then
                    call easyO_complex8_7d(fname, vname, data, dimnames=dimnames)
                else
                    call easyO_complex8_7d(fname, vname, data)
                end if
            end if
        end if
    End Subroutine easyOP_complex8_7d
    Subroutine easyIP_complex8_7d(fname, vname, data, positioin, count_lens)
        !!! #####################################
        ! This Subroutine wraps the easyI_* function except for ALLOCATABLE data
        !!! #####################################
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        complex(kind=8),pointer,intent(inout) :: data(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: positioin(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer, allocatable :: shape_manual(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Main body
        ! ========================= return if ncv doesn't exist in file
        if (.not. enc_var_exist(fname, vname)) then
            return
        end if
        
        if (.not. associated(data)) then
            if (present(count_lens)) then
                call assert(any(count_lens .gt. 1), 'Error! count_lens should have at least 1 element beyond 1')
                call remove_val(count_lens, 1, shape_manual, -1)
                allocate(data(shape_manual(1),shape_manual(2),shape_manual(3),shape_manual(4),shape_manual(5),shape_manual(6), &
    shape_manual(7))) ! 
                deallocate(shape_manual)
            else
                call enc_get_dims(fname, vname, enc_iaaT_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1, &
    3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6), &
    enc_iaaT_2d(1,7):enc_iaaT_2d(2,7))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT_1d(1),enc_iaaT_1d(2),enc_iaaT_1d(3),enc_iaaT_1d(4),enc_iaaT_1d(5),enc_iaaT_1d(6), &
    enc_iaaT_1d(7))) ! 
                end if
                deallocate(enc_iaaT_1d)
            end if
        end if
        if (present(positioin)) then
            call easyI_complex8_7d(fname, vname, data, positioin, count_lens)
        else
            call easyI_complex8_7d(fname, vname, data)
        end if
    End Subroutine easyIP_complex8_7d
    subroutine check_enc(status, errInfo)
        integer, intent ( in) :: status
        character(*), intent(in) :: errInfo
        
        if(status /= 0) then 
            print *, trim(nf90_strerror(status))
            print *, errInfo
            stop 1
        end if
    end subroutine
    
End Module
