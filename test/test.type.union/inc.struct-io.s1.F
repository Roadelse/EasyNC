
        Subroutine easyO_s1_scalar(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(s1),intent(in) :: data
            character(*),intent(in) :: fname, vname
            
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
                    call easyO(fname, trim(vname)//''//'%i1', data%i1)
        call easyOA(fname, trim(vname)//''//'%l1', data%l1)
        call easyO(fname, trim(vname)//''//'%ra1', data%ra1)
        call easyOP(fname, trim(vname)//''//'%dap1', data%dap1)
        call easyO(fname, trim(vname)//''//'%c1', data%c1)
        call easyO(fname, trim(vname)//''//'%sa1', data%sa1)

            ! call easyO(fname, trim(vname)//'%is1', data%is1)
            ! call easyO(fname, trim(vname)//'%ia1', data%ia1)
            ! call easyOA(fname, trim(vname)//'%iaa1', data%iaa1)
            
        End Subroutine easyO_s1_scalar

        Subroutine easyI_s1_scalar(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(s1),intent(inout) :: data
            character(*),intent(in) :: fname, vname
            
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
                    call easyI(fname, trim(vname)//''//'%i1', data%i1)
        call easyIA(fname, trim(vname)//''//'%l1', data%l1)
        call easyI(fname, trim(vname)//''//'%ra1', data%ra1)
        call easyIP(fname, trim(vname)//''//'%dap1', data%dap1)
        call easyI(fname, trim(vname)//''//'%c1', data%c1)
        call easyI(fname, trim(vname)//''//'%sa1', data%sa1)

            ! call easyO(fname, trim(vname)//'%is1', data%is1)
            ! call easyO(fname, trim(vname)//'%ia1', data%ia1)
            ! call easyOA(fname, trim(vname)//'%iaa1', data%iaa1)
            
        End Subroutine easyI_s1_scalar

        Subroutine easyO_s1_1d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(s1),intent(in) :: data(:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            call easyO(fname, trim(vname)//'.shape', shape(data))
            do i = 1, size(data)
                        call easyO(fname, trim(vname)//toString('(', i, ')')//'%i1', data(i)%i1)
        call easyOA(fname, trim(vname)//toString('(', i, ')')//'%l1', data(i)%l1)
        call easyO(fname, trim(vname)//toString('(', i, ')')//'%ra1', data(i)%ra1)
        call easyOP(fname, trim(vname)//toString('(', i, ')')//'%dap1', data(i)%dap1)
        call easyO(fname, trim(vname)//toString('(', i, ')')//'%c1', data(i)%c1)
        call easyO(fname, trim(vname)//toString('(', i, ')')//'%sa1', data(i)%sa1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
        End Subroutine easyO_s1_1d

        Subroutine easyI_s1_1d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(s1),intent(inout) :: data(:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            ! call easyO(fname, trim(vname)//'.shape', shape(data))

            do i = 1, size(data)
                        call easyI(fname, trim(vname)//toString('(', i, ')')//'%i1', data(i)%i1)
        call easyIA(fname, trim(vname)//toString('(', i, ')')//'%l1', data(i)%l1)
        call easyI(fname, trim(vname)//toString('(', i, ')')//'%ra1', data(i)%ra1)
        call easyIP(fname, trim(vname)//toString('(', i, ')')//'%dap1', data(i)%dap1)
        call easyI(fname, trim(vname)//toString('(', i, ')')//'%c1', data(i)%c1)
        call easyI(fname, trim(vname)//toString('(', i, ')')//'%sa1', data(i)%sa1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
        End Subroutine easyI_s1_1d

        Subroutine easyOA_s1_1d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), allocatable, intent(in) :: data(:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (allocated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],
     & order=[2,1])) ! 
                end if
                call easyO_s1_1d(fname, vname, data)
            end if

        End Subroutine

        Subroutine easyOP_s1_1d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), pointer, intent(in) :: data(:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (associated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],
     & order=[2,1])) ! 
                end if
                call easyO_s1_1d(fname, vname, data)
            end if

        End Subroutine

        Subroutine easyIA_s1_1d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), allocatable, intent(inout) :: data(:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            ! ========================= return if ncv doesn't exist in file
            if (.not. enc_var_exist(fname, trim(vname)//'.shape')) then
                return
            end if

            if (.not. allocated(data)) then
                call easyIA(fname, trim(vname)//'.shape', enc_iaaT2_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT2_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT2_1d(1)))
                end if
                deallocate(enc_iaaT2_1d)
            end if
    
            call easyI_s1_1d(fname, vname, data)

        End Subroutine

        Subroutine easyIP_s1_1d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), pointer, intent(inout) :: data(:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            ! ========================= return if ncv doesn't exist in file
            if (.not. enc_var_exist(fname, trim(vname)//'.shape')) then
                return
            end if

            if (.not. associated(data)) then
                call easyIA(fname, trim(vname)//'.shape', enc_iaaT2_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT2_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT2_1d(1)))
                end if
                deallocate(enc_iaaT2_1d)
            end if
    
            call easyI_s1_1d(fname, vname, data)

        End Subroutine

        Subroutine easyO_s1_2d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(s1),intent(in) :: data(:,:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            call easyO(fname, trim(vname)//'.shape', shape(data))
            do i = 1, size(data(:,1))
            do j = 1, size(data(1,:))
                        call easyO(fname, trim(vname)//toString('(', i,j, ')')//'%i1', data(i,j)%i1)
        call easyOA(fname, trim(vname)//toString('(', i,j, ')')//'%l1', data(i,j)%l1)
        call easyO(fname, trim(vname)//toString('(', i,j, ')')//'%ra1', data(i,j)%ra1)
        call easyOP(fname, trim(vname)//toString('(', i,j, ')')//'%dap1', data(i,j)%dap1)
        call easyO(fname, trim(vname)//toString('(', i,j, ')')//'%c1', data(i,j)%c1)
        call easyO(fname, trim(vname)//toString('(', i,j, ')')//'%sa1', data(i,j)%sa1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
            end do
        End Subroutine easyO_s1_2d

        Subroutine easyI_s1_2d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(s1),intent(inout) :: data(:,:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            ! call easyO(fname, trim(vname)//'.shape', shape(data))

            do i = 1, size(data(:,1))
            do j = 1, size(data(1,:))
                        call easyI(fname, trim(vname)//toString('(', i,j, ')')//'%i1', data(i,j)%i1)
        call easyIA(fname, trim(vname)//toString('(', i,j, ')')//'%l1', data(i,j)%l1)
        call easyI(fname, trim(vname)//toString('(', i,j, ')')//'%ra1', data(i,j)%ra1)
        call easyIP(fname, trim(vname)//toString('(', i,j, ')')//'%dap1', data(i,j)%dap1)
        call easyI(fname, trim(vname)//toString('(', i,j, ')')//'%c1', data(i,j)%c1)
        call easyI(fname, trim(vname)//toString('(', i,j, ')')//'%sa1', data(i,j)%sa1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
            end do
        End Subroutine easyI_s1_2d

        Subroutine easyOA_s1_2d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), allocatable, intent(in) :: data(:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (allocated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],
     & order=[2,1])) ! 
                end if
                call easyO_s1_2d(fname, vname, data)
            end if

        End Subroutine

        Subroutine easyOP_s1_2d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), pointer, intent(in) :: data(:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (associated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],
     & order=[2,1])) ! 
                end if
                call easyO_s1_2d(fname, vname, data)
            end if

        End Subroutine

        Subroutine easyIA_s1_2d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), allocatable, intent(inout) :: data(:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            ! ========================= return if ncv doesn't exist in file
            if (.not. enc_var_exist(fname, trim(vname)//'.shape')) then
                return
            end if

            if (.not. allocated(data)) then
                call easyIA(fname, trim(vname)//'.shape', enc_iaaT2_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT2_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT2_1d(1),enc_iaaT2_1d(2)))
                end if
                deallocate(enc_iaaT2_1d)
            end if
    
            call easyI_s1_2d(fname, vname, data)

        End Subroutine

        Subroutine easyIP_s1_2d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), pointer, intent(inout) :: data(:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            ! ========================= return if ncv doesn't exist in file
            if (.not. enc_var_exist(fname, trim(vname)//'.shape')) then
                return
            end if

            if (.not. associated(data)) then
                call easyIA(fname, trim(vname)//'.shape', enc_iaaT2_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT2_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT2_1d(1),enc_iaaT2_1d(2)))
                end if
                deallocate(enc_iaaT2_1d)
            end if
    
            call easyI_s1_2d(fname, vname, data)

        End Subroutine

        Subroutine easyO_s1_3d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(s1),intent(in) :: data(:,:,:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            call easyO(fname, trim(vname)//'.shape', shape(data))
            do i = 1, size(data(:,1,1))
            do j = 1, size(data(1,:,1))
            do k = 1, size(data(1,1,:))
                        call easyO(fname, trim(vname)//toString('(', i,j,k, ')')//'%i1', data(i,j,k)%i1)
        call easyOA(fname, trim(vname)//toString('(', i,j,k, ')')//'%l1', data(i,j,k)%l1)
        call easyO(fname, trim(vname)//toString('(', i,j,k, ')')//'%ra1', data(i,j,k)%ra1)
        call easyOP(fname, trim(vname)//toString('(', i,j,k, ')')//'%dap1', data(i,j,k)%dap1)
        call easyO(fname, trim(vname)//toString('(', i,j,k, ')')//'%c1', data(i,j,k)%c1)
        call easyO(fname, trim(vname)//toString('(', i,j,k, ')')//'%sa1', data(i,j,k)%sa1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
            end do
            end do
        End Subroutine easyO_s1_3d

        Subroutine easyI_s1_3d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(s1),intent(inout) :: data(:,:,:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            ! call easyO(fname, trim(vname)//'.shape', shape(data))

            do i = 1, size(data(:,1,1))
            do j = 1, size(data(1,:,1))
            do k = 1, size(data(1,1,:))
                        call easyI(fname, trim(vname)//toString('(', i,j,k, ')')//'%i1', data(i,j,k)%i1)
        call easyIA(fname, trim(vname)//toString('(', i,j,k, ')')//'%l1', data(i,j,k)%l1)
        call easyI(fname, trim(vname)//toString('(', i,j,k, ')')//'%ra1', data(i,j,k)%ra1)
        call easyIP(fname, trim(vname)//toString('(', i,j,k, ')')//'%dap1', data(i,j,k)%dap1)
        call easyI(fname, trim(vname)//toString('(', i,j,k, ')')//'%c1', data(i,j,k)%c1)
        call easyI(fname, trim(vname)//toString('(', i,j,k, ')')//'%sa1', data(i,j,k)%sa1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
            end do
            end do
        End Subroutine easyI_s1_3d

        Subroutine easyOA_s1_3d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), allocatable, intent(in) :: data(:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (allocated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],
     & order=[2,1])) ! 
                end if
                call easyO_s1_3d(fname, vname, data)
            end if

        End Subroutine

        Subroutine easyOP_s1_3d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), pointer, intent(in) :: data(:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (associated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],
     & order=[2,1])) ! 
                end if
                call easyO_s1_3d(fname, vname, data)
            end if

        End Subroutine

        Subroutine easyIA_s1_3d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), allocatable, intent(inout) :: data(:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            ! ========================= return if ncv doesn't exist in file
            if (.not. enc_var_exist(fname, trim(vname)//'.shape')) then
                return
            end if

            if (.not. allocated(data)) then
                call easyIA(fname, trim(vname)//'.shape', enc_iaaT2_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT2_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1,
     & 3):enc_iaaT_2d(2,3))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT2_1d(1),enc_iaaT2_1d(2),enc_iaaT2_1d(3)))
                end if
                deallocate(enc_iaaT2_1d)
            end if
    
            call easyI_s1_3d(fname, vname, data)

        End Subroutine

        Subroutine easyIP_s1_3d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), pointer, intent(inout) :: data(:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            ! ========================= return if ncv doesn't exist in file
            if (.not. enc_var_exist(fname, trim(vname)//'.shape')) then
                return
            end if

            if (.not. associated(data)) then
                call easyIA(fname, trim(vname)//'.shape', enc_iaaT2_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT2_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1,
     & 3):enc_iaaT_2d(2,3))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT2_1d(1),enc_iaaT2_1d(2),enc_iaaT2_1d(3)))
                end if
                deallocate(enc_iaaT2_1d)
            end if
    
            call easyI_s1_3d(fname, vname, data)

        End Subroutine

        Subroutine easyO_s1_4d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(s1),intent(in) :: data(:,:,:,:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            call easyO(fname, trim(vname)//'.shape', shape(data))
            do i = 1, size(data(:,1,1,1))
            do j = 1, size(data(1,:,1,1))
            do k = 1, size(data(1,1,:,1))
            do l = 1, size(data(1,1,1,:))
                        call easyO(fname, trim(vname)//toString('(', i,j,k,l, ')')//'%i1', data(i,j,k,l)%i1)
        call easyOA(fname, trim(vname)//toString('(', i,j,k,l, ')')//'%l1', data(i,j,k,l)%l1)
        call easyO(fname, trim(vname)//toString('(', i,j,k,l, ')')//'%ra1', data(i,j,k,l)%ra1)
        call easyOP(fname, trim(vname)//toString('(', i,j,k,l, ')')//'%dap1', data(i,j,k,l)%dap1)
        call easyO(fname, trim(vname)//toString('(', i,j,k,l, ')')//'%c1', data(i,j,k,l)%c1)
        call easyO(fname, trim(vname)//toString('(', i,j,k,l, ')')//'%sa1', data(i,j,k,l)%sa1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
            end do
            end do
            end do
        End Subroutine easyO_s1_4d

        Subroutine easyI_s1_4d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(s1),intent(inout) :: data(:,:,:,:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            ! call easyO(fname, trim(vname)//'.shape', shape(data))

            do i = 1, size(data(:,1,1,1))
            do j = 1, size(data(1,:,1,1))
            do k = 1, size(data(1,1,:,1))
            do l = 1, size(data(1,1,1,:))
                        call easyI(fname, trim(vname)//toString('(', i,j,k,l, ')')//'%i1', data(i,j,k,l)%i1)
        call easyIA(fname, trim(vname)//toString('(', i,j,k,l, ')')//'%l1', data(i,j,k,l)%l1)
        call easyI(fname, trim(vname)//toString('(', i,j,k,l, ')')//'%ra1', data(i,j,k,l)%ra1)
        call easyIP(fname, trim(vname)//toString('(', i,j,k,l, ')')//'%dap1', data(i,j,k,l)%dap1)
        call easyI(fname, trim(vname)//toString('(', i,j,k,l, ')')//'%c1', data(i,j,k,l)%c1)
        call easyI(fname, trim(vname)//toString('(', i,j,k,l, ')')//'%sa1', data(i,j,k,l)%sa1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
            end do
            end do
            end do
        End Subroutine easyI_s1_4d

        Subroutine easyOA_s1_4d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), allocatable, intent(in) :: data(:,:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (allocated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],
     & order=[2,1])) ! 
                end if
                call easyO_s1_4d(fname, vname, data)
            end if

        End Subroutine

        Subroutine easyOP_s1_4d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), pointer, intent(in) :: data(:,:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (associated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],
     & order=[2,1])) ! 
                end if
                call easyO_s1_4d(fname, vname, data)
            end if

        End Subroutine

        Subroutine easyIA_s1_4d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), allocatable, intent(inout) :: data(:,:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            ! ========================= return if ncv doesn't exist in file
            if (.not. enc_var_exist(fname, trim(vname)//'.shape')) then
                return
            end if

            if (.not. allocated(data)) then
                call easyIA(fname, trim(vname)//'.shape', enc_iaaT2_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT2_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1,
     & 3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT2_1d(1),enc_iaaT2_1d(2),enc_iaaT2_1d(3),enc_iaaT2_1d(4)))
                end if
                deallocate(enc_iaaT2_1d)
            end if
    
            call easyI_s1_4d(fname, vname, data)

        End Subroutine

        Subroutine easyIP_s1_4d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), pointer, intent(inout) :: data(:,:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            ! ========================= return if ncv doesn't exist in file
            if (.not. enc_var_exist(fname, trim(vname)//'.shape')) then
                return
            end if

            if (.not. associated(data)) then
                call easyIA(fname, trim(vname)//'.shape', enc_iaaT2_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT2_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1,
     & 3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT2_1d(1),enc_iaaT2_1d(2),enc_iaaT2_1d(3),enc_iaaT2_1d(4)))
                end if
                deallocate(enc_iaaT2_1d)
            end if
    
            call easyI_s1_4d(fname, vname, data)

        End Subroutine

        Subroutine easyO_s1_5d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(s1),intent(in) :: data(:,:,:,:,:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            call easyO(fname, trim(vname)//'.shape', shape(data))
            do i = 1, size(data(:,1,1,1,1))
            do j = 1, size(data(1,:,1,1,1))
            do k = 1, size(data(1,1,:,1,1))
            do l = 1, size(data(1,1,1,:,1))
            do m = 1, size(data(1,1,1,1,:))
                        call easyO(fname, trim(vname)//toString('(', i,j,k,l,m, ')')//'%i1', data(i,j,k,l,m)%i1)
        call easyOA(fname, trim(vname)//toString('(', i,j,k,l,m, ')')//'%l1', data(i,j,k,l,m)%l1)
        call easyO(fname, trim(vname)//toString('(', i,j,k,l,m, ')')//'%ra1', data(i,j,k,l,m)%ra1)
        call easyOP(fname, trim(vname)//toString('(', i,j,k,l,m, ')')//'%dap1', data(i,j,k,l,m)%dap1)
        call easyO(fname, trim(vname)//toString('(', i,j,k,l,m, ')')//'%c1', data(i,j,k,l,m)%c1)
        call easyO(fname, trim(vname)//toString('(', i,j,k,l,m, ')')//'%sa1', data(i,j,k,l,m)%sa1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
            end do
            end do
            end do
            end do
        End Subroutine easyO_s1_5d

        Subroutine easyI_s1_5d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(s1),intent(inout) :: data(:,:,:,:,:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            ! call easyO(fname, trim(vname)//'.shape', shape(data))

            do i = 1, size(data(:,1,1,1,1))
            do j = 1, size(data(1,:,1,1,1))
            do k = 1, size(data(1,1,:,1,1))
            do l = 1, size(data(1,1,1,:,1))
            do m = 1, size(data(1,1,1,1,:))
                        call easyI(fname, trim(vname)//toString('(', i,j,k,l,m, ')')//'%i1', data(i,j,k,l,m)%i1)
        call easyIA(fname, trim(vname)//toString('(', i,j,k,l,m, ')')//'%l1', data(i,j,k,l,m)%l1)
        call easyI(fname, trim(vname)//toString('(', i,j,k,l,m, ')')//'%ra1', data(i,j,k,l,m)%ra1)
        call easyIP(fname, trim(vname)//toString('(', i,j,k,l,m, ')')//'%dap1', data(i,j,k,l,m)%dap1)
        call easyI(fname, trim(vname)//toString('(', i,j,k,l,m, ')')//'%c1', data(i,j,k,l,m)%c1)
        call easyI(fname, trim(vname)//toString('(', i,j,k,l,m, ')')//'%sa1', data(i,j,k,l,m)%sa1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
            end do
            end do
            end do
            end do
        End Subroutine easyI_s1_5d

        Subroutine easyOA_s1_5d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), allocatable, intent(in) :: data(:,:,:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (allocated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],
     & order=[2,1])) ! 
                end if
                call easyO_s1_5d(fname, vname, data)
            end if

        End Subroutine

        Subroutine easyOP_s1_5d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), pointer, intent(in) :: data(:,:,:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (associated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],
     & order=[2,1])) ! 
                end if
                call easyO_s1_5d(fname, vname, data)
            end if

        End Subroutine

        Subroutine easyIA_s1_5d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), allocatable, intent(inout) :: data(:,:,:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            ! ========================= return if ncv doesn't exist in file
            if (.not. enc_var_exist(fname, trim(vname)//'.shape')) then
                return
            end if

            if (.not. allocated(data)) then
                call easyIA(fname, trim(vname)//'.shape', enc_iaaT2_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT2_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1,
     & 3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT2_1d(1),enc_iaaT2_1d(2),enc_iaaT2_1d(3),enc_iaaT2_1d(4),enc_iaaT2_1d(5)))
                end if
                deallocate(enc_iaaT2_1d)
            end if
    
            call easyI_s1_5d(fname, vname, data)

        End Subroutine

        Subroutine easyIP_s1_5d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), pointer, intent(inout) :: data(:,:,:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            ! ========================= return if ncv doesn't exist in file
            if (.not. enc_var_exist(fname, trim(vname)//'.shape')) then
                return
            end if

            if (.not. associated(data)) then
                call easyIA(fname, trim(vname)//'.shape', enc_iaaT2_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT2_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1,
     & 3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT2_1d(1),enc_iaaT2_1d(2),enc_iaaT2_1d(3),enc_iaaT2_1d(4),enc_iaaT2_1d(5)))
                end if
                deallocate(enc_iaaT2_1d)
            end if
    
            call easyI_s1_5d(fname, vname, data)

        End Subroutine

        Subroutine easyO_s1_6d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(s1),intent(in) :: data(:,:,:,:,:,:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            call easyO(fname, trim(vname)//'.shape', shape(data))
            do i = 1, size(data(:,1,1,1,1,1))
            do j = 1, size(data(1,:,1,1,1,1))
            do k = 1, size(data(1,1,:,1,1,1))
            do l = 1, size(data(1,1,1,:,1,1))
            do m = 1, size(data(1,1,1,1,:,1))
            do n = 1, size(data(1,1,1,1,1,:))
                        call easyO(fname, trim(vname)//toString('(', i,j,k,l,m,n, ')')//'%i1', data(i,j,k,l,m,n)%i1)
        call easyOA(fname, trim(vname)//toString('(', i,j,k,l,m,n, ')')//'%l1', data(i,j,k,l,m,n)%l1)
        call easyO(fname, trim(vname)//toString('(', i,j,k,l,m,n, ')')//'%ra1', data(i,j,k,l,m,n)%ra1)
        call easyOP(fname, trim(vname)//toString('(', i,j,k,l,m,n, ')')//'%dap1', data(i,j,k,l,m,n)%dap1)
        call easyO(fname, trim(vname)//toString('(', i,j,k,l,m,n, ')')//'%c1', data(i,j,k,l,m,n)%c1)
        call easyO(fname, trim(vname)//toString('(', i,j,k,l,m,n, ')')//'%sa1', data(i,j,k,l,m,n)%sa1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
            end do
            end do
            end do
            end do
            end do
        End Subroutine easyO_s1_6d

        Subroutine easyI_s1_6d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(s1),intent(inout) :: data(:,:,:,:,:,:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            ! call easyO(fname, trim(vname)//'.shape', shape(data))

            do i = 1, size(data(:,1,1,1,1,1))
            do j = 1, size(data(1,:,1,1,1,1))
            do k = 1, size(data(1,1,:,1,1,1))
            do l = 1, size(data(1,1,1,:,1,1))
            do m = 1, size(data(1,1,1,1,:,1))
            do n = 1, size(data(1,1,1,1,1,:))
                        call easyI(fname, trim(vname)//toString('(', i,j,k,l,m,n, ')')//'%i1', data(i,j,k,l,m,n)%i1)
        call easyIA(fname, trim(vname)//toString('(', i,j,k,l,m,n, ')')//'%l1', data(i,j,k,l,m,n)%l1)
        call easyI(fname, trim(vname)//toString('(', i,j,k,l,m,n, ')')//'%ra1', data(i,j,k,l,m,n)%ra1)
        call easyIP(fname, trim(vname)//toString('(', i,j,k,l,m,n, ')')//'%dap1', data(i,j,k,l,m,n)%dap1)
        call easyI(fname, trim(vname)//toString('(', i,j,k,l,m,n, ')')//'%c1', data(i,j,k,l,m,n)%c1)
        call easyI(fname, trim(vname)//toString('(', i,j,k,l,m,n, ')')//'%sa1', data(i,j,k,l,m,n)%sa1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
            end do
            end do
            end do
            end do
            end do
        End Subroutine easyI_s1_6d

        Subroutine easyOA_s1_6d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), allocatable, intent(in) :: data(:,:,:,:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (allocated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],
     & order=[2,1])) ! 
                end if
                call easyO_s1_6d(fname, vname, data)
            end if

        End Subroutine

        Subroutine easyOP_s1_6d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), pointer, intent(in) :: data(:,:,:,:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (associated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],
     & order=[2,1])) ! 
                end if
                call easyO_s1_6d(fname, vname, data)
            end if

        End Subroutine

        Subroutine easyIA_s1_6d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), allocatable, intent(inout) :: data(:,:,:,:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            ! ========================= return if ncv doesn't exist in file
            if (.not. enc_var_exist(fname, trim(vname)//'.shape')) then
                return
            end if

            if (.not. allocated(data)) then
                call easyIA(fname, trim(vname)//'.shape', enc_iaaT2_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT2_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1,
     & 3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT2_1d(1),enc_iaaT2_1d(2),enc_iaaT2_1d(3),enc_iaaT2_1d(4),enc_iaaT2_1d(5),enc_iaaT2_1d(6)))
                end if
                deallocate(enc_iaaT2_1d)
            end if
    
            call easyI_s1_6d(fname, vname, data)

        End Subroutine

        Subroutine easyIP_s1_6d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), pointer, intent(inout) :: data(:,:,:,:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            ! ========================= return if ncv doesn't exist in file
            if (.not. enc_var_exist(fname, trim(vname)//'.shape')) then
                return
            end if

            if (.not. associated(data)) then
                call easyIA(fname, trim(vname)//'.shape', enc_iaaT2_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT2_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1,
     & 3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,6))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT2_1d(1),enc_iaaT2_1d(2),enc_iaaT2_1d(3),enc_iaaT2_1d(4),enc_iaaT2_1d(5),enc_iaaT2_1d(6)))
                end if
                deallocate(enc_iaaT2_1d)
            end if
    
            call easyI_s1_6d(fname, vname, data)

        End Subroutine

        Subroutine easyO_s1_7d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(s1),intent(in) :: data(:,:,:,:,:,:,:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            call easyO(fname, trim(vname)//'.shape', shape(data))
            do i = 1, size(data(:,1,1,1,1,1,1))
            do j = 1, size(data(1,:,1,1,1,1,1))
            do k = 1, size(data(1,1,:,1,1,1,1))
            do l = 1, size(data(1,1,1,:,1,1,1))
            do m = 1, size(data(1,1,1,1,:,1,1))
            do n = 1, size(data(1,1,1,1,1,:,1))
            do s = 1, size(data(1,1,1,1,1,1,:))
                        call easyO(fname, trim(vname)//toString('(', i,j,k,l,m,n,s, ')')//'%i1', data(i,j,k,l,m,n,s)%i1)
        call easyOA(fname, trim(vname)//toString('(', i,j,k,l,m,n,s, ')')//'%l1', data(i,j,k,l,m,n,s)%l1)
        call easyO(fname, trim(vname)//toString('(', i,j,k,l,m,n,s, ')')//'%ra1', data(i,j,k,l,m,n,s)%ra1)
        call easyOP(fname, trim(vname)//toString('(', i,j,k,l,m,n,s, ')')//'%dap1', data(i,j,k,l,m,n,s)%dap1)
        call easyO(fname, trim(vname)//toString('(', i,j,k,l,m,n,s, ')')//'%c1', data(i,j,k,l,m,n,s)%c1)
        call easyO(fname, trim(vname)//toString('(', i,j,k,l,m,n,s, ')')//'%sa1', data(i,j,k,l,m,n,s)%sa1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
            end do
            end do
            end do
            end do
            end do
            end do
        End Subroutine easyO_s1_7d

        Subroutine easyI_s1_7d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(s1),intent(inout) :: data(:,:,:,:,:,:,:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            ! call easyO(fname, trim(vname)//'.shape', shape(data))

            do i = 1, size(data(:,1,1,1,1,1,1))
            do j = 1, size(data(1,:,1,1,1,1,1))
            do k = 1, size(data(1,1,:,1,1,1,1))
            do l = 1, size(data(1,1,1,:,1,1,1))
            do m = 1, size(data(1,1,1,1,:,1,1))
            do n = 1, size(data(1,1,1,1,1,:,1))
            do s = 1, size(data(1,1,1,1,1,1,:))
                        call easyI(fname, trim(vname)//toString('(', i,j,k,l,m,n,s, ')')//'%i1', data(i,j,k,l,m,n,s)%i1)
        call easyIA(fname, trim(vname)//toString('(', i,j,k,l,m,n,s, ')')//'%l1', data(i,j,k,l,m,n,s)%l1)
        call easyI(fname, trim(vname)//toString('(', i,j,k,l,m,n,s, ')')//'%ra1', data(i,j,k,l,m,n,s)%ra1)
        call easyIP(fname, trim(vname)//toString('(', i,j,k,l,m,n,s, ')')//'%dap1', data(i,j,k,l,m,n,s)%dap1)
        call easyI(fname, trim(vname)//toString('(', i,j,k,l,m,n,s, ')')//'%c1', data(i,j,k,l,m,n,s)%c1)
        call easyI(fname, trim(vname)//toString('(', i,j,k,l,m,n,s, ')')//'%sa1', data(i,j,k,l,m,n,s)%sa1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
            end do
            end do
            end do
            end do
            end do
            end do
        End Subroutine easyI_s1_7d

        Subroutine easyOA_s1_7d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), allocatable, intent(in) :: data(:,:,:,:,:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (allocated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],
     & order=[2,1])) ! 
                end if
                call easyO_s1_7d(fname, vname, data)
            end if

        End Subroutine

        Subroutine easyOP_s1_7d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), pointer, intent(in) :: data(:,:,:,:,:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (associated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size(shape(data))],
     & order=[2,1])) ! 
                end if
                call easyO_s1_7d(fname, vname, data)
            end if

        End Subroutine

        Subroutine easyIA_s1_7d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), allocatable, intent(inout) :: data(:,:,:,:,:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            ! ========================= return if ncv doesn't exist in file
            if (.not. enc_var_exist(fname, trim(vname)//'.shape')) then
                return
            end if

            if (.not. allocated(data)) then
                call easyIA(fname, trim(vname)//'.shape', enc_iaaT2_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT2_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1,
     & 3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,
     & 6),enc_iaaT_2d(1,7):enc_iaaT_2d(2,7))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT2_1d(1),enc_iaaT2_1d(2),enc_iaaT2_1d(3),enc_iaaT2_1d(4),enc_iaaT2_1d(5),enc_iaaT2_1d(
     & 6),enc_iaaT2_1d(7))) ! 
                end if
                deallocate(enc_iaaT2_1d)
            end if
    
            call easyI_s1_7d(fname, vname, data)

        End Subroutine

        Subroutine easyIP_s1_7d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(s1), pointer, intent(inout) :: data(:,:,:,:,:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            ! ========================= return if ncv doesn't exist in file
            if (.not. enc_var_exist(fname, trim(vname)//'.shape')) then
                return
            end if

            if (.not. associated(data)) then
                call easyIA(fname, trim(vname)//'.shape', enc_iaaT2_1d)
                if (enc_var_exist(fname, trim(vname)//'.bounds')) then
                    allocate(enc_iaaT_2d(2, size(enc_iaaT2_1d)))
                    call easyI(fname, trim(vname) // '.bounds', enc_iaaT_2d)
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2),enc_iaaT_2d(1,
     & 3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5),enc_iaaT_2d(1,6):enc_iaaT_2d(2,
     & 6),enc_iaaT_2d(1,7):enc_iaaT_2d(2,7))) ! 
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT2_1d(1),enc_iaaT2_1d(2),enc_iaaT2_1d(3),enc_iaaT2_1d(4),enc_iaaT2_1d(5),enc_iaaT2_1d(
     & 6),enc_iaaT2_1d(7))) ! 
                end if
                deallocate(enc_iaaT2_1d)
            end if
    
            call easyI_s1_7d(fname, vname, data)

        End Subroutine

