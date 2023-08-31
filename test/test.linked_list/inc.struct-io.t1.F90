
        recursive Subroutine easyO_t1_scalar(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(t1),intent(in) :: data
            character(*),intent(in) :: fname, vname
            
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            call easyO(fname, vname, 1)

                    call easyO(fname, trim(vname)//''//'%i', data%i)
        call easyOP_t1(fname, trim(vname)//''//'%ct1', data%ct1)

            ! call easyO(fname, trim(vname)//'%is1', data%is1)
            ! call easyO(fname, trim(vname)//'%ia1', data%ia1)
            ! call easyOA(fname, trim(vname)//'%iaa1', data%iaa1)
            
        End Subroutine easyO_t1_scalar

        recursive Subroutine easyI_t1_scalar(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(t1),intent(inout) :: data
            character(*),intent(in) :: fname, vname
            
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            if (.not. enc_var_exist(fname, vname)) return

                    call easyI(fname, trim(vname)//''//'%i', data%i)
        call easyIP_t1(fname, trim(vname)//''//'%ct1', data%ct1)

            ! call easyO(fname, trim(vname)//'%is1', data%is1)
            ! call easyO(fname, trim(vname)//'%ia1', data%ia1)
            ! call easyOA(fname, trim(vname)//'%iaa1', data%iaa1)
            
        End Subroutine easyI_t1_scalar

        recursive Subroutine easyOA_t1_scalar(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), allocatable, intent(in) :: data
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (allocated(data)) then
                call easyO_t1_scalar(fname, vname, data)
            end if

        End Subroutine

        recursive Subroutine easyIA_t1_scalar(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), allocatable, intent(inout) :: data
            character(*),intent(in) :: fname, vname


            if (.not. enc_var_exist(fname, vname)) return

            if (.not. allocated(data)) then
                allocate(data)
            end if
            ! ............................................. main body
            
            call easyI_t1_scalar(fname, vname, data)

        End Subroutine

        recursive Subroutine easyOP_t1_scalar(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), pointer, intent(in) :: data
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (associated(data)) then
                call easyO_t1_scalar(fname, vname, data)
            end if

        End Subroutine

        recursive Subroutine easyIP_t1_scalar(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), pointer, intent(inout) :: data
            character(*),intent(in) :: fname, vname
            
            if (.not. enc_var_exist(fname, vname)) return

            if (.not. associated(data)) then
                allocate(data)
            end if
            ! ............................................. main body
            call easyI_t1_scalar(fname, vname, data)

        End Subroutine

        recursive Subroutine easyO_t1_1d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(t1),intent(in) :: data(:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            call easyO(fname, vname, 1)

            call easyO(fname, trim(vname)//'.shape', shape(data))
            do i = 1, size(data)
                        call easyO(fname, trim(vname)//toString('(', i, ')')//'%i', data(i)%i)
        call easyOP_t1(fname, trim(vname)//toString('(', i, ')')//'%ct1', data(i)%ct1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
        End Subroutine easyO_t1_1d

        recursive Subroutine easyI_t1_1d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(t1),intent(inout) :: data(:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            if (.not. enc_var_exist(fname, vname)) return

            do i = 1, size(data)
                        call easyI(fname, trim(vname)//toString('(', i, ')')//'%i', data(i)%i)
        call easyIP_t1(fname, trim(vname)//toString('(', i, ')')//'%ct1', data(i)%ct1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
        End Subroutine easyI_t1_1d

        recursive Subroutine easyOA_t1_1d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), allocatable, intent(in) :: data(:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (allocated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size( &
    shape(data))],order=[2,1]))
                end if
                call easyO_t1_1d(fname, vname, data)
            end if

        End Subroutine

        recursive Subroutine easyOP_t1_1d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), pointer, intent(in) :: data(:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (associated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size( &
    shape(data))],order=[2,1]))
                end if
                call easyO_t1_1d(fname, vname, data)
            end if

        End Subroutine



        recursive Subroutine easyIA_t1_1d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), allocatable, intent(inout) :: data(:)
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
    
            call easyI_t1_1d(fname, vname, data)

        End Subroutine

        recursive Subroutine easyIP_t1_1d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), pointer, intent(inout) :: data(:)
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
    
            call easyI_t1_1d(fname, vname, data)

        End Subroutine

        recursive Subroutine easyO_t1_2d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(t1),intent(in) :: data(:,:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            call easyO(fname, vname, 1)

            call easyO(fname, trim(vname)//'.shape', shape(data))
            do i = 1, size(data(:,1))
            do j = 1, size(data(1,:))
                        call easyO(fname, trim(vname)//toString('(', i,j, ')')//'%i', data(i,j)%i)
        call easyOP_t1(fname, trim(vname)//toString('(', i,j, ')')//'%ct1', data(i,j)%ct1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
            end do
        End Subroutine easyO_t1_2d

        recursive Subroutine easyI_t1_2d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(t1),intent(inout) :: data(:,:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            if (.not. enc_var_exist(fname, vname)) return

            do i = 1, size(data(:,1))
            do j = 1, size(data(1,:))
                        call easyI(fname, trim(vname)//toString('(', i,j, ')')//'%i', data(i,j)%i)
        call easyIP_t1(fname, trim(vname)//toString('(', i,j, ')')//'%ct1', data(i,j)%ct1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
            end do
        End Subroutine easyI_t1_2d

        recursive Subroutine easyOA_t1_2d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), allocatable, intent(in) :: data(:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (allocated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size( &
    shape(data))],order=[2,1]))
                end if
                call easyO_t1_2d(fname, vname, data)
            end if

        End Subroutine

        recursive Subroutine easyOP_t1_2d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), pointer, intent(in) :: data(:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (associated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size( &
    shape(data))],order=[2,1]))
                end if
                call easyO_t1_2d(fname, vname, data)
            end if

        End Subroutine



        recursive Subroutine easyIA_t1_2d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), allocatable, intent(inout) :: data(:,:)
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
    
            call easyI_t1_2d(fname, vname, data)

        End Subroutine

        recursive Subroutine easyIP_t1_2d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), pointer, intent(inout) :: data(:,:)
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
    
            call easyI_t1_2d(fname, vname, data)

        End Subroutine

        recursive Subroutine easyO_t1_3d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(t1),intent(in) :: data(:,:,:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            call easyO(fname, vname, 1)

            call easyO(fname, trim(vname)//'.shape', shape(data))
            do i = 1, size(data(:,1,1))
            do j = 1, size(data(1,:,1))
            do k = 1, size(data(1,1,:))
                        call easyO(fname, trim(vname)//toString('(', i,j,k, ')')//'%i', data(i,j,k)%i)
        call easyOP_t1(fname, trim(vname)//toString('(', i,j,k, ')')//'%ct1', data(i,j,k)%ct1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
            end do
            end do
        End Subroutine easyO_t1_3d

        recursive Subroutine easyI_t1_3d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(t1),intent(inout) :: data(:,:,:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            if (.not. enc_var_exist(fname, vname)) return

            do i = 1, size(data(:,1,1))
            do j = 1, size(data(1,:,1))
            do k = 1, size(data(1,1,:))
                        call easyI(fname, trim(vname)//toString('(', i,j,k, ')')//'%i', data(i,j,k)%i)
        call easyIP_t1(fname, trim(vname)//toString('(', i,j,k, ')')//'%ct1', data(i,j,k)%ct1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
            end do
            end do
        End Subroutine easyI_t1_3d

        recursive Subroutine easyOA_t1_3d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), allocatable, intent(in) :: data(:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (allocated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size( &
    shape(data))],order=[2,1]))
                end if
                call easyO_t1_3d(fname, vname, data)
            end if

        End Subroutine

        recursive Subroutine easyOP_t1_3d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), pointer, intent(in) :: data(:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (associated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size( &
    shape(data))],order=[2,1]))
                end if
                call easyO_t1_3d(fname, vname, data)
            end if

        End Subroutine



        recursive Subroutine easyIA_t1_3d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), allocatable, intent(inout) :: data(:,:,:)
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
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2), &
    enc_iaaT_2d(1,3):enc_iaaT_2d(2,3)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT2_1d(1),enc_iaaT2_1d(2),enc_iaaT2_1d(3)))
                end if
                deallocate(enc_iaaT2_1d)
            end if
    
            call easyI_t1_3d(fname, vname, data)

        End Subroutine

        recursive Subroutine easyIP_t1_3d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), pointer, intent(inout) :: data(:,:,:)
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
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2), &
    enc_iaaT_2d(1,3):enc_iaaT_2d(2,3)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT2_1d(1),enc_iaaT2_1d(2),enc_iaaT2_1d(3)))
                end if
                deallocate(enc_iaaT2_1d)
            end if
    
            call easyI_t1_3d(fname, vname, data)

        End Subroutine

        recursive Subroutine easyO_t1_4d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(t1),intent(in) :: data(:,:,:,:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            call easyO(fname, vname, 1)

            call easyO(fname, trim(vname)//'.shape', shape(data))
            do i = 1, size(data(:,1,1,1))
            do j = 1, size(data(1,:,1,1))
            do k = 1, size(data(1,1,:,1))
            do l = 1, size(data(1,1,1,:))
                        call easyO(fname, trim(vname)//toString('(', i,j,k,l, ')')//'%i', data(i,j,k,l)%i)
        call easyOP_t1(fname, trim(vname)//toString('(', i,j,k,l, ')')//'%ct1', data(i,j,k,l)%ct1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
            end do
            end do
            end do
        End Subroutine easyO_t1_4d

        recursive Subroutine easyI_t1_4d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(t1),intent(inout) :: data(:,:,:,:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            if (.not. enc_var_exist(fname, vname)) return

            do i = 1, size(data(:,1,1,1))
            do j = 1, size(data(1,:,1,1))
            do k = 1, size(data(1,1,:,1))
            do l = 1, size(data(1,1,1,:))
                        call easyI(fname, trim(vname)//toString('(', i,j,k,l, ')')//'%i', data(i,j,k,l)%i)
        call easyIP_t1(fname, trim(vname)//toString('(', i,j,k,l, ')')//'%ct1', data(i,j,k,l)%ct1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
            end do
            end do
            end do
        End Subroutine easyI_t1_4d

        recursive Subroutine easyOA_t1_4d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), allocatable, intent(in) :: data(:,:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (allocated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size( &
    shape(data))],order=[2,1]))
                end if
                call easyO_t1_4d(fname, vname, data)
            end if

        End Subroutine

        recursive Subroutine easyOP_t1_4d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), pointer, intent(in) :: data(:,:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (associated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size( &
    shape(data))],order=[2,1]))
                end if
                call easyO_t1_4d(fname, vname, data)
            end if

        End Subroutine



        recursive Subroutine easyIA_t1_4d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), allocatable, intent(inout) :: data(:,:,:,:)
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
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2), &
    enc_iaaT_2d(1,3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT2_1d(1),enc_iaaT2_1d(2),enc_iaaT2_1d(3),enc_iaaT2_1d(4)))
                end if
                deallocate(enc_iaaT2_1d)
            end if
    
            call easyI_t1_4d(fname, vname, data)

        End Subroutine

        recursive Subroutine easyIP_t1_4d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), pointer, intent(inout) :: data(:,:,:,:)
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
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2), &
    enc_iaaT_2d(1,3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT2_1d(1),enc_iaaT2_1d(2),enc_iaaT2_1d(3),enc_iaaT2_1d(4)))
                end if
                deallocate(enc_iaaT2_1d)
            end if
    
            call easyI_t1_4d(fname, vname, data)

        End Subroutine

        recursive Subroutine easyO_t1_5d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(t1),intent(in) :: data(:,:,:,:,:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            call easyO(fname, vname, 1)

            call easyO(fname, trim(vname)//'.shape', shape(data))
            do i = 1, size(data(:,1,1,1,1))
            do j = 1, size(data(1,:,1,1,1))
            do k = 1, size(data(1,1,:,1,1))
            do l = 1, size(data(1,1,1,:,1))
            do m = 1, size(data(1,1,1,1,:))
                        call easyO(fname, trim(vname)//toString('(', i,j,k,l,m, ')')//'%i', data(i,j,k,l,m)%i)
        call easyOP_t1(fname, trim(vname)//toString('(', i,j,k,l,m, ')')//'%ct1', data(i,j,k,l,m)%ct1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
            end do
            end do
            end do
            end do
        End Subroutine easyO_t1_5d

        recursive Subroutine easyI_t1_5d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(t1),intent(inout) :: data(:,:,:,:,:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            if (.not. enc_var_exist(fname, vname)) return

            do i = 1, size(data(:,1,1,1,1))
            do j = 1, size(data(1,:,1,1,1))
            do k = 1, size(data(1,1,:,1,1))
            do l = 1, size(data(1,1,1,:,1))
            do m = 1, size(data(1,1,1,1,:))
                        call easyI(fname, trim(vname)//toString('(', i,j,k,l,m, ')')//'%i', data(i,j,k,l,m)%i)
        call easyIP_t1(fname, trim(vname)//toString('(', i,j,k,l,m, ')')//'%ct1', data(i,j,k,l,m)%ct1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
            end do
            end do
            end do
            end do
        End Subroutine easyI_t1_5d

        recursive Subroutine easyOA_t1_5d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), allocatable, intent(in) :: data(:,:,:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (allocated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size( &
    shape(data))],order=[2,1]))
                end if
                call easyO_t1_5d(fname, vname, data)
            end if

        End Subroutine

        recursive Subroutine easyOP_t1_5d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), pointer, intent(in) :: data(:,:,:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (associated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size( &
    shape(data))],order=[2,1]))
                end if
                call easyO_t1_5d(fname, vname, data)
            end if

        End Subroutine



        recursive Subroutine easyIA_t1_5d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), allocatable, intent(inout) :: data(:,:,:,:,:)
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
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2), &
    enc_iaaT_2d(1,3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT2_1d(1),enc_iaaT2_1d(2),enc_iaaT2_1d(3),enc_iaaT2_1d(4),enc_iaaT2_1d(5)))
                end if
                deallocate(enc_iaaT2_1d)
            end if
    
            call easyI_t1_5d(fname, vname, data)

        End Subroutine

        recursive Subroutine easyIP_t1_5d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), pointer, intent(inout) :: data(:,:,:,:,:)
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
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2), &
    enc_iaaT_2d(1,3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT2_1d(1),enc_iaaT2_1d(2),enc_iaaT2_1d(3),enc_iaaT2_1d(4),enc_iaaT2_1d(5)))
                end if
                deallocate(enc_iaaT2_1d)
            end if
    
            call easyI_t1_5d(fname, vname, data)

        End Subroutine

        recursive Subroutine easyO_t1_6d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(t1),intent(in) :: data(:,:,:,:,:,:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            call easyO(fname, vname, 1)

            call easyO(fname, trim(vname)//'.shape', shape(data))
            do i = 1, size(data(:,1,1,1,1,1))
            do j = 1, size(data(1,:,1,1,1,1))
            do k = 1, size(data(1,1,:,1,1,1))
            do l = 1, size(data(1,1,1,:,1,1))
            do m = 1, size(data(1,1,1,1,:,1))
            do n = 1, size(data(1,1,1,1,1,:))
                        call easyO(fname, trim(vname)//toString('(', i,j,k,l,m,n, ')')//'%i', data(i,j,k,l,m,n)%i)
        call easyOP_t1(fname, trim(vname)//toString('(', i,j,k,l,m,n, ')')//'%ct1', data(i,j,k,l,m,n)%ct1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
            end do
            end do
            end do
            end do
            end do
        End Subroutine easyO_t1_6d

        recursive Subroutine easyI_t1_6d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(t1),intent(inout) :: data(:,:,:,:,:,:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            if (.not. enc_var_exist(fname, vname)) return

            do i = 1, size(data(:,1,1,1,1,1))
            do j = 1, size(data(1,:,1,1,1,1))
            do k = 1, size(data(1,1,:,1,1,1))
            do l = 1, size(data(1,1,1,:,1,1))
            do m = 1, size(data(1,1,1,1,:,1))
            do n = 1, size(data(1,1,1,1,1,:))
                        call easyI(fname, trim(vname)//toString('(', i,j,k,l,m,n, ')')//'%i', data(i,j,k,l,m,n)%i)
        call easyIP_t1(fname, trim(vname)//toString('(', i,j,k,l,m,n, ')')//'%ct1', data(i,j,k,l,m,n)%ct1)

                ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
                ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
                ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
            end do
            end do
            end do
            end do
            end do
            end do
        End Subroutine easyI_t1_6d

        recursive Subroutine easyOA_t1_6d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), allocatable, intent(in) :: data(:,:,:,:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (allocated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size( &
    shape(data))],order=[2,1]))
                end if
                call easyO_t1_6d(fname, vname, data)
            end if

        End Subroutine

        recursive Subroutine easyOP_t1_6d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), pointer, intent(in) :: data(:,:,:,:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (associated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size( &
    shape(data))],order=[2,1]))
                end if
                call easyO_t1_6d(fname, vname, data)
            end if

        End Subroutine



        recursive Subroutine easyIA_t1_6d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), allocatable, intent(inout) :: data(:,:,:,:,:,:)
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
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2), &
    enc_iaaT_2d(1,3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5), &
    enc_iaaT_2d(1,6):enc_iaaT_2d(2,6)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT2_1d(1),enc_iaaT2_1d(2),enc_iaaT2_1d(3),enc_iaaT2_1d(4),enc_iaaT2_1d(5),enc_iaaT2_1d(6)))
                end if
                deallocate(enc_iaaT2_1d)
            end if
    
            call easyI_t1_6d(fname, vname, data)

        End Subroutine

        recursive Subroutine easyIP_t1_6d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), pointer, intent(inout) :: data(:,:,:,:,:,:)
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
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2), &
    enc_iaaT_2d(1,3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5), &
    enc_iaaT_2d(1,6):enc_iaaT_2d(2,6)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT2_1d(1),enc_iaaT2_1d(2),enc_iaaT2_1d(3),enc_iaaT2_1d(4),enc_iaaT2_1d(5),enc_iaaT2_1d(6)))
                end if
                deallocate(enc_iaaT2_1d)
            end if
    
            call easyI_t1_6d(fname, vname, data)

        End Subroutine

        recursive Subroutine easyO_t1_7d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(t1),intent(in) :: data(:,:,:,:,:,:,:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            call easyO(fname, vname, 1)

            call easyO(fname, trim(vname)//'.shape', shape(data))
            do i = 1, size(data(:,1,1,1,1,1,1))
            do j = 1, size(data(1,:,1,1,1,1,1))
            do k = 1, size(data(1,1,:,1,1,1,1))
            do l = 1, size(data(1,1,1,:,1,1,1))
            do m = 1, size(data(1,1,1,1,:,1,1))
            do n = 1, size(data(1,1,1,1,1,:,1))
            do s = 1, size(data(1,1,1,1,1,1,:))
                        call easyO(fname, trim(vname)//toString('(', i,j,k,l,m,n,s, ')')//'%i', data(i,j,k,l,m,n,s)%i)
        call easyOP_t1(fname, trim(vname)//toString('(', i,j,k,l,m,n,s, ')')//'%ct1', data(i,j,k,l,m,n,s)%ct1)

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
        End Subroutine easyO_t1_7d

        recursive Subroutine easyI_t1_7d(fname, vname, data)
            !!! #####################################
            ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
            !!! #####################################
            implicit none

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
            type(t1),intent(inout) :: data(:,:,:,:,:,:,:)
            character(*),intent(in) :: fname, vname
            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
            integer :: i, j, k, l, m, n, s

            ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            if (.not. enc_var_exist(fname, vname)) return

            do i = 1, size(data(:,1,1,1,1,1,1))
            do j = 1, size(data(1,:,1,1,1,1,1))
            do k = 1, size(data(1,1,:,1,1,1,1))
            do l = 1, size(data(1,1,1,:,1,1,1))
            do m = 1, size(data(1,1,1,1,:,1,1))
            do n = 1, size(data(1,1,1,1,1,:,1))
            do s = 1, size(data(1,1,1,1,1,1,:))
                        call easyI(fname, trim(vname)//toString('(', i,j,k,l,m,n,s, ')')//'%i', data(i,j,k,l,m,n,s)%i)
        call easyIP_t1(fname, trim(vname)//toString('(', i,j,k,l,m,n,s, ')')//'%ct1', data(i,j,k,l,m,n,s)%ct1)

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
        End Subroutine easyI_t1_7d

        recursive Subroutine easyOA_t1_7d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), allocatable, intent(in) :: data(:,:,:,:,:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (allocated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size( &
    shape(data))],order=[2,1]))
                end if
                call easyO_t1_7d(fname, vname, data)
            end if

        End Subroutine

        recursive Subroutine easyOP_t1_7d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), pointer, intent(in) :: data(:,:,:,:,:,:,:)
            character(*),intent(in) :: fname, vname

            ! ............................................. main body
            if (associated(data) .and. size(data) .gt. 0) then
                if (any(lbound(data) .ne. 1)) then
                    call easyO(fname, trim(vname)//'.bounds', reshape([lbound(data), ubound(data)], [2, size( &
    shape(data))],order=[2,1]))
                end if
                call easyO_t1_7d(fname, vname, data)
            end if

        End Subroutine



        recursive Subroutine easyIA_t1_7d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), allocatable, intent(inout) :: data(:,:,:,:,:,:,:)
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
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2), &
    enc_iaaT_2d(1,3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5), &
    enc_iaaT_2d(1,6):enc_iaaT_2d(2,6),enc_iaaT_2d(1,7):enc_iaaT_2d(2,7)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT2_1d(1),enc_iaaT2_1d(2),enc_iaaT2_1d(3),enc_iaaT2_1d(4),enc_iaaT2_1d( &
    5),enc_iaaT2_1d(6),enc_iaaT2_1d(7)))
                end if
                deallocate(enc_iaaT2_1d)
            end if
    
            call easyI_t1_7d(fname, vname, data)

        End Subroutine

        recursive Subroutine easyIP_t1_7d(fname, vname, data)
            implicit none
            ! ............................................. Arguments
            type(t1), pointer, intent(inout) :: data(:,:,:,:,:,:,:)
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
                    allocate(data(enc_iaaT_2d(1,1):enc_iaaT_2d(2,1),enc_iaaT_2d(1,2):enc_iaaT_2d(2,2), &
    enc_iaaT_2d(1,3):enc_iaaT_2d(2,3),enc_iaaT_2d(1,4):enc_iaaT_2d(2,4),enc_iaaT_2d(1,5):enc_iaaT_2d(2,5), &
    enc_iaaT_2d(1,6):enc_iaaT_2d(2,6),enc_iaaT_2d(1,7):enc_iaaT_2d(2,7)))
                    deallocate(enc_iaaT_2d)
                else
                    allocate(data(enc_iaaT2_1d(1),enc_iaaT2_1d(2),enc_iaaT2_1d(3),enc_iaaT2_1d(4),enc_iaaT2_1d( &
    5),enc_iaaT2_1d(6),enc_iaaT2_1d(7)))
                end if
                deallocate(enc_iaaT2_1d)
            end if
    
            call easyI_t1_7d(fname, vname, data)

        End Subroutine

