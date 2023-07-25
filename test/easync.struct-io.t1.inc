
Subroutine easyO_t1_scalar(fname, vname, data)
    !!! #####################################
    ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
    ! Also, the scalar variable can also be put in an array record in netcdf, 
    ! via setting optional argument "shape_total" and "position"
    !!! #####################################
    implicit none

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    type(t1),intent(in) :: data
    character(*),intent(in) :: fname, vname
    
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
            call easyO(fname, trim(vname)//''//'%is1', data%is1)
        call easyO(fname, trim(vname)//''//'%ia1', data%ia1)
        call easyOA(fname, trim(vname)//''//'%iaa1', data%iaa1)

    ! call easyO(fname, trim(vname)//'%is1', data%is1)
    ! call easyO(fname, trim(vname)//'%ia1', data%ia1)
    ! call easyOA(fname, trim(vname)//'%iaa1', data%iaa1)
    
End Subroutine easyO_t1_scalar


Subroutine easyO_t1_1d(fname, vname, data)
    !!! #####################################
    ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
    ! Also, the scalar variable can also be put in an array record in netcdf, 
    ! via setting optional argument "shape_total" and "position"
    !!! #####################################
    implicit none

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    type(t1),intent(in) :: data(:)
    character(*),intent(in) :: fname, vname
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    integer :: i, j, k, l, m, n, s

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    call easyO(fname, trim(vname)//'.shape', shape(data))
    do i = 1, size(data)
                call easyO(fname, trim(vname)//toString('(', i, ')')//'%is1', data(i)%is1)
        call easyO(fname, trim(vname)//toString('(', i, ')')//'%ia1', data(i)%ia1)
        call easyOA(fname, trim(vname)//toString('(', i, ')')//'%iaa1', data(i)%iaa1)

        ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
        ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
        ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
    end do
End Subroutine easyO_t1_1d
Subroutine easyO_t1_2d(fname, vname, data)
    !!! #####################################
    ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
    ! Also, the scalar variable can also be put in an array record in netcdf, 
    ! via setting optional argument "shape_total" and "position"
    !!! #####################################
    implicit none

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    type(t1),intent(in) :: data(:,:)
    character(*),intent(in) :: fname, vname
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    integer :: i, j, k, l, m, n, s

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    call easyO(fname, trim(vname)//'.shape', shape(data))
    do i = 1, size(data(:,1))
    do j = 1, size(data(1,:))
                call easyO(fname, trim(vname)//toString('(', i,j, ')')//'%is1', data(i,j)%is1)
        call easyO(fname, trim(vname)//toString('(', i,j, ')')//'%ia1', data(i,j)%ia1)
        call easyOA(fname, trim(vname)//toString('(', i,j, ')')//'%iaa1', data(i,j)%iaa1)

        ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
        ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
        ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
    end do
    end do
End Subroutine easyO_t1_2d
Subroutine easyO_t1_3d(fname, vname, data)
    !!! #####################################
    ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
    ! Also, the scalar variable can also be put in an array record in netcdf, 
    ! via setting optional argument "shape_total" and "position"
    !!! #####################################
    implicit none

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    type(t1),intent(in) :: data(:,:,:)
    character(*),intent(in) :: fname, vname
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    integer :: i, j, k, l, m, n, s

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    call easyO(fname, trim(vname)//'.shape', shape(data))
    do i = 1, size(data(:,1,1))
    do j = 1, size(data(1,:,1))
    do k = 1, size(data(1,1,:))
                call easyO(fname, trim(vname)//toString('(', i,j,k, ')')//'%is1', data(i,j,k)%is1)
        call easyO(fname, trim(vname)//toString('(', i,j,k, ')')//'%ia1', data(i,j,k)%ia1)
        call easyOA(fname, trim(vname)//toString('(', i,j,k, ')')//'%iaa1', data(i,j,k)%iaa1)

        ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
        ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
        ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
    end do
    end do
    end do
End Subroutine easyO_t1_3d
Subroutine easyO_t1_4d(fname, vname, data)
    !!! #####################################
    ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
    ! Also, the scalar variable can also be put in an array record in netcdf, 
    ! via setting optional argument "shape_total" and "position"
    !!! #####################################
    implicit none

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    type(t1),intent(in) :: data(:,:,:,:)
    character(*),intent(in) :: fname, vname
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
    integer :: i, j, k, l, m, n, s

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
    call easyO(fname, trim(vname)//'.shape', shape(data))
    do i = 1, size(data(:,1,1,1))
    do j = 1, size(data(1,:,1,1))
    do k = 1, size(data(1,1,:,1))
    do l = 1, size(data(1,1,1,:))
                call easyO(fname, trim(vname)//toString('(', i,j,k,l, ')')//'%is1', data(i,j,k,l)%is1)
        call easyO(fname, trim(vname)//toString('(', i,j,k,l, ')')//'%ia1', data(i,j,k,l)%ia1)
        call easyOA(fname, trim(vname)//toString('(', i,j,k,l, ')')//'%iaa1', data(i,j,k,l)%iaa1)

        ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
        ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
        ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
    end do
    end do
    end do
    end do
End Subroutine easyO_t1_4d
Subroutine easyO_t1_5d(fname, vname, data)
    !!! #####################################
    ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
    ! Also, the scalar variable can also be put in an array record in netcdf, 
    ! via setting optional argument "shape_total" and "position"
    !!! #####################################
    implicit none

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    type(t1),intent(in) :: data(:,:,:,:,:)
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
                call easyO(fname, trim(vname)//toString('(', i,j,k,l,m, ')')//'%is1', data(i,j,k,l,m)%is1)
        call easyO(fname, trim(vname)//toString('(', i,j,k,l,m, ')')//'%ia1', data(i,j,k,l,m)%ia1)
        call easyOA(fname, trim(vname)//toString('(', i,j,k,l,m, ')')//'%iaa1', data(i,j,k,l,m)%iaa1)

        ! call easyO(fname, trim(vname)//'('//  //')%is1', data(i)%is1, [size(data)], [i])
        ! call easyO(fname, trim(vname)//'('//  //')%ia1', data(i)%ia1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
        ! call easyOA(fname, trim(vname)//'('// //')%iaa1', data(i)%iaa1, [size(data), size(data(i)%ia1)], [i, 1], [1, size(data(i)%ia1)])
    end do
    end do
    end do
    end do
    end do
End Subroutine easyO_t1_5d
Subroutine easyO_t1_6d(fname, vname, data)
    !!! #####################################
    ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
    ! Also, the scalar variable can also be put in an array record in netcdf, 
    ! via setting optional argument "shape_total" and "position"
    !!! #####################################
    implicit none

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    type(t1),intent(in) :: data(:,:,:,:,:,:)
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
                call easyO(fname, trim(vname)//toString('(', i,j,k,l,m,n, ')')//'%is1', data(i,j,k,l,m,n)%is1)
        call easyO(fname, trim(vname)//toString('(', i,j,k,l,m,n, ')')//'%ia1', data(i,j,k,l,m,n)%ia1)
        call easyOA(fname, trim(vname)//toString('(', i,j,k,l,m,n, ')')//'%iaa1', data(i,j,k,l,m,n)%iaa1)

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
Subroutine easyO_t1_7d(fname, vname, data)
    !!! #####################################
    ! This Subroutine aims to write a scalar int4 variable into a netcdf dataset
    ! Also, the scalar variable can also be put in an array record in netcdf, 
    ! via setting optional argument "shape_total" and "position"
    !!! #####################################
    implicit none

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
    type(t1),intent(in) :: data(:,:,:,:,:,:,:)
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
                call easyO(fname, trim(vname)//toString('(', i,j,k,l,m,n,s, ')')//'%is1', data(i,j,k,l,m,n,s)%is1)
        call easyO(fname, trim(vname)//toString('(', i,j,k,l,m,n,s, ')')//'%ia1', data(i,j,k,l,m,n,s)%ia1)
        call easyOA(fname, trim(vname)//toString('(', i,j,k,l,m,n,s, ')')//'%iaa1', data(i,j,k,l,m,n,s)%iaa1)

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
