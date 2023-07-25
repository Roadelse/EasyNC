
    Subroutine easyO_logical_scalar(fname, vname, val, shape_total, position)
        ! almost Duplicated with easyO_int
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,intent(in) :: val
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)      ! total shape
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
        integer :: b, i

        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        b = 0
        if(val) b = 1

        if (present(shape_total)) then
            call easyO_int4_scalar(fname, vname, b, shape_total, position)
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


    Subroutine easyO_logical_1d(fname, vname, val, shape_total, position, count_lens)
        ! almost Duplicated with easyO_int
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,intent(in) :: val(:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)      ! total shape
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
        integer, allocatable :: b(:)
        integer :: i

        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        allocate(b(size(val)))
        b = 0
        where(val) b = 1

        if (present(shape_total)) then
            call easyO_int4_1d(fname, vname, b, shape_total, position, count_lens)
        else
            call easyO_int4_1d(fname, vname, b)
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
    Subroutine easyO_logical_2d(fname, vname, val, shape_total, position, count_lens)
        ! almost Duplicated with easyO_int
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,intent(in) :: val(:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)      ! total shape
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
        integer, allocatable :: b(:,:)
        integer :: i

        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        allocate(b(size(val(:,1)), size(val(1,:))))
        b = 0
        where(val) b = 1

        if (present(shape_total)) then
            call easyO_int4_2d(fname, vname, b, shape_total, position, count_lens)
        else
            call easyO_int4_2d(fname, vname, b)
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
    Subroutine easyO_logical_3d(fname, vname, val, shape_total, position, count_lens)
        ! almost Duplicated with easyO_int
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,intent(in) :: val(:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)      ! total shape
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
        integer, allocatable :: b(:,:,:)
        integer :: i

        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        allocate(b(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:))))
        b = 0
        where(val) b = 1

        if (present(shape_total)) then
            call easyO_int4_3d(fname, vname, b, shape_total, position, count_lens)
        else
            call easyO_int4_3d(fname, vname, b)
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
    Subroutine easyO_logical_4d(fname, vname, val, shape_total, position, count_lens)
        ! almost Duplicated with easyO_int
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,intent(in) :: val(:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)      ! total shape
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
        integer, allocatable :: b(:,:,:,:)
        integer :: i

        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        allocate(b(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:))))
        b = 0
        where(val) b = 1

        if (present(shape_total)) then
            call easyO_int4_4d(fname, vname, b, shape_total, position, count_lens)
        else
            call easyO_int4_4d(fname, vname, b)
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
    Subroutine easyO_logical_5d(fname, vname, val, shape_total, position, count_lens)
        ! almost Duplicated with easyO_int
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,intent(in) :: val(:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)      ! total shape
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
        integer, allocatable :: b(:,:,:,:,:)
        integer :: i

        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        allocate(b(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:))))
        b = 0
        where(val) b = 1

        if (present(shape_total)) then
            call easyO_int4_5d(fname, vname, b, shape_total, position, count_lens)
        else
            call easyO_int4_5d(fname, vname, b)
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
    Subroutine easyO_logical_6d(fname, vname, val, shape_total, position, count_lens)
        ! almost Duplicated with easyO_int
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,intent(in) :: val(:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)      ! total shape
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
        integer, allocatable :: b(:,:,:,:,:,:)
        integer :: i

        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        allocate(b(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))))
        b = 0
        where(val) b = 1

        if (present(shape_total)) then
            call easyO_int4_6d(fname, vname, b, shape_total, position, count_lens)
        else
            call easyO_int4_6d(fname, vname, b)
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
        allocate(b(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:))))

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
    Subroutine easyO_logical_7d(fname, vname, val, shape_total, position, count_lens)
        ! almost Duplicated with easyO_int
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical,intent(in) :: val(:,:,:,:,:,:,:)
        character(*),intent(in) :: fname, vname
        integer, intent(in), optional :: shape_total(:)      ! total shape
        integer, intent(in), optional :: position(:)         ! `start` in netcdf
        integer, intent(in), optional :: count_lens(:)       ! `count` in netcdf
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> local variables
        integer, allocatable :: b(:,:,:,:,:,:,:)
        integer :: i

        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        allocate(b(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:))))
        b = 0
        where(val) b = 1

        if (present(shape_total)) then
            call easyO_int4_7d(fname, vname, b, shape_total, position, count_lens)
        else
            call easyO_int4_7d(fname, vname, b)
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
        allocate(b(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:))))

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
