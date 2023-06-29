Module EasyNC
    ! ................................................. use modules
    use nc4fortran, only: netcdf_file
    use rdee_fortran

    ! ................................................. module variables
    integer :: enc_i, enc_j, enc_k
    interface easyO
        module procedure easyO_0d
        module procedure easyO_1d
        module procedure easyO_2d
        module procedure easyO_3d
        module procedure easyO_4d
        module procedure easyO_5d
        module procedure easyO_6d
        module procedure easyO_7d
    end interface

    ! interface easyI
    ! end interface

contains
    Subroutine easyO_0d(filename, varname, data)
        implicit none
        ! ................................................. Arguments
        character(*), intent(in) :: filename, varname
        class(*), intent(in) :: data
        ! ................................................. local variable
        type(netcdf_file) :: ncf
        logical :: istatus, isExist
        ! ................................................. main body
        ! =================== open netcdf file
        call ncf%open(filename, 'a')

        select type(data)
            type is (integer(kind=4))
                call ncf%write(varname, data)
            type is (integer(kind=8))
                call ncf%write(varname, data)
            type is (real(kind=4))
                call ncf%write(varname, data)
            type is (real(kind=8))
                call ncf%write(varname, data)
            type is (character(*))
                call ncf%write(varname, data)
            type is (logical)
                print *, 'to be done'
                continue
            type is (complex)
                print *, 'to be done'
                continue
            class default
                stop 'Error in EasyNC/easyO_0d, unknown data type for data'
        end select

        call ncf%close()
    End Subroutine
    Subroutine easyO_1d(filename, varname, data)
        implicit none
        ! ................................................. Arguments
        character(*), intent(in) :: filename, varname
        class(*), intent(in) :: data(:)
        ! ................................................. local variable
        type(netcdf_file) :: ncf
        logical :: istatus, isExist
        ! ................................................. main body
        ! =================== open netcdf file
        call ncf%open(filename, 'a')

        select type(data)
            type is (integer(kind=4))
                call ncf%write(varname, data)
            type is (integer(kind=8))
                call ncf%write(varname, data)
            type is (real(kind=4))
                call ncf%write(varname, data)
            type is (real(kind=8))
                call ncf%write(varname, data)
            type is (character(*))
                call ncf%write(varname, data)
            type is (logical)
                print *, 'to be done'
                continue
            type is (complex)
                print *, 'to be done'
                continue
            class default
                stop 'Error in EasyNC/easyO_1d, unknown data type for data'
        end select

        call ncf%close()
    End Subroutine
    Subroutine easyO_2d(filename, varname, data)
        implicit none
        ! ................................................. Arguments
        character(*), intent(in) :: filename, varname
        class(*), intent(in) :: data(:,:)
        ! ................................................. local variable
        type(netcdf_file) :: ncf
        logical :: istatus, isExist
        ! ................................................. main body
        ! =================== open netcdf file
        call ncf%open(filename, 'a')

        select type(data)
            type is (integer(kind=4))
                call ncf%write(varname, data)
            type is (integer(kind=8))
                call ncf%write(varname, data)
            type is (real(kind=4))
                call ncf%write(varname, data)
            type is (real(kind=8))
                call ncf%write(varname, data)
            type is (character(*))
                call ncf%write(varname, data)
            type is (logical)
                print *, 'to be done'
                continue
            type is (complex)
                print *, 'to be done'
                continue
            class default
                stop 'Error in EasyNC/easyO_2d, unknown data type for data'
        end select

        call ncf%close()
    End Subroutine
    Subroutine easyO_3d(filename, varname, data)
        implicit none
        ! ................................................. Arguments
        character(*), intent(in) :: filename, varname
        class(*), intent(in) :: data(:,:,:)
        ! ................................................. local variable
        type(netcdf_file) :: ncf
        logical :: istatus, isExist
        ! ................................................. main body
        ! =================== open netcdf file
        call ncf%open(filename, 'a')

        select type(data)
            type is (integer(kind=4))
                call ncf%write(varname, data)
            type is (integer(kind=8))
                call ncf%write(varname, data)
            type is (real(kind=4))
                call ncf%write(varname, data)
            type is (real(kind=8))
                call ncf%write(varname, data)
            type is (character(*))
                call ncf%write(varname, data)
            type is (logical)
                print *, 'to be done'
                continue
            type is (complex)
                print *, 'to be done'
                continue
            class default
                stop 'Error in EasyNC/easyO_3d, unknown data type for data'
        end select

        call ncf%close()
    End Subroutine
    Subroutine easyO_4d(filename, varname, data)
        implicit none
        ! ................................................. Arguments
        character(*), intent(in) :: filename, varname
        class(*), intent(in) :: data(:,:,:,:)
        ! ................................................. local variable
        type(netcdf_file) :: ncf
        logical :: istatus, isExist
        ! ................................................. main body
        ! =================== open netcdf file
        call ncf%open(filename, 'a')

        select type(data)
            type is (integer(kind=4))
                call ncf%write(varname, data)
            type is (integer(kind=8))
                call ncf%write(varname, data)
            type is (real(kind=4))
                call ncf%write(varname, data)
            type is (real(kind=8))
                call ncf%write(varname, data)
            type is (character(*))
                call ncf%write(varname, data)
            type is (logical)
                print *, 'to be done'
                continue
            type is (complex)
                print *, 'to be done'
                continue
            class default
                stop 'Error in EasyNC/easyO_4d, unknown data type for data'
        end select

        call ncf%close()
    End Subroutine
    Subroutine easyO_5d(filename, varname, data)
        implicit none
        ! ................................................. Arguments
        character(*), intent(in) :: filename, varname
        class(*), intent(in) :: data(:,:,:,:,:)
        ! ................................................. local variable
        type(netcdf_file) :: ncf
        logical :: istatus, isExist
        ! ................................................. main body
        ! =================== open netcdf file
        call ncf%open(filename, 'a')

        select type(data)
            type is (integer(kind=4))
                call ncf%write(varname, data)
            type is (integer(kind=8))
                call ncf%write(varname, data)
            type is (real(kind=4))
                call ncf%write(varname, data)
            type is (real(kind=8))
                call ncf%write(varname, data)
            type is (character(*))
                call ncf%write(varname, data)
            type is (logical)
                print *, 'to be done'
                continue
            type is (complex)
                print *, 'to be done'
                continue
            class default
                stop 'Error in EasyNC/easyO_5d, unknown data type for data'
        end select

        call ncf%close()
    End Subroutine
    Subroutine easyO_6d(filename, varname, data)
        implicit none
        ! ................................................. Arguments
        character(*), intent(in) :: filename, varname
        class(*), intent(in) :: data(:,:,:,:,:,:)
        ! ................................................. local variable
        type(netcdf_file) :: ncf
        logical :: istatus, isExist
        ! ................................................. main body
        ! =================== open netcdf file
        call ncf%open(filename, 'a')

        select type(data)
            type is (integer(kind=4))
                call ncf%write(varname, data)
            type is (integer(kind=8))
                call ncf%write(varname, data)
            type is (real(kind=4))
                call ncf%write(varname, data)
            type is (real(kind=8))
                call ncf%write(varname, data)
            type is (character(*))
                call ncf%write(varname, data)
            type is (logical)
                print *, 'to be done'
                continue
            type is (complex)
                print *, 'to be done'
                continue
            class default
                stop 'Error in EasyNC/easyO_6d, unknown data type for data'
        end select

        call ncf%close()
    End Subroutine
    Subroutine easyO_7d(filename, varname, data)
        implicit none
        ! ................................................. Arguments
        character(*), intent(in) :: filename, varname
        class(*), intent(in) :: data(:,:,:,:,:,:,:)
        ! ................................................. local variable
        type(netcdf_file) :: ncf
        logical :: istatus, isExist
        ! ................................................. main body
        ! =================== open netcdf file
        call ncf%open(filename, 'a')

        select type(data)
            type is (integer(kind=4))
                call ncf%write(varname, data)
            type is (integer(kind=8))
                call ncf%write(varname, data)
            type is (real(kind=4))
                call ncf%write(varname, data)
            type is (real(kind=8))
                call ncf%write(varname, data)
            type is (character(*))
                call ncf%write(varname, data)
            type is (logical)
                print *, 'to be done'
                continue
            type is (complex)
                print *, 'to be done'
                continue
            class default
                stop 'Error in EasyNC/easyO_7d, unknown data type for data'
        end select

        call ncf%close()
    End Subroutine
End Module