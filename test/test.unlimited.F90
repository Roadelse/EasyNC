program main
    use EasyNC
    implicit none

    character(*), parameter :: fname = 'test.unlimited.nc'

    enc_use_nc4 = .true.

    call test_in_classic()

contains
    subroutine test_in_classic()
        implicit none

        if (.not. enc_use_nc4) then
            
            call easyO(fname, 'vu1', [1,2,3], shape_total = [3, 0], position = [1, 2], count_lens = [3, 1])
        
        else
        
            call easyO(fname, 'vu1', [1,2,3], shape_total = [0, 0], position = [2, 3], count_lens = [3, 1])
        
        end if
            
    end subroutine




end program