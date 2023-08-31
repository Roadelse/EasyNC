program test_linked_list
    use EasyNC
    implicit none


    character(*), parameter :: fname = 'test.linked_list.nc'


    Type :: t1
        integer :: i 
        type(t1), pointer :: ct1
    End Type


    call test_base()

contains

    Subroutine test_base()
        implicit none
        
        integer :: i
        type(t1), pointer :: p => Null(), p2 => Null()

        allocate(p)

        p2 => p
        do i = 1, 5
            allocate(p2%ct1)
            p2 => p2%ct1
        end do

        call easyOP_t1(fname, 'p', p)

    End Subroutine


end program
