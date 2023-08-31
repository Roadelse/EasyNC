program test_linked_list
    use EasyNC
    implicit none


    character(*), parameter :: fname = 'test.linked_list.nc'


    Type :: t1
        integer :: i 
        type(t1), pointer :: ct1 => Null()
    End Type
    include 'inc.struct-io-interface.t1.F90'

    call test_base()

contains
include 'inc.struct-io.t1.F90'

    Subroutine test_base()
        implicit none
        
        integer :: i
        type(t1), pointer :: p => Null(), p2 => Null(), p3=>null()

        allocate(p)
        p%i = 0
        

        p2 => p
        do i = 1, 5
            allocate(p2%ct1)
            p2 => p2%ct1
            p2%i = i
        end do

        call easyOP_t1(fname, 'p', p)

        nullify(p3)
        call easyIP_t1(fname, 'p', p3)

        i = 0
        do while (associated(p3))
            print *, 'i along p3: ',p3%i
            call assert(p3%i .eq. i, 'Error in test_base')
            i = i + 1
            p3 => p3%ct1
        end do


    End Subroutine


end program
