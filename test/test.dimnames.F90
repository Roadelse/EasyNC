program main
    use rdee_fortran
    use EasyNC
    implicit none

    character(*), parameter :: fname = 'test.dimnames.nc'

    call test_basic

contains
    subroutine test_basic()
        implicit none

        integer :: ia1(2,3,4), ia2(2,4), ia3(3,2)

        ia1 = reshape(ispan(1, 24), [2,3,4])
        ia2 = reshape(ispan(1, 8), [2,4])
        ia3 = reshape(ispan(1, 6), [3, 2])

        call easyO(fname, 'ia1', ia1, dimnames=string1dBuilder('dim1', 'dimension2', 'd3'))
        call easyO(fname, 'ia2', ia2, dimnames=string1dBuilder('dim1', 'd3'))
        call easyO(fname, 'ia3', ia3, dimnames=string1dBuilder('dimension2', 'dim1'))
        call easyO(fname, 'ia4', 1, shape_total = [4, 2], position=[1,2], dimnames=string1dBuilder('', 'dim1'))

    end subroutine


end program
