program test_linked_list
    use EasyNC
    implicit none


    character(*), parameter :: fname = 'test.linked_list.nc'


    Type :: t1
        integer :: i 
        type(t1), pointer :: ct1 => Null()
    End Type


end program
