program main
    use EasyNC
    implicit none

    integer,allocatable :: iaf1(:)
    integer, allocatable :: A(:)

    allocate(iaf1(10:15))
    iaf1 = [1,2,3,4,5,6]

    call easyOA('testAB.nc', 'ia1', iaf1)

    call easyIA('testAB.nc', 'ia1', A)


    print *, A
    print *, ubound(A), lbound(A)
    if (any(ubound(A) .ne. 15)) stop 1
    if (any(lbound(A) .ne. 10)) stop 2

end program