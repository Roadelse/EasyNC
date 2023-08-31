      Module m1
      implicit none

      type :: s1
        integer ::  i1
        logical, allocatable :: L1(:)
        real(kind=4) :: ra1(3:5)
        real(kind=8), pointer :: dap1(:)
        complex :: c1
        character(20) :: sa1(3)
      end type



      end Module
