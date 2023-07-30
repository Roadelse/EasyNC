      Module m1
      use easync
      implicit none

      type :: s1
        integer ::  i1
        logical, allocatable :: L1(:)
        real(kind=4) :: ra1(3:5)
        real(kind=8), pointer :: dap1(:) => null()
        complex :: c1
        character(20) :: sa1(3)
      end type

#include "inc.struct-io-interface.s1.F"

      contains
#include "inc.struct-io.s1.F"


      end Module
