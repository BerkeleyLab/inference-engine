! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module activation_strategy_m
  implicit none

  private
  public :: activation_strategy_t

  type, abstract :: activation_strategy_t
  contains
     procedure(activation_interface), nopass, deferred :: activation
  end type

  abstract interface

    elemental function activation_interface(x) result(y)
      implicit none
      real, intent(in) :: x
      real y
    end function

  end interface

end module activation_strategy_m
