module activation_strategy_m
  implicit none

  private
  public :: activation_strategy_t

  type, abstract :: activation_strategy_t
  contains
     procedure(activation_interface), nopass, deferred :: activation
  end type

  abstract interface

    pure function activation_interface(x) result(y)
      implicit none
      real, intent(in) :: x
      real y
    end function

  end interface

end module activation_strategy_m
