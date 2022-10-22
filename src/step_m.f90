module step_m
  use activation_strategy_m, only : activation_strategy_t
  implicit none

  private
  public :: step_t 

  type, extends(activation_strategy_t) :: step_t
  contains
     procedure, nopass :: activation
  end type

  interface

    elemental module function activation(x) result(y)
      implicit none
      real, intent(in) :: x
      real y
    end function

  end interface

end module step_m
