! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module sigmoid_m
  use activation_strategy_m, only : activation_strategy_t
  implicit none

  private
  public :: sigmoid_t 

  type, extends(activation_strategy_t) :: sigmoid_t
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

end module sigmoid_m
