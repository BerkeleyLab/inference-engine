! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module step_m
  use activation_strategy_m, only : activation_strategy_t
  use kind_parameters_m, only : rkind
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
      real(rkind), intent(in) :: x
      real(rkind) y
    end function

  end interface

end module step_m
