! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module swish_m
  use differentiable_activation_strategy_m, only : differentiable_activation_strategy_t
  use kind_parameters_m, only : rkind
  implicit none

  private
  public :: swish_t 

  type, extends(differentiable_activation_strategy_t) :: swish_t
  contains
     procedure, nopass :: activation
     procedure, nopass :: activation_derivative
  end type

  interface

    elemental module function activation(x) result(y)
      implicit none
      real(rkind), intent(in) :: x
      real(rkind) y
    end function

    elemental module function activation_derivative(x) result(y)
      implicit none
      real(rkind), intent(in) :: x
      real(rkind) y
    end function

  end interface

end module swish_m
