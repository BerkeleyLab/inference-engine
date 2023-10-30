! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module sigmoid_m
  use differentiable_activation_strategy_m, only : differentiable_activation_strategy_t
  use kind_parameters_m, only : rkind
  use sourcery_m, only : string_t
  implicit none

  private
  public :: sigmoid_t 

  type, extends(differentiable_activation_strategy_t) :: sigmoid_t
  contains
     procedure, nopass :: activation
     procedure, nopass :: activation_derivative
     procedure, nopass :: function_name
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

    elemental module function function_name() result(string)
      implicit none
      type(string_t) string
    end function

  end interface

end module sigmoid_m
