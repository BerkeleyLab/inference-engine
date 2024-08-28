! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module relu_m
  use differentiable_activation_strategy_m, only : differentiable_activation_strategy_t
  use julienne_string_m, only : string_t
  implicit none

  private
  public :: relu_t 

  type, extends(differentiable_activation_strategy_t) :: relu_t
  contains
     procedure, nopass :: default_real_activation
     procedure, nopass :: default_real_activation_derivative
     procedure :: function_name
  end type

  interface

    elemental module function default_real_activation(x) result(y)
      implicit none
      real, intent(in) :: x
      real y
    end function

    elemental module function default_real_activation_derivative(x) result(y)
      implicit none
      real, intent(in) :: x
      real y
    end function

    elemental module function function_name(self) result(string)
      implicit none
      class(relu_t), intent(in) :: self
      type(string_t) string
    end function

  end interface

end module relu_m
