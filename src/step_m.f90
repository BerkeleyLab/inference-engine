! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module step_m
  use activation_strategy_m, only : activation_strategy_t
  use kind_parameters_m, only : rkind
  use julienne_string_m, only : string_t
  implicit none

  private
  public :: step_t 

  type, extends(activation_strategy_t) :: step_t
  contains
     procedure, nopass :: activation
     procedure :: function_name
  end type

  interface

    elemental module function activation(x) result(y)
      implicit none
      real(rkind), intent(in) :: x
      real(rkind) y
    end function

    elemental module function function_name(self) result(string)
      implicit none
      class(step_t), intent(in) :: self
      type(string_t) string
    end function

  end interface

end module step_m
