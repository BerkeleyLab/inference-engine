! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module activation_strategy_m

  ! External dependencies
  use kind_parameters_m, only : rkind
  use string_m, only : string_t
  implicit none

  private
  public :: activation_strategy_t
  public :: activation_i
  public :: function_name_i

  type, abstract :: activation_strategy_t
  contains
     procedure(activation_i), nopass, deferred :: activation
     procedure(function_name_i), nopass, deferred :: function_name
  end type

  abstract interface

    elemental function activation_i(x) result(y)
      import rkind
      implicit none
      real(rkind), intent(in) :: x
      real(rkind) y
    end function

    elemental module function function_name_i() result(string)
      implicit none
      type(string_t) string
    end function

  end interface

end module activation_strategy_m
