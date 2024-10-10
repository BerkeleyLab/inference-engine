! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module activation_strategy_m

  ! External dependencies
  use julienne_string_m, only : string_t
  implicit none

  private
  public :: activation_strategy_t
  public :: default_real_activation_i
  public :: double_precision_activation_i
  public :: function_name_i

  type, abstract :: activation_strategy_t
  contains
     procedure(default_real_activation_i), nopass, deferred :: default_real_activation
     procedure(double_precision_activation_i), nopass, deferred :: double_precision_activation
     generic :: activation => default_real_activation, double_precision_activation
     procedure(function_name_i), deferred :: function_name
  end type

  abstract interface

    elemental function default_real_activation_i(x) result(y)
      implicit none
      real, intent(in) :: x
      real y
    end function

    elemental function double_precision_activation_i(x) result(y)
      implicit none
      double precision, intent(in) :: x
      double precision y
    end function

    elemental function function_name_i(self) result(string)
      import string_t, activation_strategy_t
      implicit none
      class(activation_strategy_t), intent(in) :: self
      type(string_t) string
    end function

  end interface

end module activation_strategy_m
