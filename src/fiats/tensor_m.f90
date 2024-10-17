! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module tensor_m
  use kind_parameters_m, only : default_real, double_precision
  implicit none

  private
  public :: tensor_t

  type tensor_t(k)
    integer, kind :: k = default_real 
    real(k), allocatable, private :: values_(:)
  contains
    generic   :: values => default_real_values, double_precision_values
    procedure, private, non_overridable ::  default_real_values, double_precision_values
    generic :: num_components => default_real_num_components, double_precision_num_components
    procedure, private ::        default_real_num_components, double_precision_num_components
  end type

  interface tensor_t

    pure module function construct_default_real(values) result(tensor)
      implicit none
      real, intent(in) :: values(:)
      type(tensor_t) tensor
    end function

    pure module function construct_double_precision(values) result(tensor)
      implicit none
      double precision, intent(in) :: values(:)
      type(tensor_t(double_precision)) tensor
    end function

  end interface

  interface

    pure module function default_real_values(self) result(tensor_values)
      implicit none
      class(tensor_t), intent(in) :: self
      real, allocatable :: tensor_values(:)
    end function

    pure module function double_precision_values(self) result(tensor_values)
      implicit none
      class(tensor_t(double_precision)), intent(in) :: self
      double precision, allocatable :: tensor_values(:)
    end function

    pure module function default_real_num_components(self) result(n)
      implicit none
      class(tensor_t), intent(in) :: self
      integer n
    end function

    pure module function double_precision_num_components(self) result(n)
      implicit none
      class(tensor_t(double_precision)), intent(in) :: self
      integer n
    end function

  end interface

end module tensor_m
