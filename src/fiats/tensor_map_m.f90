! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module tensor_map_m
  use tensor_m, only : tensor_t
  use julienne_m, only : string_t
  use kind_parameters_m, only : default_real, double_precision
  use double_precision_string_m, only : double_precision_string_t
  implicit none
  
  private
  public :: tensor_map_t

  type tensor_map_t(k)
    integer, kind :: k = default_real 
    character(len=:),      allocatable, private :: layer_
    real(k), dimension(:), allocatable, private :: intercept_, slope_
  contains
    generic :: map_to_training_range => default_real_map_to_training_range, double_precision_map_to_training_range
    procedure, private, non_overridable ::               default_real_map_to_training_range, double_precision_map_to_training_range
    generic :: map_from_training_range => default_real_map_from_training_range, double_precision_map_from_training_range
    procedure, private, non_overridable ::                 default_real_map_from_training_range, double_precision_map_from_training_range
    generic :: to_json => default_real_to_json, double_precision_to_json
    procedure, private :: default_real_to_json, double_precision_to_json
    generic :: operator(==) => default_real_equals, double_precision_equals
    procedure, private ::      default_real_equals, double_precision_equals
  end type


  interface tensor_map_t

    pure module function construct_default_real(layer, minima, maxima) result(tensor_map)
      implicit none
      character(len=*), intent(in) :: layer
      real, dimension(:), intent(in) :: minima, maxima
      type(tensor_map_t) tensor_map
    end function

    pure module function construct_double_precision(layer, minima, maxima) result(tensor_map)
      implicit none
      character(len=*), intent(in) :: layer
      double precision, dimension(:), intent(in) :: minima, maxima
      type(tensor_map_t(double_precision)) tensor_map
    end function

    module function from_json(lines) result(tensor_map)
      implicit none
      type(string_t), intent(in) :: lines(:)
      type(tensor_map_t) tensor_map
    end function

    module function double_precision_from_json(lines) result(tensor_map)
      implicit none
      type(double_precision_string_t), intent(in) :: lines(:)
      type(tensor_map_t(double_precision)) tensor_map
    end function

  end interface

  interface

    elemental module function default_real_map_to_training_range(self, tensor) result(normalized_tensor)
      implicit none
      class(tensor_map_t), intent(in) :: self
      type(tensor_t), intent(in) :: tensor
      type(tensor_t) normalized_tensor
    end function

    elemental module function double_precision_map_to_training_range(self, tensor) result(normalized_tensor)
      implicit none
      class(tensor_map_t(double_precision)), intent(in) :: self
      type(tensor_t(double_precision)), intent(in) :: tensor
      type(tensor_t(double_precision)) normalized_tensor
    end function

    elemental module function default_real_map_from_training_range(self, tensor) result(unnormalized_tensor)
      implicit none
      class(tensor_map_t), intent(in) :: self
      type(tensor_t), intent(in) :: tensor
      type(tensor_t) unnormalized_tensor
    end function

    elemental module function double_precision_map_from_training_range(self, tensor) result(unnormalized_tensor)
      implicit none
      class(tensor_map_t(double_precision)), intent(in) :: self
      type(tensor_t(double_precision)), intent(in) :: tensor
      type(tensor_t(double_precision)) unnormalized_tensor
    end function

    pure module function default_real_to_json(self) result(lines)
      implicit none
      class(tensor_map_t), intent(in) :: self
      type(string_t), allocatable :: lines(:)
    end function

    pure module function double_precision_to_json(self) result(lines)
      implicit none
      class(tensor_map_t(double_precision)), intent(in) :: self
      type(string_t), allocatable :: lines(:)
    end function

    elemental module function default_real_equals(lhs, rhs) result(lhs_equals_rhs)
      implicit none
      class(tensor_map_t), intent(in) :: lhs, rhs
      logical lhs_equals_rhs
    end function

    elemental module function double_precision_equals(lhs, rhs) result(lhs_equals_rhs)
      implicit none
      class(tensor_map_t(double_precision)), intent(in) :: lhs, rhs
      logical lhs_equals_rhs
    end function

  end interface

end module tensor_map_m
