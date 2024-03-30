! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module tensor_range_m
  use tensor_m, only : tensor_t
  use sourcery_m, only : string_t
  implicit none
  
  private
  public :: tensor_range_t

  type tensor_range_t
    private
    character(len=:), allocatable :: layer_
    real, allocatable, dimension(:) :: minima_, maxima_
  contains
    procedure map_to_unit_range
    procedure map_from_unit_range
    procedure to_json
    generic :: operator(==) => equals
    procedure, private :: equals
  end type

  interface tensor_range_t

    pure module function from_components(layer, minima, maxima) result(tensor_range)
      implicit none
      character(len=*), intent(in) :: layer
      real, dimension(:), intent(in) :: minima, maxima
      type(tensor_range_t) tensor_range
    end function

    module function from_json(lines) result(tensor_range)
      implicit none
      type(string_t), intent(in) :: lines(:)
      type(tensor_range_t) tensor_range
    end function

  end interface

  interface

    elemental module function map_to_unit_range(self, tensor) result(normalized_tensor)
      implicit none
      class(tensor_range_t), intent(in) :: self
      type(tensor_t), intent(in) :: tensor
      type(tensor_t) normalized_tensor
    end function

    elemental module function map_from_unit_range(self, tensor) result(unnormalized_tensor)
      implicit none
      class(tensor_range_t), intent(in) :: self
      type(tensor_t), intent(in) :: tensor
      type(tensor_t) unnormalized_tensor
    end function

    pure module function to_json(self) result(lines)
      implicit none
      class(tensor_range_t), intent(in) :: self
      type(string_t), allocatable :: lines(:)
    end function

    elemental module function equals(lhs, rhs) result(lhs_equals_rhs)
      implicit none
      class(tensor_range_t), intent(in) :: lhs, rhs
      logical lhs_equals_rhs
    end function

  end interface

end module tensor_range_m
