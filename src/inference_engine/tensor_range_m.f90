! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module tensor_range_m
  use tensor_m, only : tensor_t
  use julienne_m, only : string_t
  use kind_parameters_m, only : rkind
  implicit none
  
  private
  public :: tensor_range_t
#ifdef __INTEL_COMPILER
  public :: from_json
#endif
  public :: phase_space_bin_t

  type phase_space_bin_t
    integer, allocatable :: loc(:)
  end type

  type tensor_range_t
    private
    character(len=:), allocatable :: layer_
    real, allocatable, dimension(:) :: minima_, maxima_, bin_widths_
  contains
    procedure map_to_training_range
    procedure map_from_training_range
    procedure to_json
    procedure bin
    procedure in_range
    generic :: operator(==) => equals
    procedure, private :: equals
  end type


  interface tensor_range_t

    pure module function from_components(layer, minima, maxima, num_bins) result(tensor_range)
      implicit none
      character(len=*), intent(in) :: layer
      real, dimension(:), intent(in) :: minima, maxima
      integer, intent(in), optional :: num_bins
      type(tensor_range_t) tensor_range
    end function

    module function from_json(lines) result(tensor_range)
      implicit none
      type(string_t), intent(in) :: lines(:)
      type(tensor_range_t) tensor_range
    end function

  end interface

  interface

    elemental module function map_to_training_range(self, tensor) result(normalized_tensor)
      implicit none
      class(tensor_range_t), intent(in) :: self
      type(tensor_t), intent(in) :: tensor
      type(tensor_t) normalized_tensor
    end function

    elemental module function map_from_training_range(self, tensor) result(unnormalized_tensor)
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

    pure module function bin(self, tensor, num_bins) result(phase_space_bin)
      implicit none
      class(tensor_range_t), intent(in) :: self
      type(tensor_t), intent(in) :: tensor
      integer, intent(in) :: num_bins
      type(phase_space_bin_t) phase_space_bin
    end function

    elemental module function in_range(self, tensor) result(is_in_range)
      implicit none
      class(tensor_range_t), intent(in) :: self
      type(tensor_t), intent(in) :: tensor
      logical is_in_range
    end function

  end interface

end module tensor_range_m
