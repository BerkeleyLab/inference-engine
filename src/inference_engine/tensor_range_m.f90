module tensor_range_m
  implicit none
  
  private
  public :: tensor_range_t

  type tensor_range_t
    private
    real, allocatable, dimension(:) :: minima_, maxima_
  contains
    procedure map_to_unit_range
    procedure map_from_unit_range
  end type

  interface tensor_range_t

    pure module function construct_from_components(minima, maxima) result(tensor_range)
      implicit none
      real, dimension(:), intent(in) :: minima, maxima
      type(tensor_range_t) tensor_range
    end function

    pure module function construct_from_json(lines) result(tensor_range)
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

  end interface

end module tensor_range_m
