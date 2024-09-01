module phase_space_bin_m
  use tensor_map_m, only : tensor_map_t
  use tensor_m, only : tensor_t
  implicit none
  
  public :: phase_space_bin_t

  type phase_space_bin_t
    integer, allocatable :: loc(:)
  end type
 
  interface phase_space_bin_t

    pure module function bin(tensor, minima, maxima, num_bins) result(phase_space_bin)
      implicit none
      type(tensor_t), intent(in) :: tensor
      real, intent(in) :: minima(:), maxima(:)
      integer, intent(in) :: num_bins
      type(phase_space_bin_t) phase_space_bin
    end function

  end interface

contains

  module procedure bin

    real, parameter :: half = 0.5

    associate(bin_widths => (maxima - minima)/real(num_bins))
      associate(tensor_values => min(tensor%values(), maxima - half*bin_widths))
        phase_space_bin%loc = (tensor_values - minima)/bin_widths + 1
      end associate
    end associate

  end procedure

end module phase_space_bin_m
