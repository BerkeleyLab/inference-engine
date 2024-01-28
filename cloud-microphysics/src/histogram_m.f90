! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module histogram_m
  implicit none

  private
  public :: histogram_t

  type histogram_t
    !private
    character(len=:), allocatable :: variable_name
    real unmapped_min, unmapped_max
    real, allocatable :: frequency(:), bin_midpoint(:)
  end type

  interface histogram_t

    pure module function histogram_on_unit_interval(v, variable_name, num_bins) result(histogram)
      implicit none
      real, intent(in) :: v(:,:,:,:)
      character(len=*), intent(in) :: variable_name
      integer, intent(in) :: num_bins
      type(histogram_t) histogram
    end function

  end interface

end module histogram_m
