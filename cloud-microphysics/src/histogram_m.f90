! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module histogram_m
  implicit none

  private
  public :: histogram_t

  type histogram_t
    private
    character(len=:), allocatable :: variable_name_
    real unmapped_min_, unmapped_max_
    real, allocatable :: frequency_(:), bin_midpoint_(:)
  contains
    procedure variable_name
    procedure unmapped_range
    procedure num_bins
    procedure bin_midpoint
    procedure bin_frequency
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

  interface

  pure module function num_bins(self) result(bins)
    implicit none
    class(histogram_t), intent(in) :: self
    integer bins
  end function

    pure module function variable_name(self) result(name)
      implicit none
      class(histogram_t), intent(in) :: self
      character(len=:), allocatable :: name
    end function

    pure module function unmapped_range(self) result(raw_range)
      implicit none
      class(histogram_t), intent(in) :: self
      integer, parameter :: num_end_points = 2
      real raw_range(num_end_points)
    end function

    pure module function bin_midpoint(self, bin) result(midpoint)
      implicit none
      class(histogram_t), intent(in) :: self
      integer, intent(in) :: bin
      real midpoint
    end function

    pure module function bin_frequency(self, bin) result(frequency)
      implicit none
      class(histogram_t), intent(in) :: self
      integer, intent(in) :: bin
      real frequency
    end function

  end interface

end module histogram_m
