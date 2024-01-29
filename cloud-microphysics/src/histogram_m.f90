! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module histogram_m
  !! Generate and represent histograms
  use sourcery_m, only : file_t
  implicit none

  private
  public :: histogram_t, to_file

  type histogram_t
    !! encapsulate the primary data associated with histograms
    private
    character(len=:), allocatable :: variable_name_
    real unmapped_min_, unmapped_max_
    real, allocatable :: frequency_(:), bin_midpoint_(:)
  contains
    procedure variable_name
    procedure unmapped_range
    procedure unmapped_min
    procedure unmapped_max
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

    !pure module function to_file(histograms) result(file)
    module function to_file(histograms) result(file)
      implicit none
      type(histogram_t), intent(in) :: histograms(:)
      type(file_t) file
    end function

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

    elemental module function unmapped_min(self) result(range_minimum)
      implicit none
      class(histogram_t), intent(in) :: self
      real range_minimum
    end function

    elemental module function unmapped_max(self) result(range_maximum)
      implicit none
      class(histogram_t), intent(in) :: self
      real range_maximum
    end function

    elemental module function bin_midpoint(self, bin) result(midpoint)
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
