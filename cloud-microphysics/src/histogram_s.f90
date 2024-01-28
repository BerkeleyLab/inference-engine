! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(histogram_m) histogram_s
  use assert_m, only : assert, intrinsic_array_t
  use kind_parameters_m, only : rkind
  implicit none

contains

  pure function normalize(x, x_min, x_max) result(x_normalized)
    implicit none
    real(rkind), intent(in) :: x(:,:,:,:), x_min, x_max
    real(rkind), allocatable :: x_normalized(:,:,:,:)
    call assert(x_min/=x_max, "histogram_m(normalize): x_min/=x_max", intrinsic_array_t([x_min, x_max]))
    x_normalized = (x - x_min)/(x_max - x_min)
  end function

  module procedure histogram_on_unit_interval

    real, parameter :: v_mapped_min = 0., v_mapped_max = 1.
    integer, allocatable :: in_bin(:)
    integer i

    histogram%variable_name = variable_name
    histogram%unmapped_min = minval(v)
    histogram%unmapped_max = maxval(v)

    allocate(   histogram%frequency(num_bins))
    allocate(histogram%bin_midpoint(num_bins))
    allocate(                in_bin(num_bins))

    associate(v_min => (histogram%unmapped_min), v_max => (histogram%unmapped_max), cardinality => size(v))
      associate(v_mapped => normalize(v, v_min, v_max), dv => (v_mapped_max - v_mapped_min)/real(num_bins))
        associate(v_bin_min => [(v_mapped_min + (i-1)*dv, i=1,num_bins)])
          associate(smidgen => .0001*abs(dv)) ! use to make the high end of the bin range inclusive of the max value
            associate(v_bin_max => [v_bin_min(2:), v_mapped_max + smidgen])
              do concurrent(i = 1:num_bins)
                in_bin(i) = count(v_mapped >= v_bin_min(i) .and. v_mapped < v_bin_max(i)) ! replace with Fortran 2023 reduction
                histogram%frequency(i) = real(in_bin(i)) / real(cardinality)
                histogram%bin_midpoint(i) = v_bin_min(i) + 0.5*dv
              end do
            end associate
          end associate
        end associate
      end associate
      associate(binned => sum(in_bin))
        call assert(cardinality == binned, "histogram_m(normalize): lossless binning", intrinsic_array_t([cardinality, binned]))
      end associate
    end associate

  end procedure

end submodule histogram_s
