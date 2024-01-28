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
    call assert(x_min/=x_max, "histogram_m(normalize): x_min/=x_max")
    x_normalized = (x - x_min)/(x_max - x_min)
  end function

  module procedure histogram_on_unit_interval

    real, parameter :: v_mapped_min = 0., v_mapped_max = 1.
    integer, allocatable :: in_bin(:)
    integer i

    histogram%variable_name = variable_name

    associate(v_min => minval(v), v_max => maxval(v), data_set_size => size(v))

      histogram%unmapped_min = v_min
      histogram%unmapped_max = v_max

      associate(v_mapped => normalize(v, v_min, v_max), dv => (v_mapped_max - v_mapped_min)/real(num_bins))

        allocate(histogram%frequency(num_bins), histogram%bin_midpoint(num_bins), in_bin(num_bins))

        do concurrent(i = 1:num_bins) 
          associate(v_bin_min => v_mapped_min + (i-1)*dv, v_bin_max => merge(i*dv, v_mapped_max + .1*abs(dv), i/=num_bins))
            in_bin(i) = count(v_mapped >= v_bin_min .and. v_mapped < v_bin_max) ! replace with Fortran 2023 reduction when available
            histogram%frequency(i) = real(in_bin(i)) / real(data_set_size)
            histogram%bin_midpoint(i) = v_bin_min + 0.5*dv
          end associate
        end do
    
        call assert(data_set_size == sum(in_bin), "histogram_m(normalize): x_min/=x_max", &
          intrinsic_array_t([data_set_size, sum(in_bin)]))
      end associate
    end associate

  end procedure

end submodule histogram_s
