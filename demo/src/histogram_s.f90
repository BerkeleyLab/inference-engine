! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(histogram_m) histogram_s
  use assert_m, only : assert, intrinsic_array_t
  use kind_parameters_m, only : rkind
  use julienne_m, only : string_t, operator(.cat.)
  implicit none

  real(rkind), parameter :: zero = 0._rkind, one = 1._rkind, two=2._rkind, half = one/two

contains

  module procedure variable_name
    name = self%variable_name_
  end procedure

  module procedure unmapped_range
    raw_range = [self%unmapped_min_, self%unmapped_max_]
  end procedure

  module procedure unmapped_max
    range_maximum = self%unmapped_max_
  end procedure

  module procedure unmapped_min
    range_minimum = self%unmapped_min_
  end procedure

  module procedure num_bins
    bins = size(self%bin_value_)
  end procedure

  module procedure bin_value
    v = self%bin_value_(bin)
  end procedure

  module procedure bin_frequency
    frequency = self%bin_frequency_(bin)
  end procedure

  module procedure to_separate_file
     file = to_common_file([histogram])
  end procedure

  module procedure to_common_file
    type(string_t), allocatable :: comments(:), columns(:)
    type(string_t) column_headings

    associate(num_histograms => size(histograms))

      allocate(comments(num_histograms))

      block
        integer line
        do line  = 1, size(comments)
          associate( &
            mode_frequency => string_t(int(100*maxval(histograms(line)%bin_frequency_))), &
            range_min => string_t(histograms(line)%unmapped_min()), &
            range_max =>  string_t(histograms(line)%unmapped_max()) &
           )
             comments(line) = "# " &
               // trim(histograms(line)%variable_name_) &
               // " range: [" // trim(range_min%string()) // ", " //  trim(range_max%string()) // "]" &
               // ", mode frequency: " // trim(mode_frequency%string()) // "%"
          end associate
        end do
      end block

      block
        integer h
        column_headings = "    bin    " // .cat. [("    " // string_t(histograms(h)%variable_name()) // "    ", h=1,num_histograms)]
      end block

      associate(num_bins =>  histograms(1)%num_bins())

        block 
          integer h, b ! histogram number, bin number

          call assert(num_bins > 0, "histogram_s(to_file): num_bins > 0")
          call assert(all(histograms(1)%num_bins() == [(histograms(h)%num_bins() , h=1,size(histograms))]), &
            "histogram_s(to_file): uniform number of bins")

          allocate(columns(num_bins))
          do b = 1, num_bins
            columns(b) =  string_t(histograms(1)%bin_value_(b)) // &
              .cat. [("  " // string_t(histograms(h)%bin_frequency(b)), h=1,num_histograms)]
          end do
        end block

      end associate

      file = file_t([comments, column_headings, columns])

    end associate
 
  end procedure

  pure function normalize(x, x_min, x_max) result(x_normalized)
    implicit none
    real(rkind), intent(in) :: x(:,:,:,:), x_min, x_max
    real(rkind), allocatable :: x_normalized(:,:,:,:)
    call assert(x_min/=x_max, "histogram_m(normalize): x_min/=x_max", intrinsic_array_t([x_min, x_max]))
    x_normalized = (x - x_min)/(x_max - x_min)
  end function

  module procedure construct

    integer i, j, k, n
    integer, allocatable :: bin_count(:)
    integer, parameter :: performance_threshold = 80
    real, parameter :: capture_maxval = 1.0001_rkind ! ensure maxval(v_max) falls within the highest bin
    real, allocatable :: v_mapped(:,:,:,:)

    histogram%variable_name_ = variable_name
    histogram%unmapped_min_ = minval(v)
    histogram%unmapped_max_ = maxval(v)

    allocate(histogram%bin_frequency_(num_bins))
    allocate(histogram%bin_value_(num_bins))
    allocate(bin_count(num_bins))

    associate(v_min => (histogram%unmapped_min_), v_max => (histogram%unmapped_max_), cardinality => size(v))
      if (raw)  then
        v_mapped = v
      else
        v_mapped = normalize(v, v_min, v_max)
      end if
      associate(v_mapped_min => merge(v_min, zero, raw), v_mapped_max => capture_maxval*merge(v_max, one, raw))
        associate(dv => (v_mapped_max - v_mapped_min)/real(num_bins))
          associate(v_bin_min => [(v_mapped_min + (i-1)*dv, i=1,num_bins)])
            associate(v_bin_max => [v_bin_min(2:), v_mapped_max])
              histogram%bin_value_ = half*[v_bin_min + v_bin_max] ! switching to average yields problems likely related to roundoff
              if (num_bins < performance_threshold) then
                do concurrent(i = 1:num_bins)
                  bin_count(i) = count(v_mapped >= v_bin_min(i) .and. v_mapped < v_bin_max(i))
                end do
              else
                bin_count = 0
                do i = 1,size(v_mapped,1)
                  do j = 1,size(v_mapped,2)
                    do k = 1,size(v_mapped,3)
                      do n = 1,size(v_mapped,4)
                        associate(bin => floor((v_mapped(i,j,k,n) - v_mapped_min)/dv) + 1)
                          bin_count(bin) = bin_count(bin) + 1
                        end associate
                      end do
                    end do
                  end do
                end do
                histogram%bin_frequency_ = real(bin_count,rkind) / real(cardinality,rkind)
              end if
            end associate
          end associate
        end associate
      end associate
      associate(binned => sum(bin_count))
        call assert(cardinality == binned, "histogram_s(construct): lossless binning", intrinsic_array_t([cardinality, binned]))
      end associate
    end associate

  end procedure

end submodule histogram_s
