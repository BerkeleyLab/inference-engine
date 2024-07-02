! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module tensor_range_test_m
  !! Test tensor_range_t object input/output, construction, and linear mappings

  ! External dependencies
  use assert_m, only : assert
  use julienne_m, only : string_t, test_t, test_result_t, test_description_t, test_description_substring, file_t
  use inference_engine_m, only : tensor_range_t, tensor_t
#ifdef __GFORTRAN__
  use julienne_m, only : test_function_i
#endif

  ! Internal dependencies
  use tensor_range_m, only : tensor_range_t

  implicit none

  private
  public :: tensor_range_test_t

  type, extends(test_t) :: tensor_range_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "A tensor_range_t object"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#ifndef __GFORTRAN__
    test_descriptions = [ & 
      test_description_t("component-wise construction followed by conversion to and from JSON", write_then_read_tensor_range), &
      test_description_t("mapping to and from the unit interval as an identity transformation", map_to_from_training_range) &
    ]
#else
    procedure(test_function_i), pointer :: check_write_then_read_ptr, check_map_to_from_range_ptr 
    check_write_then_read_ptr   => write_then_read_tensor_range
    check_map_to_from_range_ptr => map_to_from_training_range

    test_descriptions = [ &
      test_description_t("component-wise construction followed by conversion to and from JSON", check_write_then_read_ptr), &
      test_description_t("mapping to and from the unit interval as an identity transformation", check_map_to_from_range_ptr) &
    ]
#endif
    associate( &
      substring_in_subject => index(subject(), test_description_substring) /= 0, &
      substring_in_description => test_descriptions%contains_text(string_t(test_description_substring)) &
    )
      test_descriptions = pack(test_descriptions, substring_in_subject .or. substring_in_description)
    end associate
    test_results = test_descriptions%run()
  end function

  function write_then_read_tensor_range() result(test_passes)
    logical test_passes
    type(file_t) :: json_file
#ifdef _CRAYFTN
    type(tensor_range_t) :: tensor_range
    tensor_range = tensor_range_t(layer="inputs", minima=[-1., 0., 1.], maxima=[1., 2., 4.])
#else
    associate(tensor_range => tensor_range_t(layer="inputs", minima=[-1., 0., 1.], maxima=[1., 2., 4.]))
#endif
      associate(from_json => tensor_range_t(tensor_range%to_json()))
        test_passes = tensor_range == from_json
      end associate
#ifndef _CRAYFTN
    end associate
#endif
  end function

  function map_to_from_training_range() result(test_passes)
    logical test_passes
    real, parameter :: tolerance = 1.E-07
#ifdef _CRAYFTN
    type(tensor_range_t) :: tensor_range
    type(tensor_t) :: tensor, round_trip
    tensor_range = tensor_range_t(layer="output", minima=[-4., 0., 1., -1.], maxima=[0., 2., 5., 1.])
    tensor = tensor_t([-2., 0., 5., 0.])
    round_trip = tensor_range%map_from_training_range(tensor_range%map_to_training_range(tensor))
    test_passes = all(abs(tensor%values() - round_trip%values()) < tolerance)
#else
    associate(tensor_range => tensor_range_t(layer="output", minima=[-4., 0., 1., -1.], maxima=[0., 2., 5., 1.]))
      associate(tensor => tensor_t([-2., 0., 5., 0.]))
        associate(round_trip => tensor_range%map_from_training_range(tensor_range%map_to_training_range(tensor)))
          test_passes = all(abs(tensor%values() - round_trip%values()) < tolerance)
        end associate
      end associate
    end associate
#endif
  end function

end module tensor_range_test_m
