! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module hyperparameters_test_m
  !! Test hyperparameters_t object I/O and construction

  ! External dependencies
  use assert_m, only : assert
  use inference_engine_m, only : hyperparameters_t
  use sourcery_m, only : test_t, test_result_t, test_description_t, test_description_substring, string_t
#ifdef __GFORTRAN__
  use sourcery_m, only : test_function_i
#endif

  ! Internal dependencies
  use hyperparameters_m, only : hyperparameters_t

  implicit none

  private
  public :: hyperparameters_test_t

  type, extends(test_t) :: hyperparameters_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "A hyperparameters_t object"
  end function

  function results() result(test_results)
    type(test_description_t), allocatable :: test_descriptions(:)
    type(test_result_t), allocatable :: test_results(:)

#ifndef __GFORTRAN__
    test_descriptions = [ & 
      test_description_t( &
        string_t("component-wise construction followed by conversion to and from JSON"), &
        write_then_read_hyperparameters) &
    ]
#else
    procedure(test_function_i), pointer :: check_write_then_read_ptr
    check_write_then_read_ptr => write_then_read_hyperparameters

    test_descriptions = [ &
      test_description_t( &
        string_t("component-wise construction followed by conversion to and from JSON"), &
        check_write_then_read_ptr) &
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

  function write_then_read_hyperparameters() result(test_passes)
    logical test_passes

    associate(hyperparameters => hyperparameters_t(mini_batches=5, learning_rate=1., optimizer = "stochastic gradient descent"))
      associate(from_json => hyperparameters_t(hyperparameters%to_json()))
        test_passes = hyperparameters == from_json
      end associate
    end associate
  end function

end module hyperparameters_test_m