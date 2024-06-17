! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module training_configuration_test_m
  !! Test training_configuration_t object I/O and construction

  ! External dependencies
  use inference_engine_m, only : training_configuration_t, hyperparameters_t, network_configuration_t
  use julienne_m, only : test_t, test_result_t, test_description_t, test_description_substring, string_t, file_t
#ifdef __GFORTRAN__
  use julienne_m, only : test_function_i
#endif

  ! Internal dependencies
  use training_configuration_m, only : training_configuration_t
  implicit none

  private
  public :: training_configuration_test_t

  type, extends(test_t) :: training_configuration_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "A training_configuration_t object"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#ifndef __GFORTRAN__
    test_descriptions = [ & 
      test_description_t( &
        string_t("component-wise construction followed by conversion to and from JSON"), &
        construct_and_convert_to_and_from_json) & 
    ]
#else
    procedure(test_function_i), pointer :: check_write_then_read_ptr
    check_write_then_read_ptr => construct_and_convert_to_and_from_json

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

  function construct_and_convert_to_and_from_json() result(test_passes)
    logical test_passes
#ifdef _CRAYFTN
    type(training_configuration_t) :: training_configuration, from_json
    training_configuration = training_configuration_t( &
      hyperparameters_t(mini_batches=5, learning_rate=1., optimizer = "adam"), &
      network_configuration_t(skip_connections=.false., nodes_per_layer=[2,72,2], activation_name="sigmoid"))
    from_json = training_configuration_t(file_t(training_configuration%to_json()))
#else
    associate(training_configuration => training_configuration_t( &
      hyperparameters_t(mini_batches=5, learning_rate=1., optimizer = "adam"), &
      network_configuration_t(skip_connections=.false., nodes_per_layer=[2,72,2], activation_name="sigmoid") &
    ))
      associate(from_json => training_configuration_t(file_t(training_configuration%to_json())))
#endif
        test_passes = training_configuration == from_json
#ifndef _CRAYFTN
      end associate
    end associate
#endif
  end function

end module training_configuration_test_m
