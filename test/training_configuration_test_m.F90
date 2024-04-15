! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module training_configuration_test_m
  !! Test training_configuration_t object I/O and construction

  ! External dependencies
  use assert_m, only : assert
  use sourcery_m, only : string_t, test_t, test_result_t, file_t
  use inference_engine_m, only : training_configuration_t, hyperparameters_t, network_configuration_t

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

    character(len=*), parameter :: longest_description = &
          "component-wise construction followed by conversion to and from JSON"

    associate( &
      descriptions => &
        [ character(len=len(longest_description)) :: &
          "component-wise construction followed by conversion to and from JSON" &
        ], &
      outcomes => &
        [ construct_and_convert_to_and_from_json() & 
        ] & 
    )
      call assert(size(descriptions) == size(outcomes),"training_configuration_test_m(results): size(descriptions)==size(outcomes)")
      test_results = test_result_t(descriptions, outcomes)
    end associate
       
  end function

  function construct_and_convert_to_and_from_json() result(test_passes)
    logical test_passes
#ifdef _CRAYFTN
    type(training_configuration_t) :: training_configuration
    training_configuration = training_configuration_t( &
      hyperparameters_t(mini_batches=5, learning_rate=1., optimizer = "adam"), &
      network_configuration_t(skip_connections=.false., nodes_per_layer=[2,72,2], activation_name="sigmoid"))
#else
    associate(training_configuration => training_configuration_t( &
      hyperparameters_t(mini_batches=5, learning_rate=1., optimizer = "adam"), &
      network_configuration_t(skip_connections=.false., nodes_per_layer=[2,72,2], activation_name="sigmoid") &
    ))
#endif
      associate(from_json => training_configuration_t(file_t(training_configuration%to_json())))
        test_passes = training_configuration == from_json
      end associate
#ifndef _CRAYFTN
    end associate
#endif
  end function

end module training_configuration_test_m
