! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module hyperparameters_test_m
  !! Test hyperparameters_t object I/O and construction

  ! External dependencies
  use assert_m, only : assert
  use sourcery_m, only : string_t, test_t, test_result_t, file_t
  use inference_engine_m, only : hyperparameters_t

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
    type(test_result_t), allocatable :: test_results(:)

    character(len=*), parameter :: longest_description = &
          "component-wise construction followed by conversion to and from JSON"

    associate( &
      descriptions => &
        [ character(len=len(longest_description)) :: &
          "component-wise construction followed by conversion to and from JSON" &
        ], &
      outcomes => &
        [ write_then_read_hyperparameters() & 
        ] & 
    )
      call assert(size(descriptions) == size(outcomes),"hyperparameters_test_m(results): size(descriptions) == size(outcomes)")
      test_results = test_result_t(descriptions, outcomes)
    end associate
       
  end function

  function write_then_read_hyperparameters() result(test_passes)
    logical test_passes
#ifdef _CRAYFTN
    type(hyperparameters_t) :: hyperparameters
    hyperparameters = hyperparameters_t(mini_batches=5, learning_rate=1., optimizer = "stochastic gradient descent")
#else
    associate(hyperparameters => hyperparameters_t(mini_batches=5, learning_rate=1., optimizer = "stochastic gradient descent"))
#endif
      associate(from_json => hyperparameters_t(hyperparameters%to_json()))
        test_passes = hyperparameters == from_json
      end associate
#ifndef _CRAYFTN
    end associate
#endif
  end function

end module hyperparameters_test_m
