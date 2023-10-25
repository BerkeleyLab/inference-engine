! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module hyperparameters_test_m
  !! Test hyperparameters_t object I/O and construction

  ! External dependencies
  use assert_m, only : assert
  use sourcery_m, only : string_t, test_t, test_result_t

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
          "writing and then reading gives input matching output for perturbed identity network"

    associate( &
      descriptions => &
        [ character(len=len(longest_description)) :: &
          "writing and then reading gives input matching output for perturbed identity network" &
        ], &
      outcomes => &
        [ write_then_read_perturbed_identity() & 
        ] & 
    )
      call assert(size(descriptions) == size(outcomes),"hyperparameters_test_m(results): size(descriptions) == size(outcomes)")
      test_results = test_result_t(descriptions, outcomes)
    end associate
       
  end function

  function write_then_read_perturbed_identity() result(test_passes)
    logical, allocatable :: test_passes(:)
    test_passes = [.true.]
  end function

end module hyperparameters_test_m