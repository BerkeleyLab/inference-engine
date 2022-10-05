module inference_engine_test
  !! Define inference tests and procedures required for reporting results
  use test_m, only : test_t
  use test_result_m, only : test_result_t
  use inference_engine_m, only : inference_engine_t
  implicit none

  private
  public :: inference_engine_test_t

  type, extends(test_t) :: inference_engine_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The inference_engine_t type" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)

    test_results = [ & 
      test_result_t("a trivial inference gives the expected result", trivial_inference_gives_expected_answer()) &
    ]   
  end function
  
  function trivial_inference_gives_expected_answer() result(test_passes)
    logical test_passes

    type(inference_engine_t) inference_engine

    ! test goes here

    test_passes = .true.
  end function
   

end module inference_engine_test
