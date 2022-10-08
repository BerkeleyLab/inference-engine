module inference_engine_test
  !! Define inference tests and procedures required for reporting results
  use test_m, only : test_t
  use test_result_m, only : test_result_t
  use inference_engine_m, only : inference_engine_t, activation_function
  implicit none

  private
  public :: inference_engine_test_t

  type, extends(test_t) :: inference_engine_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  real, parameter :: false = 0., true = 1.

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "An inference_engine_t that expresses an XOR gate" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)

    test_results = test_result_t( &
      ["mapping (false,false) to false", "mapping (false,true) to true  "], &
      xor_false_true_is_true() &
    )
  end function

  pure function step(x) result(y)
    real, intent(in) :: x
    real y
    y = merge(1., 0., x>0.)
  end function
  
  function xor_false_true_is_true() result(test_passes)
    logical, allocatable :: test_passes(:)
    integer i, j
    integer, parameter :: identity(*,*,*) = reshape([((merge(1,0,i==j), i=1,3), j=1,3)], shape=[3,3,1])
    real, allocatable :: output(:)
    procedure(activation_function), pointer :: f
    type(inference_engine_t) xor
    
    f => step
   
    xor = inference_engine_t( &
      input_weights = real(reshape([1,1,0,0,1,1], [3,2])), &
      hidden_weights = real(identity), &
      output_weights = real(reshape([1,-2,1], [1,3])), &
      biases = reshape([0.,-1.99,0., 0.,0.,0.], [3,2]), &
      activation = f &
    )

    output = xor%infer(input=[true,false])

    block
      real, parameter :: tolerance = 1.E-01, expected_output=1.

      test_passes = [ &
       size(output)==1 .and. abs(output(1) - expected_output)/expected_output < tolerance, &
       size(output)==1 .and. abs(output(1) - expected_output)/expected_output < tolerance  &
      ]
    end block
  end function
   

end module inference_engine_test
