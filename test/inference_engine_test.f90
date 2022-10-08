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

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "An inference_engine_t that encodes an XOR gate" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)

    test_results = test_result_t( &
      [ character(len=len("mapping (false,false) to false")) :: &
        "mapping (true,true) to false", &
        "mapping (false,true) to true", &
        "mapping (true,false) to true", &
        "mapping (false,false) to false" &
      ], xor_truth_table() &
    )
  end function

  function xor_truth_table() result(test_passes)
    logical, allocatable :: test_passes(:)

    procedure(activation_function), pointer :: f
    type(inference_engine_t) xor
    integer i, j
    integer, parameter :: identity(*,*,*) = reshape([((merge(1,0,i==j), i=1,3), j=1,3)], shape=[3,3,1])

    f => step
   
    xor = inference_engine_t( &
      input_weights = real(reshape([1,1,0,0,1,1], [3,2])), &
      hidden_weights = real(identity), &
      output_weights = real(reshape([1,-2,1], [1,3])), &
      biases = reshape([0.,-1.99,0., 0.,0.,0.], [3,2]), &
      activation = f &
    )

    block
      real, parameter :: tolerance = 1.E-08, false = 0., true = 1.

      associate( &
        true_true => xor%infer(input=[true,true]), & 
        true_false => xor%infer(input=[true,false]), &
        false_true => xor%infer(input=[false,true]), &
        false_false => xor%infer(input=[false,false]) &
      )
        test_passes = [ &
          size(true_true)==1 .and. abs(true_true(1) - false) < tolerance, &
          size(true_false)==1 .and. abs(true_false(1) - true) < tolerance,  &
          size(false_true)==1 .and. abs(false_true(1) - true) < tolerance, &
          size(false_false)==1 .and. abs(false_false(1) - false) < tolerance  &
        ]
      end associate
    end block

  contains

    pure function step(x) result(y)
      real, intent(in) :: x
      real y
      y = merge(1., 0., x>0.)
    end function
  
  end function

end module inference_engine_test
