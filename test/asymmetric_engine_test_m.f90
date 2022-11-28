! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module asymmetric_engine_test_m
  !! Define inference tests and procedures required for reporting results
  use test_m, only : test_t
  use test_result_m, only : test_result_t
  use inference_engine_m, only : inference_engine_t
  use matmul_m, only : matmul_t
  implicit none

  private
  public :: asymmetric_engine_test_t

  type, extends(test_t) :: asymmetric_engine_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "An inference_engin_t object represening XOR-AND-the-2nd-input and using the default inference_strategy" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)

    test_results = test_result_t( &
      [ character(len=len("mapping (true,true) to false using the default ('do concurrent'/dot_product) inference method")) :: &
        "mapping (true,true) to false using the default ('do concurrent'/dot_product) inference method", &
        "mapping (false,true) to true using the default inference method", &
        "mapping (true,false) to false using the default inference method", &
        "mapping (false,false) to false using the default inference method" &
      ], [xor_and_left_truth_table()] &
    )
  end function

  function xor_and_left_network() result(inference_engine)

    type(inference_engine_t) inference_engine
    integer, parameter :: n_in = 2 ! number of inputs
    integer, parameter :: n_out = 1 ! number of outputs
    integer, parameter :: neurons = 4 ! number of neurons per layer
    integer, parameter :: n_hidden = 2 ! number of hidden layers 
    integer i, j 
    real xor_into_neuron_2(neurons,neurons,n_hidden-1)
    xor_into_neuron_2 = 0.
    xor_into_neuron_2(1:3,2,1) = [1., -2., 1.]
    xor_into_neuron_2(4,4,1) = 1.
      
    inference_engine = inference_engine_t( &
      input_weights  = real(reshape([1,0,1,1,0,1,0,1], [n_in, neurons])), &
      hidden_weights = xor_into_neuron_2, &
      output_weights = real(reshape([0,1,0,1], [n_out, neurons])), &
      biases = reshape([0.,-1.99,0.,0., 0.,0.,0.,0.], [neurons, n_hidden]), &
      output_biases = [-1.] &
    )
  end function

  function xor_and_left_truth_table() result(test_passes)
    logical, allocatable :: test_passes(:)

    type(inference_engine_t) inference_engine

    inference_engine = xor_and_left_network()

    block
      real, parameter :: tolerance = 1.E-08, false = 0., true = 1.

      associate( &
        true_true => inference_engine%infer(input=[true,true]), & 
        true_false => inference_engine%infer(input=[true,false]), &
        false_true => inference_engine%infer(input=[false,true]), &
        false_false => inference_engine%infer(input=[false,false]) &
      )
        test_passes = [ &
          size(true_true)==1 .and. abs(true_true(1) - false) < tolerance, &
          size(true_false)==1 .and. abs(true_false(1) - false) < tolerance,  &
          size(false_true)==1 .and. abs(false_true(1) - true) < tolerance, &
          size(false_false)==1 .and. abs(false_false(1) - false) < tolerance  &
        ]
      end associate
    end block

  end function

end module asymmetric_engine_test_m
