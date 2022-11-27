! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module inference_engine_test_m
  !! Define inference tests and procedures required for reporting results
  use string_m, only : string_t
  use test_m, only : test_t
  use test_result_m, only : test_result_t
  use inference_engine_m, only : inference_engine_t
  use matmul_m, only : matmul_t
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
      [ character(len=len("mapping (true,true) to false using the default ('do concurrent'/dot_product) inference method")) :: &
        "mapping (true,true) to false using the default ('do concurrent'/dot_product) inference method", &
        "mapping (false,true) to true using the default inference method", &
        "mapping (true,false) to true using the default inference method", &
        "mapping (false,false) to false using the default inference method", &
        "writing and then reading itself to and from a file", &
        "mapping (true,true) to false using `matmul`-based inference method", &
        "mapping (false,true) to true using `matmul`-based inference method", &
        "mapping (true,false) to true using `matmul`-based inference method", &
        "mapping (false,false) to false using `matmul`-based inference method" &
      ], [xor_truth_table(), write_then_read(), matmul_inference()] &
    )
  end function

  function xor_network() result(inference_engine)

    type(inference_engine_t) inference_engine
    integer, parameter :: n_in = 2 ! number of inputs
    integer, parameter :: n_out = 1 ! number of outputs
    integer, parameter :: neurons = 3 ! number of neurons per layer
    integer, parameter :: n_hidden = 2 ! number of hidden layers 
    integer i, j 
    integer, parameter :: identity(*,*,*) = &
      reshape([((merge(1,0,i==j), i=1,neurons), j=1,neurons)], shape=[neurons,neurons,n_hidden-1])
   
    inference_engine = inference_engine_t( &
      input_weights = real(reshape([1,0,1,1,0,1], [n_in, neurons])), &
      hidden_weights = real(identity), &
      output_weights = real(reshape([1,-2,1], [n_out, neurons])), &
      biases = reshape([0.,-1.99,0., 0.,0.,0.], [neurons, n_hidden]), &
      output_biases = [0.] &
    )
  end function

  function xor_matmul_network() result(inference_engine)

    type(inference_engine_t) inference_engine
    integer, parameter :: n_in = 2 ! number of inputs
    integer, parameter :: n_out = 1 ! number of outputs
    integer, parameter :: neurons = 3 ! number of neurons per layer
    integer, parameter :: n_hidden = 2 ! number of hidden layers 
    integer i, j 
    integer, parameter :: identity(*,*,*) = &
      reshape([((merge(1,0,i==j), i=1,neurons), j=1,neurons)], shape=[neurons,neurons,n_hidden-1])
   
    inference_engine = inference_engine_t( &
      input_weights = real(reshape([1,0,1,1,0,1], [n_in, neurons])), &
      hidden_weights = real(identity), &
      output_weights = real(reshape([1,-2,1], [n_out, neurons])), &
      biases = reshape([0.,-1.99,0., 0.,0.,0.], [neurons, n_hidden]), &
      output_biases = [0.], &
      inference_strategy = matmul_t() & 
    )
  end function

  function write_then_read() result(test_passes)
    logical, allocatable :: test_passes

    type(inference_engine_t) xor_written, xor_read, difference
    integer i, j
    integer, parameter :: identity(*,*,*) = reshape([((merge(1,0,i==j), i=1,3), j=1,3)], shape=[3,3,1])
    integer, parameter :: num_inputs=2, num_outputs=1, nuerons_per_layer=3

    xor_written = xor_network()
    call xor_written%write_network(string_t("build/write_then_read_test_specimen"))
    call xor_read%read_network(string_t("build/write_then_read_test_specimen"))

    block 
      type(inference_engine_t) difference
      real, parameter :: tolerance = 1.0E-06

      difference = xor_read - xor_written
      test_passes = difference%norm() < tolerance
    end block
  end function

  function xor_truth_table() result(test_passes)
    logical, allocatable :: test_passes(:)

    type(inference_engine_t) inference_engine

    inference_engine = xor_network()

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
          size(true_false)==1 .and. abs(true_false(1) - true) < tolerance,  &
          size(false_true)==1 .and. abs(false_true(1) - true) < tolerance, &
          size(false_false)==1 .and. abs(false_false(1) - false) < tolerance  &
        ]
      end associate
    end block

  end function

  function matmul_inference() result(test_passes)
    logical, allocatable :: test_passes(:)

    type(inference_engine_t) inference_engine
   
    inference_engine = xor_matmul_network()

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
          size(true_false)==1 .and. abs(true_false(1) - true) < tolerance,  &
          size(false_true)==1 .and. abs(false_true(1) - true) < tolerance, &
          size(false_false)==1 .and. abs(false_false(1) - false) < tolerance  &
        ]
      end associate
    end block

  end function

end module inference_engine_test_m
