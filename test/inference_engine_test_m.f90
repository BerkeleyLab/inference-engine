! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module inference_engine_test_m
  !! Define inference tests and procedures required for reporting results
  use string_m, only : string_t
  use test_m, only : test_t
  use test_result_m, only : test_result_t
  use inference_engine_m, only : inference_engine_t
  use inputs_m, only : inputs_t
  use outputs_m, only : outputs_t
  use inference_strategy_m, only : inference_strategy_t
  use concurrent_dot_products_m, only : concurrent_dot_products_t
  use step_m, only : step_t
  use matmul_m, only : matmul_t
  use file_m, only : file_t
  use kind_parameters_m, only : rkind
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
      [ character(len=len("mapping (false,false) to false using the concurrent_dot_products_t() inference strategy")) :: &
        "mapping (true,true) to false using the concurrent_dot_products_t() inference strategy", &
        "mapping (true,false) to true using the concurrent_dot_products_t() inference strategy", &
        "mapping (false,true) to true using the concurrent_dot_products_t() inference strategy", &
        "mapping (false,false) to false using the concurrent_dot_products_t() inference strategy", &
        "mapping (true,true) to false using the matmul_t() inference strategy", &
        "mapping (true,false) to true using the matmul_t() inference strategy", &
        "mapping (false,true) to true using the matmul_t() inference strategy", &
        "mapping (false,false) to false using the matmul_t() inference strategy", &
        "converting to and from JSON format",  &
        "performing inference with encapsulated inputs and outputs", &
        "performing inference with a single-layer perceptron" &
      ], &
      [ convert_to_and_from_json(), xor_truth_table(concurrent_dot_products_t()), xor_truth_table(matmul_t()), &
        elemental_inference(), single_layer_inference() &
       ] &
    )
  end function

  function single_layer_perceptron() result(inference_engine)
    type(inference_engine_t) inference_engine
    integer, parameter :: n_in = 2 ! number of inputs
    integer, parameter :: n_out = 1 ! number of outputs
    integer, parameter :: neurons = 3 ! number of neurons per layer
    integer, parameter :: n_hidden = 1 ! number of hidden layers 
   
    inference_engine = inference_engine_t( &
      metadata = [string_t("Single-Layer XOR"), string_t("Damian Rouson"), string_t("2023-05-09"), string_t("step"), string_t("false")], &
      input_weights = real(reshape([1,0,1,1,0,1], [n_in, neurons]), rkind), &
      hidden_weights = reshape([real(rkind)::], [neurons,neurons,n_hidden-1]), &
      output_weights = real(reshape([1,-2,1], [n_out, neurons]), rkind), &
      biases = reshape([real(rkind):: 0.,-1.99,0., 0.,0.,0.], [neurons, n_hidden]), &
      output_biases = [real(rkind):: 0.] &
    )
  end function

  function single_layer_inference(inference_strategy) result(test_passes)
    logical, allocatable :: test_passes(:)
    class(inference_strategy_t), intent(in), optional :: inference_strategy
    type(inference_engine_t) inference_engine

    inference_engine = single_layer_perceptron()

    block
      type(outputs_t), allocatable :: truth_table(:)
      real(rkind), parameter :: tolerance = 1.E-08_rkind, false = 0._rkind, true = 1._rkind
      integer i

      associate(array_of_inputs => [inputs_t([true,true]), inputs_t([true,false]), inputs_t([false,true]), inputs_t([false,false])])
        truth_table = inference_engine%infer(array_of_inputs, [(matmul_t(), i=1,size(array_of_inputs))])
      end associate
      test_passes = [ &
        abs(truth_table(1)%outputs() - false) < tolerance .and. abs(truth_table(2)%outputs() - true) < tolerance .and. &
        abs(truth_table(3)%outputs() - true) < tolerance .and. abs(truth_table(4)%outputs() - false) < tolerance &
      ]
    end block
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
      metadata = [string_t("XOR"), string_t("Damian Rouson"), string_t("2023-02-18"), string_t("step"), string_t("false")], &
      input_weights = real(reshape([1,0,1,1,0,1], [n_in, neurons]), rkind), &
      hidden_weights = real(identity, rkind), &
      output_weights = real(reshape([1,-2,1], [n_out, neurons]), rkind), &
      biases = reshape([real(rkind):: 0.,-1.99,0., 0.,0.,0.], [neurons, n_hidden]), &
      output_biases = [real(rkind):: 0.] &
    )
  end function

  function convert_to_and_from_json() result(test_passes)
    logical, allocatable :: test_passes
    type(inference_engine_t) xor, difference
    real, parameter :: tolerance = 1.0E-06

    xor = xor_network()
    difference = inference_engine_t(xor%to_json()) - xor
    test_passes = difference%norm() < tolerance
  end function

  function xor_truth_table(inference_strategy) result(test_passes)
    logical, allocatable :: test_passes(:)
    class(inference_strategy_t), intent(in), optional :: inference_strategy

    type(inference_engine_t) inference_engine

    inference_engine = xor_network()

    block
      real(rkind), parameter :: tolerance = 1.E-08_rkind, false = 0._rkind, true = 1._rkind
      type(outputs_t) :: true_true, true_false, false_true, false_false

        true_true = inference_engine%infer([true,true], matmul_t())
        true_false = inference_engine%infer([true,false], matmul_t())
        false_true = inference_engine%infer([false,true], matmul_t())
        false_false = inference_engine%infer([false,false], matmul_t())

        associate( &
          true_true_outputs => true_true%outputs(), &
          true_false_outputs => true_false%outputs(), &
          false_true_outputs => false_true%outputs(), &
          false_false_outputs => false_false%outputs() &
        )
          test_passes = [ &
            size(true_true_outputs)==1 .and. abs(true_true_outputs(1) - false) < tolerance, &
            size(true_false_outputs)==1 .and. abs(true_false_outputs(1) - true) < tolerance,  &
            size(false_true_outputs)==1 .and. abs(false_true_outputs(1) - true) < tolerance, &
            size(false_false_outputs)==1 .and. abs(false_false_outputs(1) - false) < tolerance  &
          ]
        end associate
    end block

  end function

  function elemental_inference(inference_strategy) result(test_passes)
    logical, allocatable :: test_passes(:)
    class(inference_strategy_t), intent(in), optional :: inference_strategy
    type(inference_engine_t) inference_engine

    inference_engine = xor_network()

    block
      type(outputs_t), allocatable :: truth_table(:)
      real(rkind), parameter :: tolerance = 1.E-08_rkind, false = 0._rkind, true = 1._rkind
      integer i

      associate(array_of_inputs => [inputs_t([true,true]), inputs_t([true,false]), inputs_t([false,true]), inputs_t([false,false])])
        truth_table = inference_engine%infer(array_of_inputs, [(matmul_t(), i=1,size(array_of_inputs))])
      end associate
      test_passes = [ &
        abs(truth_table(1)%outputs() - false) < tolerance .and. abs(truth_table(2)%outputs() - true) < tolerance .and. &
        abs(truth_table(3)%outputs() - true) < tolerance .and. abs(truth_table(4)%outputs() - false) < tolerance &
      ]
    end block
  end function

end module inference_engine_test_m
