! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module inference_engine_test_m
  !! Define inference tests and procedures required for reporting results

  ! External dependencies
  use assert_m, only : assert
  use kind_parameters_m, only : rkind
  use string_m, only : string_t
  use test_m, only : test_t
  use test_result_m, only : test_result_t

  ! Internal dependencies
  use inference_engine_m, only : inference_engine_t, inputs_t, outputs_t, difference_t

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

    character(len=*), parameter :: longest_description = &
          "converting a single-hidden-layer network to and from JSON format"

    associate( &
      descriptions => &
        [ character(len=len(longest_description)) :: &
          "performing elemental inference with 1 hidden layer", &
          "performing elemental inference with 2 hidden layers", &
          "converting a single-hidden-layer network to and from JSON format",  &
          "converting a multi-hidden-layer network to and from JSON format"  &
        ], &
      outcomes => &
        [ elemental_infer_with_1_hidden_layer_xor_net(), elemental_infer_with_2_hidden_layer_xor_net(), &
          single_hidden_layer_net_to_from_json(), multi_hidden_layer_net_to_from_json() &
        ] &
    )
      call assert(size(descriptions) == size(outcomes), "inference_engine_test(results): size(descriptions) == size(outcomes)")
      test_results = test_result_t(descriptions, outcomes)
    end associate
  end function

  function single_hidden_layer_xor_network() result(inference_engine)
    type(inference_engine_t) inference_engine
    integer, parameter :: nodes_per_layer(*) = [2, 3, 1]
    integer, parameter :: max_n = maxval(nodes_per_layer), layers = size(nodes_per_layer)

    inference_engine = inference_engine_t( &
      metadata = [string_t("XOR"), string_t("Damian Rouson"), string_t("2023-07-02"), string_t("step"), string_t("false")], &
      weights = reshape([real(rkind):: [1,1,0, 0,1,1, 0,0,0], [1,0,0, -2,0,0, 1,0,0]], [max_n, max_n, layers-1]), &
      biases = reshape([[0.,-1.99,0.], [0., 0., 0.]], [max_n, layers-1]), &
      nodes = nodes_per_layer &
    )
  end function

  function multi_layer_xor_network() result(inference_engine)
    type(inference_engine_t) inference_engine
    integer, parameter :: nodes_per_layer(*) = [2, 3, 3, 1]
    integer, parameter :: max_n = maxval(nodes_per_layer), layers = size(nodes_per_layer)

    inference_engine = inference_engine_t( &
      metadata = [string_t("XOR"), string_t("Damian Rouson"), string_t("2023-07-02"), string_t("step"), string_t("false")], &
      weights = reshape([real(rkind):: [1,1,0, 0,1,1, 1,0,0, 1,0,0, 0,1,0, 0,0,1], [1,0,0, -2,0,0, 1,0,0]], &
        [max_n, max_n, layers-1]), &
      biases = reshape([[0.,-1.99,0.], [0., 0., 0.], [0., 0., 0.]], [max_n, layers-1]), &
      nodes = nodes_per_layer &
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
      biases = reshape([real(rkind):: 0.,-1.99,0.], [neurons, n_hidden]), &
      output_biases = [real(rkind):: 0.] &
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
      metadata = [string_t("XOR"), string_t("Damian Rouson"), string_t("2023-02-18"), string_t("step"), string_t("false")], &
      input_weights = real(reshape([1,0,1,1,0,1], [n_in, neurons]), rkind), &
      hidden_weights = real(identity, rkind), &
      output_weights = real(reshape([1,-2,1], [n_out, neurons]), rkind), &
      biases = reshape([real(rkind):: 0.,-1.99,0., 0.,0.,0.], [neurons, n_hidden]), &
      output_biases = [real(rkind):: 0.] &
    )
  end function

  function multi_hidden_layer_net_to_from_json() result(test_passes)
    logical, allocatable :: test_passes
    type(inference_engine_t) xor
    type(difference_t) difference
    real, parameter :: tolerance = 1.0E-06

    xor = xor_network()
    difference = inference_engine_t(xor%to_json()) - xor
    test_passes = difference%norm() < tolerance
  end function

  function single_hidden_layer_net_to_from_json() result(test_passes)
    logical, allocatable :: test_passes
    type(inference_engine_t) one_hidden_layer_network
    type(difference_t) difference

    real, parameter :: tolerance = 1.0E-06

    one_hidden_layer_network = single_layer_perceptron()
    difference = inference_engine_t(one_hidden_layer_network%to_json()) - one_hidden_layer_network
    test_passes = difference%norm() < tolerance
  end function

  function elemental_infer_with_1_hidden_layer_xor_net() result(test_passes)
    logical, allocatable :: test_passes(:)
    type(inference_engine_t) inference_engine

    inference_engine = single_hidden_layer_xor_network()

    block
      type(outputs_t), allocatable :: truth_table(:)
      real(rkind), parameter :: tolerance = 1.E-08_rkind, false = 0._rkind, true = 1._rkind
      integer i

      associate(array_of_inputs => [inputs_t([true,true]), inputs_t([true,false]), inputs_t([false,true]), inputs_t([false,false])])
        truth_table = inference_engine%infer(array_of_inputs)
      end associate
      test_passes = [ &
        abs(truth_table(1)%outputs() - false) < tolerance .and. abs(truth_table(2)%outputs() - true) < tolerance .and. &
        abs(truth_table(3)%outputs() - true) < tolerance .and. abs(truth_table(4)%outputs() - false) < tolerance &
      ]
    end block
  end function

  function elemental_infer_with_2_hidden_layer_xor_net() result(test_passes)
    logical, allocatable :: test_passes(:)
    type(inference_engine_t) inference_engine

    inference_engine = multi_layer_xor_network()

    block
      type(outputs_t), allocatable :: truth_table(:)
      real(rkind), parameter :: tolerance = 1.E-08_rkind, false = 0._rkind, true = 1._rkind
      integer i

      associate(array_of_inputs => [inputs_t([true,true]), inputs_t([true,false]), inputs_t([false,true]), inputs_t([false,false])])
        truth_table = inference_engine%infer(array_of_inputs)
      end associate
      test_passes = [ &
        abs(truth_table(1)%outputs() - false) < tolerance .and. abs(truth_table(2)%outputs() - true) < tolerance .and. &
        abs(truth_table(3)%outputs() - true) < tolerance .and. abs(truth_table(4)%outputs() - false) < tolerance &
      ]
    end block
  end function

end module inference_engine_test_m
