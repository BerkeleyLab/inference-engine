! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module inference_engine_test_m
  !! Define inference tests and procedures required for reporting results

  ! External dependencies
  use assert_m, only : assert
  use kind_parameters_m, only : rkind
  use julienne_m, only : test_t, test_result_t, test_description_t, test_description_substring, string_t, file_t
#ifdef __GFORTRAN__
  use julienne_m, only : test_function_i
#endif

  ! Internal dependencies
  use inference_engine_m, only : inference_engine_t, tensor_t, difference_t

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
    type(test_description_t), allocatable :: test_descriptions(:)

#ifndef __GFORTRAN__
    test_descriptions = [ &
      test_description_t("performing elemental inference with 1 hidden layer", elemental_infer_with_1_hidden_layer_xor_net), &
      test_description_t("performing elemental inference with 2 hidden layers", elemental_infer_with_2_hidden_layer_xor_net), &
      test_description_t("converting a network with 2 hidden layers to and from JSON format", multi_hidden_layer_net_to_from_json) &
    ]
#else
    procedure(test_function_i), pointer :: elemental_infer_1_ptr, elemental_infer_2_ptr, multi_hidden_ptr
    elemental_infer_1_ptr => elemental_infer_with_1_hidden_layer_xor_net
    elemental_infer_2_ptr => elemental_infer_with_2_hidden_layer_xor_net
    multi_hidden_ptr => multi_hidden_layer_net_to_from_json

    test_descriptions = [ &
      test_description_t("performing elemental inference with 1 hidden layer", elemental_infer_1_ptr), &
      test_description_t("performing elemental inference with 2 hidden layers", elemental_infer_2_ptr), &
      test_description_t("converting a network with 2 hidden layers to and from JSON format", multi_hidden_ptr) &
    ]
#endif
    associate( &
      substring_in_subject => index(subject(), test_description_substring) /= 0, &
      substring_in_description => test_descriptions%contains_text(string_t(test_description_substring)) &
    )
      test_descriptions = pack(test_descriptions, substring_in_subject .or. substring_in_description)
    end associate
    test_results = test_descriptions%run()
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

  function distinct_parameters() result(inference_engine)
    type(inference_engine_t) inference_engine
    integer, parameter :: inputs = 2, hidden = 3, outputs = 1 ! number of neurons in input, output, and hidden layers
    integer, parameter :: n(*) = [inputs, hidden, hidden, outputs]    ! nodes per layer
    integer, parameter :: n_max = maxval(n), layers=size(n)   ! max layer width, number of layers
    integer, parameter :: w_shape(*) = [n_max, n_max, layers-1], b_shape(*) = [n_max, n_max]
    integer i
    real(rkind), allocatable :: w(:,:,:), b(:,:)

    w = reshape( [(i, i=1,product(w_shape))], w_shape)
    b = reshape( [(maxval(w) + i, i=1,product(b_shape))], b_shape)

    inference_engine = inference_engine_t( &
      metadata = [string_t("random"), string_t("Damian Rouson"), string_t("2023-07-15"), string_t("sigmoid"), string_t("false")], &
      weights = w, biases = b, nodes = n &
    )   
  end function

  function multi_hidden_layer_net_to_from_json() result(test_passes)
    logical test_passes
    type(inference_engine_t) inference_engine, from_json
    type(file_t) json_file
    type(difference_t) difference
    real, parameter :: tolerance = 1.0E-06

    inference_engine = distinct_parameters()
    json_file = inference_engine%to_json()
    from_json = inference_engine_t(json_file)
    difference = inference_engine  - from_json
    test_passes = difference%norm() < tolerance
  end function

  function elemental_infer_with_1_hidden_layer_xor_net() result(test_passes)
    logical test_passes
    type(inference_engine_t) inference_engine

    inference_engine = single_hidden_layer_xor_network()

    block
      type(tensor_t), allocatable :: truth_table(:)
      real(rkind), parameter :: tolerance = 1.E-08_rkind, false = 0._rkind, true = 1._rkind
      integer i

      associate(array_of_inputs => [tensor_t([true,true]), tensor_t([true,false]), tensor_t([false,true]), tensor_t([false,false])])
        truth_table = inference_engine%infer(array_of_inputs)
      end associate
      test_passes = all( &
        abs(truth_table(1)%values() - false) < tolerance .and. abs(truth_table(2)%values() - true) < tolerance .and. &
        abs(truth_table(3)%values() - true) < tolerance .and. abs(truth_table(4)%values() - false) < tolerance &
      )
    end block
  end function

  function elemental_infer_with_2_hidden_layer_xor_net() result(test_passes)
    logical test_passes
    type(inference_engine_t) inference_engine

    inference_engine = multi_layer_xor_network()

    block
      type(tensor_t), allocatable :: truth_table(:)
      real(rkind), parameter :: tolerance = 1.E-08_rkind, false = 0._rkind, true = 1._rkind
      integer i

      associate(array_of_inputs => [tensor_t([true,true]), tensor_t([true,false]), tensor_t([false,true]), tensor_t([false,false])])
        truth_table = inference_engine%infer(array_of_inputs)
      end associate
      test_passes = all( &
        abs(truth_table(1)%values() - false) < tolerance .and. abs(truth_table(2)%values() - true) < tolerance .and. &
        abs(truth_table(3)%values() - true) < tolerance .and. abs(truth_table(4)%values() - false) < tolerance &
      )
    end block
  end function

end module inference_engine_test_m
