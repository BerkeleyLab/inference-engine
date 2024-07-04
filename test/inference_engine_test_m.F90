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
       test_description_t("performing elemental inference with 1 hidden layer", elemental_infer_with_1_hidden_layer_xor_net) &
      ,test_description_t("performing elemental inference with 2 hidden layers", elemental_infer_with_2_hidden_layer_xor_net) &
      ,test_description_t("converting a network with 2 hidden layers to and from JSON format", multi_hidden_layer_net_to_from_json)&
      ,test_description_t("converting a network with varying-width hidden layers to/from JSON", varying_width_net_to_from_json) &
      ,test_description_t("performing inference with a network with hidden layers of varying width", infer_with_varying_width_net) &
    ]
#else
    procedure(test_function_i), pointer :: &
      elemental_infer_1_ptr, elemental_infer_2_ptr, multi_hidden_ptr, vary_width_ptr, vary_width_infer_ptr
    elemental_infer_1_ptr => elemental_infer_with_1_hidden_layer_xor_net
    elemental_infer_2_ptr => elemental_infer_with_2_hidden_layer_xor_net
    multi_hidden_ptr => multi_hidden_layer_net_to_from_json
    vary_width_ptr => varying_width_net_to_from_json
    vary_width_infer_ptr => infer_with_varying_width_net

    test_descriptions = [ &
       test_description_t("performing elemental inference with 1 hidden layer", elemental_infer_1_ptr) &
      ,test_description_t("performing elemental inference with 2 hidden layers", elemental_infer_2_ptr) &
      ,test_description_t("converting a network with 2 hidden layers to and from JSON format", multi_hidden_ptr) &
      ,test_description_t("converting a network with varying-width hidden layers to/from JSON", vary_width_ptr) &
      ,test_description_t("performing inference with varyring-width hidden layers", vary_width_infer_ptr)  &
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

  function decrement_split_combine_increment() result(inference_engine)
    !! Define a network that produces outputs identical to the 2 inputs for any input greater than or equal to 1
    !! based on the following algorithm:
    !! 1. A 1st hidden layer that forwards input 1 unmolested and decrements input 2 by 1,
    !! 2. A 2nd hidden layer that forwards input 1 unmolested and splits input 2 into two halves,
    !! 3. An output layer that recombines those two halves and increments the result by 1.
    type(inference_engine_t) inference_engine
    integer, parameter :: inputs = 2, hidden(*) = [2,3], outputs = 2 ! number of neurons in input, output, and hidden layers
    integer, parameter :: n(*) = [inputs, hidden(1), hidden(2), outputs] ! nodes per layer
    integer, parameter :: n_max = maxval(n), layers=size(n) ! max layer width, number of layers
    integer, parameter :: w_shape(*) = [n_max, n_max, layers-1], b_shape(*) = [n_max, n_max]
    real(rkind), parameter :: & 
      w(*,*,*) = reshape( [1.,0.,0., 0.,1.,0., 0.,0.,0., 1.,0.,0., 0.,.5,.5, 0.,0.,0., 1.,0.,0., 0.,1.,0., 0.,1.,1.], w_shape), &
      b(*,*) = reshape( [0.,-1.,0., 0.,0.,0., 0.,1.,0.], b_shape)

    inference_engine = inference_engine_t( &
      metadata = [ &
        string_t("Decrement/Split/Combine/Increment => Identity") &
       ,string_t("Damian Rouson") &
       ,string_t("2024-07-03") &
       ,string_t("relu") &
       ,string_t("false") &
     ], weights = w, biases = b, nodes = n &
    )
  end function

  function varying_width() result(inference_engine)
    type(inference_engine_t) inference_engine
    integer, parameter :: inputs = 2, hidden(*) = [2,3], outputs = 2 ! number of neurons in input, output, and hidden layers
    integer, parameter :: n(*) = [inputs, hidden(1), hidden(2), outputs] ! nodes per layer
    integer, parameter :: n_max = maxval(n), layers=size(n)   ! max layer width, number of layers
    integer, parameter :: w_shape(*) = [n_max, n_max, layers-1], b_shape(*) = [n_max, n_max]
    integer i
    real(rkind), allocatable :: w(:,:,:), b(:,:)

    w = reshape( [(i, i=1,product(w_shape))], w_shape)
    b = reshape( [(maxval(w) + i, i=1,product(b_shape))], b_shape)

    inference_engine = inference_engine_t( &
      metadata = [string_t("random"), string_t("Damian Rouson"), string_t("2024-07-03"), string_t("sigmoid"), string_t("false")], &
      weights = w, biases = b, nodes = n &
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

  function varying_width_net_to_from_json() result(test_passes)
    logical test_passes
    real, parameter :: tolerance = 1.0E-06
    type(difference_t) difference

    associate(inference_engine => varying_width())
      associate(from_json => inference_engine_t( inference_engine%to_json() ))
        difference = inference_engine  - from_json
        test_passes = difference%norm() < tolerance
      end associate
    end associate
  end function

  function infer_with_varying_width_net() result(test_passes)
    logical test_passes
    type(inference_engine_t) inference_engine
    type(tensor_t) inputs, outputs
    real(rkind), parameter :: tolerance = 1.E-08_rkind

    inference_engine = decrement_split_combine_increment()
    inputs = tensor_t([1.1, 2.7])
    outputs = inference_engine%infer(inputs)
    test_passes = all(abs(inputs%values() - outputs%values()) < tolerance)
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
