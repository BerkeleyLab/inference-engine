! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module neural_network_test_m
  !! Define inference tests and procedures required for reporting results

  ! External dependencies
  use assert_m, only : assert
  use kind_parameters_m, only : double_precision
  use julienne_m, only : test_t, test_result_t, test_description_t, test_description_substring, string_t, file_t
#ifdef __GFORTRAN__
  use julienne_m, only : test_function_i
#endif

  ! Internal dependencies
  use fiats_m, only : neural_network_t, tensor_t, metadata_t

  implicit none

  private
  public :: neural_network_test_t

  type, extends(test_t) :: neural_network_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "An neural_network_t that encodes an XOR gate" 
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
      ,test_description_t("double-precision inference", double_precision_inference) &
    ]
#else
    procedure(test_function_i), pointer :: elemental_infer_1_ptr, elemental_infer_2_ptr, multi_hidden_ptr, vary_width_ptr, &
      vary_width_infer_ptr, double_precision_inference_ptr

    elemental_infer_1_ptr => elemental_infer_with_1_hidden_layer_xor_net
    elemental_infer_2_ptr => elemental_infer_with_2_hidden_layer_xor_net
    multi_hidden_ptr => multi_hidden_layer_net_to_from_json
    vary_width_ptr => varying_width_net_to_from_json
    vary_width_infer_ptr => infer_with_varying_width_net
    double_precision_inference_ptr => double_precision_inference

    test_descriptions = [ &
       test_description_t("performing elemental inference with 1 hidden layer", elemental_infer_1_ptr) &
      ,test_description_t("performing elemental inference with 2 hidden layers", elemental_infer_2_ptr) &
      ,test_description_t("converting a network with 2 hidden layers to and from JSON format", multi_hidden_ptr) &
      ,test_description_t("converting a network with varying-width hidden layers to/from JSON", vary_width_ptr) &
      ,test_description_t("performing inference with varying-width hidden layers", vary_width_infer_ptr)  &
      ,test_description_t("double-precision inference", double_precision_inference_ptr) &
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

  function single_hidden_layer_xor_network() result(neural_network)
    type(neural_network_t) neural_network
    integer, parameter :: nodes_per_layer(*) = [2, 3, 1]
    integer, parameter :: max_n = maxval(nodes_per_layer), layers = size(nodes_per_layer)

    neural_network = neural_network_t( &
      metadata = [string_t("XOR"), string_t("Damian Rouson"), string_t("2023-07-02"), string_t("step"), string_t("false")], &
      weights = reshape([real:: [1,1,0, 0,1,1, 0,0,0], [1,0,0, -2,0,0, 1,0,0]], [max_n, max_n, layers-1]), &
      biases = reshape([[0.,-1.99,0.], [0., 0., 0.]], [max_n, layers-1]), &
      nodes = nodes_per_layer &
    )
  end function

  function multi_layer_xor_network() result(neural_network)
    type(neural_network_t) neural_network
    integer, parameter :: nodes_per_layer(*) = [2, 3, 3, 1]
    integer, parameter :: max_n = maxval(nodes_per_layer), layers = size(nodes_per_layer)

    neural_network = neural_network_t( &
      metadata = [string_t("XOR"), string_t("Damian Rouson"), string_t("2023-07-02"), string_t("step"), string_t("false")], &
      weights = reshape([real:: [1,1,0, 0,1,1, 1,0,0, 1,0,0, 0,1,0, 0,0,1], [1,0,0, -2,0,0, 1,0,0]], &
        [max_n, max_n, layers-1]), &
      biases = reshape([[0.,-1.99,0.], [0., 0., 0.], [0., 0., 0.]], [max_n, layers-1]), &
      nodes = nodes_per_layer &
    )
  end function

  function decrement_split_combine_increment() result(neural_network)
    !! Define a network that produces outputs identical to the 2 inputs for any input greater than or equal to 1
    !! based on the following algorithm:
    !! 1. A 1st hidden layer that forwards input 1 unmolested and decrements input 2 by 1,
    !! 2. A 2nd hidden layer that forwards input 1 unmolested and splits input 2 into two halves,
    !! 3. An output layer that recombines those two halves and increments the result by 1.
    type(neural_network_t) neural_network
    integer, parameter :: inputs = 2, hidden(*) = [2,3], outputs = 2 ! number of neurons in input, output, and hidden layers
    integer, parameter :: n(*) = [inputs, hidden(1), hidden(2), outputs] ! nodes per layer
    integer, parameter :: n_max = maxval(n), layers=size(n) ! max layer width, number of layers
    integer, parameter :: w_shape(*) = [n_max, n_max, layers-1], b_shape(*) = [n_max, n_max]
    real, parameter :: &
      w(*,*,*) = reshape( [1.,0.,0., 0.,1.,0., 0.,0.,0., 1.,0.,0., 0.,.5,.5, 0.,0.,0., 1.,0.,0., 0.,1.,0., 0.,1.,1.], w_shape), &
      b(*,*) = reshape( [0.,-1.,0., 0.,0.,0., 0.,1.,0.], b_shape)

    neural_network = neural_network_t( &
      metadata = [ &
        string_t("Decrement/Split/Combine/Increment => Identity") &
       ,string_t("Damian Rouson") &
       ,string_t("2024-07-03") &
       ,string_t("relu") &
       ,string_t("false") &
     ], weights = w, biases = b, nodes = n &
    )
  end function

  function double_precision_network() result(neural_network)
    !! The result is a double-precision version of the this network defined in the decrement_split_combine_increment function
    type(neural_network_t(double_precision)) neural_network
    integer, parameter :: inputs = 2, hidden(*) = [2,3], outputs = 2 ! number of neurons in input, output, and hidden layers
    integer, parameter :: n(*) = [inputs, hidden(1), hidden(2), outputs] ! nodes per layer
    integer, parameter :: n_max = maxval(n), layers=size(n) ! max layer width, number of layers
    integer, parameter :: w_shape(*) = [n_max, n_max, layers-1], b_shape(*) = [n_max, n_max]
    double precision, parameter :: &
      w(*,*,*) = reshape( &
        [double precision :: 1.,0.,0., 0.,1.,0., 0.,0.,0., 1.,0.,0., 0.,.5,.5, 0.,0.,0., 1.,0.,0., 0.,1.,0., 0.,1.,1.], w_shape &
      ), b(*,*) = reshape( [double precision :: 0.,-1.,0., 0.,0.,0., 0.,1.,0.], b_shape)

    neural_network = neural_network_t( &
      metadata = metadata_t( &
        string_t("Double-Precision"), string_t("Damian Rouson"), string_t("2024-09-02"), string_t("relu"), string_t("false") &
     ), weights = w, biases = b, nodes = n &
    )
  end function

  function varying_width() result(neural_network)
    type(neural_network_t) neural_network
    integer, parameter :: inputs = 2, hidden(*) = [2,3], outputs = 2 ! number of neurons in input, output, and hidden layers
    integer, parameter :: n(*) = [inputs, hidden(1), hidden(2), outputs] ! nodes per layer
    integer, parameter :: n_max = maxval(n), layers=size(n)   ! max layer width, number of layers
    integer, parameter :: w_shape(*) = [n_max, n_max, layers-1], b_shape(*) = [n_max, n_max]
    integer i
    real, allocatable :: w(:,:,:), b(:,:)

    w = reshape( [(i, i=1,product(w_shape))], w_shape)
    b = reshape( [(maxval(w) + i, i=1,product(b_shape))], b_shape)

    neural_network = neural_network_t( &
      metadata = [string_t("random"), string_t("Damian Rouson"), string_t("2024-07-03"), string_t("sigmoid"), string_t("false")], &
      weights = w, biases = b, nodes = n &
    )
  end function

  function distinct_parameters() result(neural_network)
    type(neural_network_t) neural_network
    integer, parameter :: inputs = 2, hidden = 3, outputs = 1 ! number of neurons in input, output, and hidden layers
    integer, parameter :: n(*) = [inputs, hidden, hidden, outputs]    ! nodes per layer
    integer, parameter :: n_max = maxval(n), layers=size(n)   ! max layer width, number of layers
    integer, parameter :: w_shape(*) = [n_max, n_max, layers-1], b_shape(*) = [n_max, n_max]
    integer i
    real, allocatable :: w(:,:,:), b(:,:)

    w = reshape( [(i, i=1,product(w_shape))], w_shape)
    b = reshape( [(maxval(w) + i, i=1,product(b_shape))], b_shape)

    neural_network = neural_network_t( &
      metadata = [string_t("random"), string_t("Damian Rouson"), string_t("2023-07-15"), string_t("sigmoid"), string_t("false")], &
      weights = w, biases = b, nodes = n &
    )   
  end function

  function multi_hidden_layer_net_to_from_json() result(test_passes)
    logical test_passes
    type(neural_network_t) neural_network, from_json
    type(file_t) json_file

    neural_network = distinct_parameters()
    json_file = neural_network%to_json()
    from_json = neural_network_t(json_file)
    test_passes = neural_network == from_json 
  end function

  function varying_width_net_to_from_json() result(test_passes)
    logical test_passes

    associate(neural_network => varying_width())
      associate(from_json => neural_network_t( neural_network%to_json() ))
        test_passes = neural_network == from_json 
      end associate
    end associate
  end function

  function infer_with_varying_width_net() result(test_passes)
    logical test_passes
    type(neural_network_t) neural_network
    type(tensor_t) inputs, outputs
    real, parameter :: tolerance = 1.E-08

    neural_network = decrement_split_combine_increment()
    inputs = tensor_t([1.1, 2.7])
    outputs = neural_network%infer(inputs)
    test_passes = all(abs(inputs%values() - outputs%values()) < tolerance)
  end function

  function double_precision_inference() result(test_passes)
    logical test_passes
    type(neural_network_t(double_precision)) neural_network
    type(tensor_t(double_precision)) inputs, outputs
    real, parameter :: tolerance = 1.D-08

    neural_network = double_precision_network()
    inputs = tensor_t([1.1D0, 2.7D0])
    outputs = neural_network%infer(inputs)
    test_passes = all(abs(inputs%values() - outputs%values()) < tolerance)
  end function

  function elemental_infer_with_1_hidden_layer_xor_net() result(test_passes)
    logical test_passes
    type(neural_network_t) neural_network

    neural_network = single_hidden_layer_xor_network()

    block
      type(tensor_t), allocatable :: truth_table(:)
      real, parameter :: tolerance = 1.E-08, false = 0., true = 1.
      integer i

      associate(array_of_inputs => [tensor_t([true,true]), tensor_t([true,false]), tensor_t([false,true]), tensor_t([false,false])])
        truth_table = neural_network%infer(array_of_inputs)
      end associate
      test_passes = all( &
        abs(truth_table(1)%values() - false) < tolerance .and. abs(truth_table(2)%values() - true) < tolerance .and. &
        abs(truth_table(3)%values() - true) < tolerance .and. abs(truth_table(4)%values() - false) < tolerance &
      )
    end block
  end function

  function elemental_infer_with_2_hidden_layer_xor_net() result(test_passes)
    logical test_passes
    type(neural_network_t) neural_network

    neural_network = multi_layer_xor_network()

    block
      type(tensor_t), allocatable :: truth_table(:)
      real, parameter :: tolerance = 1.E-08, false = 0., true = 1.
      integer i

      associate(array_of_inputs => [tensor_t([true,true]), tensor_t([true,false]), tensor_t([false,true]), tensor_t([false,false])])
        truth_table = neural_network%infer(array_of_inputs)
      end associate
      test_passes = all( &
        abs(truth_table(1)%values() - false) < tolerance .and. abs(truth_table(2)%values() - true) < tolerance .and. &
        abs(truth_table(3)%values() - true) < tolerance .and. abs(truth_table(4)%values() - false) < tolerance &
      )
    end block
  end function

end module neural_network_test_m
