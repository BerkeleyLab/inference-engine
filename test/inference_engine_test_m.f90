! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module inference_engine_test_m
  !! Define inference tests and procedures required for reporting results

  ! External dependencies
  use assert_m, only : assert
  use kind_parameters_m, only : rkind
  use sourcery_m, only : string_t, test_t, test_result_t, file_t

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

    character(len=*), parameter :: longest_description = &
          "converting a network with 2 hidden layers to and from JSON format"

    associate( &
      descriptions => &
        [ character(len=len(longest_description)) :: &
          "performing elemental inference with 1 hidden layer", &
          "performing elemental inference with 2 hidden layers", &
          "performing batch inference", &
          "converting a network with 2 hidden layers to and from JSON format"  &
        ], &
      outcomes => &
        [ elemental_infer_with_1_hidden_layer_xor_net(), elemental_infer_with_2_hidden_layer_xor_net(), &
          batch_inference(), multi_hidden_layer_net_to_from_json() &
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

  function batch_inference() result(test_passes)
    logical, allocatable :: test_passes(:)
    type(inference_engine_t) inference_engine
    integer, parameter :: lat=1, lon=1, lev=1 ! latitudes, longitudes, levels (elevations)
    real(rkind), parameter :: tolerance = 1.E-08_rkind, false = 0._rkind, true = 1._rkind
    integer i, j, k

    inference_engine = multi_layer_xor_network()

    associate(num_inputs => inference_engine%num_inputs())
      associate(inputs_batch => reshape([true,true], shape=[lon,lev,lat,num_inputs]))    
        associate(outputs_batch => inference_engine%infer(inputs_batch))
          test_passes = &
            [all([(((abs(outputs_batch(i,k,j,:) - xor_gate(inputs_batch(i,k,j,:))) < tolerance, i=1,lon), k=1,lev), j=1,lat)])]
        end associate
      end associate
    end associate

  contains
      
    pure function xor_gate(inputs) result(expected_output)
      real(rkind), intent(in) :: inputs(:)
      real(rkind) expected_output
      call assert(size(inputs) == inference_engine%num_inputs(), "xor(inputs): size(inputs) == num_inputs")
      associate(sum_inputs => sum(inputs))
        expected_output = merge(true, false, sum_inputs > 0.99 .and. sum_inputs < 1.01)
      end associate
    end function

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
    logical, allocatable :: test_passes
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
    logical, allocatable :: test_passes(:)
    type(inference_engine_t) inference_engine

    inference_engine = single_hidden_layer_xor_network()

    block
      type(tensor_t), allocatable :: truth_table(:)
      real(rkind), parameter :: tolerance = 1.E-08_rkind, false = 0._rkind, true = 1._rkind
      integer i

      associate(array_of_inputs => [tensor_t([true,true]), tensor_t([true,false]), tensor_t([false,true]), tensor_t([false,false])])
        truth_table = inference_engine%infer(array_of_inputs)
      end associate
      test_passes = [ &
        abs(truth_table(1)%values() - false) < tolerance .and. abs(truth_table(2)%values() - true) < tolerance .and. &
        abs(truth_table(3)%values() - true) < tolerance .and. abs(truth_table(4)%values() - false) < tolerance &
      ]
    end block
  end function

  function elemental_infer_with_2_hidden_layer_xor_net() result(test_passes)
    logical, allocatable :: test_passes(:)
    type(inference_engine_t) inference_engine

    inference_engine = multi_layer_xor_network()

    block
      type(tensor_t), allocatable :: truth_table(:)
      real(rkind), parameter :: tolerance = 1.E-08_rkind, false = 0._rkind, true = 1._rkind
      integer i

      associate(array_of_inputs => [tensor_t([true,true]), tensor_t([true,false]), tensor_t([false,true]), tensor_t([false,false])])
        truth_table = inference_engine%infer(array_of_inputs)
      end associate
      test_passes = [ &
        abs(truth_table(1)%values() - false) < tolerance .and. abs(truth_table(2)%values() - true) < tolerance .and. &
        abs(truth_table(3)%values() - true) < tolerance .and. abs(truth_table(4)%values() - false) < tolerance &
      ]
    end block
  end function

end module inference_engine_test_m
