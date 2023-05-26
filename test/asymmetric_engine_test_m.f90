! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module asymmetric_engine_test_m
  !! Define asymmetric tests and procedures required for reporting results
  use assert_m, only : assert
  use string_m, only : string_t
  use test_m, only : test_t
  use test_result_m, only : test_result_t
  use inference_engine_m, only : inference_engine_t
  use inputs_m, only :inputs_t
  use outputs_m, only :outputs_t
  use inference_strategy_m, only : inference_strategy_t
  use concurrent_dot_products_m, only : concurrent_dot_products_t
  use step_m, only : step_t
  use matmul_m, only : matmul_t
  use file_m, only : file_t
  use kind_parameters_m, only : rkind
  use outputs_m, only : outputs_t
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
    specimen = "An inference_engine_t object encoding an asymmetric XOR-AND-the-2nd-input network"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)

    character(len=*), parameter :: longest_description = &
        "mapping (false,false) to false using the concurrent_dot_products_t() inference strategy"

    associate( &
      descriptions => &
        [ character(len=len(longest_description)) :: &
          "mapping (true,true) to false using the concurrent_dot_products_t() inference strategy", &
          "mapping (true,false) to false using the concurrent_dot_products_t() inference strategy", &
          "mapping (false,true) to true using the concurrent_dot_products_t() inference strategy", &
          "mapping (false,false) to false using the concurrent_dot_products_t() inference strategy", &
          "mapping (true,true) to false using the matmul_t() inference strategy", &
          "mapping (true,false) to false using the matmul_t() inference strategy", &
          "mapping (false,true) to true using the matmul_t() inference strategy", &
          "mapping (false,false) to false using the matmul_t() inference strategy", &
          "counting the number of hidden layers", &
          "counting the number of neurons per layer", &
          "counting the number of inputs", &
          "counting the number of outputs", &
          "getting the activation function name" &
        ], &
      outcomes => &
        [ xor_and_2nd_input_truth_table(concurrent_dot_products_t()), xor_and_2nd_input_truth_table(matmul_t()), &
          test_num_hidden_layers(),  test_neurons_per_layer(), test_num_inputs(), test_num_outputs(), &
          test_activation_name() &
        ] & 
    )
      call assert(size(descriptions) == size(outcomes),"assymetric_engine_test_m(results): size(descriptions) == size(outcomes)")
      test_results = test_result_t(descriptions, outcomes)
    end associate
       
  end function

 function xor_and_2nd_input_network() result(inference_engine)
    type(inference_engine_t) inference_engine
    integer, parameter :: n_in = 2 ! number of inputs
    integer, parameter :: n_out = 1 ! number of outputs
    integer, parameter :: neurons = 4 ! number of neurons per layer
    integer, parameter :: n_hidden = 2 ! number of hidden layers 
    integer i, j 
    real(rkind) xor_into_neuron_2(neurons,neurons,n_hidden-1)

    xor_into_neuron_2 = 0.
    xor_into_neuron_2(1:3,2,1) = [1., -2., 1.] 
    xor_into_neuron_2(4,4,1) = 1.
    
    inference_engine = inference_engine_t( &
      metadata = &
        [string_t("XOR AND 2nd input"), string_t("Damian Rouson"), string_t("2023-02-19"), string_t("step"), string_t("false")], &
      input_weights  = real(reshape([1,0,1,1,0,1,0,1], [n_in, neurons]), rkind), &
      hidden_weights = xor_into_neuron_2, &
      output_weights = real(reshape([0,1,0,1], [n_out, neurons]), rkind), &
      biases = reshape([real(rkind):: 0.,-1.99,0.,0., 0.,0.,0.,0.], [neurons, n_hidden]), &
      output_biases = [real(rkind):: -1.] &
    )   
  end function

  function xor_and_2nd_input_truth_table(inference_strategy) result(test_passes)
    logical, allocatable :: test_passes(:)
    class(inference_strategy_t), intent(in) :: inference_strategy

    type(inference_engine_t) asymmetric_engine

    asymmetric_engine = xor_and_2nd_input_network()

    block
      real(rkind), parameter :: tolerance = 1.E-08_rkind, false = 0._rkind, true = 1._rkind
      type(outputs_t) true_true, true_false, false_true, false_false

      true_true = asymmetric_engine%infer([true,true], inference_strategy)
      true_false = asymmetric_engine%infer([true,false], inference_strategy)
      false_true = asymmetric_engine%infer([false,true], inference_strategy)
      false_false = asymmetric_engine%infer([false,false], inference_strategy)

      associate( &
        true_true_outputs => true_true%outputs(), &
        true_false_outputs => true_false%outputs(), &
        false_true_outputs => false_true%outputs(), &
        false_false_outputs => false_false%outputs() &
      )
        test_passes = [ &
          size(true_true_outputs)==1 .and. abs(true_true_outputs(1) - false) < tolerance, &
          size(true_false_outputs)==1 .and. abs(true_false_outputs(1) - false) < tolerance,  &
          size(false_true_outputs)==1 .and. abs(false_true_outputs(1) - true) < tolerance, &
          size(false_false_outputs)==1 .and. abs(false_false_outputs(1) - false) < tolerance  &
        ]
      end associate
    end block

  end function

  function test_num_hidden_layers() result(test_passes)
    logical test_passes

    type(inference_engine_t) inference_engine

    inference_engine = xor_and_2nd_input_network()
    test_passes = inference_engine%num_hidden_layers() == 2
  end function

  function test_neurons_per_layer() result(test_passes)
    logical test_passes

    type(inference_engine_t) inference_engine

    inference_engine = xor_and_2nd_input_network()
    test_passes = inference_engine%neurons_per_layer() == 4
  end function

  function test_num_inputs() result(test_passes)
    logical test_passes

    type(inference_engine_t) inference_engine

    inference_engine = xor_and_2nd_input_network()
    test_passes = inference_engine%num_inputs() == 2
  end function

  function test_num_outputs() result(test_passes)
    logical test_passes

    type(inference_engine_t) inference_engine

    inference_engine = xor_and_2nd_input_network()
    test_passes = inference_engine%num_outputs() == 1
  end function

  function test_activation_name() result(test_passes)
    logical test_passes
    type(string_t) :: function_name
    type(inference_engine_t) inference_engine

    inference_engine = xor_and_2nd_input_network()
    function_name = inference_engine%activation_function_name() 
    test_passes = function_name%string() == "step"
  end function

end module asymmetric_engine_test_m
