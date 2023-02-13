! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module skip_connections_test_m
  !! Define inference tests and procedures required for reporting results
  use test_m, only : test_t
  use test_result_m, only : test_result_t
  use inference_engine_m, only : inference_engine_t
  use inference_strategy_m, only : inference_strategy_t
  use matmul_with_skip_m, only : matmul_with_skip_t
  use concurrent_dot_products_with_skip_m, only : concurrent_dot_products_with_skip_t
  use kind_parameters_m, only : rkind
  implicit none

  private
  public :: skip_connections_test_t

  type, extends(test_t) :: skip_connections_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "An inference_engine_t object encoding 2nd-input-AND-NOT-1st-input logic with skip connections"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)

    test_results = test_result_t( &
      [ character(len=len("mapping (false,false) to false using the concurrent_dot_products_t() inference strategy")) :: &
        "mapping (true,true) to false using the concurrent_dot_products_with_skip_t() inference strategy", &
        "mapping (true,false) to false using the concurrent_dot_products_with_skip_t() inference strategy", &
        "mapping (false,true) to true using the concurrent_dot_products_with_skip_t() inference strategy", &
        "mapping (false,false) to false using the concurrent_dot_products_with_skip_t() inference strategy", &
        "mapping (true,true) to false using the matmul_with_skip_t() inference strategy", &
        "mapping (true,false) to false using the matmul_with_skip_t() inference strategy", &
        "mapping (false,true) to true using the matmul_with_skip_t() inference strategy", &
        "mapping (false,false) to false using the matmul_with_skip_t() inference strategy" &
      ], &
      [ not_1st_and_2nd_truth_table(concurrent_dot_products_with_skip_t()), not_1st_and_2nd_truth_table(matmul_with_skip_t()) &
      ] &
    )
  end function

  function not_1st_and_2nd_network(inference_strategy) result(inference_engine)
    class(inference_strategy_t), intent(in) :: inference_strategy
    type(inference_engine_t) inference_engine
    integer, parameter :: n_in = 2 ! number of inputs
    integer, parameter :: n_out = 1 ! number of outputs
    integer, parameter :: neurons = 4 ! number of neurons per layer
    integer, parameter :: n_hidden = 2 ! number of hidden layers 
    integer i 
      
    inference_engine = inference_engine_t( &
      input_weights  = real(reshape([1,0,0,0,0,0,0,1], [n_in, neurons]), rkind), &
      hidden_weights = real(reshape([(0., i=1,neurons*neurons)], [neurons, neurons, n_hidden-1]), rkind), &
      output_weights = real(reshape([-2,0,0,1], [n_out, neurons]), rkind), &
      biases = reshape([real(rkind):: 0.,0, 0.,0., 0.,0.,0.,0.], [neurons, n_hidden]), &
      output_biases = [real(rkind):: 0.], &
      inference_strategy = inference_strategy &
    )
  end function

  function not_1st_and_2nd_truth_table(inference_strategy) result(test_passes)
    class(inference_strategy_t), intent(in), optional :: inference_strategy
    logical, allocatable :: test_passes(:)
    type(inference_engine_t) inference_engine
    real(rkind), parameter :: tolerance = 1.E-08_rkind, false = 0._rkind, true = 1._rkind

    inference_engine = not_1st_and_2nd_network(inference_strategy)

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

  end function

end module skip_connections_test_m
