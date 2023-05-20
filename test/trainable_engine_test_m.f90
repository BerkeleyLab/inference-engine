! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module trainable_engine_test_m
  !! Define inference tests and procedures required for reporting results
  use string_m, only : string_t
  use test_m, only : test_t
  use test_result_m, only : test_result_t
  use trainable_engine_m, only : trainable_engine_t
  use inputs_m, only : inputs_t
  use outputs_m, only : outputs_t
  use expected_outputs_m, only : expected_outputs_t
  use matmul_m, only : matmul_t
  use kind_parameters_m, only : rkind
  use sigmoid_m, only : sigmoid_t
  use input_output_pair_m, only : input_output_pair_t 
  use mini_batch_m, only : mini_batch_t
  implicit none

  private
  public :: trainable_engine_test_t

  type, extends(test_t) :: trainable_engine_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "A trainable_engine_t" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)

    test_results = test_result_t( &
      [character(len=len("learning to map (false,false) -> false from mini-batches")) :: &
       "learning to map two fixed inputs to one fixed output", &
       "learning to map (true,true) -> false from mini-batches", &
       "learning to map (false,true) -> true from mini-batches", &
       "learning to map (true,false) -> true from mini-batches", &
       "learning to map (false,false) -> false from mini-batches" &
      ], &
      [train_on_fixed_input_output_pair(), &
       train_on_truth_table_mini_batch() &
      ] &
    )
  end function

  function trainable_single_layer_perceptron() result(trainable_engine)
    type(trainable_engine_t) trainable_engine
    integer, parameter :: n_in = 2 ! number of inputs
    integer, parameter :: n_out = 1 ! number of outputs
    integer, parameter :: neurons = 3 ! number of neurons per layer
    integer, parameter :: n_hidden = 1 ! number of hidden layers 
   
    trainable_engine = trainable_engine_t( &
      metadata = [ &
       string_t("Trainable XOR"), string_t("Damian Rouson"), string_t("2023-05-09"), string_t("sigmoid"), string_t("false") &
      ], &
      input_weights = real(reshape([1,0,1,1,0,1], [n_in, neurons]), rkind), &
      hidden_weights = reshape([real(rkind)::], [neurons,neurons,n_hidden-1]), &
      output_weights = real(reshape([1,-2,1], [n_out, neurons]), rkind), &
      biases = reshape([real(rkind):: 0.,-1.99,0.], [neurons, n_hidden]), &
      output_biases = [real(rkind):: 0.], &
      differentiable_activation_strategy = sigmoid_t() &
    )
  end function

  function train_on_fixed_input_output_pair() result(test_passes)
    logical, allocatable :: test_passes(:)
    type(outputs_t) actual_output ! gfortran doesn't allow this to be an association
    type(trainable_engine_t) trainable_engine
    real(rkind), parameter :: tolerance = 1.E-02_rkind, false = 0._rkind, true = 1._rkind
    integer i

    trainable_engine = trainable_single_layer_perceptron()

    call trainable_engine%train( &
       [(mini_batch_t(input_output_pair_t([inputs_t([true,true])], [expected_outputs_t([false])])), i=1,2000)], &
       matmul_t() &
    )
    actual_output = trainable_engine%infer([true,true], matmul_t())
    test_passes = [all(abs(actual_output%outputs() - false) < tolerance)]
  end function

  function train_on_truth_table_mini_batch() result(test_passes)
    logical, allocatable :: test_passes(:)
    type(trainable_engine_t) trainable_engine
    real(rkind), parameter :: tolerance = 1.E-02_rkind, false = 0._rkind, true = 1._rkind
    type(outputs_t), allocatable :: truth_table(:)
    integer i

    trainable_engine = trainable_single_layer_perceptron()

    associate(array_of_inputs => [inputs_t([true,true]), inputs_t([false,true]), inputs_t([true,false]), inputs_t([false,false])])
      truth_table = trainable_engine%infer(array_of_inputs, [(matmul_t(), i=1,size(array_of_inputs))])
    end associate
    print *,"initial truth_table ",[(truth_table(i)%outputs(), i=1,4)]

    call trainable_engine%train( &
      [( & 
       mini_batch_t( &
         input_output_pair_t( &
           [inputs_t([true,true]), inputs_t([false,true]), inputs_t([true,false]), inputs_t([false,false])], &
           [expected_outputs_t([false]), expected_outputs_t([true]), expected_outputs_t([true]), expected_outputs_t([false])] &
       ) ), i=1,1000000 &
      )], matmul_t() & 
    )
    associate(array_of_inputs => [inputs_t([true,true]), inputs_t([false,true]), inputs_t([true,false]), inputs_t([false,false])])
      truth_table = trainable_engine%infer(array_of_inputs, [(matmul_t(), i=1,size(array_of_inputs))])
    end associate
    print *,"final truth_table ",[(truth_table(i)%outputs(), i=1,4)]
    test_passes = [ & 
      abs(truth_table(1)%outputs() - false) < tolerance, abs(truth_table(2)%outputs() - true) < tolerance, &
      abs(truth_table(3)%outputs() - true) < tolerance , abs(truth_table(4)%outputs() - false) < tolerance &
    ]
  end function

end module trainable_engine_test_m
