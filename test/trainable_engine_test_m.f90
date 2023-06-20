! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module trainable_engine_test_m
  !! Define inference tests and procedures required for reporting results
  use assert_m, only : assert
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

    character(len=*), parameter :: longest_description = &
        "learning the mapping (false,false) -> false when trained on a fixed input/output pair"

    associate( &
      descriptions => &
      [ character(len=len(longest_description)) :: &
        "learning the mapping (true,true) -> false when trained on a fixed input/output pair", &
        "learning the mapping (false,true) -> true when trained on a fixed input/output pair", &
        "learning the mapping (true,false) -> true when trained on a fixed input/output pair", &
        "learning the mapping (false,false) -> false when trained on a fixed input/output pair", &
        "learning the mapping (true,true) -> false trained on mini-batches", &
        "learning the mapping (false,true) -> true trained on mini-batches", &
        "learning the mapping (true,false) -> true trained on mini-batches", &
        "learning the mapping (false,false) -> false trained on mini-batches", &
        "learning the mapping (true,true) -> true using two hidden layers", &
        "learning the mapping (false,true) -> false using two hidden layers", &
        "learning the mapping (true,false) -> false using two hidden layers", &
        "learning the mapping (false,false) -> false using two hidden layers" &
      ], outcomes => [ &
        train_on_fixed_input_output_pair(), &
        train_on_xor_truth_table_mini_batch(), &
        train_on_and_truth_table_mini_batch() &
      ] &
    )
      call assert(size(descriptions) == size(outcomes), "trainable_engine_test_m(results): size(descritions) == size(outcomes)")
      test_results = test_result_t(descriptions, outcomes)
    end associate
  end function

  function trainable_hidden_layer() result(trainable_engine)
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
    real(rkind), parameter :: tolerance = 1.E-02_rkind, false = 0._rkind, true = 1._rkind
    type(outputs_t), dimension(4) :: actual_output
    type(expected_outputs_t), dimension(4) :: expected_outputs !gfortran doesn't allow replacing with an association
    type(trainable_engine_t) trainable_engine
    integer i, j

    expected_outputs = [ &
      expected_outputs_t([false]), expected_outputs_t([true]), expected_outputs_t([true]), expected_outputs_t([false]) &
    ] 
    associate( &
      inputs => [ &
        inputs_t([true,true]), inputs_t([false,true]), inputs_t([true,false]), inputs_t([false,false]) &
      ] &
    )
     loop_over_truth_table_entries: &
      do j =1, size(inputs)
        trainable_engine = trainable_hidden_layer()
        call trainable_engine%train([(mini_batch_t(input_output_pair_t([inputs(j)], [expected_outputs(j)])), i=1,3000)], matmul_t())
        actual_output(j) = trainable_engine%infer(inputs(j), matmul_t())
      end do loop_over_truth_table_entries
      test_passes = [(abs(actual_output(j)%outputs() - expected_outputs(j)%outputs()) < tolerance, j=1, size(inputs))]
    end associate
  end function

  function wide_hidden_layer() result(trainable_engine)
    type(trainable_engine_t) trainable_engine
    integer, parameter :: n_in = 2 ! number of inputs
    integer, parameter :: n_out = 1 ! number of outputs
    integer, parameter :: neurons = 36 ! number of neurons per layer
    integer, parameter :: n_hidden = 1 ! number of hidden layers 
    integer n
   
    trainable_engine = trainable_engine_t( &
      metadata = [ & 
       string_t("Wide 1-layer network"), string_t("Damian Rouson"), string_t("2023-05-24"), string_t("sigmoid"), string_t("false") &
      ], &
      input_weights = real(reshape([([1,0,1,1,0,1], n=1,12 )], [n_in, neurons]), rkind), &
      hidden_weights = reshape([real(rkind)::], [neurons,neurons,n_hidden-1]), &
      output_weights = real(reshape([([1,-2,1], n=1,12)], [n_out, neurons]), rkind), &
      biases = reshape([real(rkind):: [(0.,-1.99,0., n=1,12)] ], [neurons, n_hidden]), &
      output_biases = [real(rkind):: 0.], &
      differentiable_activation_strategy = sigmoid_t() &
    )   
  end function

  function train_on_xor_truth_table_mini_batch() result(test_passes)
    logical, allocatable :: test_passes(:)
    type(trainable_engine_t) trainable_engine
    real(rkind), parameter :: tolerance = 1.E-02_rkind, false = 0._rkind, true = 1._rkind
    type(outputs_t), allocatable :: actual_output(:)
    type(inputs_t), allocatable :: inputs(:)
    type(expected_outputs_t), allocatable :: expected_outputs(:) !gfortran doesn't allow replacing with an association
    type(mini_batch_t), allocatable :: mini_batches(:) !gfortran doesn't allow replacing with an association
    integer i, m

    inputs = [ & 
      inputs_t([true,true]), inputs_t([false,true]), inputs_t([true,false]), inputs_t([false,false]) &
    ]
    expected_outputs = [ & 
      expected_outputs_t([false]), expected_outputs_t([true]), expected_outputs_t([true]), expected_outputs_t([false]) &
    ]
    mini_batches = [(mini_batch_t( input_output_pair_t( inputs, expected_outputs ) ), m=1,100000)]
    trainable_engine = wide_hidden_layer()
    call trainable_engine%train(mini_batches, matmul_t())
    actual_output = trainable_engine%infer(inputs, matmul_t())
    test_passes = [(abs(actual_output(i)%outputs() - expected_outputs(i)%outputs()) < tolerance, i=1, size(actual_output))]
  end function

  function train_on_and_truth_table_mini_batch() result(test_passes)
    logical, allocatable :: test_passes(:)
    type(trainable_engine_t) trainable_engine
    integer, parameter :: mini_batch_size = 200, num_inputs=2, num_outputs=1, num_iterations=1 !50000
    type(inputs_t) inputs(mini_batch_size)
    type(outputs_t), allocatable :: actual_output(:)
    type(expected_outputs_t) expected_outputs(mini_batch_size)
    type(mini_batch_t) mini_batches(mini_batch_size)
    real(rkind) harvest(mini_batch_size, num_inputs)
    integer pair, iter, i
    real(rkind), parameter :: false = 0._rkind, true = 1._rkind

    call random_init(image_distinct=.true., repeatable=.true.)

    trainable_engine = two_hidden_layers()
    
    !do iter = 1, num_iterations
    !  call random_number(harvest)
    !  do pair = 1, mini_batch_size
    !    inputs(pair) = inputs_t(harvest(pair,:))
    !    expected_outputs(pair) = and(inputs(pair))
    !  end do
    !  mini_batches = mini_batch_t(input_output_pair_t(inputs, expected_outputs))
    !  call trainable_engine%train(mini_batches, matmul_t())
    !  actual_output = trainable_engine%infer(inputs, matmul_t())
    !end do

    block
      type(inputs_t), allocatable :: inputs(:)
      type(expected_outputs_t), allocatable :: expected_outputs(:)
      real(rkind), parameter :: tolerance = 1.E-02_rkind

      inputs = [ & 
        inputs_t([true,true]), inputs_t([false,true]), inputs_t([true,false]), inputs_t([false,false]) &
      ]
      expected_outputs = [ & 
        expected_outputs_t([true]), expected_outputs_t([false]), expected_outputs_t([false]), expected_outputs_t([false]) &
      ]
      actual_output = trainable_engine%infer(inputs, matmul_t())
      test_passes = [(abs(actual_output(i)%outputs() - expected_outputs(i)%outputs()) < tolerance, i=1, size(actual_output))]
    end block

  contains
    
    elemental function and(inputs_object) result(expected_outputs_object)
       type(inputs_t), intent(in) :: inputs_object 
       type(expected_outputs_t) expected_outputs_object 
       expected_outputs_object = expected_outputs_t([merge(false, true, sum(inputs_object%values())<=1.5_rkind)])
    end function

    function two_hidden_layers() result(trainable_engine)
      type(trainable_engine_t) trainable_engine
      integer, parameter :: n_in = 2 ! number of inputs
      integer, parameter :: n_out = 1 ! number of outputs
      integer, parameter :: neurons = 3 ! number of neurons per layer
      integer, parameter :: n_hidden = 2 ! number of hidden layers 
      integer n
     
      trainable_engine = trainable_engine_t( &
        metadata = [ & 
         string_t("2-hidden-layer network"), string_t("Damian Rouson"), string_t("2023-05-30"), string_t("sigmoid"), &
         string_t("false") &
        ], &
        input_weights = reshape([(0._rkind, n=1, n_in*neurons)], [n_in, neurons]), &
        hidden_weights = reshape([(0._rkind, n=1, neurons*neurons*(n_hidden-1))], [neurons,neurons,n_hidden-1]), &
        output_weights = reshape([(0._rkind, n=1, n_out*neurons)], [n_out, neurons]), &
        biases = reshape([(0.,n=1, neurons*n_hidden)], [neurons, n_hidden]), &
        output_biases = [0._rkind], &
        differentiable_activation_strategy = sigmoid_t() &
      )   
    end function
  end function

end module trainable_engine_test_m
