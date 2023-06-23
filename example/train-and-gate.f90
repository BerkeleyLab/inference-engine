! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module trainable_engine_demo_m
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
  public :: trainable_engine_demo_t

  type, extends(test_t) :: trainable_engine_demo_t
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
        "learning the mapping (true,true) -> true using two hidden layers", &
        "learning the mapping (false,true) -> false using two hidden layers", &
        "learning the mapping (true,false) -> false using two hidden layers", &
        "learning the mapping (false,false) -> false using two hidden layers" &
      ], outcomes => [ &
        train_on_and_truth_table_mini_batch() &
      ] &
    )
      call assert(size(descriptions) == size(outcomes), "trainable_engine_demo_m(results): size(descritions) == size(outcomes)")
      test_results = test_result_t(descriptions, outcomes)
    end associate
  end function

  function train_on_and_truth_table_mini_batch() result(test_passes)
    logical, allocatable :: test_passes(:)
    type(mini_batch_t), allocatable :: mini_batches(:)
    real(rkind), parameter :: false = 0._rkind, true = 1._rkind

    define_training_data: &
    block
      type(inputs_t), allocatable :: inputs(:,:), tmp(:)
      type(expected_outputs_t), allocatable :: expected_outputs(:,:)
      real(rkind), allocatable :: harvest(:,:,:)
      integer, parameter :: num_inputs=2, mini_batch_size = 200, num_iterations=30000
      integer batch, iter

      call random_init(image_distinct=.true., repeatable=.true.)
      allocate(harvest(num_inputs, mini_batch_size, num_iterations))
      call random_number(harvest)

      ! The following temporary copy is required by gfortran bug 100650 and possibly 49324
      ! See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=100650 and https://gcc.gnu.org/bugzilla/show_bug.cgi?id=49324
      tmp = [([(inputs_t(merge(true, false, harvest(:,batch,iter) < 0.5E0)), batch=1, mini_batch_size)], iter=1, num_iterations)]
      inputs = reshape(tmp, [mini_batch_size, num_iterations])
      expected_outputs = and(inputs)
      mini_batches = [(mini_batch_t(input_output_pair_t(inputs(:,iter), expected_outputs(:,iter))), iter=1, num_iterations)]
    end block define_training_data
        
    train_and_test: &
    block
      type(trainable_engine_t) trainable_engine
      type(inputs_t), allocatable :: test_inputs(:)
      type(outputs_t), allocatable :: actual_output(:)
      type(expected_outputs_t), allocatable :: expected_test_outputs(:)
      real(rkind), parameter :: tolerance = 1.E-02_rkind
      integer i

      trainable_engine = two_zeroed_hidden_layers()
      call trainable_engine%train(mini_batches)
      test_inputs = [inputs_t([true,true]), inputs_t([false,true]), inputs_t([true,false]), inputs_t([false,false])]
      expected_test_outputs = and(test_inputs)
      actual_output = trainable_engine%infer(test_inputs, matmul_t())
      test_passes = [(abs(actual_output(i)%outputs() - expected_test_outputs(i)%outputs()) < tolerance, i=1, size(actual_output))]
    end block train_and_test

  contains
    
    elemental function and(inputs_object) result(expected_outputs_object)
       type(inputs_t), intent(in) :: inputs_object 
       type(expected_outputs_t) expected_outputs_object 
       expected_outputs_object = expected_outputs_t([merge(false, true, sum(inputs_object%values())<=1.5_rkind)])
    end function

    function two_zeroed_hidden_layers() result(trainable_engine)
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

end module trainable_engine_demo_m

program main
  use trainable_engine_demo_m, only : trainable_engine_demo_t  
  implicit none

  type(trainable_engine_demo_t) trainable_engine_demo
  real t_start, t_finish

  integer :: passes=0, tests=0

  call cpu_time(t_start)
  call trainable_engine_demo%report(passes, tests)
  call cpu_time(t_finish)

  print *
  print *,"Test suite execution time: ",t_finish - t_start
  print *
  print '(*(a,:,g0))',"_________ In total, ",passes," of ",tests, " tests pass. _________"
  sync all
  print *
  if (passes/=tests) error stop "-------- One or more tests failed. See the above report. ---------"
end program
