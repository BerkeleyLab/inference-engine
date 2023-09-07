! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program train_and_write
  !! This program demonstrates how to train a neural network and write it to a JSON file.
  use inference_engine_m, only : &
    inference_engine_t, trainable_engine_t, rkind, sigmoid_t, mini_batch_t, tensor_t, input_output_pair_t
  use sourcery_m, only : string_t, file_t, command_line_t
  implicit none

  type(string_t) file_name
  type(command_line_t) command_line
  real(rkind), parameter :: false=0._rkind, true=1._rkind

  file_name = string_t(command_line%flag_value("--output-file"))

  if (len(file_name%string())==0) then
    error stop new_line('a') // new_line('a') // &
      'Usage: ./build/run-fpm.sh run --example train-and-write -- --output-file "<file-name>"' 
  end if

  and_gate_with_skewed_training_data: &
  block
    logical, allocatable :: test_passes(:)
    type(mini_batch_t), allocatable :: mini_batches(:)
    type(tensor_t), allocatable, dimension(:,:) :: training_inputs, training_outputs
    type(tensor_t), allocatable, dimension(:) :: tmp, tmp2, test_inputs, expected_test_outputs, actual_outputs
    type(trainable_engine_t) trainable_engine
    type(inference_engine_t) inference_engine
    real(rkind), parameter :: tolerance = 1.E-02_rkind
    real(rkind), allocatable :: harvest(:,:,:)
    integer, parameter :: num_inputs=2, mini_batch_size = 1, num_iterations=20000
    integer batch, iter, i
    type(file_t) json_file

    allocate(harvest(num_inputs, mini_batch_size, num_iterations))
    call random_number(harvest)
    harvest = 2.*(harvest - 0.5) ! skew toward more input values being true

    ! The following temporary copies are required by gfortran bug 100650 and possibly 49324
    ! See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=100650 and https://gcc.gnu.org/bugzilla/show_bug.cgi?id=49324
    tmp = [([(tensor_t(merge(true, false, harvest(:,batch,iter) < 0.5E0)), batch=1, mini_batch_size)], iter=1, num_iterations)]
    training_inputs = reshape(tmp, [mini_batch_size, num_iterations])

    tmp2 = [([(and(training_inputs(batch, iter)), batch = 1, mini_batch_size)], iter = 1, num_iterations )]
    training_outputs = reshape(tmp2, [mini_batch_size, num_iterations])

    mini_batches = [(mini_batch_t(input_output_pair_t(training_inputs(:,iter), training_outputs(:,iter))), iter=1, num_iterations)]    
    trainable_engine = two_zeroed_hidden_layers()

    call trainable_engine%train(mini_batches,adam=.true.)

    test_inputs = [tensor_t([true,true]), tensor_t([false,true]), tensor_t([true,false]), tensor_t([false,false])]
    expected_test_outputs = [(and(test_inputs(i)), i=1, size(test_inputs))]
    actual_outputs = trainable_engine%infer(test_inputs)

    print *,"  Input 1          Input 2         Expected output   Actual output"
    do i = 1, size(test_inputs)
       print *, test_inputs(i)%values(), actual_outputs(i)%values(), expected_test_outputs(i)%values()
    end do

    inference_engine = trainable_engine%to_inference_engine()
    json_file = inference_engine%to_json()
    call json_file%write_lines(file_name)

  end block and_gate_with_skewed_training_data

contains

  elemental function and(inputs_object) result(expected_outputs_object)
    type(tensor_t), intent(in) :: inputs_object 
    type(tensor_t) expected_outputs_object 
    expected_outputs_object = tensor_t([merge(true, false, sum(inputs_object%values()) > 1.99_rkind)])
  end function

  function two_zeroed_hidden_layers() result(trainable_engine)
    type(trainable_engine_t) trainable_engine
    integer, parameter :: inputs = 2, outputs = 1, hidden = 3 ! number of neurons in input, output, and hidden layers
    integer, parameter :: neurons(*) = [inputs, hidden, hidden, outputs] ! neurons per layer
    integer, parameter :: max_neurons = maxval(neurons), layers=size(neurons) ! max layer width, number of layers
    real(rkind) w(max_neurons, max_neurons, layers-1), b(max_neurons, max_neurons)

    w = 0.
    b = 0.

    trainable_engine = trainable_engine_t( &
      nodes = neurons, weights = w, biases = b, differentiable_activation_strategy = sigmoid_t(), metadata = & 
      [string_t("2-hide|3-wide"), string_t("Damian Rouson"), string_t("2023-06-30"), string_t("sigmoid"), string_t("false")] &
    )   
  end function

end program
