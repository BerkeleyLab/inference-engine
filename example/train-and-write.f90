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

  block
    type(trainable_engine_t) trainable_engine
    type(inference_engine_t) inference_engine
    type(file_t) json_file
    type(mini_batch_t), allocatable :: mini_batches(:)
    type(tensor_t), allocatable :: training_inputs(:,:), tmp(:), inputs(:)
    type(tensor_t), allocatable :: training_outputs(:,:), tmp2(:), expected_outputs(:)
    real(rkind) t_start, t_end
    real(rkind), allocatable :: harvest(:,:,:)
    integer, parameter :: num_inputs=2, mini_batch_size = 1, num_iterations=8000000
    integer batch, iter, i

    allocate(harvest(num_inputs, mini_batch_size, num_iterations))
    call random_number(harvest)

    ! The following temporary copies are required by gfortran bug 100650 and possibly 49324
    ! See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=100650 and https://gcc.gnu.org/bugzilla/show_bug.cgi?id=49324
    tmp = [([(tensor_t(merge(true, false, harvest(:,batch,iter) < 0.5E0)), batch=1, mini_batch_size)], iter=1, num_iterations)]
    training_inputs = reshape(tmp, [mini_batch_size, num_iterations])

    tmp2 = [([(xor(training_inputs(batch, iter)), batch = 1, mini_batch_size)], iter = 1, num_iterations )]
    training_outputs = reshape(tmp2, [mini_batch_size, num_iterations])

    mini_batches = [(mini_batch_t(input_output_pair_t(training_inputs(:,iter), training_outputs(:,iter))), iter=1, num_iterations)]    
    trainable_engine = one_random_hidden_layer()

    call cpu_time(t_start)
    call trainable_engine%train(mini_batches)
    call cpu_time(t_end)

    print *,"Training time: ",t_end - t_start

    inputs = [tensor_t([true,true]), tensor_t([true,false]), tensor_t([false,true]), tensor_t([false,false])]
    print *, "sample inputs:    ",("[",inputs(i)%values(),"]", i=1, size(inputs))
    expected_outputs = xor(inputs)
    print *, "expected outputs: ",(expected_outputs(i)%values(), i=1, size(expected_outputs))
    associate(outputs => trainable_engine%infer(inputs))
      print *, "actual outputs:   ",(outputs(i)%values(), i=1, size(outputs))
    end associate

     inference_engine = trainable_engine%to_inference_engine()
     json_file = inference_engine%to_json()
     call json_file%write_lines(file_name)
  end block

contains

  elemental function xor(inputs) result(expected_outputs)
    type(tensor_t), intent(in) :: inputs
    type(tensor_t) expected_outputs
    associate(sum_inputs => sum(inputs%values()))
     expected_outputs = tensor_t([merge(true, false, sum_inputs > 0.99 .and. sum_inputs < 1.01)])
    end associate
  end function

  function one_random_hidden_layer() result(trainable_engine)
    type(trainable_engine_t) trainable_engine
    integer, parameter :: inputs = 2, outputs = 1, hidden = 2 ! number of neurons in input, output, and hidden layers
    integer, parameter :: n(*) = [inputs, hidden, outputs] ! neurons per layer
    integer, parameter :: n_max = maxval(n), layers=size(n) ! max layer width, number of layers
    real(rkind) w(n_max, n_max, layers-1), b(n_max, n_max)

    call random_number(b)
    call random_number(w)

    trainable_engine = trainable_engine_t( &
      nodes = n, weights = w, biases = b, differentiable_activation_strategy = sigmoid_t(), & 
      metadata = [string_t("1 hide|2 wide"), string_t("D. Rouson"), string_t("2023-06-30"), string_t("sigmoid"), string_t("false")]&
    ) 
  end function

end program
