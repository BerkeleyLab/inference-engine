! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program train_and_write
  !! This program demonstrates how to train a simple neural network starting from a randomized initial condition and 
  !! how to write the initial network and the trained network to separate JSON files.  The network has two hiden layers.
  !! The input, hidden, and output layers are all two nodes wide.  The training data has outputs that identically match
  !! the corresponding inputs.  Hence, the desired network represents an identity mapping.  With RELU activation functions,
  !! the desired network therefore contains weights corresponding to identity matrices and biases that vanish everywhere.
  !! The initial condition corresponds to the desired network with all weights and biases perturbed by a random variable
  !! that is uniformly distributed on the range [0,0.1].
  use inference_engine_m, only : &
    inference_engine_t, trainable_engine_t, mini_batch_t, tensor_t, input_output_pair_t, shuffle, relu_t
  use sourcery_m, only : string_t, file_t, command_line_t, bin_t
  use assert_m, only : assert, intrinsic_array_t
  implicit none

  type(string_t) intial_network_file, final_network_file
  type(command_line_t) command_line

  final_network_file = string_t(command_line%flag_value("--output-file"))

  if (len(final_network_file%string())==0) then
    error stop new_line('a') // new_line('a') // &
      'Usage: ./build/run-fpm.sh run --example train-and-write -- --output-file "<file-name>"' 
  end if

  block
    integer, parameter :: num_pairs = 5, num_epochs = 500, num_mini_batches= 3 ! num_pairs =  # input/output pairs in training data

    type(mini_batch_t), allocatable :: mini_batches(:)
    type(input_output_pair_t), allocatable :: input_output_pairs(:)
    type(tensor_t), allocatable :: inputs(:)
    type(trainable_engine_t)  trainable_engine
    type(bin_t), allocatable :: bins(:)
    real, allocatable :: cost(:), random_numbers(:)

    call random_init(image_distinct=.true., repeatable=.true.)

    trainable_engine = perturbed_identity_network(perturbation_magnitude=0.2)
    call output(trainable_engine%to_inference_engine(), string_t("initial-network.json"))

    associate(num_inputs => trainable_engine%num_inputs(), num_outputs => trainable_engine%num_outputs())

      call assert(num_inputs == num_outputs,"trainable_engine_test_m(identity_mapping): # inputs == # outputs", &
        intrinsic_array_t([num_inputs, num_outputs]) &
      )
      block
        integer i, j
        inputs = [(tensor_t(real([(j*i, j = 1,num_inputs)])/(num_inputs*num_pairs)), i = 1, num_pairs)]
      end block
      associate(outputs => inputs)
        input_output_pairs = input_output_pair_t(inputs, outputs)
      end associate
      block 
        integer b
        bins = [(bin_t(num_items=num_pairs, num_bins=num_mini_batches, bin_number=b), b = 1, num_mini_batches)]
      end block

      allocate(random_numbers(2:size(input_output_pairs)))

      print *,"Cost"
      block
        integer e, b
        do e = 1,num_epochs
          call random_number(random_numbers)
          call shuffle(input_output_pairs)
          mini_batches = [(mini_batch_t(input_output_pairs(bins(b)%first():bins(b)%last())), b = 1, size(bins))]
          call trainable_engine%train(mini_batches, cost, adam=.true., learning_rate=1.5)
          print *,sum(cost)/size(cost)
        end do
      end block

      block
        real, parameter :: tolerance = 1.E-06
        integer p

        associate(network_outputs => trainable_engine%infer(inputs))
          print *," Outputs                          |&
                   Desired outputs                    |&
                   Errors"
          do p = 1, num_pairs
            print *,network_outputs(p)%values(),"|", inputs(p)%values(), "|",  network_outputs(p)%values() - inputs(p)%values()
          end do
        end associate
      end block

   end associate

   call output(trainable_engine%to_inference_engine(), final_network_file)

  end block

contains

  subroutine output(inference_engine, file_name)
    type(inference_engine_t), intent(in) :: inference_engine
    type(string_t), intent(in) :: file_name
    type(file_t) json_file
    json_file = inference_engine%to_json()
    call json_file%write_lines(file_name)
  end subroutine

  function perturbed_identity_network(perturbation_magnitude) result(trainable_engine)
    type(trainable_engine_t) trainable_engine
    real, intent(in) :: perturbation_magnitude
    integer, parameter :: nodes_per_layer(*) = [2, 2, 2, 2]
    integer, parameter :: max_n = maxval(nodes_per_layer), layers = size(nodes_per_layer)
    integer i
    real, parameter :: identity(*,*,*) = &
      reshape(real([( [1,0], [0,1], i=1,layers-1 )]), [max_n, max_n, layers-1])
    real w_harvest(size(identity,1), size(identity,2), size(identity,3)), b_harvest(size(identity,1), size(identity,3))

    call random_number(w_harvest)
    call random_number(b_harvest)

    associate(w => identity + perturbation_magnitude*(w_harvest-0.5)/0.5, b => perturbation_magnitude*(b_harvest-0.5)/0.5)

      trainable_engine = trainable_engine_t( &
        nodes = nodes_per_layer, weights = w, biases = b, differentiable_activation_strategy = relu_t(), &
        metadata = &
          [string_t("Perturbed Identity"), string_t("Damian Rouson"), string_t("2023-09-23"), string_t("relu"), string_t("false")] &
      )

    end associate
  end function

end program