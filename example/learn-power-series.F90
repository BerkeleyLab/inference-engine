! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module power_series
  !! Define a function that produces the desired network output for a given network input
  use inference_engine_m, only : tensor_t
  use assert_m, only : assert
  implicit none

contains
  elemental function y(x_in) result(a)
    type(tensor_t), intent(in) :: x_in
    type(tensor_t) a
    associate(x => x_in%values())
      call assert(ubound(x,1)>=7 .and. lbound(x,1)<=2,"y(x) :: sufficient input")
      a = tensor_t([1 + x(1) + (x(1)**2)/2 + (x(1)**3)/6, x(2), x(3), x(4), x(5), x(6)])
    end associate
  end function

end module

program learn_power_series
  !! This trains a neural network to learn the following six polynomial functions of its eight inputs.
  use inference_engine_m, only : &
    inference_engine_t, trainable_engine_t, mini_batch_t, tensor_t, input_output_pair_t, shuffle, relu_t
  use julienne_m, only : string_t, file_t, command_line_t, bin_t
  use assert_m, only : assert, intrinsic_array_t
  use power_series, only : y
  implicit none

  type(string_t) final_network_file
  type(command_line_t) command_line

  final_network_file = string_t(command_line%flag_value("--output-file"))

  if (len(final_network_file%string())==0) then
    error stop new_line('a') // new_line('a') // &
      'Usage: fpm run --example learn-power-series --profile release --flag "-fopenmp" -- --output-file "<file-name>"'
  end if

  block
    integer, parameter :: num_pairs = 10, num_epochs = 10000, num_mini_batches= 2 ! num_pairs =  # input/output pairs in training data

    type(mini_batch_t), allocatable :: mini_batches(:)
    type(input_output_pair_t), allocatable :: input_output_pairs(:)
    type(tensor_t), allocatable :: inputs(:), desired_outputs(:)
    type(trainable_engine_t)  trainable_engine
    type(bin_t), allocatable :: bins(:)
    real, allocatable :: cost(:), random_numbers(:)

    call random_init(image_distinct=.true., repeatable=.true.)
    trainable_engine = perturbed_identity_network(perturbation_magnitude=0.05)
    call output(trainable_engine%to_inference_engine(), string_t("initial-network.json"))

    associate(num_inputs => trainable_engine%num_inputs(), num_outputs => trainable_engine%num_outputs())

      block
        integer i, j
        integer, allocatable :: output_sizes(:)
        real white_noise(1:num_inputs,1:num_pairs)
        call random_number(white_noise)
        inputs = [(tensor_t(real([(white_noise(j,i), j = 1,num_inputs)])), i = 1, num_pairs)]
        desired_outputs = y(inputs)
        output_sizes = [(size(desired_outputs(i)%values()),i=1,size(desired_outputs))]
        call assert(all([num_outputs==output_sizes]), "fit-polynomials: # outputs", intrinsic_array_t([num_outputs,output_sizes])) 
      end block
      input_output_pairs = input_output_pair_t(inputs, desired_outputs)
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
#if defined _CRAYFTN || __GFORTRAN__
        type(tensor_t), allocatable :: network_outputs(:)
        network_outputs = trainable_engine%infer(inputs)
#else
        associate(network_outputs => trainable_engine%infer(inputs))
#endif
          print "(a,69x,a)","  Outputs", "| Desired outputs"
          do p = 1, num_pairs
            print "(6G13.5, a1, 6G13.5)",network_outputs(p)%values(),       "|", desired_outputs(p)%values()
          end do
#if defined _CRAYFTN || __GFORTRAN__
#else
        end associate
#endif
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

  pure function e(j,n) result(unit_vector)
    integer, intent(in) :: j, n
    integer k
    real, allocatable :: unit_vector(:)
    unit_vector = real([(merge(1,0,j==k),k=1,n)])
  end function

  function perturbed_identity_network(perturbation_magnitude) result(trainable_engine)
    type(trainable_engine_t) trainable_engine
    real, intent(in) :: perturbation_magnitude
    integer, parameter :: n(*) = [8, 196, 196, 196, 196, 6]
    integer, parameter :: n_max = maxval(n), layers = size(n)
    integer k, l
    real, allocatable :: identity(:,:,:), w_harvest(:,:,:), b_harvest(:,:)

    identity =  reshape( [( [(e(k,n_max), k=1,n_max)], l = 1, layers-1 )], [n_max, n_max, layers-1])

    allocate(w_harvest, mold = identity)
    allocate(b_harvest(size(identity,1), size(identity,3)))

    call random_number(w_harvest)
    call random_number(b_harvest)

    associate(w => identity + perturbation_magnitude*(w_harvest-0.5)/0.5, b => perturbation_magnitude*(b_harvest-0.5)/0.5)

      trainable_engine = trainable_engine_t( &
        nodes = n, weights = w, biases = b, metadata = &
          [string_t("Perturbed Identity"), string_t("Damian Rouson"), string_t("2023-09-23"), string_t("relu"), string_t("false")] &
      )

    end associate
  end function

end program
