! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module saturated_mixing_ratio_m
  !! Define a function that produces the desired network output for a given network input
  use inference_engine_m, only : tensor_t
  use assert_m, only : assert
  implicit none
 
  private
  public :: T, p, y

  real, parameter :: freezing_threshold = 273.15       ! [K]
  real, parameter :: T_min = 236.352524, T_max = 307.610779
  real, parameter :: p_min = 29671.1348, p_max = 98596.7578
  integer, parameter :: resolution = 10
  integer i
  real, parameter :: T(*) = [(real(i)/real(resolution), i=0,resolution)]
  real, parameter :: p(*) = [(real(i)/real(resolution), i=0,resolution)]

contains

  pure function saturated_mixing_ratio(T_normalized, p_normalized) result(sat_mr)
    !! Calculate the saturated mixing ratio for normalized tempetatures (k) and pressures (Pa)
    real,intent(in) :: T_normalized, p_normalized
    real sat_mr

    associate( &
     temperature => T_min + (T_max - T_min)*T_normalized, &
     pressure => p_min + (p_max - p_min)*p_normalized &
    )
      associate(below_freezing => temperature < freezing_threshold)
        associate( &
          a => merge(21.8745584, 17.2693882, below_freezing), &
          b => merge(7.66, 35.86, below_freezing) &
        )
          associate(p_threshold => 610.78 * exp(a * (temperature - 273.16) / (temperature - b))) !(Pa))
            associate(e_s => merge(pressure * 0.99999, p_threshold, (pressure - p_threshold) <= 0))
              sat_mr = 0.6219907 * e_s / (pressure - e_s) !(kg/kg)
            end associate
          end associate
        end associate
      end associate
    end associate
  end function

  elemental function y(x_in) result(a)
    type(tensor_t), intent(in) :: x_in
    type(tensor_t) a
    associate(x => x_in%values())
      call assert(lbound(x,1)==1 .and. ubound(x,1)==2,"y(x) :: sufficient input")
      a = tensor_t([saturated_mixing_ratio(x(1),x(2))])
    end associate
  end function
 

end module

program train_saturated_mixture_ratio
  !! This program trains a neural network to learn the saturated mixing ratio function of ICAR.
  use inference_engine_m, only : &
    inference_engine_t, trainable_engine_t, mini_batch_t, tensor_t, input_output_pair_t, shuffle, relu_t
  use sourcery_m, only : string_t, file_t, command_line_t, bin_t, csv
  use assert_m, only : assert, intrinsic_array_t
  use saturated_mixing_ratio_m, only : y, T, p
  use iso_fortran_env, only : int64, output_unit
  implicit none

  type(string_t) network_file
  type(command_line_t) command_line
  integer(int64) counter_start, counter_end, clock_rate

  network_file = string_t(command_line%flag_value("--output-file"))

  if (len(network_file%string())==0) then
    error stop new_line('a') // new_line('a') // &
      'Usage: ./build/run-fpm.sh run --example learn-saturated-mixing-ratio -- --output-file "<file-name>"' 
  end if

  call system_clock(counter_start, clock_rate)

  block
    integer, parameter :: num_epochs = 10000000, num_mini_batches = 6
    integer num_pairs ! number of input/output pairs

    type(mini_batch_t), allocatable :: mini_batches(:)
    type(input_output_pair_t), allocatable :: input_output_pairs(:)
    type(tensor_t), allocatable :: inputs(:), desired_outputs(:)
    type(trainable_engine_t)  trainable_engine
    type(bin_t), allocatable :: bins(:)
    real, allocatable :: cost(:), random_numbers(:)
    integer io_status, network_unit, plot_unit, previous_epoch
    integer, parameter :: io_success=0
    integer, parameter :: nodes_per_layer(*) = [2, 31, 31, 1]

    call random_init(image_distinct=.true., repeatable=.true.)

    open(newunit=network_unit, file=network_file%string(), form='formatted', status='old', iostat=io_status, action='read')

    if (io_status == io_success) then
      print *,"Reading network from file " // network_file%string()
      trainable_engine = trainable_engine_t(inference_engine_t(file_t(network_file)))
      close(network_unit)
    else
      close(network_unit)
      print *,"Initializing a new network"
      trainable_engine = perturbed_identity_network(perturbation_magnitude=0.05, n = nodes_per_layer)
    end if
    call output(trainable_engine%to_inference_engine(), string_t("initial-network.json"))

    associate(num_inputs => trainable_engine%num_inputs(), num_outputs => trainable_engine%num_outputs())

      block
        integer i, j
        integer, allocatable :: output_sizes(:)
        inputs = [( [(tensor_t([T(i), p(j)]), j=1,size(p))], i = 1,size(T))]
        num_pairs = size(inputs)
        call assert(num_pairs == size(T)*size(p), "train_cloud_microphysics: inputs tensor array complete")
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

      print *, "        Epoch | Cost Function| System_Clock | Nodes per Layer"

      call open_plot_file_for_appending("cost.plt", plot_unit, previous_epoch)

      block
        integer e, b, stop_unit
        do e = previous_epoch + 1, previous_epoch + num_epochs
          call random_number(random_numbers)
          call shuffle(input_output_pairs, random_numbers)
          mini_batches = [(mini_batch_t(input_output_pairs(bins(b)%first():bins(b)%last())), b = 1, size(bins))]
          call trainable_engine%train(mini_batches, cost, adam=.true.)
          associate(cost_avg => sum(cost)/size(cost))
            if (mod(e, 1000)==0) then
              call system_clock(counter_end, clock_rate)
              write(output_unit,fmt='(3(g13.5,2x))', advance='no') e, cost_avg,  real(counter_end - counter_start) / real(clock_rate)
              write(output_unit, fmt=csv) nodes_per_layer
              write(plot_unit,fmt='(3(g13.5,2x))', advance='no') e, cost_avg,  real(counter_end - counter_start) / real(clock_rate)
              write(plot_unit, fmt=csv) nodes_per_layer
            end if
            if (mod(e, 10000)==0) call output(trainable_engine%to_inference_engine(), network_file)
            if (cost_avg < 1.E-08) exit
            open(newunit=stop_unit, file="stop", form='formatted', status='old', iostat=io_status)
            if (io_status==0) exit
          end associate
        end do

        call system_clock(counter_end, clock_rate)

        write(output_unit,fmt='(3(g13.5,2x))', advance='no') e, sum(cost)/size(cost),  real(counter_end - counter_start) / real(clock_rate)
        write(output_unit, fmt=csv) nodes_per_layer
        write(plot_unit,fmt='(3(g13.5,2x))', advance='no') e, sum(cost)/size(cost),  real(counter_end - counter_start) / real(clock_rate)
        write(plot_unit, fmt=csv) nodes_per_layer

        block
          integer p

          associate(network_outputs => trainable_engine%infer(inputs))
            print *,"Inputs (normalized)          | Outputs      | Desired outputs"
            do p = 1, num_pairs
              print "(4(G13.5,2x))", inputs(p)%values(), network_outputs(p)%values(), desired_outputs(p)%values()
            end do
          end associate
        end block

      end block

      close(plot_unit)

   end associate

   call output(trainable_engine%to_inference_engine(), network_file)

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

  function perturbed_identity_network(perturbation_magnitude, n) result(trainable_engine)
    type(trainable_engine_t) trainable_engine
    real, intent(in) :: perturbation_magnitude
    integer, intent(in)  :: n(:)
    integer j, k, l
    real, allocatable :: identity(:,:,:), w_harvest(:,:,:), b_harvest(:,:)

    associate(n_max => maxval(n), layers => size(n))
      identity =  reshape( [( [(e(k,n_max), k=1,n_max)], l = 1, layers-1 )], [n_max, n_max, layers-1])

      allocate(w_harvest, mold = identity)
      allocate(b_harvest(size(identity,1), size(identity,3)))

      call random_number(w_harvest)
      call random_number(b_harvest)

      associate(w => identity + perturbation_magnitude*(w_harvest-0.5)/0.5, b => perturbation_magnitude*(b_harvest-0.5)/0.5)

        trainable_engine = trainable_engine_t( &
          nodes = n, weights = w, biases = b, differentiable_activation_strategy = relu_t(), &
          metadata = &
            [string_t("Perturbed Identity"), string_t("Damian Rouson"), string_t("2023-09-23"), string_t("relu"), string_t("false")] &
        )
      end associate
    end associate
  end function

  subroutine open_plot_file_for_appending(plot_file_name, plot_unit, previous_epoch)
    character(len=*), intent(in) :: plot_file_name
    integer, intent(out) :: plot_unit, previous_epoch

    type(file_t) plot_file
    type(string_t), allocatable :: lines(:)
    character(len=:), allocatable :: last_line
    integer io_status
    integer, parameter :: io_success = 0
    logical preexisting_plot_file

    inquire(file=plot_file_name, exist=preexisting_plot_file)
    open(newunit=plot_unit,file="cost.plt",status="unknown",position="append")

    associate(header => "Epoch | Nodes/layer | System_clock      | Cost function")
      if (.not. preexisting_plot_file) then
        write(plot_unit,*) header
        previous_epoch = 0
      else
        plot_file = file_t(string_t(plot_file_name))
        lines = plot_file%lines()
        last_line = lines(size(lines))%string()
        read(last_line,*, iostat=io_status) previous_epoch
        if ((io_status /= io_success .and. last_line == header) .or. len(trim(last_line))==0) previous_epoch = 0
      end if
    end associate

  end subroutine

end program
