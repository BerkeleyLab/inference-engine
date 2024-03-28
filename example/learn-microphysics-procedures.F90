! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program learn_microphysics_procedures
  !! Train a neural network proxies for procedures in the Thompson microphysics model
  !! in of ICAR (https://github.com/BerkeleyLab/icar).
  use inference_engine_m, only : &
    inference_engine_t, trainable_engine_t, mini_batch_t, tensor_t, input_output_pair_t, shuffle, sigmoid_t 
  use sourcery_m, only : string_t, file_t, command_line_t, bin_t, csv
  use assert_m, only : assert, intrinsic_array_t
  use thompson_tensors_m, only : y, T, p
  use iso_fortran_env, only : int64, output_unit
  implicit none

  type(string_t) network_file
  type(command_line_t) command_line
  integer(int64) counter_start, counter_end, clock_rate

  network_file = string_t(command_line%flag_value("--output-file"))

  if (len(network_file%string())==0) then
    error stop new_line('a') // new_line('a') // &
      'Usage: fpm run learn-microphysics-procedures --profile release --flag "-fopenmp" -- --output-file "<file-name>"'
  end if

  call system_clock(counter_start, clock_rate)

  block
    integer, parameter :: max_num_epochs = 10000000, num_mini_batches = 10
    integer num_pairs ! number of input/output pairs

    type(mini_batch_t), allocatable :: mini_batches(:)
    type(input_output_pair_t), allocatable :: input_output_pairs(:)
    type(tensor_t), allocatable :: inputs(:), desired_outputs(:)
    type(trainable_engine_t)  trainable_engine
    type(bin_t), allocatable :: bins(:)
    real, allocatable :: cost(:), random_numbers(:)
    integer io_status, network_unit, plot_unit
    integer, parameter :: io_success=0, diagnostics_print_interval = 1000, network_save_interval = 10000
    integer, parameter :: nodes_per_layer(*) = [2, 72, 2]
    real, parameter :: cost_tolerance = 1.E-08

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

      block
        integer e, b, stop_unit, previous_epoch
        real previous_clock_time

        call open_plot_file_for_appending("cost.plt", plot_unit, previous_epoch, previous_clock_time)
        print *, "        Epoch | Cost Function| System_Clock | Nodes per Layer"
        allocate(random_numbers(2:size(input_output_pairs)))

        do e = previous_epoch + 1, previous_epoch + max_num_epochs
          call random_number(random_numbers)
          call shuffle(input_output_pairs)
          mini_batches = [(mini_batch_t(input_output_pairs(bins(b)%first():bins(b)%last())), b = 1, size(bins))]
          call trainable_engine%train(mini_batches, cost, adam=.true., learning_rate=1.5)
          call system_clock(counter_end, clock_rate)

          associate( &
            cost_avg => sum(cost)/size(cost), &
            cumulative_clock_time => previous_clock_time +  real(counter_end - counter_start) / real(clock_rate), &
            loop_ending => e == previous_epoch + max_num_epochs &
          )
            write_and_exit_if_converged: &
            if (cost_avg < cost_tolerance) then
              call print_diagnostics(plot_unit, e, cost_avg, cumulative_clock_time, nodes_per_layer)
              call output(trainable_engine%to_inference_engine(), network_file)
              exit
            end if write_and_exit_if_converged

            open(newunit=stop_unit, file="stop", form='formatted', status='old', iostat=io_status)

            write_and_exit_if_stop_file_exists: &
            if (io_status==0) then
              call print_diagnostics(plot_unit, e, cost_avg, cumulative_clock_time, nodes_per_layer)
              call output(trainable_engine%to_inference_engine(), network_file)
              exit
            end if write_and_exit_if_stop_file_exists

            if (mod(e,diagnostics_print_interval)==0 .or. loop_ending) &
              call print_diagnostics(plot_unit, e, cost_avg, cumulative_clock_time, nodes_per_layer)
            if (mod(e,network_save_interval)==0 .or. loop_ending) call output(trainable_engine%to_inference_engine(), network_file)
          end associate
        end do

        close(plot_unit)

        report_network_performance: &
        block
          integer p

          associate(network_outputs => trainable_engine%infer(inputs))
            print *," Inputs (normalized)          | Outputs                    | Desired outputs"
            do p = 1, num_pairs
              print "(6(G13.5,2x))", inputs(p)%values(), network_outputs(p)%values(), desired_outputs(p)%values()
            end do
          end associate
        end block report_network_performance

      end block

   end associate

   call output(trainable_engine%to_inference_engine(), network_file)

  end block

contains

  subroutine print_diagnostics(plot_file_unit, epoch, cost, clock, nodes)
     integer, intent(in) :: plot_file_unit, epoch, nodes(:)
     real, intent(in) :: cost, clock

     write(unit=output_unit, fmt='(3(g13.5,2x))', advance='no') epoch, cost, clock
     write(unit=output_unit, fmt=csv) nodes
     write(unit=plot_file_unit, fmt='(3(g13.5,2x))', advance='no') epoch, cost, clock
     write(unit=plot_file_unit, fmt=csv) nodes
  end subroutine

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
          nodes = n, weights = w, biases = b, differentiable_activation_strategy = sigmoid_t(), &
          metadata = &
            [string_t("Thompson microphysics procedures"), string_t("Damian Rouson"), string_t("2023-09-23"), string_t("sigmoid"), &
             string_t("false")] &
        )
      end associate
    end associate
  end function

  subroutine open_plot_file_for_appending(plot_file_name, plot_unit, previous_epoch, previous_clock)
    character(len=*), intent(in) :: plot_file_name
    integer, intent(out) :: plot_unit, previous_epoch
    real, intent(out) :: previous_clock

    type(file_t) plot_file
    type(string_t), allocatable :: lines(:)
    character(len=:), allocatable :: last_line
    integer io_status
    integer, parameter :: io_success = 0
    logical preexisting_plot_file
    real cost

    inquire(file=plot_file_name, exist=preexisting_plot_file)
    open(newunit=plot_unit,file="cost.plt",status="unknown",position="append")

    associate(header => "        Epoch | Cost Function| System_Clock | Nodes per Layer")
      if (.not. preexisting_plot_file) then
        write(plot_unit,*) header
        previous_epoch = 0
        previous_clock = 0
      else
        plot_file = file_t(string_t(plot_file_name))
        lines = plot_file%lines()
        last_line = lines(size(lines))%string()
        read(last_line,*, iostat=io_status) previous_epoch, cost, previous_clock
        if ((io_status /= io_success .and. last_line == header) .or. len(trim(last_line))==0) then
          previous_epoch = 0
          previous_clock = 0
        end if
      end if
    end associate

  end subroutine

end program
