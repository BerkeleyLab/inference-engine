! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program train_on_flat_distribution
  !! Train a neural network to represent the simplest cloud microphysics model from
  !! the Intermediate Complexity Atmospheric Research Model (ICAR) at
  !! https://github.com/BerkeleyLab/icar.

  !! Intrinic modules :
  use ieee_arithmetic, only : ieee_is_nan
  use iso_fortran_env, only : int64, real64

  !! External dependencies:
  use julienne_m, only : string_t, file_t, command_line_t, bin_t
  use assert_m, only : assert, intrinsic_array_t
  use inference_engine_m, only : &
    inference_engine_t, mini_batch_t, input_output_pair_t, tensor_t, trainable_engine_t, rkind, tensor_map_t, &
    training_configuration_t, shuffle

  !! Internal dependencies;
  use phase_space_bin_m, only : phase_space_bin_t
  use NetCDF_file_m, only: NetCDF_file_t
  use ubounds_m, only : ubounds_t
  implicit none

  character(len=*), parameter :: usage = &
    new_line('a') // new_line('a') // &
    'Usage: ' // new_line('a') // new_line('a') // &
    './build/run-fpm.sh run train-cloud-microphysics -- \' // new_line('a') // &
    '  --base <string> --epochs <integer> \' // new_line('a') // &
    '  [--start <integer>] [--end <integer>] [--stride <integer>] [--bins <integer>] [--report <integer>] [--tolerance <real>]'// &
    new_line('a') // new_line('a') // &
    'where angular brackets denote user-provided values and square brackets denote optional arguments.' // new_line('a') // &
    'The presence of a file named "stop" halts execution gracefully.'

  character(len=*), parameter :: training_config_file_name = "training_configuration.json"
  character(len=*), parameter :: plot_file_name = "cost.plt"

  integer(int64) t_start, t_finish, clock_rate
  integer plot_unit, num_epochs, previous_epoch, start_step, stride, num_bins, report_interval
  integer, allocatable :: end_step
  character(len=:), allocatable :: base_name
  real cost_tolerance

  call system_clock(t_start, clock_rate)
  call get_command_line_arguments(base_name, num_epochs, start_step, end_step, stride, num_bins, report_interval, cost_tolerance)
  call create_or_append_to(plot_file_name, plot_unit, previous_epoch)
  call read_train_write( &
    training_configuration_t(file_t(string_t(training_config_file_name))), base_name, plot_unit, previous_epoch, num_epochs &
  )
  call system_clock(t_finish)

  print *,"System clock time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
  print *,new_line('a') // "______training_cloud_microhpysics done _______"

contains

  subroutine create_or_append_to(plot_file_name, plot_unit, previous_epoch)
    character(len=*), intent(in) :: plot_file_name
    integer, intent(out) :: plot_unit, previous_epoch
 
    !local variables:
    logical preexisting_plot_file

    inquire(file=plot_file_name, exist=preexisting_plot_file)
    open(newunit=plot_unit,file="cost.plt",status="unknown",position="append")

    if (.not. preexisting_plot_file) then
      write(plot_unit,*) "      Epoch  Cost (avg)"
      previous_epoch = 0
    else
      associate(plot_file => file_t(string_t(plot_file_name)))
        associate(lines => plot_file%lines())
          associate(num_lines => size(lines))
            if (num_lines == 0) then
              previous_epoch = 0
            else
              block
                character(len=:), allocatable :: last_line
                last_line = lines(size(lines))%string()
                read(last_line,*) previous_epoch
              end block
            end if
          end associate
        end associate
      end associate
    end if
  end subroutine

  subroutine get_command_line_arguments &
    (base_name, num_epochs, start_step, end_step, stride, num_bins, report_interval, cost_tolerance)
    character(len=:), allocatable, intent(out) :: base_name
    integer, intent(out) :: num_epochs, start_step, stride, num_bins, report_interval
    integer, intent(out), allocatable :: end_step
    real, intent(out) :: cost_tolerance

    ! local variables
    type(command_line_t) command_line
    character(len=:), allocatable :: &
      stride_string, epochs_string, start_string, end_string, bins_string, report_string, tolerance_string

    base_name = command_line%flag_value("--base") ! gfortran 13 seg faults if this is an association
    epochs_string = command_line%flag_value("--epochs")
    start_string = command_line%flag_value("--start")
    end_string = command_line%flag_value("--end")
    stride_string = command_line%flag_value("--stride")
    bins_string = command_line%flag_value("--bins")
    report_string = command_line%flag_value("--report")
    tolerance_string = command_line%flag_value("--tolerance")

    associate(required_arguments => len(base_name)/=0 .and. len(epochs_string)/=0)
       if (.not. required_arguments) error stop usage 
    end associate

    read(epochs_string,*) num_epochs

    if (len(stride_string)==0) then
      stride = 1
    else
      read(stride_string,*) stride
    end if

    if (len(start_string)==0) then
      start_step = 1
    else
      read(start_string,*) start_step
    end if

    if (len(report_string)==0) then
      report_interval = 1
    else
      read(report_string,*) report_interval
    end if

    if (len(bins_string)/=0) then
      read(bins_string,*) num_bins
    else
      num_bins = 1
    end if

    if (len(end_string)/=0) then
      if (.not. allocated(end_step)) allocate(end_step)
      read(end_string,*) end_step
    end if
 
    if (len(start_string)==0) then
      start_step = 1
    else
      read(start_string,*) start_step
    end if

    if (len(tolerance_string)==0) then
      cost_tolerance = 5.0E-08
    else
      read(tolerance_string,*) cost_tolerance
    end if

  end subroutine get_command_line_arguments

  subroutine read_train_write(training_configuration, base_name, plot_unit, previous_epoch, num_epochs)
    type(training_configuration_t), intent(in) :: training_configuration
    character(len=*), intent(in) :: base_name
    integer, intent(in) :: plot_unit, previous_epoch, num_epochs

    ! local variables:
    real, allocatable, dimension(:,:,:,:) :: &
      pressure_in , potential_temperature_in , temperature_in , &
      pressure_out, potential_temperature_out, temperature_out, &
      qv_out, qc_out, qr_out, qs_out, &
      qv_in , qc_in , qr_in , qs_in , &
      dpt_dt, dqv_dt, dqc_dt, dqr_dt, dqs_dt
    type(ubounds_t), allocatable :: ubounds(:)
    double precision, allocatable, dimension(:) :: time_in, time_out
    integer, allocatable :: lbounds(:)
    integer t, b, t_end
    logical stop_requested

    associate( network_file => base_name // "_network.json")
      associate(network_input => base_name // "_input.nc")
        print *,"Reading network inputs from " // network_input
        associate(network_input_file => netCDF_file_t(network_input))
          ! Skipping the following unnecessary inputs that are in the current file format as of 14 Aug 2023:
          ! precipitation, snowfall
          call network_input_file%input("pressure", pressure_in)
          call network_input_file%input("potential_temperature", potential_temperature_in)
          call network_input_file%input("temperature", temperature_in)
          call network_input_file%input("qv", qv_in)
          call network_input_file%input("qc", qc_in)
          call network_input_file%input("qr", qr_in)
          call network_input_file%input("qs", qs_in)
          call network_input_file%input("time", time_in)
          t_end = size(time_in)
          lbounds = [lbound(pressure_in), lbound(temperature_in), lbound(qv_in), lbound(qc_in), lbound(qr_in), lbound(qs_in)]
          ubounds = &
            [ubounds_t(ubound(qv_in)), ubounds_t(ubound(qc_in)), ubounds_t(ubound(qr_in)), ubounds_t(ubound(qs_in)), &
             ubounds_t(ubound(pressure_in)), ubounds_t(ubound(temperature_in)) &
            ]
        end associate
      end associate

      associate(network_output => base_name // "_output.nc")
        print *,"Reading network outputs from " // network_output
        associate(network_output_file => netCDF_file_t(network_output))
          call network_output_file%input("potential_temperature", potential_temperature_out)
          ! Skipping the following unnecessary outputs that are in the current file format as of 14 Aug 2023:
          ! pressure, temperature, precipitation, snowfall
          call network_output_file%input("qv", qv_out)
          call network_output_file%input("qc", qc_out)
          call network_output_file%input("qr", qr_out)
          call network_output_file%input("qs", qs_out)
          call network_output_file%input("time", time_out)
          lbounds = [lbounds, lbound(qv_out), lbound(qc_out), lbound(qr_out), lbound(qs_out)]
          ubounds = [ubounds, ubounds_t(ubound(qv_out)), ubounds_t(ubound(qc_out)), &
            ubounds_t(ubound(qr_out)), ubounds_t(ubound(qs_out))]
          call assert(all(lbounds == 1), "main: default input/output lower bounds", intrinsic_array_t(lbounds))
          call assert(all(ubounds == ubounds(1)), "main: matching input/output upper bounds")
          block
            double precision, parameter :: time_tolerance = 1.E-07
            associate(matching_time_stamps => all(abs(time_in(2:t_end) - time_out(1:t_end-1))<time_tolerance))
              call assert(matching_time_stamps, "main: matching time stamps")
            end associate
          end block
        end associate
      end associate

      print *,"Calculating time derivatives"
  
      allocate(dpt_dt, mold = potential_temperature_out)
      allocate(dqv_dt, mold = qv_out)
      allocate(dqc_dt, mold = qc_out)
      allocate(dqr_dt, mold = qr_out)
      allocate(dqs_dt, mold = qs_out)

      associate(dt => real(time_out - time_in, rkind))
        do concurrent(t = 1:t_end)
          dpt_dt(:,:,:,t) = (potential_temperature_out(:,:,:,t) - potential_temperature_in(:,:,:,t))/dt(t)
          dqv_dt(:,:,:,t) = (qv_out(:,:,:,t)- qv_in(:,:,:,t))/dt(t)
          dqc_dt(:,:,:,t) = (qc_out(:,:,:,t)- qc_in(:,:,:,t))/dt(t)
          dqr_dt(:,:,:,t) = (qr_out(:,:,:,t)- qr_in(:,:,:,t))/dt(t)
          dqs_dt(:,:,:,t) = (qs_out(:,:,:,t)- qs_in(:,:,:,t))/dt(t)
        end do
      end associate

      call assert(.not. any(ieee_is_nan(dpt_dt)), ".not. any(ieee_is_nan(dpt_dt)")
      call assert(.not. any(ieee_is_nan(dqv_dt)), ".not. any(ieee_is_nan(dqv_dt)")
      call assert(.not. any(ieee_is_nan(dqc_dt)), ".not. any(ieee_is_nan(dqc_dt)")
      call assert(.not. any(ieee_is_nan(dqr_dt)), ".not. any(ieee_is_nan(dqr_dt)")
      call assert(.not. any(ieee_is_nan(dqs_dt)), ".not. any(ieee_is_nan(dqs_dt)")

      train_network: &
      block
        type(trainable_engine_t) trainable_engine
        type(mini_batch_t), allocatable :: mini_batches(:)
        type(bin_t), allocatable :: bins(:)
        type(input_output_pair_t), allocatable :: input_output_pairs(:)
        type(tensor_t), allocatable, dimension(:) :: inputs, outputs
        real(rkind), allocatable :: cost(:)
        integer i, batch, lon, lat, level, time, network_unit, io_status, epoch
        integer(int64) start_training, finish_training

        open(newunit=network_unit, file=network_file, form='formatted', status='old', iostat=io_status, action='read')

        if (.not. allocated(end_step)) end_step = t_end

        print *,"Defining tensors from time step", start_step, "through", end_step, "with strides of", stride

        ! The following temporary copies are required by gfortran bug 100650 and possibly 49324
        ! See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=100650 and https://gcc.gnu.org/bugzilla/show_bug.cgi?id=49324
        inputs = [( [( [( [( &
          tensor_t( &
          [ pressure_in(lon,lat,level,time), potential_temperature_in(lon,lat,level,time), temperature_in(lon,lat,level,time), &
            qv_in(lon,lat,level,time), qc_in(lon,lat,level,time), qr_in(lon,lat,level,time), qs_in(lon,lat,level,time) &
          ] &
          ), lon = 1, size(qv_in,1))], lat = 1, size(qv_in,2))], level = 1, size(qv_in,3))], time = start_step, end_step, stride)]
 
        outputs = [( [( [( [( &
          tensor_t( &
            [dpt_dt(lon,lat,level,time), dqv_dt(lon,lat,level,time), dqc_dt(lon,lat,level,time), dqr_dt(lon,lat,level,time), &
             dqs_dt(lon,lat,level,time) &
            ] &
          ), lon = 1, size(qv_in,1))], lat = 1, size(qv_in,2))], level = 1, size(qv_in,3))], time = start_step, end_step, stride)]

        print *,"Calculating output tensor component ranges."
        associate( &
          output_minima => [minval(dpt_dt), minval(dqv_dt), minval(dqc_dt), minval(dqr_dt), minval(dqs_dt)], &
          output_maxima => [maxval(dpt_dt), maxval(dqv_dt), maxval(dqc_dt), maxval(dqr_dt), maxval(dqs_dt)] &
        )
        associate( output_map => tensor_map_t(layer = "outputs", minima = output_minima, maxima = output_maxima))
          read_or_initialize_engine: &
          if (io_status==0) then
            print *,"Reading network from file " // network_file
            trainable_engine = trainable_engine_t(inference_engine_t(file_t(string_t(network_file))))
            close(network_unit)
          else
            close(network_unit)

            initialize_network: &
            block
              character(len=len('YYYYMMDD')) date

              call date_and_time(date)

              print *,"Calculating input tensor component ranges."
              associate( &
                input_map => tensor_map_t( &
                  layer  = "inputs", &
                  minima = [minval(pressure_in), minval(potential_temperature_in), minval(temperature_in), &
                    minval(qv_in), minval(qc_in), minval(qr_in), minval(qs_in)], &
                  maxima = [maxval(pressure_in), maxval(potential_temperature_in), maxval(temperature_in), &
                    maxval(qv_in), maxval(qc_in), maxval(qr_in), maxval(qs_in)] &
              ) ) 
                associate(activation => training_configuration%differentiable_activation_strategy())
                  associate(residual_network => string_t(trim(merge("true ", "false", training_configuration%skip_connections())))) 
                    trainable_engine = trainable_engine_t( &
                      training_configuration,  &
                      perturbation_magnitude = 0.05, &
                      metadata = [ &
                        string_t("Simple microphysics"), string_t("train-on-flat-dist"), string_t(date), activation%function_name(), &
                        residual_network &
                      ], input_map = input_map, output_map = output_map &
                    )
                  end associate
                end associate
              end associate ! input_map, date_string
            end block initialize_network
          end if read_or_initialize_engine

          print *, "Conditionally sampling for a flat distribution of output values"
          block
            integer i
            logical occupied(num_bins, num_bins, num_bins, num_bins, num_bins)
            logical keepers(size(outputs))
            type(phase_space_bin_t), allocatable :: bin(:)
            occupied = .false.
            keepers = .false.

            bin = [(phase_space_bin_t(outputs(i), output_minima, output_maxima, num_bins), i=1,size(outputs))]

            do i = 1, size(outputs)
              if (occupied(bin(i)%loc(1),bin(i)%loc(2),bin(i)%loc(3),bin(i)%loc(4),bin(i)%loc(5))) cycle
              occupied(bin(i)%loc(1),bin(i)%loc(2),bin(i)%loc(3),bin(i)%loc(4),bin(i)%loc(5)) = .true.
              keepers(i) = .true.
            end do 
            input_output_pairs = input_output_pair_t(pack(inputs, keepers), pack(outputs, keepers))
            print *, "Kept ", size(input_output_pairs), " out of ", size(outputs, kind=int64), " input/output pairs " // &
                     " in ", count(occupied)," out of ", size(occupied, kind=int64), " bins."
          end block
        end associate ! output_map
        end associate 

        print *,"Normalizing the remaining input and output tensors"
        input_output_pairs = trainable_engine%map_to_training_ranges(input_output_pairs)

        associate( &
          num_pairs => size(input_output_pairs), &
          n_bins => training_configuration%mini_batches(), &
          adam => merge(.true., .false., training_configuration%optimizer_name() == "adam"), &
          learning_rate => training_configuration%learning_rate() &
        )
          bins = [(bin_t(num_items=num_pairs, num_bins=n_bins, bin_number=b), b = 1, n_bins)]

          print *,"Training network"
          print *, "       Epoch  Cost (avg)"

          call system_clock(start_training)

          associate(starting_epoch => previous_epoch + 1, ending_epoch => previous_epoch + num_epochs)
            epochs: &
            do epoch = starting_epoch, ending_epoch

              if (size(bins)>1) call shuffle(input_output_pairs) ! set up for stochastic gradient descent
              mini_batches = [(mini_batch_t(input_output_pairs(bins(b)%first():bins(b)%last())), b = 1, size(bins))]
              call trainable_engine%train(mini_batches, cost, adam, learning_rate)

              associate(average_cost => sum(cost)/size(cost))
                associate(converged => average_cost <= cost_tolerance)
                  if (any([converged, epoch == [starting_epoch, ending_epoch], mod(epoch, report_interval)==0])) then
                    print *, epoch, average_cost
                    write(plot_unit,*) epoch, average_cost
                    open(newunit=network_unit, file=network_file, form='formatted', status='unknown', iostat=io_status, action='write')
                    associate(inference_engine => trainable_engine%to_inference_engine())
                      associate(json_file => inference_engine%to_json())
                        call json_file%write_lines(string_t(network_file))
                      end associate
                    end associate
                    close(network_unit)
                  end if
                  signal_convergence: & 
                  if (converged) then
                    block
                      integer unit
                      open(newunit=unit, file="converged", status="unknown") ! The train.sh script detects & removes this file.
                      close(unit)
                      exit epochs
                    end block
                  end if signal_convergence
                end associate
              end associate

              inquire(file="stop", exist=stop_requested)

              graceful_exit: &
              if (stop_requested) then
                print *,'Shutting down because a file named "stop" was found.'
                return
              end if graceful_exit

            end do epochs
          end associate
        end associate

        call system_clock(finish_training)
        print *,"Training time: ", real(finish_training - start_training, real64)/real(clock_rate, real64),"for",num_epochs,"epochs"

      end block train_network
    end associate ! network_file

    close(plot_unit)

  end subroutine read_train_write

  pure function normalize(x, x_min, x_max) result(x_normalized)
    real(rkind), intent(in) :: x(:,:,:,:), x_min, x_max
    real(rkind), allocatable :: x_normalized(:,:,:,:)
    call assert(x_min/=x_max, "train_cloud_microphysics(normaliz): x_min/=x_max")
    x_normalized = (x - x_min)/(x_max - x_min)
  end function

end program train_on_flat_distribution
