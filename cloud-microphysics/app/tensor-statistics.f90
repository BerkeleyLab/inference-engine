! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program tensor_statistics
  !! Train a neural network to represent the simplest cloud microphysics model from
  !! the Intermediate Complexity Atmospheric Research Model (ICAR) at
  !! https://github.com/BerkeleyLab/icar.

  !! External dependencies:
  use sourcery_m, only : string_t, file_t, command_line_t, bin_t
  use assert_m, only : assert, intrinsic_array_t
  use ieee_arithmetic, only : ieee_is_nan
  use iso_fortran_env, only : int64, real64

  !! Internal dependencies;
  use inference_engine_m, only : &
    inference_engine_t, mini_batch_t, input_output_pair_t, tensor_t, trainable_engine_t, rkind, &
    training_configuration_t, shuffle
  use NetCDF_file_m, only: NetCDF_file_t
  use ubounds_m, only : ubounds_t
  implicit none

  type histogram_on_unit_interval_t
    character(len=:), allocatable :: variable_name
    real unmapped_min, unmapped_max
    real, allocatable :: frequency(:), bin_midpoint(:)
  end type

  character(len=*), parameter :: usage = &
    new_line('a') // new_line('a') // &
    'Usage: ' // new_line('a') // new_line('a') // &
    './build/run-fpm.sh run train-cloud-microphysics -- \' // new_line('a') // &
    '  --base <string> --epochs <integer> \' // new_line('a') // &
    '  [--start <integer>] [--end <integer>] [--stride <integer>]' // &
    new_line('a') // new_line('a') // &
    'where angular brackets denote user-provided values and square brackets denote optional arguments.' // new_line('a') // &
    'The presence of a file named "stop" halts execution gracefully.'

  character(len=*), parameter :: training_config_file_name = "training_configuration.json"
  character(len=*), parameter :: plot_file_name = "cost.plt"

  integer(int64) t_start, t_finish, clock_rate
  integer plot_unit, num_epochs, previous_epoch, start_step, stride
  integer, allocatable :: end_step
  character(len=:), allocatable :: base_name

  call system_clock(t_start, clock_rate)
  call get_command_line_arguments(base_name, num_epochs, start_step, end_step, stride)
  call read_train_write( &
    training_configuration_t(file_t(string_t(training_config_file_name))), base_name, plot_unit, previous_epoch, num_epochs &
  )
  call system_clock(t_finish)

  print *,"System clock time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
  print *,new_line('a') // "______training_cloud_microhpysics done _______"

contains

  pure function normalize(x, x_min, x_max) result(x_normalized)
    real(rkind), intent(in) :: x(:,:,:,:), x_min, x_max
    real(rkind), allocatable :: x_normalized(:,:,:,:)
    call assert(x_min/=x_max, "train_cloud_microphysics(normaliz): x_min/=x_max")
    x_normalized = (x - x_min)/(x_max - x_min)
  end function

  pure function histogram_on_unit_interval(v, variable_name, num_bins) result(histogram)
    real, intent(in) :: v(:,:,:,:)
    character(len=*), intent(in) :: variable_name
    integer, intent(in) :: num_bins
    type(histogram_on_unit_interval_t) histogram

    real, parameter :: v_mapped_min = 0., v_mapped_max = 1.
    integer, allocatable :: in_bin(:)
    integer i

    histogram%variable_name = variable_name

    associate(v_min => minval(v), v_max => maxval(v), data_set_size => size(v))

      histogram%unmapped_min = v_min
      histogram%unmapped_max = v_max

      associate(v_mapped => normalize(v, v_min, v_max), dv => (v_mapped_max - v_mapped_min)/real(num_bins))

        allocate(histogram%frequency(num_bins), histogram%bin_midpoint(num_bins), in_bin(num_bins))

        do concurrent(i = 1:num_bins) 
          associate(v_bin_min => v_mapped_min + (i-1)*dv, v_bin_max => merge(i*dv, v_mapped_max + .1*abs(dv), i/=num_bins))
            in_bin(i) = count(v_mapped >= v_bin_min .and. v_mapped < v_bin_max) ! replace with Fortran 2023 reduction when available
            histogram%frequency(i) = real(in_bin(i)) / real(data_set_size)
            histogram%bin_midpoint(i) = v_bin_min + 0.5*dv
          end associate
        end do
        call assert(data_set_size == sum(in_bin), "histogram: lossless binning", intrinsic_array_t([data_set_size, sum(in_bin)]))
      end associate
    end associate
  end function

  subroutine get_command_line_arguments(base_name, num_epochs, start_step, end_step, stride)
    character(len=:), allocatable, intent(out) :: base_name
    integer, intent(out) :: num_epochs, start_step, stride
    integer, intent(out), allocatable :: end_step

    ! local variables
    type(command_line_t) command_line
    character(len=:), allocatable :: stride_string, epochs_string, start_string, end_string

    base_name = command_line%flag_value("--base") ! gfortran 13 seg faults if this is an association
    epochs_string = command_line%flag_value("--epochs")
    start_string = command_line%flag_value("--start")
    end_string = command_line%flag_value("--end")
    stride_string = command_line%flag_value("--stride")

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

    if (len(end_string)/=0) then
      if (.not. allocated(end_step)) allocate(end_step)
      read(end_string,*) end_step
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
    double precision, parameter :: tolerance = 1.E-07
    integer, allocatable :: lbounds(:)
    integer t, b, t_end
    logical stop_requested

    associate( &
      network_input => base_name // "_input.nc", &
      network_output => base_name // "_output.nc", &
      network_file => base_name // "_network.json" &
    )
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

        block
          integer line, h
          integer, parameter :: num_inputs = 7, num_bins=10
          type(histogram_on_unit_interval_t) :: histogram(num_inputs)

          histogram(1) = histogram_on_unit_interval(pressure_in, "p_in", num_bins)
          histogram(2) = histogram_on_unit_interval(potential_temperature_in, "pt_in", num_bins)
          histogram(3) = histogram_on_unit_interval(temperature_in, "T_in", num_bins)
          histogram(4) = histogram_on_unit_interval(qv_in, "qv_in", num_bins)
          histogram(5) = histogram_on_unit_interval(qc_in, "qc_in", num_bins)
          histogram(6) = histogram_on_unit_interval(qr_in, "qr_in", num_bins)
          histogram(7) = histogram_on_unit_interval(qs_in, "qs_in", num_bins)

          do h = 1, size(histogram)
            print *,"# unmapped range for ", histogram(h)%variable_name,":", histogram(h)%unmapped_min, histogram(h)%unmapped_max
          end do

          print *,"bin", (histogram(h)%variable_name, h=1,size(histogram)) ! header

          do line = 1, size(histogram(1)%bin_midpoint)
            print *, histogram(1)%bin_midpoint(line), (histogram(h)%frequency(line), h=1,size(histogram))
          end do
        end block

      end associate

      stop "---------> made it <---------"

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
        call assert(all(abs(time_in(2:t_end) - time_out(1:t_end-1))<tolerance), "main: matching time stamps")
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
        real(rkind), parameter :: keep = 0.01
        real(rkind), allocatable :: cost(:)
        real(rkind), allocatable :: harvest(:)
        integer i, batch, lon, lat, level, time, network_unit, io_status, final_step, epoch
        integer(int64) start_training, finish_training


        if (.not. allocated(end_step)) end_step = t_end
        
        print *,"Normalizing input tensors"
        pressure_in = normalize(pressure_in, minval(pressure_in), maxval(pressure_in))
        potential_temperature_in = &
          normalize(potential_temperature_in, minval(potential_temperature_in), maxval(potential_temperature_in))
        temperature_in = normalize(temperature_in, minval(temperature_in), maxval(temperature_in))
        qv_in = normalize(qv_in, min(minval(qv_in), minval(qv_out)), max(maxval(qv_in), maxval(qv_out)))
        qc_in = normalize(qc_in, min(minval(qc_in), minval(qc_out)), max(maxval(qc_in), maxval(qc_out)))
        qr_in = normalize(qr_in, min(minval(qr_in), minval(qr_out)), max(maxval(qr_in), maxval(qr_out)))
        qs_in = normalize(qs_in, min(minval(qs_in), minval(qs_out)), max(maxval(qs_in), maxval(qs_out)))

        print *,"Normalizing output tensors"
        dpt_dt = normalize(dpt_dt, minval(dpt_dt), maxval(dpt_dt))
        dqv_dt = normalize(dqv_dt, minval(dqv_dt), maxval(dqv_dt))
        dqc_dt = normalize(dqc_dt, minval(dqc_dt), maxval(dqc_dt))
        dqr_dt = normalize(dqr_dt, minval(dqr_dt), maxval(dqr_dt))
        dqs_dt = normalize(dqs_dt, minval(dqs_dt), maxval(dqs_dt))

        print *,"Defining tensors from time step", start_step, "through", end_step, "with strides of", stride

      end block train_network

    end associate

    close(plot_unit)

  end subroutine read_train_write

end program tensor_statistics
