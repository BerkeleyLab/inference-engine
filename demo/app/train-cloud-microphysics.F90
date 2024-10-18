! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program train_cloud_microphysics
  !! Train a neural network to represent the simplest cloud microphysics model from
  !! the Intermediate Complexity Atmospheric Research Model (ICAR) at
  !! https://github.com/BerkeleyLab/icar.

  !! Intrinic modules :
  use ieee_arithmetic, only : ieee_is_nan
  use iso_fortran_env, only : int64, real64

  !! External dependencies:
  use julienne_m, only : string_t, file_t, command_line_t, bin_t
  use assert_m, only : assert, intrinsic_array_t
  use fiats_m, only : &
    neural_network_t, mini_batch_t, input_output_pair_t, tensor_t, trainable_network_t, tensor_map_t, training_configuration_t, &
    shuffle
    
  !! Internal dependencies:
  use phase_space_bin_m, only : phase_space_bin_t
  use NetCDF_file_m, only: NetCDF_file_t
  use NetCDF_variable_m, only: NetCDF_variable_t
  implicit none

  character(len=*), parameter :: usage =                                                        new_line('a') // new_line('a') // &
    'Usage: ' //                                                                                new_line('a') // new_line('a') // &
    './build/run-fpm.sh run train-cloud-microphysics -- \'                                                    // new_line('a') // &
    '  --base <string> --epochs <integer> \'                                                                  // new_line('a') // &
    '  [--start <integer>] [--end <integer>] [--stride <integer>] [--bins <integer>] [--report <integer>] [--tolerance <real>]'// &
                                                                                                new_line('a') // new_line('a') // &
    'where angular brackets denote user-provided values and square brackets denote optional arguments.'       // new_line('a') // &
    'The presence of a file named "stop" halts execution gracefully.'

  type command_line_arguments_t 
    integer num_epochs, start_step, stride, num_bins, report_interval
    integer, allocatable :: end_step
    character(len=:), allocatable :: base_name
    real cost_tolerance
  end type

  type plot_file_t
    character(len=:), allocatable :: file_name
    integer plot_unit, previous_epoch
  end type

  integer(int64) t_start, t_finish, clock_rate
  character(len=*), parameter :: config= "training_configuration.json"

  call system_clock(t_start, clock_rate)
  
#if defined(MULTI_IMAGE_SUPPORT)
  if (this_image()==1) then
#endif
    call read_train_write(training_configuration_t(file_t(config)), get_command_line_arguments(), create_or_append_to("cost.plt"))
#if defined(MULTI_IMAGE_SUPPORT)
  else
    call read_train_write(training_configuration_t(file_t(config)), get_command_line_arguments())
  end if
#endif

  call system_clock(t_finish)

  print *,"System clock time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
  print *,new_line('a') // "______train_cloud_microhpysics done _______"

contains

  function create_or_append_to(plot_file_name) result(plot_file)
    type(plot_file_t) plot_file
    character(len=*), intent(in) :: plot_file_name
    integer plot_unit, previous_epoch
    logical preexisting_plot_file

    inquire(file=plot_file_name, exist=preexisting_plot_file)

    if (.not. preexisting_plot_file) then
      open(newunit=plot_unit, file=plot_file_name, status="new", action="write")
      write(plot_unit,*) "      Epoch  Cost (avg)"
      previous_epoch = 0
    else
      associate(plot_file => file_t(string_t(plot_file_name)))
        associate(lines => plot_file%lines())
          associate(num_lines => size(lines))
            if (num_lines == 0 .or. num_lines == 1) then
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

    plot_file = plot_file_t(plot_file_name, plot_unit, previous_epoch)

  end function

  function get_command_line_arguments() result(command_line_arguments)
    type(command_line_arguments_t) command_line_arguments
    type(command_line_t) command_line
    character(len=:), allocatable :: &
      base_name, epochs_string, start_string, end_string, stride_string, bins_string, report_string, tolerance_string
    real cost_tolerance
    integer, allocatable :: end_step
    integer num_epochs, num_bins, start_step, stride, report_interval

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
      allocate(end_step)
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

    if (allocated(end_step)) then 
      command_line_arguments = command_line_arguments_t( &
        num_epochs, start_step, stride, num_bins, report_interval, end_step, base_name, cost_tolerance &
      )
    else
      command_line_arguments = command_line_arguments_t( &
        num_epochs, start_step, stride, num_bins, report_interval, null(), base_name, cost_tolerance &
      )
    end if
    
  end function get_command_line_arguments

  subroutine read_train_write(training_configuration, args, plot_file)
    type(training_configuration_t), intent(in) :: training_configuration
    type(command_line_arguments_t), intent(in) :: args
    type(plot_file_t), intent(in), optional :: plot_file 
    type(NetCDF_variable_t), allocatable :: input_variable(:), output_variable(:)
    type(NetCDF_variable_t) input_time, output_time

    ! local variables:
    integer t, b, t_end, v
    logical stop_requested

    enum, bind(C)
      enumerator :: pressure=1, potential_temperature, temperature, qv, qc, qr, qs
    end enum

    enum, bind(C)
      enumerator :: dpotential_temperature_t=1, dqv_dt, dqc_dt, dqr_dt, dqs_dt
    end enum

    associate(input_names => &
      [string_t("pressure"), string_t("potential_temperature"), string_t("temperature"), &
       string_t("qv"), string_t("qc"), string_t("qr"), string_t("qs")] &
    )
      allocate(input_variable(size(input_names)))

      associate(input_file_name => args%base_name // "_input.nc")

        print *,"Reading network inputs from " // input_file_name 

        associate(input_file => netCDF_file_t(input_file_name))

          do v=1, size(input_variable) 
            print *,"- reading ", input_names(v)%string()
            call input_variable(v)%input(input_names(v), input_file, rank=4)
          end do

          do v = 2, size(input_variable)
            call assert(input_variable(v)%conformable_with(input_variable(1)), "train_cloud_microphysics: input variable conformance")
          end do

          print *,"- reading time"
          call input_time%input("time", input_file, rank=1)

        end associate
      end associate
    end associate

    associate(output_names => [string_t("potential_temperature"),string_t("qv"), string_t("qc"), string_t("qr"), string_t("qs")])

      allocate(output_variable(size(output_names)))

      associate(output_file_name => args%base_name // "_output.nc")

        print *,"Reading network outputs from " // output_file_name 

        associate(output_file => netCDF_file_t(output_file_name))

          do v=1, size(output_variable)
            print *,"- reading ", output_names(v)%string()
            call output_variable(v)%input(output_names(v), output_file, rank=4)
          end do

          do v = 1, size(output_variable)
            call assert(output_variable(v)%conformable_with(input_variable(1)), "train_cloud_microphysics: output variable conformance")
          end do

          print *,"- reading time"
          call output_time%input("time", output_file, rank=1)

          call assert(output_time%conformable_with(input_time), "train_cloud_microphysics: input/output time conformance")

        end associate
      end associate

      block
        type(NetCDF_variable_t) derivative(size(output_variable))

        print *,"Calculating time derivatives"

        associate(dt => NetCDF_variable_t(output_time - input_time, "dt"))
          do v = 1, size(derivative)
            associate(derivative_name => "d" // output_names(v)%string() // "/dt")
              print *,"- " // derivative_name
              derivative(v) = NetCDF_variable_t( input_variable(v) - output_variable(v) / dt, derivative_name)
            end associate
          end do
        end associate
      end block

    end associate

    !t_end = size(time_in)

    !associate(dt => real(time_out - time_in))
    !  do concurrent(t = 1:t_end)
    !    dpt_dt(:,:,:,t) = (potential_temperature_out(:,:,:,t) - potential_temperature_in(:,:,:,t))/dt(t)
    !    dqv_dt(:,:,:,t) = (qv_out(:,:,:,t)- qv_in(:,:,:,t))/dt(t)
    !    dqc_dt(:,:,:,t) = (qc_out(:,:,:,t)- qc_in(:,:,:,t))/dt(t)
    !    dqr_dt(:,:,:,t) = (qr_out(:,:,:,t)- qr_in(:,:,:,t))/dt(t)
    !    dqs_dt(:,:,:,t) = (qs_out(:,:,:,t)- qs_in(:,:,:,t))/dt(t)
    !  end do
    !end associate
     
    !call assert(.not. any(ieee_is_nan(dpt_dt)), ".not. any(ieee_is_nan(dpt_dt)")
    !call assert(.not. any(ieee_is_nan(dqv_dt)), ".not. any(ieee_is_nan(dqv_dt)")
    !call assert(.not. any(ieee_is_nan(dqc_dt)), ".not. any(ieee_is_nan(dqc_dt)")
    !call assert(.not. any(ieee_is_nan(dqr_dt)), ".not. any(ieee_is_nan(dqr_dt)")
    !call assert(.not. any(ieee_is_nan(dqs_dt)), ".not. any(ieee_is_nan(dqs_dt)")
     
    !train_network: &
    !block
    !    type(trainable_network_t) trainable_network
    !    type(mini_batch_t), allocatable :: mini_batches(:)
    !    type(bin_t), allocatable :: bins(:)
    !    type(input_output_pair_t), allocatable :: input_output_pairs(:)
    !    type(tensor_t), allocatable, dimension(:) :: inputs, outputs
    !    real, allocatable :: cost(:)
    !    integer i, lon, lat, level, time, network_unit, io_status, epoch, end_step
    !    integer(int64) start_training, finish_training
       
    !    associate( network_file => args%base_name // "_network.json")
       
    !    open(newunit=network_unit, file=network_file, form='formatted', status='old', iostat=io_status, action='read')
       
    !    if (allocated(args%end_step)) then
    !      end_step = args%end_step
    !    else
    !      end_step = t_end
    !    end if
       
    !    print *,"Defining tensors from time step", args%start_step, "through", end_step, "with strides of", args%stride
       
    !    ! The following temporary copies are required by gfortran bug 100650 and possibly 49324
    !    ! See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=100650 and https://gcc.gnu.org/bugzilla/show_bug.cgi?id=49324
    !    inputs = [( [( [( [( &
    !      tensor_t( &
    !      [ pressure_in(lon,lat,level,time), potential_temperature_in(lon,lat,level,time), temperature_in(lon,lat,level,time), &
    !        qv_in(lon,lat,level,time), qc_in(lon,lat,level,time), qr_in(lon,lat,level,time), qs_in(lon,lat,level,time) &
    !      ] &
    !      ), lon = 1, size(qv_in,1))], lat = 1, size(qv_in,2))], level = 1, size(qv_in,3))], &
    !      time = args%start_step, end_step, args%stride)]
       
    !    outputs = [( [( [( [( &
    !      tensor_t( &
    !        [dpt_dt(lon,lat,level,time), dqv_dt(lon,lat,level,time), dqc_dt(lon,lat,level,time), dqr_dt(lon,lat,level,time), &
    !         dqs_dt(lon,lat,level,time) &
    !        ] &
    !      ), lon = 1, size(qv_in,1))], lat = 1, size(qv_in,2))], level = 1, size(qv_in,3))], &
    !      time = args%start_step, end_step, args%stride)]
       
    !    print *,"Calculating output tensor component ranges."
    !    output_extrema: &
    !    associate( &
    !      output_minima => [minval(dpt_dt), minval(dqv_dt), minval(dqc_dt), minval(dqr_dt), minval(dqs_dt)], &
    !      output_maxima => [maxval(dpt_dt), maxval(dqv_dt), maxval(dqc_dt), maxval(dqr_dt), maxval(dqs_dt)] &
    !    )
    !      output_map: &
    !      associate( output_map => tensor_map_t(layer = "outputs", minima = output_minima, maxima = output_maxima))
    !        read_or_initialize_network: &
    !        if (io_status==0) then
    !          print *,"Reading network from file " // network_file
    !          trainable_network = trainable_network_t(neural_network_t(file_t(string_t(network_file))))
    !          close(network_unit)
    !        else
    !          close(network_unit)
       
    !          initialize_network: &
    !          block
    !            character(len=len('YYYYMMDD')) date
       
    !            call date_and_time(date)
       
    !            print *,"Calculating input tensor component ranges."
    !            associate( &
    !              input_map => tensor_map_t( &
    !                layer  = "inputs", &
    !                minima = [minval(pressure_in), minval(potential_temperature_in), minval(temperature_in), &
    !                  minval(qv_in), minval(qc_in), minval(qr_in), minval(qs_in)], &
    !                maxima = [maxval(pressure_in), maxval(potential_temperature_in), maxval(temperature_in), &
    !                  maxval(qv_in), maxval(qc_in), maxval(qr_in), maxval(qs_in)] &
    !            ) )
    !              associate(activation => training_configuration%differentiable_activation())
    !                associate(residual_network=> string_t(trim(merge("true ", "false", training_configuration%skip_connections()))))
    !                  trainable_network = trainable_network_t( &
    !                    training_configuration,  &
    !                    perturbation_magnitude = 0.05, &
    !                    metadata = [ &
    !                      string_t("Simple microphysics"), string_t("train-on-flat-dist"), string_t(date), &
    !                      activation%function_name(), residual_network &
    !                    ], input_map = input_map, output_map = output_map &
    !                  )
    !                end associate
    !              end associate
    !            end associate ! input_map, date_string
    !          end block initialize_network
    !        end if read_or_initialize_network
       
    !        print *, "Conditionally sampling for a flat distribution of output values"
    !        block
    !          integer i
    !          logical occupied(argsnum_bins, args%num_bins, args%num_bins, args%num_bins, args%num_bins)
    !          logical keepers(size(outputs))
    !          type(phase_space_bin_t), allocatable :: bin(:)
    !          occupied = .false.
    !          keepers = .false.

    !        bin = [(phase_space_bin_t(outputs(i), output_minima, output_maxima, args%num_bins), i=1,size(outputs))]

    !        do i = 1, size(outputs)
    !          if (occupied(bin(i)%loc(1),bin(i)%loc(2),bin(i)%loc(3),bin(i)%loc(4),bin(i)%loc(5))) cycle
    !          occupied(bin(i)%loc(1),bin(i)%loc(2),bin(i)%loc(3),bin(i)%loc(4),bin(i)%loc(5)) = .true.
    !          keepers(i) = .true.
    !        end do
    !        input_output_pairs = input_output_pair_t(pack(inputs, keepers), pack(outputs, keepers))
    !        print *, "Kept ", size(input_output_pairs), " out of ", size(outputs, kind=int64), " input/output pairs " // &
    !                 " in ", count(occupied)," out of ", size(occupied, kind=int64), " bins."
    !      end block
    !    end associate output_map
    !  end associate output_extrema

    !  print *,"Normalizing the remaining input and output tensors"
    !  input_output_pairs = trainable_network%map_to_training_ranges(input_output_pairs)

    !  associate( &
    !    num_pairs => size(input_output_pairs), &
    !    n_bins => training_configuration%mini_batches(), &
    !    adam => merge(.true., .false., training_configuration%optimizer_name() == "adam"), &
    !    learning_rate => training_configuration%learning_rate() &
    !  )
    !    bins = [(bin_t(num_items=num_pairs, num_bins=n_bins, bin_number=b), b = 1, n_bins)]

    !    print *,"Training network"
    !    print *, "       Epoch  Cost (avg)"

    !    call system_clock(start_training)
    !  
    !    train_write_and_maybe_exit: &
    !    block
    !      integer first_epoch
    !      integer me


#if defined(MULTI_IMAGE_SUPPORT)
      !    me = this_image()
#else
      !    me = 1
#endif
      !      if (me==1) first_epoch = plot_file%previous_epoch + 1

#if defined(MULTI_IMAGE_SUPPORT)
      !      call co_broadcast(first_epoch, source_image=1)
#endif
      !      associate(last_epoch => first_epoch + args%num_epochs - 1)
      !        epochs: &
      !        do epoch = first_epoch, last_epoch

      !          if (size(bins)>1) call shuffle(input_output_pairs) ! set up for stochastic gradient descent
      !          mini_batches = [(mini_batch_t(input_output_pairs(bins(b)%first():bins(b)%last())), b = 1, size(bins))]

      !          call trainable_network%train(mini_batches, cost, adam, learning_rate)

      !          associate(average_cost => sum(cost)/size(cost))
      !            associate(converged => average_cost <= args%cost_tolerance)

      !              image_1_maybe_writes: &
      !              if (me==1 .and. any([converged, epoch==[first_epoch,last_epoch], mod(epoch,args%report_interval)==0])) then

      !                print *, epoch, average_cost
      !                write(plot_file%plot_unit,*) epoch, average_cost

      !                associate(json_file => trainable_network%to_json())
      !                  call json_file%write_lines(string_t(network_file))
      !                end associate

      !              end if image_1_maybe_writes

      !              signal_convergence: & 
      !              if (converged) then
      !                block
      !                  integer unit
      !                  open(newunit=unit, file="converged", status="unknown") ! The train.sh script detects & removes this file.
      !                  close(unit)
      !                  exit epochs
      !                end block
      !              end if signal_convergence
      !            end associate
      !          end associate

      !          inquire(file="stop", exist=stop_requested)

      !          graceful_exit: &
      !          if (stop_requested) then
      !            print *,'Shutting down because a file named "stop" was found.'
      !            return
      !          end if graceful_exit

      !        end do epochs
      !      end associate
      !    end block train_write_and_maybe_exit

      !  end associate

      !  call system_clock(finish_training)
      !  print *,"Training time: ", real(finish_training - start_training, real64)/real(clock_rate, real64),"for", &
      !    args%num_epochs,"epochs"

        !end associate ! network_file
      !end block train_network

    !close(plot_file%plot_unit)

  end subroutine read_train_write

end program train_cloud_microphysics
