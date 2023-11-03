! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
#ifndef __INTEL_FORTRAN
!! Due to a suspected bug in the Intel ifx compiler, the above C preprocessor macro
!! effectively eliminates this file's source code when building with an Intel compiler.

module ubounds_m
  !! This module serves only to support array bounds checking in the main program below
  implicit none

  type ubounds_t
    integer, allocatable :: ubounds_(:)
  contains
    procedure equals
    generic :: operator(==) => equals
  end type
 
contains

  elemental function equals(lhs, rhs) result(lhs_equals_rhs)
    class(ubounds_t), intent(in) :: lhs, rhs
    logical lhs_equals_rhs
    lhs_equals_rhs = all(lhs%ubounds_ == rhs%ubounds_)
  end function

end module

program train_cloud_microphysics
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
    inference_engine_t, mini_batch_t, input_output_pair_t, tensor_t, trainable_engine_t, rkind, NetCDF_file_t, sigmoid_t, &
    training_configuration_t
  use ubounds_m, only : ubounds_t
  use training_configuration_m, only : training_configuration_t
  implicit none

  integer(int64) t_start, t_finish, clock_rate
  type(command_line_t) command_line
  type(file_t) plot_file
  type(string_t), allocatable :: lines(:)
  character(len=*), parameter :: plot_file_name = "cost.plt", training_configuration_json = "training_configuration.json "
  character(len=:), allocatable :: base_name, stride_string, epochs_string, last_line
  integer plot_unit, stride, num_epochs, previous_epoch
  logical preexisting_plot_file

  call system_clock(t_start, clock_rate)

  base_name = command_line%flag_value("--base") ! gfortran 13 seg faults if this is an association
  stride_string = command_line%flag_value("--stride")
  epochs_string = command_line%flag_value("--epochs")

  if (any([len(base_name),len(stride_string),len(epochs_string)]==0)) error stop new_line('a') // new_line('a') // &
    'Usage: ./build/run-fpm.sh run train-cloud-microphysics -- --base <string> --stride <integer> --epochs <integer>'

  read(stride_string,*) stride
  read(epochs_string,*) num_epochs

  inquire(file=plot_file_name, exist=preexisting_plot_file)
  open(newunit=plot_unit,file="cost.plt",status="unknown",position="append")

  if (.not. preexisting_plot_file) then
    write(plot_unit,*) "      Epoch   Cost (min)       Cost (max)       Cost (avg)"
    previous_epoch = 0
  else
    plot_file = file_t(string_t(plot_file_name))
    lines = plot_file%lines()
    last_line = lines(size(lines))%string()
    read(last_line,*) previous_epoch
  end if

  call read_train_write(training_configuration_t(file_t(string_t(training_configuration_json))))

  close(plot_unit)
  call system_clock(t_finish)
  print *,"System clock time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
  print *,new_line('a') // "______training_cloud_microhpysics done _______"

contains

 subroutine read_train_write(training_configuration)
    type(training_configuration_t), intent(in) :: training_configuration
    real, allocatable, dimension(:,:,:,:) :: &
      pressure_in , potential_temperature_in , temperature_in , &
      pressure_out, potential_temperature_out, temperature_out, &
      qv_out, qc_out, qi_out, qr_out, qs_out, &
      qv_in , qc_in , qi_in , qr_in , qs_in , &
      dpt_dt, dqv_dt, dqc_dt, dqi_dt, dqr_dt, dqs_dt
    type(ubounds_t), allocatable :: ubounds(:)
    double precision, allocatable, dimension(:) :: time_in, time_out
    double precision, parameter :: tolerance = 1.E-07
    integer, allocatable :: lbounds(:)
    integer t, b, t_end
    character(len=:), allocatable :: network_input, network_output, network_file
    logical stop_requested
 
    network_input = base_name // "_input.nc"
    network_output = base_name // "_output.nc"
    network_file = base_name // "_network.json"

    print *,"Reading network inputs from " // network_input

    associate(network_input_file => netCDF_file_t(network_input))
      ! Skipping the following unnecessary inputs that are in the current file format as of 14 Aug 2023:
      ! precipitation, snowfall
      call network_input_file%input("pressure", pressure_in)
      call network_input_file%input("potential_temperature", potential_temperature_in)
      call network_input_file%input("temperature", temperature_in)
      call network_input_file%input("qv", qv_in)
      call network_input_file%input("qc", qc_in)
      call network_input_file%input("qi", qi_in)
      call network_input_file%input("qr", qr_in)
      call network_input_file%input("qs", qs_in)
      call network_input_file%input("time", time_in)
      t_end = size(time_in)
      lbounds = &
        [lbound(pressure_in), lbound(temperature_in), lbound(qv_in), lbound(qc_in), lbound(qi_in), lbound(qr_in), lbound(qs_in)]
      ubounds = &
        [ubounds_t(ubound(qv_in)), ubounds_t(ubound(qc_in)), ubounds_t(ubound(qi_in)), &
         ubounds_t(ubound(qr_in)), ubounds_t(ubound(qs_in)), ubounds_t(ubound(pressure_in)), ubounds_t(ubound(temperature_in)) &
        ]
    end associate

    print *,"Reading network outputs from " // network_output

    associate(network_output_file => netCDF_file_t(network_output))
      call network_output_file%input("potential_temperature", potential_temperature_out)
      ! Skipping the following unnecessary outputs that are in the current file format as of 14 Aug 2023:
      ! pressure, temperature, precipitation, snowfall
      call network_output_file%input("qv", qv_out)
      call network_output_file%input("qc", qc_out)
      call network_output_file%input("qi", qi_out)
      call network_output_file%input("qr", qr_out)
      call network_output_file%input("qs", qs_out)
      call network_output_file%input("time", time_out)
      lbounds = [lbounds, lbound(qv_out), lbound(qc_out), lbound(qi_out), lbound(qr_out), lbound(qs_out)]
      ubounds = [ubounds, ubounds_t(ubound(qv_out)), ubounds_t(ubound(qc_out)), ubounds_t(ubound(qi_out)), &
        ubounds_t(ubound(qr_out)), ubounds_t(ubound(qs_out))]
      call assert(all(lbounds == 1), "main: default input/output lower bounds", intrinsic_array_t(lbounds))
      call assert(all(ubounds == ubounds(1)), "main: matching input/output upper bounds")
      call assert(all(abs(time_in(2:t_end) - time_out(1:t_end-1))<tolerance), "main: matching time stamps")
    end associate

    print *,"Calculating time derivatives"
  
    allocate(dpt_dt, mold = potential_temperature_out)
    allocate(dqv_dt, mold = qv_out)
    allocate(dqc_dt, mold = qc_out)
    allocate(dqi_dt, mold = qi_out)
    allocate(dqr_dt, mold = qr_out)
    allocate(dqs_dt, mold = qs_out)

    associate(dt => real(time_out - time_in, rkind))
      do concurrent(t = 1:t_end)
        dpt_dt(:,:,:,t) = (potential_temperature_out(:,:,:,t) - potential_temperature_in(:,:,:,t))/dt(t)
        dqv_dt(:,:,:,t) = (qv_out(:,:,:,t)- qv_in(:,:,:,t))/dt(t)
        dqc_dt(:,:,:,t) = (qc_out(:,:,:,t)- qc_in(:,:,:,t))/dt(t)
        dqi_dt(:,:,:,t) = (qi_out(:,:,:,t)- qi_in(:,:,:,t))/dt(t)
        dqr_dt(:,:,:,t) = (qr_out(:,:,:,t)- qr_in(:,:,:,t))/dt(t)
        dqs_dt(:,:,:,t) = (qs_out(:,:,:,t)- qs_in(:,:,:,t))/dt(t)
      end do
    end associate

    call assert(.not. any(ieee_is_nan(dpt_dt)), ".not. any(ieee_is_nan(dpt_dt)")
    call assert(.not. any(ieee_is_nan(dqv_dt)), ".not. any(ieee_is_nan(dqv_dt)")
    call assert(.not. any(ieee_is_nan(dqc_dt)), ".not. any(ieee_is_nan(dqc_dt)")
    call assert(.not. any(ieee_is_nan(dqi_dt)), ".not. any(ieee_is_nan(dqi_dt)")
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

      open(newunit=network_unit, file=network_file, form='formatted', status='old', iostat=io_status, action='read')

      if (io_status==0) then
        print *,"Reading network from file " // network_file
        trainable_engine = trainable_engine_t(inference_engine_t(file_t(string_t(network_file))))
        close(network_unit)
      else
        close(network_unit)
        print *,"Initializing a new network"
        trainable_engine = new_engine(num_hidden_layers=6, nodes_per_hidden_layer=16, num_inputs=8, num_outputs=6, random=.false.)
      end if
      
      print *,"Defining tensors from time steps 1 through", t_end, "with strides of", stride

      ! The following temporary copies are required by gfortran bug 100650 and possibly 49324
      ! See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=100650 and https://gcc.gnu.org/bugzilla/show_bug.cgi?id=49324
      inputs = [( [( [( [( &
        tensor_t( &
        [ pressure_in(lon,lat,level,time), potential_temperature_in(lon,lat,level,time), temperature_in(lon,lat,level,time),&
          qv_in(lon,lat,level,time), qc_in(lon,lat,level,time), qi_in(lon,lat,level,time), qr_in(lon,lat,level,time), &
          qs_in(lon,lat,level,time) &
        ] &
        ), lon = 1, size(qv_in,1))], lat = 1, size(qv_in,2))], level = 1, size(qv_in,3))], time = 1, t_end, stride)]

      outputs = [( [( [( [( &
        tensor_t( &
          [dpt_dt(lon,lat,level,time), dqv_dt(lon,lat,level,time), dqc_dt(lon,lat,level,time), &
           dqi_dt(lon,lat,level,time), dqr_dt(lon,lat,level,time), dqs_dt(lon,lat,level,time) &
          ] &
        ), lon = 1, size(qv_in,1))], lat = 1, size(qv_in,2))], level = 1, size(qv_in,3))], time = 1, t_end, stride)]
      
      print *, "Eliminating",int(100*(1.-keep)),"% of the grid points that have all-zero time derivatives"

      associate(num_grid_pts => size(outputs))
        if (allocated(harvest)) deallocate(harvest)
        allocate(harvest(num_grid_pts))
        call random_number(harvest)
        associate(keepers => [(any(outputs(i)%values()/=0.) .or. harvest(i)<keep, i=1,num_grid_pts)])
          input_output_pairs = input_output_pair_t(pack(inputs, keepers), pack(outputs, keepers))
          print *, size(input_output_pairs), "points retained out of ", num_grid_pts, " points total"
        end associate
      end associate

      associate(num_pairs => size(input_output_pairs), n_bins => training_configuration%mini_batches())
        bins = [(bin_t(num_items=num_pairs, num_bins=n_bins, bin_number=b), b = 1, n_bins)]

        print *,"Training network"
        print *, "       Epoch   Cost (min)       Cost (max)       Cost (avg)"

        do epoch = previous_epoch + 1, previous_epoch + num_epochs

          call shuffle(input_output_pairs) ! set up for stochastic gradient descent
          mini_batches = [(mini_batch_t(input_output_pairs(bins(b)%first():bins(b)%last())), b = 1, size(bins))]
          call trainable_engine%train(mini_batches, cost)
          print *, epoch, minval(cost), maxval(cost), sum(cost)/size(cost)
          write(plot_unit,*) epoch, minval(cost), maxval(cost), sum(cost)/size(cost)

          open(newunit=network_unit, file=network_file, form='formatted', status='unknown', iostat=io_status, action='write')
          associate(inference_engine => trainable_engine%to_inference_engine())
            associate(json_file => inference_engine%to_json())
              call json_file%write_lines(string_t(network_file))
            end associate
          end associate

          close(network_unit)

          inquire(file="stop-training", exist=stop_requested)

          graceful_exit: &
          if (stop_requested) then
            print *,'Shutting down because a file named "stop-training" was found.'
            return
          end if graceful_exit

        end do
      end associate

    end block train_network

  end subroutine read_train_write

  function new_engine(num_hidden_layers, nodes_per_hidden_layer, num_inputs, num_outputs, random) result(trainable_engine)
    integer, intent(in) ::  num_hidden_layers, nodes_per_hidden_layer, num_inputs, num_outputs
    logical, intent(in) :: random
    type(trainable_engine_t) trainable_engine
    real(rkind), allocatable :: w(:,:,:), b(:,:)
    integer l

    associate(nodes => [num_inputs, [(nodes_per_hidden_layer, l = 1, num_hidden_layers)], num_outputs])
      associate(max_nodes => maxval(nodes), layers => size(nodes))

        allocate(w(max_nodes, max_nodes, layers-1), b(max_nodes, max_nodes))

        if (random) then
          call random_number(b)
          call random_number(w)
        else
          b = 0.
          w = 0.
        end if

        trainable_engine = trainable_engine_t( &
          nodes = nodes, weights = w, biases = b, differentiable_activation_strategy = sigmoid_t(), metadata = & 
          [string_t("Microphysics"), string_t("Damian Rouson"), string_t("2023-08-18"), string_t("sigmoid"), string_t("false")] &
        )
      end associate
    end associate
  end function

  subroutine shuffle(pairs)
    type(input_output_pair_t), intent(inout) :: pairs(:)
    type(input_output_pair_t) temp
    real harvest(2:size(pairs))
    integer i, j

    call random_init(image_distinct=.true., repeatable=.true.)
    call random_number(harvest)

    durstenfeld_shuffle: &
    do i = size(pairs), 2, -1
      j = 1 + int(harvest(i)*i)
      temp     = pairs(i) 
      pairs(i) = pairs(j)
      pairs(j) = temp
    end do durstenfeld_shuffle

  end subroutine

end program train_cloud_microphysics
#endif // __INTEL_FORTRAN
