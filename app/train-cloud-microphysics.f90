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
    inference_engine_t, mini_batch_t, input_output_pair_t, tensor_t, trainable_engine_t, rkind, NetCDF_file_t, sigmoid_t
  use ubounds_m, only : ubounds_t
  implicit none

  integer(int64) t_start, t_finish, clock_rate
  type(command_line_t) command_line
  character(len=:), allocatable :: base_name, steps
  integer plot_file, dash, starting_step, ending_step
  logical user_specified_time_range

  call system_clock(t_start, clock_rate)

  base_name = command_line%flag_value("--base-name") ! gfortran 13 seg faults if this is an association
  steps = command_line%flag_value("--steps")

  if (len(base_name)==0) error stop new_line('a') // new_line('a') // &
    'Usage: ./build/run-fpm.sh run train-cloud-microphysics -- --base-name "<file-base-name>"'

  if (len(steps)==0) then
    user_specified_time_range = .false.
    print *,"No user-specified time step range. All steps will be used."
  else
    dash = scan(steps,"-")
    read(steps(1:dash-1),*) starting_step
    read(steps(dash+1:len(steps)),*) ending_step 
    user_specified_time_range = .true.
    print *,"User-specified time step range: ", starting_step, "-", ending_step
  end if

  associate( &
    network_input => base_name // "_input.nc", &
    network_output => base_name // "_output.nc", &
    network_file => base_name // "_network.json" &
  )

    read_and_train: &
    block
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
        type(inference_engine_t) inference_engine
        type(mini_batch_t), allocatable :: mini_batches(:)
        type(bin_t), allocatable :: bins(:)
        type(input_output_pair_t), allocatable :: input_output_pairs(:)
        type(tensor_t), allocatable, dimension(:) :: inputs, outputs
        type(file_t) json_file
        real(rkind), allocatable :: cost(:)
        real(rkind), parameter :: keep = 0.3
        integer, parameter :: mini_batch_size=1
        integer batch, lon, lat, level, time, file_unit, io_status, final_step

        open(newunit=file_unit, file=network_file, form='formatted', status='unknown', iostat=io_status, action='write')
        if (io_status==0) then
          print *,"Reading network from file " // network_file
          inference_engine = inference_engine_t(file_t(string_t(network_file)))
          trainable_engine = trainable_engine_t(inference_engine%to_exchange())
        else
          print *,"Initializing a new network"
          trainable_engine= hidden_layers(num_hidden_layers=12, nodes_per_hidden_layer=16, num_inputs=8, num_outputs=6, random=.true.)
        end if
        
        open(newunit=plot_file,file="cost.plt",status="replace")
        write(plot_file,*) "step       min(cost)       max(cost)        avg(cost)"

        if (.not. user_specified_time_range) then
          starting_step = 1
          ending_step = size(time_in)
        end if

        print *,"Training using data from step", starting_step, "to", ending_step

        do time = starting_step, ending_step

          if (time==1) print *,"Defining tensors for step ",time

          ! The following temporary copies are required by gfortran bug 100650 and possibly 49324
          ! See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=100650 and https://gcc.gnu.org/bugzilla/show_bug.cgi?id=49324
          inputs = [( [( [( &
            tensor_t( &
            [ pressure_in(lon,lat,level,time), potential_temperature_in(lon,lat,level,time), temperature_in(lon,lat,level,time),&
              qv_in(lon,lat,level,time), qc_in(lon,lat,level,time), qi_in(lon,lat,level,time), qr_in(lon,lat,level,time), &
              qs_in(lon,lat,level,time) &
            ] &
            ), lon = 1, size(qv_in,1))], lat = 1, size(qv_in,2))], level = 1, size(qv_in,3))]

          outputs = [( [( [( &
            tensor_t( &
              [dpt_dt(lon,lat,level,time), dqv_dt(lon,lat,level,time), dqc_dt(lon,lat,level,time), &
               dqi_dt(lon,lat,level,time), dqr_dt(lon,lat,level,time), dqs_dt(lon,lat,level,time) &
              ] &
            ), lon = 1, size(qv_in,1))], lat = 1, size(qv_in,2))], level = 1, size(qv_in,3))]
          
          eliminate_zero_derivatives: &
          block
            integer i
            real, allocatable :: harvest(:)

            if (time==1) print *,"Eliminating",100*(1._rkind-keep),"% of the grid points where all time derivatives are zero"

            associate(num_grid_pts => size(outputs))
              allocate(harvest(num_grid_pts))
              call random_number(harvest)
              associate(non_zero_derivatives => [(any(outputs(i)%values()/=0.) .or. harvest(i)<keep, i=1,num_grid_pts)])
                outputs = pack(outputs, non_zero_derivatives)
                inputs = pack(inputs, non_zero_derivatives)
                if (time==1) print *, size(outputs), "points retained out of ", num_grid_pts, " points total"
              end associate
            end associate

          end block eliminate_zero_derivatives

          input_output_pairs = input_output_pair_t(inputs, outputs)

          call shuffle(input_output_pairs) ! set up for stochastic gradient descent

          associate(num_pairs => size(input_output_pairs), n_bins => size(input_output_pairs)/10000)
            bins = [(bin_t(num_items=num_pairs, num_bins=n_bins, bin_number=b), b = 1, n_bins)]
            mini_batches = [(mini_batch_t(input_output_pairs(bins(b)%first():bins(b)%last())), b = 1, size(bins))]
          end associate

          if (time==1) print *,"Training network"
          call trainable_engine%train(mini_batches, cost)
          print *, "step, cost_{min,max,avg}: ", time, minval(cost), maxval(cost), sum(cost)/size(cost)
          write(plot_file,*) time, minval(cost), maxval(cost), sum(cost)/size(cost)

        end do

        inference_engine = trainable_engine%to_inference_engine()
        json_file = inference_engine%to_json()
        print *,"Writing network to " // network_file
        call json_file%write_lines(string_t(network_file))

      end block train_network
    end block read_and_train
  end associate

  close(plot_file)

  call system_clock(t_finish)
  print *,"System clock time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
  print *,new_line('a') // "______training_cloud_microhpysics done _______"

contains

  function hidden_layers(num_hidden_layers, nodes_per_hidden_layer, num_inputs, num_outputs, random) result(trainable_engine)
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
