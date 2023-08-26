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
  use sourcery_m, only : string_t, file_t, command_line_t
  use assert_m, only : assert, intrinsic_array_t
  use ieee_arithmetic, only : ieee_is_nan

  !! Internal dependencies;
  use inference_engine_m, only : &
    inference_engine_t, mini_batch_t, input_output_pair_t, tensor_t, trainable_engine_t, rkind, NetCDF_file_t, sigmoid_t
  use ubounds_m, only : ubounds_t
  implicit none

  type(command_line_t) command_line
  character(len=:), allocatable :: base_name

  base_name = command_line%flag_value("--base-name") ! gfortran 13 seg faults if this is an association

  if (len(base_name)==0) error stop new_line('a') // new_line('a') // &
    'Usage: ./build/run-fpm.sh run train-cloud-microphysics -- --base-name "<file-base-name>"'

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
      integer, parameter :: initial = 1
      integer t_end, t
 
      print *,"Starting to read network inputs from " // network_input

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

      print *,"Starting to read network outputs from " // network_output

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

      print *,"Estimating time derivatives"
  
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
        ! As a first test, let's check whether we can train based on a tiny subset of the training data.
        ! For this purposes, we somewhat arbitrarily gather input/output pairs along a line of constsant 
        ! longitude and mid-level elevation at the final time step
        type(trainable_engine_t) trainable_engine
        type(inference_engine_t) inference_engine
        type(mini_batch_t), allocatable :: mini_batches(:)
        type(tensor_t), allocatable, dimension(:,:) :: inputs, outputs
        type(tensor_t), allocatable, dimension(:) :: tmp1, tmp2
        real(rkind) t_start, t_end
        integer, parameter :: mini_batch_size=1
        integer batch, lon, lat, level
        type(file_t) json_file

        associate(num_mini_batches => size(qc_in,1)*size(qc_in,2)*size(qc_in,3), time=> ubound(qc_in,4))

          print *,"Defining tensors"

          ! The following temporary copies are required by gfortran bug 100650 and possibly 49324
          ! See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=100650 and https://gcc.gnu.org/bugzilla/show_bug.cgi?id=49324
          tmp1 = [( [( [( &
            tensor_t( &
              [pressure_in(lon,lat,level,time), potential_temperature_in(lon,lat,level,time), temperature_in(lon,lat,level,time),&
               qv_in(lon,lat,level,time), qc_in(lon,lat,level,time), qi_in(lon,lat,level,time), qr_in(lon,lat,level,time), &
               qs_in(lon,lat,level,time) &
              ] &
            ), lon = 1, size(qv_in,1))], lat = 1, size(qv_in,2))], level = 1, size(qv_in,3))]
          inputs = reshape(tmp1, [mini_batch_size, num_mini_batches])

          tmp2 = [( [( [( &
            tensor_t( &
              [dpt_dt(lon,lat,level,time), dqv_dt(lon,lat,level,time), dqc_dt(lon,lat,level,time), &
               dqi_dt(lon,lat,level,time), dqr_dt(lon,lat,level,time), dqs_dt(lon,lat,level,time) &
              ] &
            ), lon = 1, size(qv_in,1))], lat = 1, size(qv_in,2))], level = 1, size(qv_in,3))]
          outputs = reshape(tmp2, [mini_batch_size, num_mini_batches])

          mini_batches = [(mini_batch_t(input_output_pair_t(inputs(:,batch), outputs(:,batch))), batch = 1, num_mini_batches)]
        end associate

        print *,"Training network"

        trainable_engine = random_hidden_layers( &
          num_inputs = inputs(1,1)%num_components(), num_outputs = outputs(1,1)%num_components() &
        )
        call cpu_time(t_start)
        call trainable_engine%train(mini_batches)
        call cpu_time(t_end)

        print *, "Training time: ", t_end - t_start
 
        inference_engine = trainable_engine%to_inference_engine()
        json_file = inference_engine%to_json()
        call json_file%write_lines(string_t(network_file))

      end block train_network
    end block read_and_train
  end associate

  print *,new_line('a') // "______training_cloud_microhpysics done _______"

contains

  function random_hidden_layers(num_inputs, num_outputs) result(trainable_engine)
    integer, intent(in) :: num_inputs, num_outputs
    type(trainable_engine_t) trainable_engine
    integer l
    integer, parameter :: nodes_per_hidden_layer = 8, num_hidden_layers = 4
    real(rkind), allocatable :: w(:,:,:), b(:,:)

    associate(nodes => [num_inputs, [(nodes_per_hidden_layer, l = 1, num_hidden_layers)], num_outputs])
      associate(max_nodes => maxval(nodes), layers => size(nodes))

        allocate(w(max_nodes, max_nodes, layers-1), b(max_nodes, max_nodes))

        call random_number(b)
        call random_number(w)

        trainable_engine = trainable_engine_t( &
          nodes = nodes, weights = w, biases = b, differentiable_activation_strategy = sigmoid_t(), metadata = & 
          [string_t("Microphysics"), string_t("Damian Rouson"), string_t("2023-08-18"), string_t("sigmoid"), string_t("false")] &
        )
      end associate
    end associate
   
  end function

end program train_cloud_microphysics
#endif // __INTEL_FORTRAN
