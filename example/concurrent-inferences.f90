! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program concurrent_inference
  !! This program demonstrates how to read a neural network from a JSON file
  !! and use the network to perform concurrent inferences.
  use inference_engine_m, only : inference_engine_t, tensor_t
  use sourcery_m, only : string_t, command_line_t, file_t
  use assert_m, only : assert
  use iso_fortran_env, only : int64, real64
  implicit none

  type(string_t) network_file_name
  type(command_line_t) command_line

  network_file_name = string_t(command_line%flag_value("--network"))

  if (len(network_file_name%string())==0) then
    error stop new_line('a') // new_line('a') // &
      'Usage: ./build/run-fpm.sh run --example identity -- --network "<file-name>"' 
  end if

  block 
    type(inference_engine_t) network, inference_engine
    type(tensor_t), allocatable :: inputs(:,:,:), outputs(:,:,:) 
    real, allocatable :: input_components(:,:,:,:)
    integer, parameter :: lat=263, lon=317, lev=15 ! latitudes, longitudes, levels (elevations)
    integer i, j, k

    print *, "Constructing a new inference_engine_t object from the file " // network_file_name%string()
    inference_engine = inference_engine_t(file_t(network_file_name))

    print *,"Defining an array of tensor_t input objects with random normalized components"
    allocate(inputs(lat,lon,lev))
    allocate(input_components(lat,lon,lev,inference_engine%num_inputs()))
    call random_number(input_components)

    do concurrent(i=1:lat, j=1:lon, k=1:lev)
      inputs(i,j,k) = tensor_t(input_components(i,j,k,:))
    end do

    block 
      integer(int64) t_start, t_finish, clock_rate

      print *,"Performing elemental inferences"
      call system_clock(t_start, clock_rate)
      outputs = inference_engine%infer(inputs)  ! implicit allocation of outputs array
      call system_clock(t_finish)
      print *,"Elemental inference time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)

      call assert(all(shape(outputs) == shape(inputs)), "all(shape(outputs) == shape(inputs))")

      print *,"Performing loop-based inference"
      call system_clock(t_start)
      do k=1,lev
        do j=1,lon
          do i=1,lat
            outputs(i,j,k) = inference_engine%infer(inputs(i,j,k))
          end do
        end do
      end do
      call system_clock(t_finish)
      print *,"Looping inference time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)

      print *,"Performing concurrent inference"
      call system_clock(t_start)
      do concurrent(i=1:lat, j=1:lon, k=1:lev)
        outputs(i,j,k) = inference_engine%infer(inputs(i,j,k))           
      end do
      call system_clock(t_finish)
      print *,"Concurrent inference time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
    end block
  end block

end program
