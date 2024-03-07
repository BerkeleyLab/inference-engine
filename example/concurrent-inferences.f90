! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program concurrent_inferences
  !! This program demonstrates how to read a neural network from a JSON file
  !! and use the network to perform concurrent inferences.
  use inference_engine_m, only : inference_engine_t, tensor_t, infer
  use sourcery_m, only : string_t, command_line_t, file_t
  use assert_m, only : assert, intrinsic_array_t
  use iso_fortran_env, only : int64, real64
  implicit none

  type(string_t) network_file_name
  type(command_line_t) command_line

  network_file_name = string_t(command_line%flag_value("--network"))

  if (len(network_file_name%string())==0) then
    error stop new_line('a') // new_line('a') // &
      'Usage: fpm run --example concurrent-inferences --profile release --flag "-fopenmp" -- --network "<file-name>"'
  end if

  block 
    type(inference_engine_t) network, inference_engine
    type(tensor_t), allocatable :: inputs(:,:,:), outputs(:,:,:)
    real, allocatable :: outputs_batch(:,:,:,:)
    real, allocatable :: input_components(:,:,:,:)
    integer, parameter :: lon=350, lev=450, lat=20 ! longitudes, levels, latitudes 
    integer i, j, k

    print *, "Constructing a new inference_engine_t object from the file " // network_file_name%string()
    inference_engine = inference_engine_t(file_t(network_file_name))

    print *,"Defining an array of tensor_t input objects with random normalized components"
    allocate(inputs(lon,lev,lat))
    allocate(input_components(lon,lev,lat,inference_engine%num_inputs()))
    call random_number(input_components)

    do concurrent(i=1:lon, k=1:lev, j=1:lat)
      inputs(i,j,k) = tensor_t(input_components(i,j,k,:))
    end do

    block 
      integer(int64) t_start, t_finish, clock_rate

      print *,"Performing elemental inferences"
      call system_clock(t_start, clock_rate)
      associate(outputs_tensors => inference_engine%infer(inputs))
        ! added this one
        outputs = outputs_tensors
      end associate
      call system_clock(t_finish)
      print *,"Elemental inference time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
      call assert(all(shape(outputs) == shape(inputs)), "all(shape(outputs) == shape(inputs))")

      print *,"Performing loop-based inference"
      call system_clock(t_start)
      do j=1,lat
        do k=1,lev
          do i=1,lon
            outputs(i,k,j) = inference_engine%infer(inputs(i,k,j))
          end do
        end do
      end do
      call system_clock(t_finish)
      print *,"Looping inference time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)

      print *,"Performing concurrent inference"
      call system_clock(t_start)
      do concurrent(i=1:lon, k=1:lev, j=1:lat)
        outputs(i,k,j) = inference_engine%infer(inputs(i,k,j))
      end do
      call system_clock(t_finish)
      print *,"Concurrent inference time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)

      print *,"Performing concurrent inference with a non-type-bound inference procedure"
      call system_clock(t_start)
      do concurrent(i=1:lon, k=1:lev, j=1:lat)
        outputs(i,k,j) = infer(inference_engine, inputs(i,k,j))
      end do
      call system_clock(t_finish)
      print *,"Concurrent inference time with non-type-bound procedure: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
      print *,"Performing batched inferences via intrinsic-array input and output"
      block 
        integer n

        associate(num_inputs => inference_engine%num_inputs())
          associate(inputs_batch => reshape( &
            [((((inputs(i,k,j)%values(), i=1,lon), k=1,lev), j=1,lat), n=1,num_inputs)], &
            shape=[lon,lev,lat,n] &
          ))
            call system_clock(t_start, clock_rate)
            outputs_batch = inference_engine%infer(inputs_batch)
            ! associate(batch_outputs => inference_engine%infer(inputs_batch))
            !   outputs_batch = batch_outputs
            ! end associate
            call system_clock(t_finish)
          end associate
        end associate
        print *,"Batch inference time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
        block
          real, parameter :: tolerance = 1.
          !testing the result
          do concurrent(i=1:lon, k=1:lev, j=1:lat)
            associate(batch_outputs_tensor => tensor_t(outputs_batch(i,j,k,:)))
              call assert(all(abs(outputs(i,j,k)%values() - batch_outputs_tensor%values()) < tolerance), &
              "all(outputs == outputs_batch)", intrinsic_array_t(batch_outputs_tensor%values()))
            end associate
          end do
        end block
      end block 
    end block
  end block

end program
