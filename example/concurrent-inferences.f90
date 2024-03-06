! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program concurrent_inferences
  !! This program demonstrates how to read a neural network from a JSON file
  !! and use the network to perform concurrent inferences.
  use inference_engine_m, only : inference_engine_t, tensor_t, infer
  use sourcery_m, only : string_t, command_line_t, file_t
  use assert_m, only : assert, intrinsic_array_t
  use iso_fortran_env, only : int64, real64
  use, intrinsic :: omp_lib
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
    type(tensor_t), allocatable :: inputs(:,:,:), outputs_elem_infer(:,:,:), outputs(:,:,:)
    real, allocatable :: input_components(:,:,:,:)
    integer, parameter :: lat=350, lon=450, lev=20 ! latitudes, longitudes, levels (elevations)
    integer i, j, k
    real, parameter :: tolerance = 1.e-6

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
      associate(outputs_tensors => inference_engine%infer(inputs))
        ! added this one
        outputs_elem_infer = outputs_tensors
      end associate
      call system_clock(t_finish)
      print *,"Elemental inference time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
      call assert(all(shape(outputs_elem_infer) == shape(inputs)), "all(shape(outputs) == shape(inputs))")
      allocate(outputs(lat,lon,lev))

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

      !Looping inference test
      do concurrent(i=1:lat, j=1:lon, k=1:lev)
        call assert(all(abs(outputs(i,j,k)%values() - outputs_elem_infer(i,j,k)%values()) < tolerance), &
          "all(looping_outputs == outputs_elemental_infer)")
      end do  

      print *,"Performing concurrent inference"
      call system_clock(t_start)
      do concurrent(i=1:lat, j=1:lon, k=1:lev)
        outputs(i,j,k) = inference_engine%infer(inputs(i,j,k))           
      end do
      call system_clock(t_finish)
      print *,"Concurrent inference time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)

      !Concurrent inference test
      do concurrent(i=1:lat, j=1:lon, k=1:lev)
        call assert(all(abs(outputs(i,j,k)%values() - outputs_elem_infer(i,j,k)%values()) < tolerance), &
          "all(concurrent_outputs == outputs_elemental_infer)")
      end do  

      print *,"Performing concurrent inference with a non-type-bound inference procedure"
      call system_clock(t_start)
      do concurrent(i=1:lat, j=1:lon, k=1:lev)
        outputs(i,j,k) = infer(inference_engine, inputs(i,j,k))           
      end do
      call system_clock(t_finish)
      print *,"Concurrent inference time with non-type-bound procedure: ", real(t_finish - t_start, real64)/real(clock_rate, real64)

      !Concurrent inference with non-type-bound procedure test
      do concurrent(i=1:lat, j=1:lon, k=1:lev)
        call assert(all(abs(outputs(i,j,k)%values() - outputs_elem_infer(i,j,k)%values()) < tolerance), &
          "all(concurrent_with_non_type_bound_proc_outputs == outputs_elemental_infer)")
      end do  

      print *, "performing inference with openmp multi-threading"
      call system_clock(t_start)
      !$omp parallel do default(none) shared(inputs, outputs, inference_engine) schedule(static)
      do concurrent(i=1:lat, j=1:lon, k=1:lev)
        outputs(i,j,k) = inference_engine%infer(inputs(i,j,k))           
      end do
      
      ! do j=1,lon
      !   do k=1,lev
      !     do i=1,lat
      !       outputs(i,j,k) = inference_engine%infer(inputs(i,j,k))
      !     end do
      !   end do
      ! end do
      !$omp end parallel do
      call system_clock(t_finish)
      print *,"Concurrent inference time with openmp multi-threading: ", real(t_finish - t_start, real64)/real(clock_rate)
      print *,sizeof(outputs(1,1,1)%values(1))
      !Concurrent inference with non-type-bound procedure test
      do concurrent(i=1:lat, j=1:lon, k=1:lev)
        call assert(all(abs(outputs(i,j,k)%values() - outputs_elem_infer(i,j,k)%values()) < tolerance), &
          "all(openmp_multithreading_outputs == outputs_elemental_infer)")
      end do  

      print *,"Performing batched inferences via intrinsic-array input and output"
      block 
        integer n

        associate(num_inputs => inference_engine%num_inputs())
          associate(inputs_batch => reshape( &
            [((((inputs(i,j,k)%values(), i=1,lat), j=1,lon), k=1,lev), n=1,num_inputs)], &
            shape=[lat,lon,lev,n] &
          ))
            call system_clock(t_start, clock_rate)
            associate(batch_outputs => inference_engine%infer(inputs_batch))
            end associate
            call system_clock(t_finish)
          end associate
        end associate
        print *,"Batch inference time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
      end block
    end block
  end block

end program
