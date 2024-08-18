! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

program infer_aerosol

  ! Intrinsic modules:
  use iso_fortran_env, only : int64, real64

  ! External dependencies:
  use inference_engine_m, only : inference_engine_t, tensor_t, infer
  use julienne_m, only : string_t, file_t, command_line_t
  use kind_parameters_m, only : rkind
  use omp_lib

  ! Internal dependencies:
  use NetCDF_file_m, only: NetCDF_file_t
  implicit none

  type tensor_statistics_t
    real(rkind), allocatable, dimension(:) :: mean, standard_deviation
  end type

  character(len=*), parameter :: usage_info = & 
    new_line('') // &
    'Usage:  ./build/run-fpm.sh run infer-aerosol --  --file-path <string>' // &
    'where angular brackets (<>) denote user-provided input.' // &
    new_line('')

  call read_stats_and_perform_inference( file_path( stop_code = usage_info ) )

  print *,new_line('') // "______infer-aerosol done _______"

contains

  function file_path(stop_code) result(path)
    character(len=:), allocatable :: path
    character(len=*), intent(in) :: stop_code
    type(command_line_t) command_line

    path = command_line%flag_value("--file-path") // "/"
    if (len(path)==0) error stop stop_code

  end function

  subroutine read_stats_and_perform_inference(path)
    character(len=*), intent(in) :: path
    integer,          parameter :: num_inputs = 80, num_outputs = 31
    character(len=*), parameter :: network_file_name = "training_network.json"
    character(len=*), parameter :: inputs_tensor_file_name = "training_input.nc"
    real(rkind) cube_root
    real(rkind), allocatable, dimension(:,:) :: aerosol_data, input_components, output_components
    type(tensor_statistics_t) :: input_stats, output_stats
    type(inference_engine_t) inference_engine
    integer i, j

    input_stats  = read_tensor_statistics(path // "meanxp.txt", path // "stdxp.txt", num_inputs)  !for pre-processing normalization
    output_stats = read_tensor_statistics(path // "meanyp.txt", path // "stdyp.txt", num_outputs) !for post-processing normalization

    associate(inputs_tensor_file => NetCDF_file_t(path // inputs_tensor_file_name))
      print *,"Reading network inputs from ", inputs_tensor_file_name
      call inputs_tensor_file%input("input_vars", aerosol_data)
    end associate    

    ! print*,'shape aerosol array = ',shape(aerosol_data) ! convert to an asssertion
    
    allocate(input_components(size(aerosol_data,2),size(aerosol_data,1)))
    allocate(output_components(size(aerosol_data,2),num_outputs))

    !$omp parallel do shared(aerosol_data,input_components,input_stats) private(i,j,cube_root)
    pre_process: &
    do j = 1,size(aerosol_data,2)
      do i = 1,size(aerosol_data,1)
        cube_root = (abs(aerosol_data(i,j))**(1.d0/3.d0))*sign(1.0,aerosol_data(i,j))
        input_components(j,i) = (cube_root - input_stats%mean(i))/input_stats%standard_deviation(i)
      end do
    end do pre_process
    !$omp end parallel do   
      
    print *, "Reading the neural network from " // network_file_name
    inference_engine = inference_engine_t(file_t(path // network_file_name))

    time_inference: &
    block
      integer(int64) t_start, t_finish, clock_rate
      type(tensor_t), allocatable :: inputs(:), outputs(:)
      double precision start_time,end_time
      real(rkind), allocatable :: output_slice(:)
      integer i, icc

      allocate(inputs(size(input_components,1)))
      allocate(outputs(size(input_components,1)))      

      !$omp parallel do shared(inputs,input_components) 
      do i = 1,size(input_components,1)
        inputs(i) = tensor_t(input_components(i,:))
      end do
      !$omp end parallel do         

      print*, "Starting inference."
      call system_clock(t_start, clock_rate)      

      icc = size(input_components,1)

      !$ start_time = omp_get_wtime()
      !$omp parallel do shared(inputs,outputs,icc)
      do i = 1,icc
         outputs(i) = inference_engine%infer_na(inputs(i))
      end do
      !$omp end parallel do   
      !$    end_time = omp_get_wtime()       

      call system_clock(t_finish)
      print*, "Finished inference."
      print *,"System clock time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
      !$    print*,'OMP Total time = ',end_time - start_time

      allocate(output_slice(num_outputs))

      !$omp parallel do shared(outputs,output_components,icc,output_stats) private(output_slice,i,j)
      post_process: &
      do i = 1,icc
         output_slice = outputs(i)%values()
         do j = 1,num_outputs
            output_components(i,j) = (output_stats%standard_deviation(j)*output_slice(j)+output_stats%mean(j))**3
         end do
      end do post_process
      !$omp end parallel do

    end block time_inference
   
  end subroutine read_stats_and_perform_inference

  function read_tensor_statistics(mean_file, standard_deviation_file, n) result(tensor_statistics)
    type(tensor_statistics_t) tensor_statistics
    character(len=*), intent(in) :: mean_file, standard_deviation_file
    integer, intent(in) :: n
    integer i, mean_unit, standard_deviation_unit
  
    open(newunit = mean_unit, file = mean_file, status="old")
    open(newunit = standard_deviation_unit , file = standard_deviation_file, status="old")

    allocate(tensor_statistics%mean(n))
    allocate(tensor_statistics%standard_deviation(n))
  
    do i = 1, n
      read(mean_unit, *) tensor_statistics%mean(i)
      read(standard_deviation_unit , *) tensor_statistics%standard_deviation(i)
    end do

    close(mean_unit)
    close(standard_deviation_unit)
  end function

end program infer_aerosol
