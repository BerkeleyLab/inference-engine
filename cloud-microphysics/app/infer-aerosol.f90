! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

program infer_aerosol

  !! External dependencies:
  use julienne_m, only : string_t, file_t, command_line_t
  use assert_m, only : assert
  use iso_fortran_env, only : int64, real64

  !! Internal dependencies;
  use inference_engine_m, only : inference_engine_t, tensor_t, infer
  use NetCDF_file_m, only: NetCDF_file_t
  implicit none

  integer i,j,num_inputs,num_outputs
  type(string_t) network_file_name
  type(command_line_t) command_line
  type(inference_engine_t) network,inference_engine
  real, allocatable, dimension(:,:) :: aerosol_data
  real, allocatable, dimension(:) :: meanx, meany, stdx, stdy
  real cube_root,signa

  type(tensor_t), allocatable :: inputs(:), outputs(:)
  real, allocatable :: input_components(:,:)
  real, allocatable :: output_components(:,:)
  real, allocatable :: output_slice(:)
  
  character(len=*), parameter :: usage = &                                                             
    new_line('a') // new_line('a') // &                                                                
    'Usage: ' // new_line('a') // new_line('a') // &                                                   
    './build/run-fpm.sh run train-cloud-microphysics -- \' // new_line('a') // &
    '  --base <string> \' // new_line('a') // &
    new_line('a') // new_line('a') // &
    'where angular brackets denote user-provided values and square brackets denote optional arguments.\
    ' // new_line('a') // &
    'The presence of a file named "stop" halts execution gracefully.'

  character(len=*), parameter :: training_config_file_name = "training_configuration.json"

  integer(int64) t_start, t_finish, clock_rate
  character(len=:), allocatable :: base_name

  open(unit = 14,file="meanxp.txt")
  open(unit = 15,file="meanyp.txt")
  open(unit = 16,file="stdxp.txt")
  open(unit = 17,file="stdyp.txt")

  num_inputs = 80
  num_outputs = 31

  allocate(meanx(num_inputs),stdx(num_inputs))
  ! Reading means and standard deviations for pre-processing normalization
  do i = 1,num_inputs
     read(14,*) meanx(i)
     read(16,*) stdx(i)
  end do

  allocate(meany(num_outputs),stdy(num_outputs))
  ! Reading means and standard deviations for post-processing normalization  
  do i = 1,num_outputs
     read(15,*) meany(i)     
     read(17,*) stdy(i)
  end do

  call get_command_line_arguments(base_name) 
  
  associate( &
      network_input => base_name // "_input.nc", &
      network_file => base_name // "_network.json" &
    )
    print *,"Reading network inputs from " // network_input
    associate(network_input_file => netCDF_file_t(network_input))
      call network_input_file%input("input_vars",aerosol_data)
      print*,'shape aerosol array = ',shape(aerosol_data)
      
      allocate(input_components(size(aerosol_data,2),size(aerosol_data,1)))
      allocate(inputs(size(input_components,1)))
      allocate(outputs(size(input_components,1)))      
      allocate(output_components(size(aerosol_data,2),31))

      ! Pre-processing
      do i = 1,size(aerosol_data,1)
         do j = 1,size(aerosol_data,2)
            signa = sign(1.0,aerosol_data(i,j))
            cube_root = signa*(abs(aerosol_data(i,j))**(1.d0/3.d0))
            input_components(j,i) = (cube_root - meanx(i))/stdx(i)
         end do
      end do
      
      network_file_name = string_t(command_line%flag_value("--network"))
 
      print *, "Constructing a new inference_engine_t object from the file " // network_file_name%string()
      inference_engine = inference_engine_t(file_t(network_file_name))

      do concurrent(i=1:size(input_components,1))
         inputs(i) = tensor_t(input_components(i,:))
      end do
      
      call system_clock(t_start, clock_rate)      
      !      outputs = inference_engine%infer(inputs)  ! implicit allocation of outputs
      print*,'Inference'
      ! Inference
      do i = 1,size(input_components,1)
         outputs(i) = inference_engine%infer_na(inputs(i))
      end do
      print*,'End inference'
      call system_clock(t_finish)
      
      allocate(output_slice(num_outputs))

      ! Post-processing
      do i = 1,size(aerosol_data,2)
         output_slice = outputs(i)%values()
         do j = 1,num_outputs
            output_components(i,j) = (stdy(j)*output_slice(j)+meany(j))**3
            if (i .eq. 1) print*,'output slice = ',output_components(i,j)
         end do
      end do
   
    end associate
  end associate    
  
  
  print *,"System clock time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
  print *,new_line('a') // "______inference_aerosol done _______"

contains

  subroutine get_command_line_arguments(base_name)
    character(len=:), allocatable, intent(out) :: base_name

    ! local variables
    type(command_line_t) command_line

    base_name = command_line%flag_value("--base") ! gfortran 13 seg faults if this is an association

    associate(required_arguments => len(base_name)/=0)      
       if (.not. required_arguments) error stop usage 
    end associate
 
  end subroutine get_command_line_arguments

end program infer_aerosol


