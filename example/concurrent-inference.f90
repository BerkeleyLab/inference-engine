program concurrent_inferences
  !! This program demonstrates how to read multiple neural networks, each from a
  !! separate file, into an array of inference_engine_t objects and then perform
  !! inference concurrently using the resulting inference_engine_t array.
  use string_m, only : string_t, array_of_strings
  use inference_engine_m, only : inference_engine_t
  use command_line_m, only : command_line_t 
  implicit none

  logical, allocatable :: test_passes(:)
  type(inference_engine_t), allocatable :: inference_engine(:)
  type(command_line_t) command_line
  type(string_t), allocatable :: file_names(:)
  character(len=:), allocatable :: input_files
  real, allocatable :: truth_table(:)
  real, parameter :: tolerance = 1.E-08, false = 0., true = 1.
  real, parameter ::  input_array(*,*) = reshape([true, true, false, true, true, false, false, false], shape=[2,4])
  real, parameter ::  expected_result(*) = [false, true, true, false] 
  integer i

  input_files =  command_line%flag_value("--input-files")

  print *,"Defining an array of inference_engine_t objects by reading the following files: ", input_files
  if (len(input_files)==0) &
    error stop new_line('a') // new_line('a') // &
      'Usage: fpm run --example concurrent-inference -- --input-files "<space-delimited-list-of-files>"' 

  file_names = array_of_strings(input_files, delimiter=" ")

  associate(num_engines => size(file_names))
    allocate(inference_engine(num_engines))
    do i = 1, num_engines
      call inference_engine(i)%read_network(file_names(i))
    end do

    allocate(truth_table(num_engines))

    do concurrent(i = 1:num_engines)
      associate(inference => inference_engine(i)%infer(input_array(:,i)))
        truth_table(i) = inference(1)
      end associate
    end do
    
    associate(test_passed => abs(truth_table - expected_result) <  tolerance)
      print *, test_passed
    end associate
  end associate
end program
