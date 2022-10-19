program concurrent_multi_inferrence
  !! This program demonstrates how to read multiple neural networks, each from a
  !! separate file, into an array of inference_engine_t objects and then perform
  !! inference concurrently using the resulting inference_engine_t array.
  use command_line_m, only : command_line_t
  use inference_engine_m, only : inference_engine_t
  implicit none

  type(inference_engine_t), allocatable :: inference_engines(:)
  type(command_line_t) command_line
  character(len=:), allocatable :: input_files
  real, allocatable :: outputs(:), inputs(:,:)
  integer network

  input_files =  command_line%flag_value("--input-files")


  print *,"Defining an array of inference_engine_t objects by reading the files '"//input_files//"'"

  associate(file_names => array_of_strings(input_files, delimiter=" "))
  
    error stop "trim file_names"

    call inference_engines%read_network(file_names)
    do concurrent(network=1:size(inference_engines))
      outputs = inference_engines(network)%infer(inputs(:,network))
    end do
  end associate

  print *,"num_inputs = ", inference_engines%num_inputs()
  print *,"num_outputs = ", inference_engines%num_outputs()
  print *,"num_hidden_layers = ", inference_engines%num_hidden_layers()
  print *,"neurons_per_layer = ", inference_engines%neurons_per_layer()

contains

  pure function array_of_strings(delimited_strings, delimiter) result(array)
    character(len=*), intent(in) :: delimited_strings, delimiter
    character(len=len(delimited_strings)), allocatable :: array(:)
    character(len=:), allocatable :: remainder, next_string
    integer next_delimiter, string_end

    remainder = trim(adjustl(delimited_strings))
    allocate(array(0))

    do 
      next_delimiter = index(remainder, delimiter)
      string_end = merge(next_delimiter-1, len(remainder), next_delimiter/=0)
      if (string_end==len(remainder)) then
        next_string = trim(adjustl(remainder(:string_end)))
        remainder = ""
      else
        next_string = trim(adjustl(remainder(:string_end)))
        remainder = trim(adjustl(remainder(next_delimiter+1:)))
      end if
      if (len(next_string)==0) exit
      array = [character(len=len(array)):: array, next_string]
    end do

  end function

end program
