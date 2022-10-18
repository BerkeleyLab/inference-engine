program concurrent_multi_inferrence
  !! This program demonstrates how to read multiple neural networks, each from a
  !! separate file, into an array of inference_engine_t objects and then perform
  !! inference concurrently using the resulting inference_engine_t array.
  use command_line_m, only : command_line_t
  use inference_engine_m, only : inference_engine_t
  implicit none

  type(inference_engine_t), allocatable :: inference_engines(:)
  type(command_line_t) command_line
  character(len=:), allocatable :: input_file_names
  real, allocatable :: outputs(:), inputs(:,:)
  integer network

  input_file_names =  command_line%flag_value("--input-files")
  
  print *,"Defining an array of inference_engine_t objects by reading the files '"//input_file_names//"'"
  associate(file_names_array => space_delimited_strings_to_array(input_file_names))
    call inference_engines%read_network(file_names_array)
    error stop "concurrent_multi_inferrence: inputs not yet defined"
    ! inputs = 
    do concurrent(network=1:size(inference_engines))
      outputs = inference_engines(network)%infer(inputs(:,network))
    end do
  end associate

  print *,"num_inputs = ", inference_engines%num_inputs()
  print *,"num_outputs = ", inference_engines%num_outputs()
  print *,"num_hidden_layers = ", inference_engines%num_hidden_layers()
  print *,"neurons_per_layer = ", inference_engines%neurons_per_layer()

contains
  pure function space_delimited_strings_to_array(names) result(names_array)
    character(len=*), intent(in) :: names
    character(len=len(names)), allocatable :: names_array(:)
    error stop "concurrent_multi_inferrence: space_delimited_strings_to_array() function not yet implemented"
  end function
end program
