program read_and_query
  !! This program demonstrates how to read a neural network from a file into an
  !! inference_engine_t object and then query the resulting object for its properties.
  use command_line_m, only : command_line_t
  use inference_engine_m, only : inference_engine_t
  implicit none

  type(inference_engine_t) inference_engine
  type(command_line_t) command_line
  character(len=:), allocatable :: input_file_name

  input_file_name =  command_line%flag_value("--input-file")
  print *,"Defining an inference_engine_t object by reading the file '"//input_file_name//"'"
  call inference_engine%read_network(input_file_name)

  print *,"num_inputs = ", inference_engine%num_inputs()
  print *,"num_outputs = ", inference_engine%num_outputs()
  print *,"num_hidden_layers = ", inference_engine%num_hidden_layers()
  print *,"neurons_per_layer = ", inference_engine%neurons_per_layer()
end program
