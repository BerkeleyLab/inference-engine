program get_flag_value
  !! Demonstrate how to build a neural network from
  !! a file of weights and biases
  use command_line_m, only : command_line_t
  use inference_engine_m, only : inference_engine_t
  implicit none

  type(inference_engine_t) inference_engine
  type(command_line_t) command_line
  character(len=:), allocatable :: input_file_name

  input_file_name =  command_line%flag_value("--input-file")
  print *,"Defining an inference_engine_t object by reading the file '"//input_file_name//"'"
  call inference_engine%read_network(input_file_name)

end program
