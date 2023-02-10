! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program read_json
  !! This program demonstrates how to read a neural network from a JSON file.
  !! query the network for a some of its properties.
  use command_line_m, only : command_line_t
  use inference_engine_m, only : inference_engine_t
  use string_m, only : string_t
  use matmul_m, only : matmul_t
  use step_m, only : step_t
  use file_m, only : file_t
  implicit none

  type(string_t) input_file_name
  type(command_line_t) command_line
  type(inference_engine_t) inference_engine

  input_file_name = string_t(command_line%flag_value("--input-file"))

  if (len(input_file_name%string())==0) then
    error stop new_line('a') // new_line('a') // &
      'Usage: ./build/run-fpm.sh run --example read -- --input-file "<file-name>"' 
  end if

  print *, "Constructing a new inference_engine_t object by parameters from '"//input_file_name%string()//"'."
  inference_engine = inference_engine_t(file_t(input_file_name), step_t(), matmul_t())
  print *, "num_outputs = ", inference_engine%num_outputs()
  print *, "num_hidden_layers = ", inference_engine%num_hidden_layers()
  print *, "neurons_per_layer = ", inference_engine%neurons_per_layer()
end program