! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program write_read_infer
  !! This program demonstrates how to write a neural network to a JSON file,
  !! read the same network from the written file, query the network object for
  !! some of its properties, print those properties, and use the network to
  !! perform inference.
  use command_line_m, only : command_line_t
  use inference_engine_m, only : inference_engine_t
  use string_m, only : string_t
  use matmul_m, only : matmul_t
  use step_m, only : step_t
  use file_m, only : file_t
  implicit none

  type(string_t) file_name
  type(command_line_t) command_line

  file_name = string_t(command_line%flag_value("--input-file"))

  if (len(file_name%string())==0) then
    error stop new_line('a') // new_line('a') // &
      'Usage: ./build/run-fpm.sh run --example write-read-infer -- --input-file "<file-name>"' 
  end if

  call write_read_query_infer(file_name)

contains

  subroutine write_read_query_infer(input_file_name)
    type(string_t), intent(in) :: input_file_name
    type(inference_engine_t) inference_engine
    type(file_t) json_input_file

    print *, "Reading an inference_engine_t object from the same JSON file '"//input_file_name%string()//"'."
    json_input_file = file_t(input_file_name)

    print *, "Constructing a new inference_engine_t object from the parameters read."
    inference_engine = inference_engine_t(json_input_file, step_t(), matmul_t())

    print *, "Querying the new inference_engine_t object for several properties:"
    print *, "num_outputs = ", inference_engine%num_outputs()
    print *, "num_hidden_layers = ", inference_engine%num_hidden_layers()
    print *, "neurons_per_layer = ", inference_engine%neurons_per_layer()

    print *, "Performing inference:"
    print *, "inference_engine%infer([1.,2.,3.,4.,5.,6.,7.,9.,10.]) =",inference_engine%infer([1.,2.,3.,4.,5.,6.,7.,9.,10.])
  end subroutine write_read_query_infer

end program
