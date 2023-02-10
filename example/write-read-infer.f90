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
  use kind_parameters_m, only : rkind
  implicit none

  type(string_t) file_name
  type(command_line_t) command_line

  file_name = string_t(command_line%flag_value("--output-file"))

  if (len(file_name%string())==0) then
    error stop new_line('a') // new_line('a') // &
      'Usage: ./build/run-fpm.sh run --example write-read-infer -- --output-file "<file-name>"' 
  end if

  call write_read_query_infer(file_name)

contains

  subroutine write_read_query_infer(output_file_name)
    type(string_t), intent(in) :: output_file_name
    integer i, j
    integer, parameter :: num_inputs = 2, num_outputs = 1, num_neurons = 3, num_hidden_layers = 2
    integer, parameter :: identity(*,*,*) = & 
      reshape([((merge(1,0,i==j), i=1,num_neurons), j=1,num_neurons)], shape=[num_neurons,num_neurons,num_hidden_layers-1])
    type(inference_engine_t) xor_network, inference_engine
    type(file_t) json_output_file, json_input_file

    print *, "Constructing an inference_engine_t neural-network object from scratch."
    xor_network = inference_engine_t( &
      input_weights = real(reshape([1,0,1,1,0,1], [num_inputs, num_neurons]), rkind), &
      hidden_weights = real(identity, rkind), &
      output_weights = real(reshape([1,-2,1], [num_outputs, num_neurons]), rkind), &
      biases = reshape([real(rkind):: 0.,-1.99,0., 0.,0.,0.], [num_neurons, num_hidden_layers]), &
      output_biases = [real(rkind):: 0.], &
      inference_strategy = matmul_t() &
    ) 
    print *, "Converting an inference_engine_t object to a file_t object."
    json_output_file = xor_network%to_json()

    print *, "Writing an inference_engine_t object to the file '"//output_file_name%string()//"' in JSON format."
    call json_output_file%write_lines(output_file_name)

    print *, "Reading an inference_engine_t object from the same JSON file '"//output_file_name%string()//"'."
    json_input_file = file_t(output_file_name)

    print *, "Constructing a new inference_engine_t object from the parameters read."
    inference_engine = inference_engine_t(json_input_file, step_t(), matmul_t())

    print *, "Querying the new inference_engine_t object for several properties:"
    print *, "num_outputs = ", inference_engine%num_outputs()
    print *, "num_hidden_layers = ", inference_engine%num_hidden_layers()
    print *, "neurons_per_layer = ", inference_engine%neurons_per_layer()

    print *, "Performing inference:"
    print *, "inference_engine%infer([0.,1.]) =",inference_engine%infer([real(rkind):: 0.,1.])
    print *, "Correct answer for the XOR neural network: ", 1.
  end subroutine write_read_query_infer

end program