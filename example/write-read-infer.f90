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
  use step_m, only : step_t
  use file_m, only : file_t
  use kind_parameters_m, only : rkind
  use tensor_m, only : tensor_t
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

  function single_hidden_layer_xor_network() result(inference_engine)
    type(inference_engine_t) inference_engine
    integer, parameter :: nodes_per_layer(*) = [2, 3, 1]
    integer, parameter :: max_n = maxval(nodes_per_layer), layers = size(nodes_per_layer)

    inference_engine = inference_engine_t( &
      metadata = [string_t("XOR"), string_t("Damian Rouson"), string_t("2023-07-02"), string_t("step"), string_t("false")], &
      weights = reshape([real(rkind):: [1,1,0, 0,1,1, 0,0,0], [1,0,0, -2,0,0, 1,0,0]], [max_n, max_n, layers-1]), &
      biases = reshape([[0.,-1.99,0.], [0., 0., 0.]], [max_n, layers-1]), &
      nodes = nodes_per_layer &
    )
  end function

  subroutine write_read_query_infer(output_file_name)
    type(string_t), intent(in) :: output_file_name
    type(string_t) activation_name
    integer i, j
    integer, parameter :: num_neurons = 3, num_hidden_layers = 2
    type(inference_engine_t) xor_network, inference_engine
    type(file_t) json_output_file, json_input_file
    type(tensor_t) network_outputs
    real(rkind), parameter :: false = 0._rkind, true = 1._rkind

    print *, "Constructing an inference_engine_t neural-network object from scratch."
    xor_network = single_hidden_layer_xor_network()

    print *, "Converting an inference_engine_t object to a file_t object."
    json_output_file = xor_network%to_json()

    print *, "Writing an inference_engine_t object to the file '"//output_file_name%string()//"' in JSON format."
    call json_output_file%write_lines(output_file_name)

    print *, "Reading an inference_engine_t object from the same JSON file '"//output_file_name%string()//"'."
    json_input_file = file_t(output_file_name)

    print *, "Constructing a new inference_engine_t object from the parameters read."
    inference_engine = inference_engine_t(json_input_file)

    print *, "Querying the new inference_engine_t object for several properties:"
    print *, "number of outputs:", inference_engine%num_outputs()
    print *, "nodes per layer:", inference_engine%nodes_per_layer()
    activation_name = inference_engine%activation_function_name()
    print *, "activation function: ", activation_name%string()
    print *, "using skip connections: ", merge("true ", "false", inference_engine%skip())
    print *, "Performing inference:"
    network_outputs = inference_engine%infer(tensor_t([real(rkind):: false,true]))
    print *, "inference_engine%infer([0.,1.]) =", network_outputs%values()
    print *, "Correct answer for the XOR neural network: ", 1.
  end subroutine write_read_query_infer

end program
