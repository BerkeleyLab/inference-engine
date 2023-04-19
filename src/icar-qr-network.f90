! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program icar_qr_network
  !! This program demonstrates how to read a neural network from a JSON file.
  !! query the network for a some of its properties.
  use command_line_m, only : command_line_t
  use inference_engine_m, only : inference_engine_t
  use string_m, only : string_t
  use matmul_m, only : matmul_t
  use file_m, only : file_t
  use kind_parameters_m, only : rkind
  use outputs_m, only : outputs_t
  implicit none

  type(string_t) input_file_name, activation_name
  type(command_line_t) command_line
  type(inference_engine_t) inference_engine

  input_file_name = string_t(command_line%flag_value("--input-file"))

  if (len(input_file_name%string())==0) then
    error stop new_line('a') // new_line('a') // &
      'Usage: ./build/run-fpm.sh run --example icar-qr-network -- --input-file "<file-name>"' 
  end if

  print *, "Constructing a new inference_engine_t object by parameters from '"//input_file_name%string()//"'."
  inference_engine = inference_engine_t(file_t(input_file_name))
  print *, "num_inputs = ", inference_engine%num_inputs()
  print *, "num_outputs = ", inference_engine%num_outputs()
  print *, "num_hidden_layers = ", inference_engine%num_hidden_layers()
  print *, "neurons_per_layer = ", inference_engine%neurons_per_layer()
  activation_name = inference_engine%activation_function_name()
  print *, "activation function: ", activation_name%string()
  print *, "using skip connections: ", merge("true ", "false", inference_engine%skip())

  block
    real(rkind), parameter :: inputs(*) = [ real(rkind) :: &
      5.4041727707954124e-05_rkind, 1.8311046012797760e-09_rkind, 5.4236606525370767e-13_rkind, 0._rkind, 0._rkind, &
      2.3016514256596565e-02_rkind, 1.0734779465337851e-07_rkind, 3.9603170742807947e-10_rkind, 2.3047807216644287e+00_rkind, &
      5.3973675537109375e+02_rkind &
    ]
    type(outputs_t) network_outputs

    print *, "inputs: ", inputs
    network_outputs = inference_engine%infer(inputs, matmul_t())
    print *, "outputs: ", network_outputs%outputs()
  end block

end program
