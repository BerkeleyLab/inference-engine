! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program icar_qr_network
  !! This program demonstrates how to read a neural network from a JSON file.
  !! query the network for a some of its properties.
  use command_line_m, only : command_line_t
  use inference_engine_m, only : inference_engine_t, inputs_t
  use string_m, only : string_t
  use matmul_m, only : matmul_t
  use file_m, only : file_t
  implicit none

  type(string_t) input_file_name
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

  print *, inference_engine%infer([5.7010256568901241e-05, 3.6012468740409531e-07, 1.4284904636951978e-07, &
     0.0000000000000000e+00, 0.0000000000000000e+00, 1.8107765197753906e+01, 1.9922698868413136e-07, &
      4.8543064679051895e-08, 2.3069851398468018e+00, 5.4544006347656250e+02], matmul_t())*120.

end program
