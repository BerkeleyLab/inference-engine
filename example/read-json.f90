! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program read_json
  !! This program demonstrates how to 
  !! 1. Read a neural network from a JSON file into an inference_engine_t object,
  !! 2. Query the object for some of its properties (number of inputs, etc.), and
  !! 3. User the object to perform inference 
  !! This example expects 10 inputs and applies a sigmoid activation function.
  use assert_m, only : assert
  use command_line_m, only : command_line_t
  use inference_engine_m, only : inference_engine_t
  use string_m, only : string_t
  use matmul_m, only : matmul_t
  use step_m, only : step_t
  use file_m, only : file_t
  implicit none

  type(inference_engine_t) inference_engine
  type(command_line_t) command_line
  character(len=:), allocatable :: input_file_name

  input_file_name = command_line%flag_value("--input-file")

  if (len(input_file_name)==0) then
    error stop new_line('a') // new_line('a') // &
      'Usage: ./build/run-fpm.sh run --example read-and-infer -- --input-file "<file-name>"' 
  end if

  print *,"Defining an inference_engine_t object by reading the file '"//input_file_name//"'"
  call inference_engine%from_json(file_t(string_t(input_file_name)), step_t(), matmul_t())

  print *,"num_outputs = ", inference_engine%num_outputs()
  print *,"num_hidden_layers = ", inference_engine%num_hidden_layers()
  print *,"neurons_per_layer = ", inference_engine%neurons_per_layer()

  associate(num_inputs => inference_engine%num_inputs())
    print *,"num_inputs = ", num_inputs
    block
      real, parameter :: inputs(*) = [0.,1.]

      call assert(num_inputs==size(inputs),"main: num_inputs==size(inputs)", num_inputs)
      print *, inference_engine%infer(inputs)
    end block
  end associate

  block
    integer i, j
    integer, parameter :: n_in = 2 ! number of inputs
    integer, parameter :: n_out = 1 ! number of outputs
    integer, parameter :: neurons = 3 ! number of neurons per layer
    integer, parameter :: n_hidden = 2 ! number of hidden layers 
    integer, parameter :: identity(*,*,*) = & 
      reshape([((merge(1,0,i==j), i=1,neurons), j=1,neurons)], shape=[neurons,neurons,n_hidden-1])
    type(inference_engine_t) xor, difference

    xor = inference_engine_t( &
      input_weights = real(reshape([1,0,1,1,0,1], [n_in, neurons])), &
      hidden_weights = real(identity), &
      output_weights = real(reshape([1,-2,1], [n_out, neurons])), &
      biases = reshape([0.,-1.99,0., 0.,0.,0.], [neurons, n_hidden]), &
      output_biases = [0.], &
      inference_strategy = matmul_t() &
    ) 
    
      print *, xor%infer([0.,1.])

    difference = inference_engine - xor
  end block

end program
