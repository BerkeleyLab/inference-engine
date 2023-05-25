! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program train_xor_gate
  !! Define inference tests and procedures required for reporting results
  use string_m, only : string_t
  use trainable_engine_m, only : trainable_engine_t
  use inputs_m, only : inputs_t
  use outputs_m, only : outputs_t
  use expected_outputs_m, only : expected_outputs_t
  use matmul_m, only : matmul_t
  use kind_parameters_m, only : rkind
  use sigmoid_m, only : sigmoid_t
  use input_output_pair_m, only : input_output_pair_t 
  use mini_batch_m, only : mini_batch_t
  use file_m, only : file_t
  use command_line_m, only : command_line_t
  implicit none

  real(rkind), parameter :: tolerance = 1.E-02_rkind, false = 0._rkind, true = 1._rkind
  type(outputs_t), dimension(4) :: actual_output
  type(expected_outputs_t), dimension(size(actual_output)) :: expected_outputs 
  type(trainable_engine_t) trainable_engine
  type(command_line_t) command_line
  type(string_t) base_name
  type(mini_batch_t), allocatable :: mini_batches(:)
    type(inputs_t), allocatable :: inputs(:)
  character(len=5), parameter :: table_entry(*) = ["TT->F", "FT->T", "TF->T", "FF->F", "xor  "]
  integer i, m

  base_name = string_t(command_line%flag_value("--base-name"))
 
  if (len(base_name%string())==0) then
    error stop new_line('a') // new_line('a') // &
      'Usage: ./build/run-fpm.sh run --example train-xor-gate -- --base-name "<base-file-name>"'
  end if

  inputs = [ &
    inputs_t([true,true]), inputs_t([false,true]), inputs_t([true,false]), inputs_t([false,false]) &
  ]
  expected_outputs = [ &
    expected_outputs_t([false]), expected_outputs_t([true]), expected_outputs_t([true]), expected_outputs_t([false]) &
  ]
  print *,"Defining mini-batches, each containing input/output pairs corresponding to the four entries in the XOR truth table."
  mini_batches = [(mini_batch_t( input_output_pair_t( inputs, expected_outputs ) ), m=1,110000)]
  print *,"Defining an initial trainable_engine_t neural network object."
  trainable_engine = wide_single_layer_perceptron()
  print *,"Training the neural network using the mini-batches."
  block
    real t_start, t_end
    call cpu_time(t_start)
    call trainable_engine%train(mini_batches, matmul_t())
    call cpu_time(t_end)
    print *,"Training time:", t_end - t_start
  end block
  associate(file_name => string_t(base_name%string() // ".json"))
    print *,"Writing the network parameters to "
    call output(trainable_engine, file_name)
  end associate
  print *,"Verifying that the network behaves as an exclusive-or (XOR) logic gate: "
  actual_output = trainable_engine%infer(inputs, matmul_t())
  if (all([(abs(actual_output(i)%outputs() - expected_outputs(i)%outputs()) < tolerance, i=1, size(actual_output))])) then
    print *,"Yes!"
    call print_diagnostics
  else
    call print_diagnostics
    error stop &
      "The trained network does not behave as a XOR gate.  " // &
      "Please report this issue at https://github.com/BerkeleyLab/inference-engine/issues."
  end if

contains

  subroutine print_diagnostics()
    print *,"Network output: ", (actual_output(i)%outputs(), i=1,size(actual_output))
    print *,"Expected output: ", (expected_outputs(i)%outputs(), i=1,size(expected_outputs))
    print *,"Absolute Error: ", (abs(actual_output(i)%outputs() - expected_outputs(i)%outputs()), i = 1, size(actual_output))
    print *,"Tolerance: ", tolerance
  end subroutine

  subroutine output(engine, file_name)
    type(trainable_engine_t), intent(in) :: engine
    type(string_t), intent(in) :: file_name
    type(file_t) json_file
 
    json_file = trainable_engine%to_json()
    print *, "Writing an inference_engine_t object to the file '"//file_name%string()//"' in JSON format."
    call json_file%write_lines(file_name)
  end subroutine

  function wide_single_layer_perceptron() result(trainable_engine)
    type(trainable_engine_t) trainable_engine
    integer, parameter :: n_in = 2 ! number of inputs
    integer, parameter :: n_out = 1 ! number of outputs
    integer, parameter :: neurons = 36 ! number of neurons per layer
    integer, parameter :: n_hidden = 1 ! number of hidden layers 
    integer n
   
    trainable_engine = trainable_engine_t( &
      metadata = [ &
       string_t("Wide 1-layer perceptron"), string_t("Damian Rouson"), string_t("2023-05-24"), string_t("sigmoid"), string_t("false") &
      ], &
      input_weights = real(reshape([([1,0,1,1,0,1], n=1,12 )], [n_in, neurons]), rkind), &
      hidden_weights = reshape([real(rkind)::], [neurons,neurons,n_hidden-1]), &
      output_weights = real(reshape([([1,-2,1], n=1,12)], [n_out, neurons]), rkind), &
      biases = reshape([real(rkind):: [(0.,-1.99,0., n=1,12)] ], [neurons, n_hidden]), &
      output_biases = [real(rkind):: 0.], &
      differentiable_activation_strategy = sigmoid_t() &
    )
  end function

end program train_xor_gate
