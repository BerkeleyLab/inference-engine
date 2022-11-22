! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program construct_and_write
  !! This program demonstrates how to construct a neural network from arrays of weights and biases
  !! and how to write the resulting object to a file in the format that is readable by the
  !! inference_engine_t read_network type-bound procedure.
  use command_line_m, only : command_line_t
  use inference_engine_m, only : inference_engine_t
  use string_m, only : string_t
  implicit none

  type(inference_engine_t) xor
  type(command_line_t) command_line
  character(len=:), allocatable :: output_file_name
  integer i, j
  integer, parameter :: identity(*,*,*) = reshape([((merge(1,0,i==j), i=1,3), j=1,3)], shape=[3,3,1])
   
  output_file_name =  command_line%flag_value("--output-file")

  if (len(output_file_name)==0) then
    error stop new_line('a') // new_line('a') // &
      'Usage: fpm run --example construct-and-write -- --output-file "<file-name>"' 
  end if

  xor = inference_engine_t( &
    input_weights = real(reshape([1,0,1,1,0,1], [2,3])), &
    hidden_weights = real(identity), &
    output_weights = real(reshape([1,-2,1], [1,3])), &
    biases = reshape([0.,-1.99,0., 0.,0.,0.], [3,2]), &
    output_biases = [0.] &
  )  
  print *,"Writing an inference_engine_t object to the file '"//output_file_name//"'"

  call xor%write_network(string_t(output_file_name))

end program
