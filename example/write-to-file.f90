program get_flag_value
  !! Demonstrate how to construct a neural network at runtime and write
  !! it to a file in the format that inference_engine_t's read_network
  !! type-bound procedure expects.
  use command_line_m, only : command_line_t
  use inference_engine_m, only : inference_engine_t, activation_function
  implicit none

  type(inference_engine_t) xor
  type(command_line_t) command_line
  character(len=:), allocatable :: output_file_name
  procedure(activation_function), pointer :: f
  integer i, j
  integer, parameter :: identity(*,*,*) = reshape([((merge(1,0,i==j), i=1,3), j=1,3)], shape=[3,3,1])
   
  f => step

  output_file_name =  command_line%flag_value("--output-file")

   xor = inference_engine_t( &
    input_weights = real(reshape([1,0,1,1,0,1], [2,3])), &
    hidden_weights = real(identity), &
    output_weights = real(reshape([1,-2,1], [1,3])), &
    biases = reshape([0.,-1.99,0., 0.,0.,0.], [3,2]), &
    output_biases = [0.], &
    activation = f & 
  )  
  print *,"Writing an inference_engine_t object to the file '"//output_file_name//"'"

  call xor%write_network(output_file_name)

contains

    pure function step(x) result(y)
      real, intent(in) :: x
      real y
      y = merge(1., 0., x>0.)
    end function
  
end program
