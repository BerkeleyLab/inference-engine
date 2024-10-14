! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program read_query_infer
  !! This program demonstrates how to read a neural network from a JSON file,
  !! query the network object for some of its properties, print those properties,
  !! and use the network to perform inference.  
  use fiats_m, only : neural_network_t, tensor_t
  use julienne_m, only : string_t, command_line_t, file_t
  implicit none

  type(command_line_t) command_line

  associate(file_name => string_t(command_line%flag_value("--input-file")))

    if (len(file_name%string())==0) then
      error stop new_line('a') // new_line('a') // &
        'Usage: fpm run --example read-query -- --input-file "<file-name>"' 
    end if

    print *, "Reading an neural_network_t object from the same JSON file '"//file_name%string()//"'."
    associate(neural_network => neural_network_t(file_t(file_name)))

      print *, "Querying the new neural_network_t object for several properties:"
      associate(activation_name => neural_network%activation_function_name())
        print *, "Activation function: ", activation_name%string()
      end associate
      print *, "Number of outputs:", neural_network%num_outputs()
      print *, "Number of inputs:", neural_network%num_inputs()
      print *, "Nodes per layer:", neural_network%nodes_per_layer()
      print *, "Performing inference:"

      block
        integer, parameter :: tensor_size = 2, num_tests = 3, tensor_min = 1., tensor_max = 4.0
        real harvest(tensor_size)
        integer i

        call random_init(repeatable=.false., image_distinct=.true.)

        print *, "Inputs                 |       Outputs  "

        do i = 1, num_tests
          call random_number(harvest)
          associate(inputs => tensor_t(tensor_min + (tensor_max-tensor_min)*harvest))
            associate(outputs => neural_network%infer(inputs))
              print '(2(2g12.5,a,2x))', inputs%values(), "|",  outputs%values()
            end associate
          end associate
        end do

      end block
    end associate ! associate(neural_network => ...)
  end associate ! associate(file_name => ...)
end program
