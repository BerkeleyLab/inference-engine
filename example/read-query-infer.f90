! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program read_query_infer
  !! This program demonstrates how to read a neural network from a JSON file,
  !! query the network object for some of its properties, print those properties,
  !! and use the network to perform inference.  
  use inference_engine_m, only : inference_engine_t, relu_t, tensor_t
  use julienne_m, only : string_t, command_line_t, file_t
  use kind_parameters_m, only : rkind
  implicit none

  type(command_line_t) command_line

  associate(file_name => string_t(command_line%flag_value("--input-file")))

    if (len(file_name%string())==0) then
      error stop new_line('a') // new_line('a') // &
        'Usage: fpm run --example read-query -- --input-file "<file-name>"' 
    end if

    print *, "Reading an inference_engine_t object from the same JSON file '"//file_name%string()//"'."
    associate(inference_engine => inference_engine_t(file_t(file_name)))

      print *, "Querying the new inference_engine_t object for several properties:"
      associate(activation_name => inference_engine%activation_function_name())
        print *, "Activation function: ", activation_name%string()
      end associate
      print *, "Number of outputs:", inference_engine%num_outputs()
      print *, "Number of inputs:", inference_engine%num_inputs()
      print *, "Nodes per layer:", inference_engine%nodes_per_layer()
      print *, "Performing inference:"

      block
        integer, parameter :: tensor_size = 2, num_tests = 3
        real, parameter :: tensor_range = 11._rkind
        real harvest(tensor_size)
        integer i

        call random_init(repeatable=.false., image_distinct=.true.)

        print *, "Inputs                 |       Outputs  "

        do i = 1, num_tests
          call random_number(harvest)
          associate(inputs => tensor_t(tensor_range*harvest))
            associate(outputs => inference_engine%infer(inputs))
              print '(2(2g12.5,a,2x))', inputs%values(), "|",  outputs%values()
            end associate
          end associate
        end do

      end block
    end associate ! associate(inference_engine => ...)
  end associate ! associate(file_name => ...)
end program
