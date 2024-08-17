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

  set_file_name:&
  associate(file_name => string_t(command_line%flag_value("--input-file")))

    if (len(file_name%string())==0) then
      error stop new_line('a') // new_line('a') // &
        'Usage: fpm run --example read-query-infer -- --input-file "<file-name>"' 
    end if

    print *, "Reading an neural network from the file '"//file_name%string()//"'."
    construct_inference_engine: &
    associate(inference_engine => inference_engine_t(file_t(file_name)))

      print *, "Querying the neural network for several properties:"
      associate(activation_name => inference_engine%activation_function_name())
        print *, "Activation function: ", activation_name%string()
      end associate

      set_num_inputs: &
      associate(num_inputs => inference_engine%num_inputs())
        print *, "Number of inputs:", num_inputs
        print *, "Number of outputs:", inference_engine%num_outputs()
        print *, "Nodes per layer:", inference_engine%nodes_per_layer()
        print *, "Performing inference:"

        block
          real random_inputs(num_inputs)

          call random_init(repeatable=.false., image_distinct=.true.)
          call random_number(random_inputs)

          associate(inputs => tensor_t(random_inputs))
            associate(outputs => inference_engine%infer(inputs))
              print *, new_line(''), "Random Inputs: ", inputs%values()
              print *, new_line(''), "--------------"
              print *, new_line(''), "Outputs: ", outputs%values(), new_line('')
            end associate
          end associate

          end block
      end associate set_num_inputs
    end associate construct_inference_engine
  end associate set_file_name
end program
