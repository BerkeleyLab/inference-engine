program print_sample_input_file
  use inference_engine_m, only : hyperparameters_t, network_configuration_t
  implicit none

  associate(params => hyperparameters_t(mini_batches=10, learning_rate=1.5, optimizer = "adam"))
    associate(net_conf=> network_configuration_t(skip_connections=.false., nodes_per_layer=[2,72,2], activation_function="sigmoid"))
      associate(params_json => params%to_json(), net_json => net_conf%to_json())
        print *,"{"
        block 
          integer line
          do line = 1, size(params_json)
            print *, (params_json(line)%string())
          end do
          do line = 1, size(net_json)
            print *, (net_json(line)%string())
          end do
        end block
        print *,"}"
      end associate
    end associate
  end associate


end program
