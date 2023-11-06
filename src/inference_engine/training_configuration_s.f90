submodule(training_configuration_m) training_configuration_s
  use assert_m, only : assert
  use inference_engine_m, only : relu_t, sigmoid_t, swish_t
  implicit none

  character(len=*), parameter :: header="{", footer="}", separator = ","

contains

  module procedure from_components

    training_configuration%hyperparameters_ = hyperparameters
    training_configuration%network_configuration_ = network_configuration
    training_configuration%file_t = file_t([ &
      string_t(header), &
      training_configuration%hyperparameters_%to_json(), &
      string_t(separator), &
      training_configuration%network_configuration_%to_json(), &
      string_t(footer) &
    ])
  end procedure

  module procedure from_file
    integer, parameter :: hyperparameters_start=2, hyperparameters_end=6, separator_line=7   ! line numbers
    integer, parameter :: net_config_start=8, net_config_end=12                         ! line numbers
    integer, parameter :: file_start=hyperparameters_start-1, file_end=net_config_end+1 ! line numbers

    training_configuration%file_t = file_object

    associate(lines => training_configuration%file_t%lines())
      call assert(trim(adjustl(lines(file_start)%string()))==header,"training_configuration_s(from_file): header",lines(file_start))
      training_configuration%hyperparameters_ = hyperparameters_t(lines(hyperparameters_start:hyperparameters_end))
      call assert(trim(adjustl(lines(separator_line)%string()))==separator,"training_configuration_s(from_file): separator", &
        lines(file_start))
      training_configuration%network_configuration_= network_configuration_t(lines(net_config_start:net_config_end))
      call assert(trim(adjustl(lines(file_end)%string()))==footer, "training_configuration_s(from_file): footer", lines(file_end))
    end associate
  end procedure

  module procedure to_json
    json_lines = self%lines()
  end procedure

  module procedure equals
    lhs_eq_rhs = &
      lhs%hyperparameters_ == rhs%hyperparameters_ .and. &
      lhs%network_configuration_ == rhs%network_configuration_
  end procedure

  module procedure mini_batches
    num_mini_batches = self%hyperparameters_%mini_batches()
  end procedure

  module procedure optimizer_name
    identifier = self%hyperparameters_%optimizer_name()
  end procedure

  module procedure learning_rate
    rate = self%hyperparameters_%learning_rate()
  end procedure

  module procedure nodes_per_layer
    nodes = self%network_configuration_%nodes_per_layer()
  end procedure

  module procedure skip_connections
    using_skip = self%network_configuration_%skip_connections()
  end procedure

  module procedure differentiable_activation_strategy

    associate(activation_name => self%network_configuration_%activation_name())
      select case(activation_name%string())
        case ("relu")
          strategy = relu_t()
        case ("sigmoid")
          strategy = sigmoid_t()
        case ("swish")
          strategy = swish_t()
        case default
          error stop 'activation_strategy_factory_s(factory): unrecognized activation name "' // activation_name%string() // '"' 
      end select
    end associate

  end procedure

end submodule training_configuration_s
