submodule(training_configuration_m) training_configuration_s
  use relu_m, only : relu_t
  use sigmoid_m, only : sigmoid_t
  use swish_m, only : swish_t
  implicit none

  character(len=*), parameter :: header="{", footer="}", separator = ","

contains

  module procedure from_components

    training_configuration%hyperparameters_ = hyperparameters
    training_configuration%network_configuration_ = network_configuration
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
