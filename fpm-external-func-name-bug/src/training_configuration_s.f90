submodule(training_configuration_m) training_configuration_s
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

end submodule training_configuration_s
