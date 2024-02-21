submodule(training_configuration_m) training_configuration_s
  implicit none

  character(len=*), parameter :: header="{", footer="}", separator = ","

contains

  module procedure from_components

    training_configuration%hyperparameters_ = hyperparameters
  end procedure

  module procedure equals
    lhs_eq_rhs = &
      lhs%hyperparameters_ == rhs%hyperparameters_
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

end submodule training_configuration_s
