submodule(hyperparameters_m) hyperparameters_s
  implicit none

  character(len=*), parameter :: mini_batches_key  = "mini-batches"
  character(len=*), parameter :: learning_rate_key = "learning rate"
  character(len=*), parameter :: optimizer_key     = "optimizer"

contains

  module procedure from_components
    hyperparameters%mini_batches_ = mini_batches
    hyperparameters%learning_rate_ = learning_rate
    hyperparameters%optimizer_ = optimizer
  end procedure 

  module procedure equals

    real, parameter :: tolerance = 1.E-08

    lhs_equals_rhs = &
      lhs%mini_batches_ == rhs%mini_batches_ .and. &
      lhs%optimizer_ == rhs%optimizer_ .and. &
      abs(lhs%learning_rate_ - rhs%learning_rate_) <= tolerance
     
  end procedure 

  module procedure mini_batches
    num_mini_batches = self%mini_batches_
  end procedure

  module procedure optimizer_name
    identifier = string_t(self%optimizer_)
  end procedure

  module procedure learning_rate
    rate = self%learning_rate_
  end procedure

end submodule hyperparameters_s
