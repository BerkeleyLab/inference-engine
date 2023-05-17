submodule(mini_batch_m) mini_batch_s
  implicit none

contains

  module procedure inputs
    my_inputs = self%inputs_
  end procedure

end submodule mini_batch_s
