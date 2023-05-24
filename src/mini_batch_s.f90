submodule(mini_batch_m) mini_batch_s
  implicit none

contains

    module procedure construct
      mini_batch%input_output_pairs_ = input_output_pairs
    end procedure

    module procedure input_output_pairs
      my_input_output_pairs = self%input_output_pairs_
    end procedure

end submodule mini_batch_s
