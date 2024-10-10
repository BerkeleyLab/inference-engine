! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(mini_batch_m) mini_batch_s
  implicit none

contains

    module procedure default_real_construct
      mini_batch%input_output_pairs_ = input_output_pairs
    end procedure

    module procedure double_precision_construct
      mini_batch%input_output_pairs_ = input_output_pairs
    end procedure

    module procedure default_real_input_output_pairs
      my_input_output_pairs = self%input_output_pairs_
    end procedure

    module procedure double_precision_input_output_pairs
      my_input_output_pairs = self%input_output_pairs_
    end procedure

end submodule mini_batch_s
