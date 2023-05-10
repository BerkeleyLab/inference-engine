! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(outputs_m) outputs_s
  implicit none

contains

  module procedure outputs
    output_values = self%outputs_
  end procedure

  module procedure construct_from_compoents
    new_outputs_t%outputs_ = outputs
    new_outputs_t%pre_activation_in_ = pre_activation_in
    new_outputs_t%pre_activation_out_ = pre_activation_out
  end procedure

  module procedure pre_activation_out
    z_L = self%pre_activation_out_
  end procedure 

end submodule outputs_s
