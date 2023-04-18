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
  end procedure

end submodule outputs_s
