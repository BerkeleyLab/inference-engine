! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(tensor_m) tensor_s
  implicit none

contains

  module procedure construct_default_real
    tensor%values_ = values
  end procedure

  module procedure construct_double_precision
    tensor%values_ = values
  end procedure

  module procedure default_real_values
    tensor_values = self%values_
  end procedure

  module procedure double_precision_values
    tensor_values = self%values_
  end procedure

  module procedure default_real_num_components
    n = size(self%values_)
  end procedure

  module procedure double_precision_num_components
    n = size(self%values_)
  end procedure

end submodule tensor_s
