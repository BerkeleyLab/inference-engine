! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(tensor_m) tensor_s
  implicit none

contains

    module procedure construct_from_components
      tensor%values_ = values
    end procedure

    module procedure values
      tensor_values = self%values_
    end procedure

end submodule tensor_s
