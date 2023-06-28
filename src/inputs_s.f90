! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(inputs_m) inputs_s
  implicit none

contains

    module procedure construct_from_components
      inputs%values_ = values
    end procedure

    module procedure values
      inputs = self%values_
    end procedure

end submodule inputs_s
