! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(expected_outputs_m) expected_outputs_s
  implicit none

contains

    module procedure construct
      expected_outputs%outputs_ = outputs
    end procedure

    module procedure outputs
      my_outputs = self%outputs_
    end procedure

end submodule expected_outputs_s
