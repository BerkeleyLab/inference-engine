! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(swish_m) swish_s
  implicit none

contains

    module procedure activation
      y =  x*sigmoid(x)
    end procedure

end submodule swish_s
