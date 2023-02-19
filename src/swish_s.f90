! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(swish_m) swish_s
  use sigmoid_m, only : sigmoid_t
  implicit none

contains

    module procedure activation
      type(sigmoid_t) sigmoid
      y =  x*sigmoid%activation(x)
    end procedure

end submodule swish_s
