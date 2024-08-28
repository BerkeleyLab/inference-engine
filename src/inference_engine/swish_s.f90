! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(swish_m) swish_s
  use sigmoid_m, only : sigmoid_t
  implicit none

contains

    module procedure default_real_activation
      type(sigmoid_t) sigmoid
      y =  x*sigmoid%activation(x)
    end procedure

    module procedure default_real_activation_derivative
      type(sigmoid_t) sigmoid
      y =  sigmoid%activation(x) + x * sigmoid%activation_derivative(x)
    end procedure

    module procedure function_name
      string = string_t("swish")
    end procedure

end submodule swish_s
