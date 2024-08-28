! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(relu_m) relu_s
  implicit none

contains

    module procedure default_real_activation
      y = max(0., x)
    end procedure

    module procedure default_real_activation_derivative
      y = merge(1., 0., x>0.)
    end procedure

    module procedure function_name
      string = string_t("relu")
    end procedure

end submodule relu_s
