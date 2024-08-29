! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(sigmoid_m) sigmoid_s
  implicit none

contains

    module procedure default_real_activation
      y =  1./(1.+exp(-x))
    end procedure

    module procedure double_precision_activation
      y =  1./(1.+exp(-x))
    end procedure

    module procedure default_real_activation_derivative
      y =  exp(-x)/(1.+exp(-x))**2
    end procedure

    module procedure double_precision_activation_derivative
      y =  exp(-x)/(1.+exp(-x))**2
    end procedure

    module procedure function_name
      string = string_t("sigmoid")
    end procedure

end submodule sigmoid_s
