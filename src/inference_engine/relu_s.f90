! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(relu_m) relu_s
  use kind_parameters_m, only : rkind
  implicit none

contains

    module procedure activation
      y = max(0._rkind, x)
    end procedure

    module procedure function_name
      string = string_t("relu")
    end procedure

end submodule relu_s
