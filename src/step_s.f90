! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(step_m) step_s
  use kind_parameters_m, only : rkind
  implicit none

contains

    module procedure activation
      y = merge(1._rkind, 0._rkind, x>0._rkind)
    end procedure

    module procedure function_name
      string = string_t("step")
    end procedure

end submodule step_s
