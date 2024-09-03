! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(step_m) step_s
  implicit none

contains

    module procedure default_real_activation
      y = merge(1., 0., x>0.)
    end procedure

    module procedure double_precision_activation
      y = merge(1.D0, 0.D0, x>0.D0)
    end procedure

    module procedure function_name
      string = string_t("step")
    end procedure

end submodule step_s
