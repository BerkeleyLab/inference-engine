submodule(step_m) step_s
  implicit none

contains

    module procedure activation
      y = merge(1., 0., x>0)
    end procedure

end submodule step_s
