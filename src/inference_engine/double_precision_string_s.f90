submodule(double_precision_string_m)double_precision_string_s
  implicit none
contains

    module procedure construct_from_string
      double_precision_string%string_t = string
    end procedure

end submodule double_precision_string_s
