submodule(string_m) string_s
  implicit none
  
contains

    module procedure construct
      new_string%string_ = string
    end procedure

    module procedure string
      raw_string = self%string_
    end procedure

end submodule string_s
