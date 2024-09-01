submodule(double_precision_file_m) double_precision_file_s
  implicit none

contains

  module procedure double_precision_lines
    lines = double_precision_string_t(self%file_t%lines())
  end procedure

end submodule double_precision_file_s
