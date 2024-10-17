! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(double_precision_file_m) double_precision_file_s
  implicit none

contains

  module procedure construct_from_string
    double_precision_file%file_t = file_t(file_name)
  end procedure

  module procedure construct_from_character
    double_precision_file%file_t = file_t(file_name)
  end procedure

  module procedure double_precision_lines
    lines = double_precision_string_t(self%file_t%lines())
  end procedure

end submodule double_precision_file_s
