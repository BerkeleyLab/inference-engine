! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module double_precision_file_m
  use julienne_m, only : file_t, string_t
  use double_precision_string_m, only : double_precision_string_t

  implicit none

  type, extends(file_t) :: double_precision_file_t
  contains
    procedure double_precision_lines
  end type

  interface double_precision_file_t

    impure elemental module function construct_from_string(file_name) result(double_precision_file)
      implicit none
      type(string_t), intent(in) :: file_name
      type(double_precision_file_t) double_precision_file
    end function

    impure elemental module function construct_from_character(file_name) result(double_precision_file)
      implicit none
      character(len=*), intent(in) :: file_name
      type(double_precision_file_t) double_precision_file
    end function

  end interface

  interface

    pure module function double_precision_lines(self) result(lines)
      implicit none
      class(double_precision_file_t), intent(in) :: self
      type(double_precision_string_t), allocatable :: lines(:)
    end function

  end interface

end module double_precision_file_m
