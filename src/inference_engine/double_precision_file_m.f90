module double_precision_file_m
  use julienne_m, only : file_t
  use double_precision_string_m, only : double_precision_string_t

  implicit none

  type, extends(file_t) :: double_precision_file_t
  contains
    procedure double_precision_lines
  end type

  interface

    module function double_precision_lines(self) result(lines)
      implicit none
      class(double_precision_file_t), intent(in) :: self
      type(double_precision_string_t), allocatable :: lines(:)
    end function

  end interface

end module double_precision_file_m
