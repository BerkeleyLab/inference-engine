module file_m
  !! A representation of a file as an object
  use string_m, only : string_t

  private
  public :: file_t

  type file_t
    private
    type(string_t), allocatable :: lines_(:)
  contains
    procedure :: lines
  end type

  interface file_t

    module function read_lines(file_unit) result(file_object)
      implicit none
      integer, intent(in) :: file_unit
      type(file_t) file_object
    end function

  end interface

  interface

    pure module function lines(self)  result(my_lines)
      implicit none
      class(file_t), intent(in) :: self
      type(string_t), allocatable :: my_lines(:)
    end function

  end interface

end module file_m
