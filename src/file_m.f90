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
    procedure :: write_lines
  end type

  interface file_t

    impure elemental module function read_lines(file_name) result(file_object)
      implicit none
      type(string_t), intent(in) :: file_name
      type(file_t) file_object
    end function

    pure module function construct(lines) result(file_object)
      implicit none
      type(string_t), intent(in), allocatable :: lines(:)
      type(file_t) file_object
    end function

  end interface

  interface

    pure module function lines(self)  result(my_lines)
      implicit none
      class(file_t), intent(in) :: self
      type(string_t), allocatable :: my_lines(:)
    end function

    impure elemental module subroutine write_lines(self, file_name)
      implicit none
      class(file_t), intent(in) :: self
      type(string_t), intent(in), optional :: file_name
    end subroutine

  end interface

end module file_m
