module string_m
  implicit none

  type string_t
    character(len=:), allocatable :: string_
  contains
    procedure :: string
  end type

contains

  pure function string(self)
    class(string_t), intent(in) :: self
    character(len=:), allocatable :: string
    string = self%string_
  end function

end module
