module string_m
  implicit none

  type string_t
    character(len=:), allocatable :: string
  contains
    generic :: generic_value => real_value
    procedure real_value
  end type

contains

  function real_value(self)
    class(string_t), intent(in) :: self
    real real_value
    real_value = 0.
  end function

end module
