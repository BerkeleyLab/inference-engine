module string_m
  implicit none
  type string_t
    character(len=:), allocatable :: string_
  contains
    generic :: array => real_array
    procedure real_array
  end type
contains
  function real_array(self)
    class(string_t), intent(in) :: self
    real real_array(1)
    real_array = 0.
  end function
end module
