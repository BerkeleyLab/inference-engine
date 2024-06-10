module string_m
  implicit none

  type string_t
    character(len=:), allocatable :: string_
  end type
end module
