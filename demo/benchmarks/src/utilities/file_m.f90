module file_m
  use sourcery_string_m, only : string_t
  implicit none

  type file_t
    type(string_t), allocatable :: line_(:)
  end type
end module
