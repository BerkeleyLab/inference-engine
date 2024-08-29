module double_precision_string_m
  use julienne_m, only : string_t
  implicit none

  type, extends(string_t) :: double_precision_string_t
  end type

end module double_precision_string_m
