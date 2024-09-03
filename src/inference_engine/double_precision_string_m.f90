module double_precision_string_m
  use julienne_m, only : string_t
  implicit none

  type, extends(string_t) :: double_precision_string_t
  end type

  interface double_precision_string_t

    elemental module function construct_from_string(string) result(double_precision_string)
      implicit none
      type(string_t), intent(in) :: string
      type(double_precision_string_t) double_precision_string
    end function

  end interface

end module double_precision_string_m
