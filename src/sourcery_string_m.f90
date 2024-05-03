module sourcery_string_m
  implicit none
  
  type string_t
    character(len=:), allocatable :: string_
  contains
    generic :: get_json_value => get_json_real_array
    procedure get_json_real_array 
  end type

  interface
    pure module function get_json_real_array(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      real, intent(in) :: mold(:)
      real, allocatable :: value_(:)
    end function
  end interface

end module sourcery_string_m
