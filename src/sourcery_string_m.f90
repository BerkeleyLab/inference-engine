module sourcery_string_m
  implicit none
  
  type string_t
    character(len=:), allocatable :: string_
  contains
    generic :: get_json_value =>     get_json_integer_array, get_json_logical, get_json_integer, get_json_string, get_json_real, &
                                     get_json_real_array
    procedure, private            :: get_json_integer_array, get_json_logical, get_json_integer, get_json_string, get_json_real, &
                                     get_json_real_array
  end type

  interface
    pure module function get_json_real(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      real, intent(in) :: mold
      real value_
    end function

    elemental module function get_json_string(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key, mold
      type(string_t) :: value_
    end function

    pure module function get_json_integer(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      integer, intent(in) ::  mold
      integer value_
    end function

    elemental module function get_json_logical(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      logical, intent(in) :: mold
      logical value_
    end function

    pure module function get_json_integer_array(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      integer, intent(in) :: mold(:)
      integer, allocatable :: value_(:)
    end function

    pure module function get_json_real_array(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      real, intent(in) :: mold(:)
      real, allocatable :: value_(:)
    end function
  end interface
end module sourcery_string_m
