module sourcery_string_m
  implicit none
  
  type string_t
    character(len=:), allocatable :: string_
  contains
    generic :: operator(//)   => string_t_cat_string_t, string_t_cat_character, character_cat_string_t
    generic :: assignment(= ) => assign_string_t_to_character, assign_character_to_string_t
    generic :: get_json_value =>     get_json_integer_array, get_json_logical, get_json_integer, get_json_string, get_json_real, &
                                     get_json_real_array
    procedure, private            :: get_json_integer_array, get_json_logical, get_json_integer, get_json_string, get_json_real, &
                                     get_json_real_array
    procedure, private            :: assign_character_to_string_t
    procedure, private            :: string_t_cat_string_t, string_t_cat_character
    procedure, private, pass(rhs) :: character_cat_string_t
    procedure, private, pass(rhs) :: assign_string_t_to_character
  end type

  interface operator(.cat.)

   pure  module function concatenate_elements(strings) result(concatenated_strings)
      implicit none
      type(string_t), intent(in) :: strings(:)
      type(string_t) concatenated_strings
    end function

  end interface

  interface
    pure module function array_of_strings(delimited_strings, delimiter) result(strings_array)
      implicit none
      character(len=*), intent(in) :: delimited_strings, delimiter
      type(string_t), allocatable :: strings_array(:)
    end function

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

    pure module function string_t_cat_string_t(lhs, rhs) result(lhs_cat_rhs)
      implicit none
      class(string_t), intent(in) :: lhs, rhs
      type(string_t) lhs_cat_rhs
    end function

    pure module function string_t_cat_character(lhs, rhs) result(lhs_cat_rhs)
      implicit none
      class(string_t), intent(in) :: lhs
      character(len=*), intent(in) :: rhs
      type(string_t) lhs_cat_rhs
    end function

    pure module function character_cat_string_t(lhs, rhs) result(lhs_cat_rhs)
      implicit none
      character(len=*), intent(in) :: lhs
      class(string_t), intent(in) :: rhs
      type(string_t) lhs_cat_rhs
    end function

    pure module subroutine assign_character_to_string_t(lhs, rhs)
      implicit none
      class(string_t), intent(inout) :: lhs
      character(len=*), intent(in) :: rhs
    end subroutine

    pure module subroutine assign_string_t_to_character(lhs, rhs)
      implicit none
      class(string_t), intent(in) :: rhs
      character(len=:), intent(out), allocatable :: lhs
    end subroutine

  end interface
  
end module sourcery_string_m
