module sourcery_string_m
  implicit none
  type string_t
    character(len=:), allocatable :: string_
  contains
    generic :: get_json_value => get_json_real_array
    procedure get_json_real_array 
  end type
contains
  pure function get_json_real_array(self, key, mold) result(value_)
    class(string_t), intent(in) :: self, key
    real, intent(in) :: mold(:)
    real, allocatable :: value_(:), real_array(:)
    character(len=:), allocatable :: raw_line
    integer i
    raw_line = self%string_
    associate(colon => index(raw_line, ":"))
      associate(opening_bracket => colon + index(raw_line(colon+1:), "["))
        associate(closing_bracket => opening_bracket + index(raw_line(opening_bracket+1:), "]"))
          associate(commas => count("," == [(raw_line(i:i), i=opening_bracket+1,closing_bracket-1)]))
            associate(num_inputs => commas + 1)
              allocate(real_array(num_inputs))
              read(raw_line(opening_bracket+1:closing_bracket-1), fmt=*) real_array
              value_ = real_array
            end associate
          end associate
        end associate
      end associate
    end associate
  end function
end module sourcery_string_m
