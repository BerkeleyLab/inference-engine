submodule(sourcery_string_m) sourcery_string_s
  implicit none
contains
  module procedure get_json_real_array
    character(len=:), allocatable :: raw_line
    real, allocatable :: real_array(:)
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
  end procedure
end submodule
