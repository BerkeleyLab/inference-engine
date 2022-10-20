submodule(string_m) string_s
  implicit none
  
contains

    module procedure construct
      new_string%string_ = string
    end procedure

    module procedure string
      raw_string = self%string_
    end procedure

  module procedure array_of_strings
    type(string_t), allocatable :: strings(:)
    character(len=:), allocatable :: remainder, next_string
    integer next_delimiter, string_end

    remainder = trim(adjustl(delimited_strings))
    allocate(strings(0))

    do  
      next_delimiter = index(remainder, delimiter)
      string_end = merge(next_delimiter-1, len(remainder), next_delimiter/=0)
      if (string_end==len(remainder)) then
        next_string = trim(adjustl(remainder(:string_end)))
        remainder = ""
      else
        next_string = trim(adjustl(remainder(:string_end)))
        remainder = trim(adjustl(remainder(next_delimiter+1:)))
      end if
      if (len(next_string)==0) exit
      strings = [strings, string_t(next_string)]
    end do

  end function
                 

end submodule string_s
