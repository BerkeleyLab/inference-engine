module string_m
  implicit none
  
  private
  public :: string_t

  type string_t
    private
    character(len=:), allocatable :: string_
  contains
    procedure :: string
  end type

  interface string_t

    elemental module function construct(string) result(new_string)
      implicit none
      character(len=*), intent(in) :: string
      type(string_t) new_string
    end function

  end interface

  interface

    pure module function string(self) result(raw_string)
      implicit none
      class(string_t), intent(in) :: self
      character(len=:), allocatable :: raw_string
    end function
   
  end interface
  
end module string_m
