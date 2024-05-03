module characterizable_m
  !! Define an abstract class that supports object representation in character form
  implicit none

  private
  public :: characterizable_t

  type, abstract :: characterizable_t
  contains
    procedure(as_character_i), deferred :: as_character
  end type

  abstract interface
    
    pure function as_character_i(self) result(character_self)
      import characterizable_t
      implicit none
      class(characterizable_t), intent(in) :: self
      character(len=:), allocatable :: character_self
    end function
    
  end interface

end module characterizable_m
