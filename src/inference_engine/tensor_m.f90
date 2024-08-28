! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module tensor_m
  use kind_parameters_m, only : default_real
  implicit none

  private
  public :: tensor_t

  type tensor_t(k)
    integer, kind :: k = default_real 
    real, allocatable, private :: values_(:)
  contains
    procedure values
    procedure num_components
  end type

  interface tensor_t

    pure module function construct_from_components(values) result(tensor)
      implicit none
      real, intent(in) :: values(:)
      type(tensor_t) tensor
    end function

  end interface

  interface

    pure module function values(self) result(tensor_values)
      implicit none
      class(tensor_t), intent(in) :: self
      real, allocatable :: tensor_values(:)
    end function

    pure module function num_components(self) result(n)
      implicit none
      class(tensor_t), intent(in) :: self
      integer n
    end function

  end interface

end module tensor_m
