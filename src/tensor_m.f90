! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module tensor_m
  use kind_parameters_m, only : rkind
  implicit none

  type tensor_t
    real(rkind), allocatable :: values_(:)
  contains
    procedure values
    procedure num_components
  end type

  interface

    pure module function values(self) result(tensor_values)
      implicit none
      class(tensor_t), intent(in) :: self
      real(rkind), allocatable :: tensor_values(:)
    end function

    pure module function num_components(self) result(n)
      implicit none
      class(tensor_t), intent(in) :: self
      integer n
    end function

  end interface

end module tensor_m
