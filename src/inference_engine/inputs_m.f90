! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module inputs_m
  use kind_parameters_m, only : rkind
  implicit none

  private
  public :: inputs_t

  type inputs_t
    private
    real(rkind), allocatable :: values_(:)
  contains
    procedure values
  end type

  interface inputs_t

    pure module function construct_from_components(values) result(inputs)
      implicit none
      real(rkind), intent(in) :: values(:)
      type(inputs_t) inputs
    end function

  end interface

  interface

    pure module function values(self) result(inputs)
      implicit none
      class(inputs_t), intent(in) :: self
      real(rkind), allocatable :: inputs(:)
    end function

  end interface

end module inputs_m
