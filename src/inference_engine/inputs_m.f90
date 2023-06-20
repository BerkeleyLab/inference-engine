! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module inputs_m
  use kind_parameters_m, only : rkind
  implicit none

  private
  public :: inputs_t

  type inputs_t
    private
    real(rkind), allocatable :: inputs_(:)
  contains
    procedure inputs
  end type

  interface inputs_t

    pure module function construct_from_components(inputs) result(inputs_object)
      implicit none
      real(rkind), intent(in) :: inputs(:)
      type(inputs_t) inputs_object
    end function

  end interface

  interface

    pure module function inputs(self) result(my_inputs)
      implicit none
      class(inputs_t), intent(in) :: self
      real(rkind), allocatable :: my_inputs(:)
    end function

  end interface

end module inputs_m
