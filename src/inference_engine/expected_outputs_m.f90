! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module expected_outputs_m
  use kind_parameters_m, only : rkind
  implicit none

  private
  public :: expected_outputs_t

  type expected_outputs_t
    private
    real(rkind), allocatable :: outputs_(:)
  contains
    procedure :: outputs
  end type

  interface expected_outputs_t

    module function construct(outputs) result(expected_outputs)
      implicit none
      real(rkind), intent(in) :: outputs(:)
      type(expected_outputs_t) expected_outputs
    end function

  end interface

  interface

    pure module function outputs(self) result(my_outputs)
      implicit none
      class(expected_outputs_t), intent(in) :: self
      real(rkind), allocatable :: my_outputs(:)
    end function

  end interface

end module expected_outputs_m
