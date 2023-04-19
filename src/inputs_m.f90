! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module inputs_m
  use kind_parameters_m, only : rkind
  implicit none

  private
  public :: inputs_t

  type inputs_t
    real(rkind), allocatable :: inputs_(:)
  end type

end module inputs_m
