! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module kind_parameters_m
  implicit none
  private
  public :: default_real
  public :: double_precision

  integer, parameter :: default_real = kind(1.)
  integer, parameter :: double_precision = kind(1D0)
end module kind_parameters_m
