! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module kind_parameters_m
  implicit none
  private
  public :: rkind

  integer, parameter :: rkind = kind(1.0)
end module kind_parameters_m
