module kind_parameters_m
  implicit none
  private
  public :: rkind

  integer, parameter :: rkind = kind(1.0D0)
end module kind_parameters_m
