! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module thompson_tensors_m
  !! This module supports the program in the file example/learn-microphysics-procedures.f90.
  use module_mp_thompson, only : rslf, rsif
  use inference_engine_m, only : tensor_t
  use assert_m, only : assert
  implicit none
 
  private
  public :: T, p, y

  real, parameter :: T_min = 236.352524, T_max = 307.610779
  real, parameter :: p_min = 29671.1348, p_max = 98596.7578
  integer, parameter :: resolution = 10
  integer i
  real, parameter :: T(*) = [(real(i)/real(resolution), i=0,resolution)]
  real, parameter :: p(*) = [(real(i)/real(resolution), i=0,resolution)]

contains

  elemental impure function y(x_in) result(a)
    type(tensor_t), intent(in) :: x_in
    type(tensor_t) a
    associate(x => x_in%values())
      call assert(lbound(x,1)==1 .and. ubound(x,1)==2,"y(x) :: sufficient input")
      associate(temperature => T_min + (T_max - T_min)*x(1), pressure => p_min + (p_max - p_min)*x(2) )
        a = tensor_t([rslf(pressure, temperature), rsif(pressure, temperature)])
      end associate
    end associate
  end function
 
end module
