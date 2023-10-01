! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
!
! MIT License
! 
! Copyright (c) 2017 National Center for Atmospheric Research
! 
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
! 
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
! 
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
module saturated_mixing_ratio_m
  !! This module supports the program in the file example/learn-saturated-mixing-ratio.f90.
  !! The saturated_mixing_ratio function in this module resulted from refactoring the sat_mr function
  !! in the Intermediate Complexity Atmospheric Research (ICAR) model file src/physics/mp_simple.f90.
  !! ICAR is distributed under the above MIT license.  See https://github.com/ncar/icar.
  use inference_engine_m, only : tensor_t
  use assert_m, only : assert
  implicit none
 
  private
  public :: T, p, y

  real, parameter :: freezing_threshold = 273.15       ! [K]
  real, parameter :: T_min = 236.352524, T_max = 307.610779
  real, parameter :: p_min = 29671.1348, p_max = 98596.7578
  integer, parameter :: resolution = 10
  integer i
  real, parameter :: T(*) = [(real(i)/real(resolution), i=0,resolution)]
  real, parameter :: p(*) = [(real(i)/real(resolution), i=0,resolution)]

contains

  pure function saturated_mixing_ratio(T_normalized, p_normalized) result(sat_mr)
    !! Calculate the saturated mixing ratio for normalized tempetatures (k) and pressures (Pa)
    real,intent(in) :: T_normalized, p_normalized
    real sat_mr

    associate( &
     temperature => T_min + (T_max - T_min)*T_normalized, &
     pressure => p_min + (p_max - p_min)*p_normalized &
    )
      associate(below_freezing => temperature < freezing_threshold)
        associate( &
          a => merge(21.8745584, 17.2693882, below_freezing), &
          b => merge(7.66, 35.86, below_freezing) &
        )
          associate(p_threshold => 610.78 * exp(a * (temperature - 273.16) / (temperature - b))) !(Pa))
            associate(e_s => merge(pressure * 0.99999, p_threshold, (pressure - p_threshold) <= 0))
              sat_mr = 0.6219907 * e_s / (pressure - e_s) !(kg/kg)
            end associate
          end associate
        end associate
      end associate
    end associate
  end function

  elemental function y(x_in) result(a)
    type(tensor_t), intent(in) :: x_in
    type(tensor_t) a
    associate(x => x_in%values())
      call assert(lbound(x,1)==1 .and. ubound(x,1)==2,"y(x) :: sufficient input")
      a = tensor_t([saturated_mixing_ratio(x(1),x(2))])
    end associate
  end function
 
end module
