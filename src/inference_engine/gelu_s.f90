! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(gelu_m) gelu_s
  use kind_parameters_m, only : rkind
  implicit none

  real(rkind), parameter :: pi = 3.141592653589793_rkind
  real(rkind), parameter :: half = 0.5_rkind, one = 1._rkind, two=2._rkind
  real(rkind), parameter :: sqrt_2_pi = sqrt(two*pi), sqrt_2 = sqrt(two), two_over_sqrt_pi = 2._rkind/sqrt(pi)

contains

    module procedure activation
      y = half*x*(one  + erf(x/sqrt_2))
    end procedure

    module procedure activation_derivative
      y = half*(one + erf(x/sqrt_2)) + exp(-x**2)/sqrt_2_pi
    end procedure

    module procedure function_name
      string = string_t("gelu")
    end procedure

end submodule gelu_s
