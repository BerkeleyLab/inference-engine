! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(gelu_m) gelu_s
  implicit none

  real, parameter :: pi = 3.141592653589793
  real, parameter :: half = 0.5, one = 1., two=2.
  real, parameter :: sqrt_2_pi = sqrt(two*pi), sqrt_2 = sqrt(two)

contains

    module procedure default_real_activation
      y = half*x*(1. + erf(x/sqrt_2))
    end procedure

    module procedure default_real_activation_derivative
      y = half*(1. + erf(x/sqrt_2)) + x*exp(-x**2/two)/sqrt_2_pi
    end procedure

    module procedure function_name
      string = string_t("gelu")
    end procedure

end submodule gelu_s
