! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(gelu_m) gelu_s
  implicit none

  real, parameter :: pi = 3.141592653589793
  real, parameter :: half = 0.5, two = 2.D0
  real, parameter :: sqrt_2_pi = sqrt(2*pi), sqrt_2 = sqrt(2.)

  double precision, parameter :: pi_dp = 3.141592653589793D0
  double precision, parameter :: half_dp = 0.5D0, two_dp = 2.D0
  double precision, parameter :: sqrt_2_pi_dp = sqrt(two_dp*pi_dp), sqrt_2_dp = sqrt(2.D0)

contains

    module procedure default_real_activation
      y = half*x*(1. + erf(x/sqrt_2))
    end procedure

    module procedure double_precision_activation
      y = half_dp*x*(1.D0 + erf(x/sqrt_2_dp))
    end procedure

    module procedure default_real_activation_derivative
      y = half*(1. + erf(x/sqrt_2)) + x*exp(-x**2/two)/sqrt_2_pi
    end procedure

    module procedure double_precision_activation_derivative
      y = half_dp*(1.D0 + erf(x/sqrt_2_dp)) + x*exp(-x**2/2.D0)/sqrt_2_pi_dp
    end procedure

    module procedure function_name
      string = string_t("gelu")
    end procedure

end submodule gelu_s
