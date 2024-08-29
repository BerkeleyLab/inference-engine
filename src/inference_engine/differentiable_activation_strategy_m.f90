! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module differentiable_activation_strategy_m
  use activation_strategy_m, only : activation_strategy_t, default_real_activation_i, double_precision_activation_i
  implicit none

  private
  public :: differentiable_activation_strategy_t

  type, extends(activation_strategy_t), abstract :: differentiable_activation_strategy_t
  contains
     procedure(default_real_activation_i), nopass, deferred :: default_real_activation_derivative
     procedure(double_precision_activation_i), nopass, deferred :: double_precision_activation_derivative
     generic :: activation_derivative => default_real_activation_derivative, double_precision_activation_derivative
  end type

end module differentiable_activation_strategy_m
