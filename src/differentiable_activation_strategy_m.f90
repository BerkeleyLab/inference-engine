! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module differentiable_activation_strategy_m
  use activation_strategy_m, only : activation_strategy_t, activation_i
  implicit none

  private
  public :: differentiable_activation_strategy_t

  type, extends(activation_strategy_t), abstract :: differentiable_activation_strategy_t
  contains
     procedure(activation_i), nopass, deferred :: activation_derivative
  end type

end module differentiable_activation_strategy_m
