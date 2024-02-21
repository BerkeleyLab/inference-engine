module training_configuration_m
  use string_m, only : string_t
  use hyperparameters_m, only : hyperparameters_t
  use kind_parameters_m, only  : rkind
  implicit none

  private
  public :: training_configuration_t

  type :: training_configuration_t
    private
    type(hyperparameters_t) hyperparameters_
  contains
    procedure :: equals
    generic :: operator(==) => equals
    procedure :: mini_batches
    procedure :: learning_rate
  end type

  interface training_configuration_t

    module function from_components(hyperparameters) result(training_configuration)
      implicit none
      type(hyperparameters_t), intent(in) :: hyperparameters
      type(training_configuration_t) training_configuration
    end function

  end interface

  interface

    elemental module function equals(lhs, rhs) result(lhs_eq_rhs)
      implicit none
      class(training_configuration_t), intent(in) :: lhs, rhs
      logical lhs_eq_rhs
    end function

    elemental module function mini_batches(self) result(num_mini_batches)
      implicit none
      class(training_configuration_t), intent(in) :: self
      integer num_mini_batches
    end function

    elemental module function learning_rate(self) result(rate)
      implicit none
      class(training_configuration_t), intent(in) :: self
      real(rkind) rate
    end function
 
  end interface

end module
