module training_configuration_m
  use string_m, only : string_t
  use hyperparameters_m, only : hyperparameters_t
  use network_configuration_m, only : network_configuration_t
  use kind_parameters_m, only  : rkind
  implicit none

  private
  public :: training_configuration_t

  type :: training_configuration_t
    private
    type(hyperparameters_t) hyperparameters_
    type(network_configuration_t) network_configuration_
  contains
    procedure :: equals
    generic :: operator(==) => equals
    procedure :: mini_batches
    procedure :: optimizer_name
    procedure :: learning_rate
    procedure :: nodes_per_layer
    procedure :: skip_connections
  end type

  interface training_configuration_t

    module function from_components(hyperparameters, network_configuration) result(training_configuration)
      implicit none
      type(hyperparameters_t), intent(in) :: hyperparameters
      type(network_configuration_t), intent(in) :: network_configuration
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

    elemental module function optimizer_name(self) result(identifier)
      implicit none
      class(training_configuration_t), intent(in) :: self
      type(string_t) identifier
    end function

    elemental module function learning_rate(self) result(rate)
      implicit none
      class(training_configuration_t), intent(in) :: self
      real(rkind) rate
    end function
 
    pure module function nodes_per_layer(self) result(nodes)
      implicit none
      class(training_configuration_t), intent(in) :: self
      integer, allocatable :: nodes(:)
    end function
 
    elemental module function skip_connections(self) result(using_skip)
      implicit none
      class(training_configuration_t), intent(in) :: self
      logical using_skip
    end function
 
  end interface

end module