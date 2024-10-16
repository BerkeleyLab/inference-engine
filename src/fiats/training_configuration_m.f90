! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module training_configuration_m
  use activation_m, only : activation_t
  use julienne_string_m, only : string_t
  use julienne_file_m, only : file_t
  use hyperparameters_m, only : hyperparameters_t
  use network_configuration_m, only : network_configuration_t
  use kind_parameters_m, only  : default_real, double_precision
  use double_precision_file_m, only : double_precision_file_t
  implicit none

  private
  public :: training_configuration_t

  type, extends(double_precision_file_t) :: training_configuration_t(k)
    integer, kind :: k = default_real
    type(hyperparameters_t(k)),    private :: hyperparameters_
    type(network_configuration_t), private :: network_configuration_
  contains
    generic :: operator(==) => default_real_equals, double_precision_equals
    procedure, private ::      default_real_equals, double_precision_equals
    generic :: to_json => default_real_to_json, double_precision_to_json
    procedure, private :: default_real_to_json, double_precision_to_json
    generic :: mini_batches => default_real_mini_batches, double_precision_mini_batches
    procedure, private ::      default_real_mini_batches, double_precision_mini_batches
    generic :: optimizer_name => default_real_optimizer_name, double_precision_optimizer_name
    procedure, private ::        default_real_optimizer_name, double_precision_optimizer_name
    generic :: learning_rate => default_real_learning_rate, double_precision_learning_rate
    procedure, private ::       default_real_learning_rate, double_precision_learning_rate
    generic :: nodes_per_layer => default_real_nodes_per_layer, double_precision_nodes_per_layer
    procedure, private ::         default_real_nodes_per_layer, double_precision_nodes_per_layer
    generic :: skip_connections => default_real_skip_connections, double_precision_skip_connections
    procedure, private ::          default_real_skip_connections, double_precision_skip_connections
  end type

  interface training_configuration_t

    module function default_real_from_components(hyperparameters, network_configuration) result(training_configuration)
      implicit none
      type(hyperparameters_t), intent(in) :: hyperparameters
      type(network_configuration_t), intent(in) :: network_configuration
      type(training_configuration_t) training_configuration
    end function

    module function double_precision_from_components(hyperparameters, network_configuration) result(training_configuration)
      implicit none
      type(hyperparameters_t(double_precision)), intent(in) :: hyperparameters
      type(network_configuration_t), intent(in) :: network_configuration
      type(training_configuration_t(double_precision)) training_configuration
    end function

    module function default_real_from_file(file_object) result(training_configuration)
      implicit none
      type(file_t), intent(in) :: file_object
      type(training_configuration_t) training_configuration
    end function

    module function double_precision_from_file(file_object) result(training_configuration)
      implicit none
      type(double_precision_file_t), intent(in) :: file_object
      type(training_configuration_t(double_precision)) training_configuration
    end function

  end interface

  interface

    pure module function default_real_to_json(self) result(json_lines)
      implicit none
      class(training_configuration_t), intent(in) :: self
      type(string_t), allocatable :: json_lines(:)
    end function

    pure module function double_precision_to_json(self) result(json_lines)
      implicit none
      class(training_configuration_t(double_precision)), intent(in) :: self
      type(string_t), allocatable :: json_lines(:)
    end function

    elemental module function default_real_equals(lhs, rhs) result(lhs_eq_rhs)
      implicit none
      class(training_configuration_t), intent(in) :: lhs, rhs
      logical lhs_eq_rhs
    end function

    elemental module function double_precision_equals(lhs, rhs) result(lhs_eq_rhs)
      implicit none
      class(training_configuration_t(double_precision)), intent(in) :: lhs, rhs
      logical lhs_eq_rhs
    end function

    elemental module function default_real_mini_batches(self) result(num_mini_batches)
      implicit none
      class(training_configuration_t), intent(in) :: self
      integer num_mini_batches
    end function

    elemental module function double_precision_mini_batches(self) result(num_mini_batches)
      implicit none
      class(training_configuration_t(double_precision)), intent(in) :: self
      integer num_mini_batches
    end function

    elemental module function default_real_optimizer_name(self) result(identifier)
      implicit none
      class(training_configuration_t), intent(in) :: self
      type(string_t) identifier
    end function

    elemental module function double_precision_optimizer_name(self) result(identifier)
      implicit none
      class(training_configuration_t(double_precision)), intent(in) :: self
      type(string_t) identifier
    end function

    elemental module function default_real_learning_rate(self) result(rate)
      implicit none
      class(training_configuration_t), intent(in) :: self
      real rate
    end function
 
    elemental module function double_precision_learning_rate(self) result(rate)
      implicit none
      class(training_configuration_t(double_precision)), intent(in) :: self
      double precision rate
    end function

    pure module function default_real_nodes_per_layer(self) result(nodes)
      implicit none
      class(training_configuration_t), intent(in) :: self
      integer, allocatable :: nodes(:)
    end function

    pure module function double_precision_nodes_per_layer(self) result(nodes)
      implicit none
      class(training_configuration_t(double_precision)), intent(in) :: self
      integer, allocatable :: nodes(:)
    end function
 
    elemental module function default_real_skip_connections(self) result(using_skip)
      implicit none
      class(training_configuration_t), intent(in) :: self
      logical using_skip
    end function

    elemental module function double_precision_skip_connections(self) result(using_skip)
      implicit none
      class(training_configuration_t(double_precision)), intent(in) :: self
      logical using_skip
    end function
 
  end interface

end module
