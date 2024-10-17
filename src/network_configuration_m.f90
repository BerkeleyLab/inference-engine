! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module network_configuration_m
  use julienne_string_m, only : string_t
  use double_precision_string_m, only : double_precision_string_t
  implicit none

  private
  public :: network_configuration_t

  type network_configuration_t
    private
    logical :: skip_connections_ = .false.
    integer, allocatable :: nodes_per_layer_(:)
    character(len=:), allocatable :: activation_name_
  contains
    procedure :: to_json
    procedure :: equals
    generic :: operator(==) => equals
    procedure :: activation_name
    procedure :: nodes_per_layer
    procedure :: skip_connections
  end type

  interface network_configuration_t

    pure module function from_json(lines) result(network_configuration)
      implicit none
      type(string_t), intent(in) :: lines(:)
      type(network_configuration_t) network_configuration
    end function

    pure module function from_double_precision_string_json(lines) result(network_configuration)
      implicit none
      type(double_precision_string_t), intent(in) :: lines(:)
      type(network_configuration_t) network_configuration
    end function

    pure module function from_components(skip_connections, nodes_per_layer, activation_name) result(network_configuration)
      implicit none
      logical, intent(in) :: skip_connections
      integer, intent(in) :: nodes_per_layer(:)
      character(len=*), intent(in) :: activation_name
      type(network_configuration_t) network_configuration
    end function

  end interface

  interface

    pure module function to_json(self) result(lines)
      implicit none
      class(network_configuration_t), intent(in) :: self
      type(string_t), allocatable :: lines(:)
    end function

    elemental module function equals(lhs, rhs) result(lhs_equals_rhs)
      implicit none
      class(network_configuration_t), intent(in) :: lhs, rhs
      logical lhs_equals_rhs
    end function

    elemental module function activation_name(self) result(string)
      implicit none
      class(network_configuration_t), intent(in) :: self
      type(string_t) string
    end function

    pure module function nodes_per_layer(self) result(nodes)
      implicit none
      class(network_configuration_t), intent(in) :: self
      integer, allocatable :: nodes(:)
    end function

    elemental module function skip_connections(self) result(using_skip)
      implicit none
      class(network_configuration_t), intent(in) :: self
      logical using_skip
    end function


  end interface

end module
