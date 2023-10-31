module network_configuration_m
  use sourcery_m, only : string_t, file_t
  implicit none

  private
  public :: network_configuration_t

  type network_configuration_t
    private
    logical :: skip_connections_ = .false.
    integer, allocatable :: nodes_per_layer_(:)
    character(len=:), allocatable :: activation_function_
  contains
    procedure :: to_json
    procedure :: equals
    generic :: operator(==) => equals
  end type

  interface network_configuration_t

    pure module function from_json(lines) result(network_configuration)
      implicit none
      type(string_t), intent(in) :: lines(:)
      type(network_configuration_t) network_configuration
    end function

    pure module function from_components(skip_connections, nodes_per_layer, activation_function) result(network_configuration)
      implicit none
      logical, intent(in) :: skip_connections
      integer, intent(in) :: nodes_per_layer(:)
      character(len=*), intent(in) :: activation_function
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

  end interface

end module
