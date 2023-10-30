module network_configuration_m
  use sourcery_m, only : file_t
  implicit none

  private
  public :: network_configuration_t
  
  type network_configuration_t
    private
    type(string_t) activation_function_
    integer, allocatable :: nodes_per_layer_(:)
    logical skip_connections_
  contains
    procedure :: to_json
  end type

  end type

  interface network_configuration_t

    elemental module function from_json(file_) result(network_configuration)
      implicit none
      type(file_t), intent(in) :: file_
      type(network_configuration_t) network_configuration
    end function

  end interface

  interface

    elemental module function to_json(self) result(json_file)
      implicit none
      class(network_configuration_t), intent(in) :: self
      type(file_t) json_file
    end function

  end interface

end module
