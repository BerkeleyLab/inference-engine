module hyperparameters_m
  use sourcery_m, only : file_t
  implicit none

  private
  public :: hyperparameters_t
  public :: initialization_parameters_t 
  public :: initialization_t

  type initialization_parameters_t 
    real spread_
  end type

  type initialization_t
    character(len=:), allocatable :: initialization_type_
    type(initialization_parameters_t) ::  initialization_parameters_
  end type
 
  type hyperparameters_t
    private
    character(len=:), allocatable :: activation_
    integer  mini_batch_size_
    integer, allocatable :: nodes_per_layer_(:) 
    type(initialization_t) initialization_
  contains
    procedure :: to_json
  end type

  interface hyperparameters_t

    pure module function construct_from_json_file(file_) result(hyperparameters)
      implicit none
      type(file_t), intent(in) :: file_
      type(hyperparameters_t) hyperparameters
    end function

  end interface

  interface

    impure elemental module function to_json(self) result(json_file)
      implicit none
      class(hyperparameters_t), intent(in) :: self
      type(file_t) json_file
    end function

  end interface

end module
