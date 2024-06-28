module metadata_m
  use julienne_string_m, only : string_t
  implicit none

  private
  public :: metadata_t

  type metadata_t
    private
    type(string_t) modelName_, modelAuthor_, compilationDate_, activationFunction_, usingSkipConnections_
  contains
    procedure :: to_json
    procedure :: equals
    generic :: operator(==) => equals
  end type

  interface metadata_t

    pure module function from_json(lines) result(metadata)
      implicit none
      type(string_t), intent(in) :: lines(:)
      type(metadata_t) metadata
    end function

    pure module function from_components(modelName, modelAuthor, compilationDate, activationFunction, usingSkipConnections) &
      result(metadata)
      implicit none
      type(string_t), intent(in) :: modelName, modelAuthor, compilationDate, activationFunction, usingSkipConnections
      type(metadata_t) metadata
    end function

  end interface

  interface

    pure module function to_json(self) result(lines)
      implicit none
      class(metadata_t), intent(in) :: self
      type(string_t), allocatable :: lines(:)
    end function

    elemental module function equals(lhs, rhs) result(lhs_equals_rhs)
      implicit none
      class(metadata_t), intent(in) :: lhs, rhs
      logical lhs_equals_rhs
    end function

  end interface

end module
