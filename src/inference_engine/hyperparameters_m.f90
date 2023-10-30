module hyperparameters_m
  use sourcery_m, only : string_t, file_t
  implicit none

  private
  public :: hyperparameters_t

  type hyperparameters_t
    private
    integer :: mini_batches_ = 10
    real :: learning_rate_ = 1.5
    character(len=:), allocatable :: optimizer_
  contains
    procedure :: to_json
  end type

  interface hyperparameters_t

    pure module function from_json(file_) result(hyperparameters)
      implicit none
      type(file_t), intent(in) :: file_
      type(hyperparameters_t) hyperparameters
    end function

  end interface

  interface

    pure module function to_json(self) result(lines)
      implicit none
      class(hyperparameters_t), intent(in) :: self
      type(string_t), allocatable :: lines(:)
    end function

  end interface

end module
