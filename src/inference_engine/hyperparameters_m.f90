module hyperparameters_m
  use sourcery_string_m, only : string_t
  use kind_parameters_m, only : rkind
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
    procedure :: equals
    generic :: operator(==) => equals
    procedure :: mini_batches
    procedure :: optimizer_name
    procedure :: learning_rate
    end type

  interface hyperparameters_t

    pure module function from_json(lines) result(hyperparameters)
      implicit none
      type(string_t), intent(in) :: lines(:)
      type(hyperparameters_t) hyperparameters
    end function

    pure module function from_components(mini_batches, learning_rate, optimizer) result(hyperparameters)
      implicit none
      integer, intent(in) :: mini_batches
      real, intent(in) :: learning_rate
      character(len=*), intent(in) :: optimizer
      type(hyperparameters_t) hyperparameters
    end function

  end interface

  interface

    pure module function to_json(self) result(lines)
      implicit none
      class(hyperparameters_t), intent(in) :: self
      type(string_t), allocatable :: lines(:)
    end function

    elemental module function equals(lhs, rhs) result(lhs_equals_rhs)
      implicit none
      class(hyperparameters_t), intent(in) :: lhs, rhs
      logical lhs_equals_rhs
    end function

    elemental module function mini_batches(self) result(num_mini_batches)
      implicit none
      class(hyperparameters_t), intent(in) :: self
      integer num_mini_batches
    end function

    elemental module function optimizer_name(self) result(identifier)
      implicit none
      class(hyperparameters_t), intent(in) :: self
      type(string_t) identifier
     end function


     elemental module function learning_rate(self) result(rate)
       implicit none
       class(hyperparameters_t), intent(in) :: self
       real(rkind) rate
     end function
  end interface

end module
