! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module hyperparameters_m
  use julienne_string_m, only : string_t
  use kind_parameters_m, only : default_real, double_precision
  use double_precision_string_m, only : double_precision_string_t
  implicit none
  private
  public :: hyperparameters_t

  type hyperparameters_t(k)
    integer, kind :: k = default_real
    integer, private:: mini_batches_ = 10
    real(k), private :: learning_rate_ = real(1.5,k)
    character(len=:), allocatable :: optimizer_
  contains
    generic :: to_json => default_real_to_json, double_precision_to_json
    procedure, private :: default_real_to_json, double_precision_to_json
    generic :: operator(==) => default_real_equals, double_precision_equals
    procedure, private ::      default_real_equals, double_precision_equals
    generic :: mini_batches => default_real_mini_batches, double_precision_mini_batches
    procedure, private ::      default_real_mini_batches, double_precision_mini_batches
    generic :: optimizer_name => default_real_optimizer_name, double_precision_optimizer_name
    procedure, private ::        default_real_optimizer_name, double_precision_optimizer_name
    generic :: learning_rate => default_real_learning_rate, double_precision_learning_rate
    procedure, private ::       default_real_learning_rate, double_precision_learning_rate
  end type

  interface hyperparameters_t

    pure module function default_real_from_json(lines) result(hyperparameters)
      implicit none
      type(string_t), intent(in) :: lines(:)
      type(hyperparameters_t) hyperparameters
    end function

    pure module function double_precision_from_json(lines) result(hyperparameters)
      implicit none
      type(double_precision_string_t), intent(in) :: lines(:)
      type(hyperparameters_t(double_precision)) hyperparameters
    end function

    pure module function default_real_from_components(mini_batches, learning_rate, optimizer) result(hyperparameters)
      implicit none
      integer, intent(in) :: mini_batches
      real, intent(in) :: learning_rate
      character(len=*), intent(in) :: optimizer
      type(hyperparameters_t) hyperparameters
    end function

    pure module function double_precision_from_components(mini_batches, learning_rate, optimizer) result(hyperparameters)
      implicit none
      integer, intent(in) :: mini_batches
      double precision, intent(in) :: learning_rate
      character(len=*), intent(in) :: optimizer
      type(hyperparameters_t(double_precision)) hyperparameters
    end function

  end interface

  interface

    pure module function default_real_to_json(self) result(lines)
      implicit none
      class(hyperparameters_t), intent(in) :: self
      type(string_t), allocatable :: lines(:)
    end function

    pure module function double_precision_to_json(self) result(lines)
      implicit none
      class(hyperparameters_t(double_precision)), intent(in) :: self
      type(string_t), allocatable :: lines(:)
    end function

    elemental module function default_real_equals(lhs, rhs) result(lhs_equals_rhs)
      implicit none
      class(hyperparameters_t), intent(in) :: lhs, rhs
      logical lhs_equals_rhs
    end function

    elemental module function double_precision_equals(lhs, rhs) result(lhs_equals_rhs)
      implicit none
      class(hyperparameters_t(double_precision)), intent(in) :: lhs, rhs
      logical lhs_equals_rhs
    end function

    elemental module function default_real_mini_batches(self) result(num_mini_batches)
      implicit none
      class(hyperparameters_t), intent(in) :: self
      integer num_mini_batches
    end function

    elemental module function double_precision_mini_batches(self) result(num_mini_batches)
      implicit none
      class(hyperparameters_t(double_precision)), intent(in) :: self
      integer num_mini_batches
    end function

     elemental module function default_real_learning_rate(self) result(rate)
       implicit none
       class(hyperparameters_t), intent(in) :: self
       real rate
     end function

     elemental module function double_precision_learning_rate(self) result(rate)
       implicit none
       class(hyperparameters_t(double_precision)), intent(in) :: self
       double precision rate
     end function

    elemental module function default_real_optimizer_name(self) result(identifier)
      implicit none
      class(hyperparameters_t), intent(in) :: self
      type(string_t) identifier
     end function

    elemental module function double_precision_optimizer_name(self) result(identifier)
      implicit none
      class(hyperparameters_t(double_precision)), intent(in) :: self
      type(string_t) identifier
     end function

  end interface

end module
