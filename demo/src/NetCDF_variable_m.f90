! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module NetCDF_variable_m
  use NetCDF_file_m, only : NetCDF_file_t
  use kind_parameters_m, only : default_real, double_precision
  use julienne_m, only : string_t
  use fiats_m, only : tensor_t
  implicit none

  private
  public :: NetCDF_variable_t
  public :: tensors

  type NetCDF_variable_t(k)
    integer, kind :: k = default_real
    private
    real(k), allocatable :: values_1D_(:), values_2D_(:,:), values_3D_(:,:,:), values_4D_(:,:,:,:)
    character(len=:), allocatable :: name_
  contains
    generic :: input                    => default_real_input, double_precision_input, default_real_input_character_name, double_precision_input_character_name
    procedure, private, non_overridable :: default_real_input, double_precision_input, default_real_input_character_name, double_precision_input_character_name
    generic :: conformable_with         => default_real_conformable_with, double_precision_conformable_with
    procedure, private, non_overridable :: default_real_conformable_with, double_precision_conformable_with
    generic :: rank                     => default_real_rank            , double_precision_rank
    procedure, private, non_overridable :: default_real_rank            , double_precision_rank
    generic :: end_step                 => default_real_end_step        , double_precision_end_step
    procedure, private, non_overridable :: default_real_end_step        , double_precision_end_step
    generic :: any_nan                  => default_real_any_nan         , double_precision_any_nan
    procedure, private, non_overridable :: default_real_any_nan         , double_precision_any_nan
    generic :: minimum                  => default_real_minimum         , double_precision_minimum
    procedure, private, non_overridable :: default_real_minimum         , double_precision_minimum
    generic :: maximum                  => default_real_maximum         , double_precision_maximum
    procedure, private, non_overridable :: default_real_maximum         , double_precision_maximum
    generic :: operator(-)              => default_real_subtract        , double_precision_subtract
    procedure, private, non_overridable :: default_real_subtract        , double_precision_subtract
    generic :: operator(/)              => default_real_divide          , double_precision_divide
    procedure, private, non_overridable :: default_real_divide          , double_precision_divide
    generic :: assignment(=)            => default_real_assign          , double_precision_assign
    procedure, private, non_overridable :: default_real_assign          , double_precision_assign
  end type

  interface NetCDF_variable_t

    elemental module function default_real_copy(source, rename) result(NetCDF_variable)
      implicit none
      type(NetCDF_variable_t), intent(in) :: source
      type(string_t), intent(in), optional :: rename
      type(NetCDF_variable_t) NetCDF_variable
    end function

    elemental module function default_real_copy_character_name(source, rename) result(NetCDF_variable)
      implicit none
      type(NetCDF_variable_t), intent(in) :: source
      character(len=*), intent(in), optional :: rename
      type(NetCDF_variable_t) NetCDF_variable
    end function
 
    elemental module function double_precision_copy(source, rename) result(NetCDF_variable)
      implicit none
      type(NetCDF_variable_t(double_precision)), intent(in) :: source
      type(string_t), intent(in), optional :: rename
      type(NetCDF_variable_t(double_precision)) NetCDF_variable
    end function

    elemental module function double_precision_copy_character_name(source, rename) result(NetCDF_variable)
      implicit none
      type(NetCDF_variable_t(double_precision)), intent(in) :: source
      character(len=*), intent(in), optional :: rename
      type(NetCDF_variable_t(double_precision)) NetCDF_variable
    end function
 
  end interface

  interface

    impure elemental module subroutine default_real_input(self, variable_name, file, rank)
      implicit none
      class(NetCDF_variable_t), intent(inout) :: self
      type(string_t), intent(in) :: variable_name
      type(NetCDF_file_t), intent(in) :: file
      integer, intent(in) :: rank
    end subroutine

    impure elemental module subroutine double_precision_input(self, variable_name, file, rank)
      implicit none
      class(NetCDF_variable_t(double_precision)), intent(inout) :: self
      type(string_t), intent(in) :: variable_name
      type(NetCDF_file_t), intent(in) :: file
      integer, intent(in) :: rank
    end subroutine

    impure elemental module subroutine default_real_input_character_name(self, variable_name, file, rank)
      implicit none
      class(NetCDF_variable_t), intent(inout) :: self
      character(len=*), intent(in) :: variable_name
      type(NetCDF_file_t), intent(in) :: file
      integer, intent(in) :: rank
    end subroutine

    impure elemental module subroutine double_precision_input_character_name(self, variable_name, file, rank)
      implicit none
      class(NetCDF_variable_t(double_precision)), intent(inout) :: self
      character(len=*), intent(in) :: variable_name
      type(NetCDF_file_t), intent(in) :: file
      integer, intent(in) :: rank
    end subroutine

    elemental module function default_real_conformable_with(self, NetCDF_variable) result(conformable)
      implicit none
      class(NetCDF_variable_t), intent(in) :: self, NetCDF_variable
      logical conformable
    end function

    elemental module function double_precision_conformable_with(self, NetCDF_variable) result(conformable)
      implicit none
      class(NetCDF_variable_t(double_precision)), intent(in) :: self, NetCDF_variable
      logical conformable
    end function

    elemental module function default_real_rank(self) result(my_rank)
      implicit none
      class(NetCDF_variable_t), intent(in) :: self
      integer my_rank
    end function

    elemental module function double_precision_rank(self) result(my_rank)
      implicit none
      class(NetCDF_variable_t(double_precision)), intent(in) :: self
      integer my_rank
    end function

    elemental module function default_real_end_step(self) result(end_step)
      implicit none
      class(NetCDF_variable_t), intent(inout) :: self
      integer end_step
    end function

    elemental module function double_precision_end_step(self) result(end_step)
      implicit none
      class(NetCDF_variable_t(double_precision)), intent(inout) :: self
      integer end_step
    end function

    elemental module function default_real_subtract(lhs, rhs) result(difference)
      implicit none
      class(NetCDF_variable_t), intent(in) :: lhs, rhs
      type(NetCDF_variable_t) difference
    end function

    elemental module function double_precision_subtract(lhs, rhs) result(difference)
      implicit none
      class(NetCDF_variable_t(double_precision)), intent(in) :: lhs, rhs
      type(NetCDF_variable_t(double_precision)) difference
    end function

    elemental module function default_real_divide(lhs, rhs) result(ratio)
      implicit none
      class(NetCDF_variable_t), intent(in) :: lhs, rhs
      type(NetCDF_variable_t) ratio
    end function

    elemental module function double_precision_divide(lhs, rhs) result(ratio)
      implicit none
      class(NetCDF_variable_t(double_precision)), intent(in) :: lhs, rhs
      type(NetCDF_variable_t(double_precision)) ratio
    end function

    elemental module subroutine default_real_assign(lhs, rhs)
      implicit none
      class(NetCDF_variable_t), intent(inout) :: lhs
      type(NetCDF_variable_t), intent(in) :: rhs
    end subroutine

    elemental module subroutine double_precision_assign(lhs, rhs)
      implicit none
      class(NetCDF_variable_t(double_precision)), intent(inout) :: lhs
      type(NetCDF_variable_t(double_precision)), intent(in) :: rhs
    end subroutine

    elemental module function default_real_any_nan(self) result(any_nan)
      implicit none
      class(NetCDF_variable_t), intent(in) :: self
      logical any_nan
    end function

    elemental module function double_precision_any_nan(self) result(any_nan)
      implicit none
      class(NetCDF_variable_t(double_precision)), intent(in) :: self
      logical any_nan
    end function

    elemental module function default_real_minimum(self) result(minimum)
      implicit none
      class(NetCDF_variable_t), intent(in) :: self
      real minimum
    end function

    elemental module function double_precision_minimum(self) result(minimum)
      implicit none
      class(NetCDF_variable_t(double_precision)), intent(in) :: self
      real minimum
    end function

    elemental module function default_real_maximum(self) result(maximum)
      implicit none
      class(NetCDF_variable_t), intent(in) :: self
      real maximum
    end function

    elemental module function double_precision_maximum(self) result(maximum)
      implicit none
      class(NetCDF_variable_t(double_precision)), intent(in) :: self
      real maximum
    end function

    module function tensors(NetCDF_variables, step_start, step_end, step_stride)
      implicit none
      type(NetCDF_variable_t), intent(in) :: NetCDF_variables(:)
      type(tensor_t), allocatable :: tensors(:)
      integer, optional :: step_start, step_end, step_stride
    end function

    elemental module function default_real_end_time(self) result(end_time)
      implicit none
      class(NetCDF_variable_t), intent(inout) :: self
      integer end_time
    end function

    elemental module function double_precision_end_time(self) result(end_time)
      implicit none
      class(NetCDF_variable_t), intent(inout) :: self
      integer end_time
    end function

  end interface

end module 