! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module NetCDF_variable_m
  use NetCDF_file_m, only : NetCDF_file_t
  use kind_parameters_m, only : default_real, double_precision
  use julienne_m, only : string_t
  implicit none

  private
  public :: NetCDF_variable_t

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
    generic :: rank                     => default_real_rank, double_precision_rank
    procedure, private, non_overridable :: default_real_rank, double_precision_rank
    generic :: operator(-)              => default_real_subtract, double_precision_subtract
    procedure, private, non_overridable :: default_real_subtract, double_precision_subtract
  end type

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

    elemental module function default_real_subtract(lhs, rhs) result(difference)
      implicit none
      class(NetCDF_variable_t), intent(in) :: lhs, rhs
      type(NetCDF_variable_t) difference
    end function

    elemental module function double_precision_subtract(lhs, rhs) result(difference)
      implicit none
      class(NetCDF_variable_t(double_precision)), intent(in) :: lhs, rhs
      type(NetCDF_variable_t) difference
    end function

  end interface

end module 