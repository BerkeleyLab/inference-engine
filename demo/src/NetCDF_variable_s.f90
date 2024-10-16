! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(NetCDF_variable_m) NetCDF_variable_s
  use kind_parameters_m, only : default_real
  use assert_m, only : assert
  implicit none

  interface components_allocated
    module procedure default_real_components_allocated
    module procedure double_precision_components_allocated
  end interface

  interface lower_bounds
    module procedure default_real_lower_bounds
    module procedure double_precision_lower_bounds
  end interface

  interface upper_bounds
    module procedure default_real_upper_bounds
    module procedure double_precision_upper_bounds
  end interface

contains

  module procedure default_real_input
    self%name_ = variable_name
    select case (rank)
    case (1)
      call file%input(variable_name%string(), self%values_1D_)
    case (2)
      call file%input(variable_name%string(), self%values_2D_)
    case (3)
      call file%input(variable_name%string(), self%values_3D_)
    case (4)
      call file%input(variable_name%string(), self%values_4D_)
    case default
      error stop 'NetCDF_variable_s(default_real_input): unsupported rank'
    end select 
  end procedure
  
  module procedure default_real_input_character_name
    call self%default_real_input(string_t(variable_name), file, rank)
  end procedure

  module procedure double_precision_input
    self%name_ = variable_name
    select case (rank)
    case (1)
      call file%input(variable_name%string(), self%values_1D_)
    case (2)
      call file%input(variable_name%string(), self%values_2D_)
    case (3)
      call file%input(variable_name%string(), self%values_3D_)
    case (4)
      call file%input(variable_name%string(), self%values_4D_)
    case default
      error stop 'NetCDF_variable_s(double_precision_input): unsupported rank'
    end select
  end procedure

  module procedure double_precision_input_character_name
    call self%double_precision_input(string_t(variable_name), file, rank)
  end procedure

  pure function default_real_components_allocated(NetCDF_variable) result(allocation_vector)
    type(NetCDF_variable_t), intent(in) :: NetCDF_variable
    logical, allocatable :: allocation_vector(:)
    allocation_vector = [allocated(NetCDF_variable%values_1D_), allocated(NetCDF_variable%values_4D_)]
  end function

  pure function double_precision_components_allocated(NetCDF_variable) result(allocation_vector)
    type(NetCDF_variable_t(double_precision)), intent(in) :: NetCDF_variable
    logical, allocatable :: allocation_vector(:)
    allocation_vector = [allocated(NetCDF_variable%values_1D_), allocated(NetCDF_variable%values_4D_)]
  end function

  module procedure default_real_rank
    associate(allocation_vector => components_allocated(self))
      call assert(count(allocation_vector) == 1, "NetCDF_variable_s(default_real_rank): allocation count")
      my_rank = findloc(allocation_vector, .true., dim=1)
    end associate
  end procedure

  module procedure double_precision_rank
    associate(allocation_vector => components_allocated(self))
      call assert(count(allocation_vector) == 1, "NetCDF_variable_s(double_precision_rank): allocation count")
      my_rank = findloc(allocation_vector, .true., dim=1)
    end associate
  end procedure

  pure function default_real_lower_bounds(NetCDF_variable) result(lbounds)
    type(NetCDF_variable_t), intent(in) :: NetCDF_variable
    integer, allocatable :: lbounds(:)
    select case(NetCDF_variable%rank())
    case(1)
      lbounds = lbound(NetCDF_variable%values_1D_)
    case(2)
      lbounds = lbound(NetCDF_variable%values_2D_)
    case(3)
      lbounds = lbound(NetCDF_variable%values_3D_)
    case(4)
      lbounds = lbound(NetCDF_variable%values_4D_)
    case default
      error stop "NetCDF_variable_s(default_real_lower_bounds): unsupported rank"
    end select
  end function

  pure function double_precision_lower_bounds(NetCDF_variable) result(lbounds)
    type(NetCDF_variable_t(double_precision)), intent(in) :: NetCDF_variable
    integer, allocatable :: lbounds(:)
    select case(NetCDF_variable%rank())
    case(1)
      lbounds = lbound(NetCDF_variable%values_1D_)
    case(2)
      lbounds = lbound(NetCDF_variable%values_2D_)
    case(3)
      lbounds = lbound(NetCDF_variable%values_3D_)
    case(4)
      lbounds = lbound(NetCDF_variable%values_4D_)
    case default
      error stop "NetCDF_variable_s(double_precision_lower_bounds): unsupported rank"
    end select
  end function

  pure function default_real_upper_bounds(NetCDF_variable) result(ubounds)
    type(NetCDF_variable_t), intent(in) :: NetCDF_variable
    integer, allocatable :: ubounds(:)
    select case(NetCDF_variable%rank())
    case(1)
      ubounds = ubound(NetCDF_variable%values_1D_)
    case(2)
      ubounds = ubound(NetCDF_variable%values_2D_)
    case(3)
      ubounds = ubound(NetCDF_variable%values_3D_)
    case(4)
      ubounds = ubound(NetCDF_variable%values_4D_)
    case default
      error stop "NetCDF_variable_s(default_real_upper_bounds): unsupported rank"
    end select
  end function

  pure function double_precision_upper_bounds(NetCDF_variable) result(ubounds)
    type(NetCDF_variable_t(double_precision)), intent(in) :: NetCDF_variable
    integer, allocatable :: ubounds(:)
    select case(NetCDF_variable%rank())
    case(1)
      ubounds = ubound(NetCDF_variable%values_1D_)
    case(2)
      ubounds = ubound(NetCDF_variable%values_2D_)
    case(3)
      ubounds = ubound(NetCDF_variable%values_3D_)
    case(4)
      ubounds = ubound(NetCDF_variable%values_4D_)
    case default
      error stop "NetCDF_variable_s(double_precision_upper_bounds): unsupported rank"
    end select
  end function

  module procedure default_real_conformable_with

    if (self%rank() /= NetCDF_variable%rank()) then
      conformable = .false.
      return
    end if

    if (any(lower_bounds(self) /= lower_bounds(NetCDF_variable))) then
      conformable = .false.
      return
    end if

    if (any(upper_bounds(self) /= upper_bounds(NetCDF_variable))) then
      conformable = .false.
      return
    end if

    conformable = .true.

  end procedure

  module procedure double_precision_conformable_with

    if (self%rank() /= NetCDF_variable%rank()) then
      conformable = .false.
      return
    end if

    if (any(lower_bounds(self) /= lower_bounds(NetCDF_variable))) then
      conformable = .false.
      return
    end if

    conformable = .true.

  end procedure

end submodule NetCDF_variable_s