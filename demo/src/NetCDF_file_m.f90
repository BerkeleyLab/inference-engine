! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module NetCDF_file_m
  implicit none

  private
  public :: NetCDF_file_t

  type NetCDF_file_t
    private
    character(len=:), allocatable :: file_name_
  contains
    generic :: input =>   input_integer, input_double_precision, input_real
    procedure, private :: input_integer, input_double_precision, input_real
  end type

  interface NetCDF_file_t

    pure module function construct(file_name) result(NetCDF_file)
      implicit none
      character(len=*), intent(in) :: file_name
      type(NetCDF_file_t) NetCDF_file
    end function
      
  end interface

  interface

    module subroutine input_real(self, varname, values)
      implicit none
      class(NetCDF_file_t), intent(in) :: self
      character(len=*), intent(in) :: varname
      real, intent(out), allocatable :: values(..)
    end subroutine

    module subroutine input_integer(self, varname, values)
      implicit none
      class(NetCDF_file_t), intent(in) :: self
      character(len=*), intent(in) :: varname
      integer, intent(out), allocatable :: values(..)
    end subroutine

    module subroutine input_double_precision(self, varname, values)
      implicit none
      class(NetCDF_file_t), intent(in) :: self
      character(len=*), intent(in) :: varname
      double precision, intent(out), allocatable :: values(..)
    end subroutine

  end interface

end module NetCDF_file_m