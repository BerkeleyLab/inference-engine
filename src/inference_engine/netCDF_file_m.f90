! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
#ifndef __INTEL_FORTRAN
!! Due to a suspected bug in the Intel ifx compiler, the above C preprocessor macro
!! effectively eliminates this file's source code when building with an Intel compiler.
module netCDF_file_m
  implicit none

  private
  public :: netCDF_file_t

  type netCDF_file_t
    private
    character(len=:), allocatable :: file_name_
  contains
    procedure input_2D_integer
    procedure input_4D_real
    generic :: input => input_2D_integer, input_4D_real
  end type

  interface netCDF_file_t

    pure module function construct(file_name) result(netCDF_file)
      implicit none
      character(len=*), intent(in) :: file_name
      type(netCDF_file_t) netCDF_file
    end function
      
  end interface

  interface

    module subroutine input_2D_integer(self, varname, values)
      implicit none
      class(netCDF_file_t), intent(in) :: self
      character(len=*), intent(in) :: varname
      integer, intent(out), allocatable :: values(:,:)
    end subroutine

    module subroutine input_4D_real(self, varname, values)
      implicit none
      class(netCDF_file_t), intent(in) :: self
      character(len=*), intent(in) :: varname
      real, intent(out), allocatable :: values(:,:,:,:)
    end subroutine

  end interface

end module netCDF_file_m
#endif // __INTEL_FORTRAN