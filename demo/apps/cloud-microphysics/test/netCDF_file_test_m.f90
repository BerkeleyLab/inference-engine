! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
#ifndef __INTEL_FORTRAN
!! Due to a suspected bug in the Intel ifx compiler, the above C preprocessor macro
!! effectively eliminates this file's source code when building with an Intel compiler.
module NetCDF_file_test_m
  !! Define asymmetric tests for the NetCDF file interface

  ! External dependencies
  use assert_m, only : assert
  use sourcery_m, only : string_t, test_t, test_result_t
  use netcdf, only : &
     nf90_create, nf90_def_dim, nf90_def_var, nf90_enddef, nf90_put_var, nf90_inquire_dimension, & ! functions
     nf90_close, nf90_open, nf90_inq_varid, nf90_get_var, nf90_inquire_variable, &
     nf90_clobber, nf90_noerr, nf90_strerror, nf90_int, nf90_nowrite ! constants

  ! Internal dependencies
  use NetCDF_file_m, only : NetCDF_file_t

  implicit none

  private
  public :: NetCDF_file_test_t

  type, extends(test_t) :: NetCDF_file_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  interface output
    module procedure output_2D_integer, output_1D_double
  end interface

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "A NetCDF_file_t object"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)

    character(len=*), parameter :: longest_description = &
          "writing and then reading gives input matching the output for a 1D double precision array"

    associate( &
      descriptions => &
        [ character(len=len(longest_description)) :: &
          "writing and then reading gives input matching the output for a 2D integer array", &
          "writing and then reading gives input matching the output for a 1D double precision array" &
        ], &
      outcomes => &
        [ write_then_read_2D_integer(), & 
          write_then_read_1D_double() &
        ] & 
    )
      call assert(size(descriptions) == size(outcomes),"asymmetric_engine_test_m(results): size(descriptions) == size(outcomes)")
      test_results = test_result_t(descriptions, outcomes)
    end associate
       
  end function

  subroutine output_2D_integer(file_name, data_out)
    character(len=*), intent(in) :: file_name
    integer, intent(in) :: data_out(:,:)

    integer ncid, varid, x_dimid, y_dimid

    associate(nf_status => nf90_create(file_name, nf90_clobber, ncid)) ! create or ovewrite file
      call assert(nf_status == nf90_noerr, "nf90_create(file_name, nf90_clobber, ncid) succeeds",trim(nf90_strerror(nf_status)))
    end associate
    associate(nf_status => nf90_def_dim(ncid, "x", size(data_out,2), x_dimid)) ! define x dimension & get its ID
      call assert(nf_status == nf90_noerr,'nf90_def_dim(ncid,"x",size(data_out,2),x_dimid) succeeds',trim(nf90_strerror(nf_status)))
    end associate
    associate(nf_status => nf90_def_dim(ncid, "y", size(data_out,1), y_dimid)) ! define y dimension & get its ID
      call assert(nf_status==nf90_noerr, 'nf90_def_dim(ncid,"y",size(data_out,2),y_dimid) succeeds', trim(nf90_strerror(nf_status)))
    end associate
    associate(nf_status => nf90_def_var(ncid, "data", nf90_int, [y_dimid, x_dimid], varid))!define integer 'data' variable & get ID
      call assert(nf_status == nf90_noerr, 'nf90_def_var(ncid,"data",nf90_int,[y_dimid,x_dimid],varid) succeds', &
        trim(nf90_strerror(nf_status)))
    end associate
    associate(nf_status => nf90_enddef(ncid)) ! exit define mode: tell NetCDF we are done defining metadata
      call assert(nf_status == nf90_noerr, 'nff90_noerr == nf90_enddef(ncid)', trim(nf90_strerror(nf_status)))
    end associate
    associate(nf_status => nf90_put_var(ncid, varid, data_out)) ! write all data to file
      call assert(nf_status == nf90_noerr, 'nff90_noerr == nf90_put_var(ncid, varid, data_out)', trim(nf90_strerror(nf_status)))
    end associate
    associate(nf_status => nf90_close(ncid)) ! close file to free associated NetCDF resources and flush buffers
      call assert(nf_status == nf90_noerr, 'nff90_noerr == nf90_close(ncid)', trim(nf90_strerror(nf_status)))
    end associate
  end subroutine

  subroutine output_1D_double(file_name, data_out)
    character(len=*), intent(in) :: file_name
    double precision, intent(in) :: data_out(:)

    integer ncid, varid, x_dimid, y_dimid

    associate(nf_status => nf90_create(file_name, nf90_clobber, ncid)) ! create or ovewrite file
      call assert(nf_status == nf90_noerr, "nf90_create(file_name, nf90_clobber, ncid) succeeds",trim(nf90_strerror(nf_status)))
    end associate
    associate(nf_status => nf90_def_dim(ncid, "x", size(data_out,1), x_dimid)) ! define x dimension & get its ID
      call assert(nf_status == nf90_noerr,'nf90_def_dim(ncid,"x",size(data_out,2),x_dimid) succeeds',trim(nf90_strerror(nf_status)))
    end associate
    associate(nf_status => nf90_def_var(ncid, "data", nf90_int, [x_dimid], varid))!define integer 'data' variable & get ID
      call assert(nf_status == nf90_noerr, 'nf90_def_var(ncid,"data",nf90_int,[y_dimid,x_dimid],varid) succeds', &
        trim(nf90_strerror(nf_status)))
    end associate
    associate(nf_status => nf90_enddef(ncid)) ! exit define mode: tell NetCDF we are done defining metadata
      call assert(nf_status == nf90_noerr, 'nff90_noerr == nf90_enddef(ncid)', trim(nf90_strerror(nf_status)))
    end associate
    associate(nf_status => nf90_put_var(ncid, varid, data_out)) ! write all data to file
      call assert(nf_status == nf90_noerr, 'nff90_noerr == nf90_put_var(ncid, varid, data_out)', trim(nf90_strerror(nf_status)))
    end associate
    associate(nf_status => nf90_close(ncid)) ! close file to free associated NetCDF resources and flush buffers
      call assert(nf_status == nf90_noerr, 'nff90_noerr == nf90_close(ncid)', trim(nf90_strerror(nf_status)))
    end associate
  end subroutine

  function write_then_read_2D_integer() result(test_passes)
    logical, allocatable :: test_passes(:)
    integer i, j
    integer, parameter :: ny = 12,  nx = 6
    integer, parameter :: data_written(*,*) = reshape([((i*j, i=1,nx), j=1,ny)], [ny,nx])
    integer, allocatable :: data_read(:,:)
    character(len=*), parameter :: file_name = "build/NetCDF_test_2D_integer.nc"
  
    call output(file_name, data_written)

    associate(NetCDF_file => NetCDF_file_t(file_name))
      call NetCDF_file%input("data", data_read)
    end associate

    test_passes = [all(data_written == data_read)]

  end function

  function write_then_read_1D_double() result(test_passes)
    logical, allocatable :: test_passes(:)
    integer i
    integer, parameter :: nx = 6
    double precision, parameter :: data_written(*) = dble([(i, i=1,nx)]), tolerance = 1.E-15
    double precision, allocatable :: data_read(:)
    character(len=*), parameter :: file_name = "build/NetCDF_test_1D_double.nc"
  
    call output(file_name, data_written)

    associate(NetCDF_file => NetCDF_file_t(file_name))
      call NetCDF_file%input("data", data_read)
    end associate

    test_passes = [all(abs(data_written - data_read)<tolerance)]

  end function

end module NetCDF_file_test_m
#endif // __INTEL_FORTRAN