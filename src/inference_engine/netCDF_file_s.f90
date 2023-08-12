! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
#ifndef __INTEL_FORTRAN
!! Due to a suspected bug in the Intel ifx compiler, the above C preprocessor macro
!! effectively eliminates this file's source code when building with an Intel compiler.
submodule(netCDF_file_m) netCDF_file_s
  use netcdf, only : &
     nf90_create, nf90_def_dim, nf90_def_var, nf90_enddef, nf90_put_var, nf90_inquire_dimension, & ! functions
     nf90_close, nf90_open, nf90_inq_varid, nf90_get_var, nf90_inquire_variable, &
     nf90_clobber, nf90_noerr, nf90_strerror, nf90_int, nf90_nowrite ! constants
  use assert_m, only : assert
  implicit none

contains

  module procedure construct
   netCDF_file%file_name_ = file_name
  end procedure

  module procedure output

    integer ncid, varid, x_dimid, y_dimid

    associate(nf_status => nf90_create(self%file_name_, nf90_clobber, ncid)) ! create or ovewrite file
      call assert(nf_status == nf90_noerr, "nf90_create(self%file_name_, nf90_clobber, ncid) succeeds",trim(nf90_strerror(nf_status)))
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
    associate(nf_status => nf90_enddef(ncid)) ! exit define mode: tell netCDF we are done defining metadata
      call assert(nf_status == nf90_noerr, 'nff90_noerr == nf90_enddef(ncid)', trim(nf90_strerror(nf_status)))
    end associate
    associate(nf_status => nf90_put_var(ncid, varid, data_out)) ! write all data to file
      call assert(nf_status == nf90_noerr, 'nff90_noerr == nf90_put_var(ncid, varid, data_out)', trim(nf90_strerror(nf_status)))
    end associate
    associate(nf_status => nf90_close(ncid)) ! close file to free associated netCDF resources and flush buffers
      call assert(nf_status == nf90_noerr, 'nff90_noerr == nf90_close(ncid)', trim(nf90_strerror(nf_status)))
    end associate

  end procedure

  module procedure input

    integer ncid, varid, data_in_rank

    associate( nf_status => nf90_open(self%file_name_, nf90_nowrite, ncid) ) ! open file with read-only acces
      call assert(nf_status == nf90_noerr, "nf90_open(self%file_name_, NF90_NOWRITE, ncid) succeeds", trim(nf90_strerror(nf_status)))
    end associate

    associate( nf_status => nf90_inq_varid(ncid, varname, varid)) ! get variable's ID
      call assert(nf_status == nf90_noerr, 'nf90_inq_varid(ncid, varname, varid) succeeds', trim(nf90_strerror(nf_status)))
    end associate

    call set_shape(data_in, get_shape(ncid, varname))

    associate( nf_status => nf90_get_var(ncid, varid, data_in)) ! Read data
      call assert(nf_status == nf90_noerr, "nf90_get_var(ncid, varid, data_in) succeeds", trim(nf90_strerror(nf_status)))
    end associate
  contains

    subroutine set_shape(array, s)
      class(*), intent(inout) :: array(..)
      integer, intent(in) :: s(:) ! array shape

      call assert(size(s)==rank(array), "netCDF_file_s(set_shape): size(s)==rank(array)")
      
      select rank(array)
        rank(1)
          select type(array)
            type is(integer)
              !allocate(array(s(1)))
            type is(real)
              !allocate(array(s(1)))
            type is(double precision)
              !allocate(array(s(1)))
            class default
              error stop "netCDF_file_s(set_shape): unsupported rank-1 type"
          end select
        rank(2)
          select type(array)
            type is(integer)
              !allocate(array(s(1),s(2)))
            type is(real)
              !allocate(array(s(1),s(2)))
            type is(double precision)
             !allocate(array(s(1),s(2)))
            class default
              error stop "netCDF_file_s(set_shape): unsupported rank-2 type"
          end select
        rank default
          error stop "netCDF_file_s(set_shape): unsupported rank"
      end select

    end subroutine

    function get_shape(ncid, varname) result(array_shape)
      implicit none
      character(len=*), intent(in) :: varname
      integer, intent(in) :: ncid
      integer, allocatable :: array_shape(:)
      character(len=32) varid_string

      integer varid, dimlen, i, var_rank
      integer, parameter :: max_rank=15
      integer,dimension(max_rank+1) :: dims, dimIds

      associate(nf_status => nf90_inq_varid(ncid, varname, varid))
        write(varid_string, *) varid
        call assert(nf_status == nf90_noerr, "nf90_noerr == nf90_inq_varid(ncid, varname, varid) (" // &
          trim(nf90_strerror(nf_status)) // "(" // trim(varid_string)// ")")
      end associate
      associate(nf_status => nf90_inquire_variable(ncid, varid, ndims = var_rank))
        call assert(nf_status == nf90_noerr, "nf90_noerr == nf90_inquire_variable(ncid, varid, ndims = var_rank) (" // &
          trim(nf90_strerror(nf_status)) // "(" // varname // ")")
      end associate
      associate(nf_status => nf90_inquire_variable(ncid, varid, dimids = dimIds(:var_rank)))
        call assert(nf_status == nf90_noerr, "nf90_noerr == nf90_inquire_variable(ncid, varid, dimids = dimIds(:var_rank))", &
          trim(nf90_strerror(nf_status)) // "(" // varname // ")")
      end associate

      do i=1,var_rank
        associate(nf_status => nf90_inquire_dimension(ncid, dimIds(i), len = dimlen))
          call assert(nf_status == nf90_noerr, "nf90_noerr == nf90_inquire_dimension(ncid, dimIds(i), len = dimlen)", &
            trim(nf90_strerror(nf_status)) // "(" // varname // ")")
        end associate
        dims(i+1)=dimlen
      end do

      array_shape = dims(2:var_rank+1)
    end function 

  end procedure

end submodule netCDF_file_s
#endif // __INTEL_FORTRAN
