program netCDF_IO
  use netcdf, only : &
     nf90_create, nf90_def_dim, nf90_def_var, nf90_enddef, nf90_put_var, & ! functions
     nf90_close, nf90_open, nf90_inq_varid, nf90_get_var, &                ! functions
     nf90_clobber, nf90_noerr, nf90_strerror, nf90_int, nf90_nowrite       ! constants
  use assert_m, only : assert
  implicit none

  integer i, j
  integer, parameter :: nx = 6, ny = 12
  integer, parameter :: data_written(*,*) = reshape([((i*j, i=1,nx), j=1,ny)], [ny,nx])
  integer, allocatable :: data_read(:,:)
  character(len=*), parameter :: file_name = "netCDF_example.nc"
  
  call netCDF_write(file_name, data_written)
  allocate(data_read, mold = data_written)
  call netCDF_read(file_name, data_read)

  call assert(all(data_written == data_read) , "main: all(data_written == data_read)")

  print *, "-----> netCDF file '" // file_name // "' written without error <-----"

contains

  subroutine netCDF_write(file_name_, data_out)
    character(len=*), intent(in) :: file_name_
    integer, intent(in) :: data_out(:,:)
  
    integer ncid, varid, x_dimid, y_dimid

    associate(nf_status => nf90_create(file_name_, nf90_clobber, ncid)) ! create or ovewrite file
      call assert(nf_status == nf90_noerr, "nff90_noerr == nf90_create(file_name, nf90_clobber, ncid)",&
        trim(nf90_strerror(nf_status)))
    end associate
    associate(nf_status => nf90_def_dim(ncid, "x", size(data_out,2), x_dimid)) ! define x dimension & get its ID
      call assert(nf_status == nf90_noerr, 'nff90_noerr == nf90_def_dim(ncid, "x", size(data_out,2) x_dimid)', &
        trim(nf90_strerror(nf_status)))
    end associate
    associate(nf_status => nf90_def_dim(ncid, "y", size(data_out,1), y_dimid)) ! define y dimension & get its ID
      call assert(nf_status == nf90_noerr, 'nff90_noerr == nf90_def_dim(ncid, "y", size(data_out,2), y_dimid)', &
        trim(nf90_strerror(nf_status)))
    end associate
    associate(nf_status => nf90_def_var(ncid, "data", nf90_int, [y_dimid, x_dimid], varid))!define integer 'data' variable & get ID
      call assert(nf_status == nf90_noerr, 'nff90_noerr == nf90_def_var(ncid, "data", nf90_int, [y_dimid, x_dimid], varid)', &
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

  end subroutine

  subroutine netCDF_read(file_name_, data_in)
    character(len=*), intent(in) :: file_name_
    integer, intent(inout) :: data_in(:,:)
    integer ncid, varid

    associate( nf_status => nf90_open(file_name_, nf90_nowrite, ncid) ) ! open file with read-only acces
      call assert(nf_status == nf90_noerr, "nf90_noerr == nf90_open(file_name_, NF90_NOWRITE, ncid)", &
        trim(nf90_strerror(nf_status)))
    end associate
    
    associate( nf_status => nf90_inq_varid(ncid, "data", varid)) ! Get data variable's ID
      call assert(nf_status == nf90_noerr, 'nf90_noerr = nf90_inq_varid(ncid, "data", varid)', trim(nf90_strerror(nf_status)))
    end associate

    associate( nf_status => nf90_get_var(ncid, varid, data_in)) ! Read data
      call assert(nf_status == nf90_noerr, "nf90_noerr = nf90_get_var(ncid, varid, data_in)", trim(nf90_strerror(nf_status)))
    end associate

    associate(nf_status => nf90_close(ncid)) ! close file, free all resources
      call assert(nf_status == nf90_noerr, "nf90_noerr = nf90_close(ncid)", trim(nf90_strerror(nf_status))) 
    end associate

  end subroutine

end program netCDF_IO
