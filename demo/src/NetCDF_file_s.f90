! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(netCDF_file_m) netCDF_file_s
  use netcdf, only : &
     nf90_create, nf90_def_dim, nf90_def_var, nf90_enddef, nf90_put_var, nf90_inquire_dimension, & ! functions
     nf90_close, nf90_open, nf90_inq_varid, nf90_get_var, nf90_inquire_variable, &
     nf90_clobber, nf90_noerr, nf90_strerror, nf90_int, nf90_nowrite ! constants
  use assert_m, only : assert, intrinsic_array_t
  implicit none

contains

  module procedure construct
   netCDF_file%file_name_ = file_name
  end procedure

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
      call assert(nf_status == nf90_noerr, "Net_CDF_file_m(get_shape): nf90_inq_varid " // trim(nf90_strerror(nf_status)), &
        diagnostic_data = "varname '" // varname // "', varid " // trim(adjustl(varid_string)))
    end associate
    associate(nf_status => nf90_inquire_variable(ncid, varid, ndims = var_rank))
      call assert(nf_status == nf90_noerr, "Net_CDF_file_m(get_shape): nf90_inquire_variable" // trim(nf90_strerror(nf_status)), &
        trim(nf90_strerror(nf_status)) // "(" // varname // ")")
    end associate
    associate(nf_status => nf90_inquire_variable(ncid, varid, dimids = dimIds(:var_rank)))
      call assert(nf_status == nf90_noerr, "Net_CDF_file_m(get_shape): nf90_inquire_variable" // trim(nf90_strerror(nf_status)), &
        trim(nf90_strerror(nf_status)) // "(" // varname // ")")
    end associate
    do i=1,var_rank
      associate(nf_status => nf90_inquire_dimension(ncid, dimIds(i), len = dimlen))
        call assert(nf_status == nf90_noerr, "Net_CDF_file_m(get_shape): nf90_inquire_dimension" // trim(nf90_strerror(nf_status)),&
          trim(nf90_strerror(nf_status)) // "(" // varname // ")")
      end associate
      dims(i+1)=dimlen
    end do
    array_shape = dims(2:var_rank+1)
  end function 

  module procedure input_integer

    character(len=32) varid_string
    integer ncid, varid

    associate( nf_status => nf90_open(self%file_name_, nf90_nowrite, ncid) ) ! open file with read-only acces
      call assert(nf_status == nf90_noerr, &
        "Net_CDF_file_m(input_integer): nf90_open" // trim(nf90_strerror(nf_status)), &
        diagnostic_data = trim(nf90_strerror(nf_status)) // self%file_name_)
    end associate

    associate( nf_status => nf90_inq_varid(ncid, varname, varid)) ! get variable's ID
      write(varid_string, *) varid
      call assert(nf_status == nf90_noerr, "Net_CDF_file_m(input_integer): nf90_inq_varid " // trim(nf90_strerror(nf_status)), &
        diagnostic_data = "varname '" // varname // "', varid " // trim(adjustl(varid_string)))
    end associate

    select rank(values)
      rank(1)
        associate(array_shape => get_shape(ncid, varname))
          call assert(size(array_shape)==rank(values), "netCDF_file_s(input_integer): size(array_shape)==rank(values)")
          allocate(values(array_shape(1)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call assert(nf_status == nf90_noerr, "NetCDF_file_s(input_integer): nf90_get_var", trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank(2)
        associate(array_shape => get_shape(ncid, varname))
          call assert(size(array_shape)==rank(values), "netCDF_file_s(input_integer): size(array_shape)==rank(values)")
          allocate(values(array_shape(1), array_shape(2)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call assert(nf_status == nf90_noerr, "NetCDF_file_s(input_integer): nf90_get_var", trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank(3)
        associate(array_shape => get_shape(ncid, varname))
          call assert(size(array_shape)==rank(values), "netCDF_file_s(input_integer): size(array_shape)==rank(values)")
          allocate(values(array_shape(1), array_shape(2), array_shape(3)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call assert(nf_status == nf90_noerr, "NetCDF_file_s(input_integer): nf90_get_var", trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank(4)
        associate(array_shape => get_shape(ncid, varname))
          call assert(size(array_shape)==rank(values), "netCDF_file_s(input_integer): size(array_shape)==rank(values)")
          allocate(values(array_shape(1), array_shape(2), array_shape(3), array_shape(4)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call assert(nf_status == nf90_noerr, "NetCDF_file_s(input_integer): nf90_get_var", trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank default
        error stop "NetCDF_file_s(input_integer): unsupported rank"
    end select

  end procedure

  module procedure input_double_precision
    character(len=32) varid_string
    integer ncid, varid

    associate( nf_status => nf90_open(self%file_name_, nf90_nowrite, ncid) ) ! open file with read-only acces
      call assert(nf_status == nf90_noerr, "nf90_open(self%file_name_, NF90_NOWRITE, ncid)", &
        trim(nf90_strerror(nf_status)) // self%file_name_)
    end associate

    associate( nf_status => nf90_inq_varid(ncid, varname, varid)) ! get variable's ID
      write(varid_string, *) varid
      call assert(nf_status == nf90_noerr, "Net_CDF_file_m(input_double): nf90_inq_varid " // trim(nf90_strerror(nf_status)), &
        diagnostic_data = "varname '" // varname // "', varid " // trim(adjustl(varid_string)))
    end associate

    select rank(values)
      rank(1)
        associate(array_shape => get_shape(ncid, varname))
          call assert(size(array_shape)==rank(values), "netCDF_file_s(input_double): size(array_shape)==rank(values)", &
            intrinsic_array_t([size(array_shape),rank(values)]))
          allocate(values(array_shape(1)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call assert(nf_status == nf90_noerr, "nf90_get_var(ncid, varid, array)", trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank(2)
        associate(array_shape => get_shape(ncid, varname))
          call assert(size(array_shape)==rank(values), "netCDF_file_s(input_double): size(array_shape)==rank(values)", &
            intrinsic_array_t([size(array_shape),rank(values)]))
          allocate(values(array_shape(1),array_shape(2)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call assert(nf_status == nf90_noerr, "nf90_get_var(ncid, varid, array)", trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank(3)
        associate(array_shape => get_shape(ncid, varname))
          call assert(size(array_shape)==rank(values), "netCDF_file_s(input_double): size(array_shape)==rank(values)", &
            intrinsic_array_t([size(array_shape),rank(values)]))
          allocate(values(array_shape(1),array_shape(2),array_shape(3)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call assert(nf_status == nf90_noerr, "nf90_get_var(ncid, varid, array)", trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank(4)
        associate(array_shape => get_shape(ncid, varname))
          call assert(size(array_shape)==rank(values), "netCDF_file_s(input_double): size(array_shape)==rank(values)", &
            intrinsic_array_t([size(array_shape),rank(values)]))
          allocate(values(array_shape(1),array_shape(2),array_shape(3),array_shape(4)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call assert(nf_status == nf90_noerr, "nf90_get_var(ncid, varid, array)", trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank default
        error stop "NetCDF_file_s(input_double): unsupported rank"
    end select
  end procedure

  module procedure input_real

    character(len=32) varid_string
    integer ncid, varid

    associate( nf_status => nf90_open(self%file_name_, nf90_nowrite, ncid) ) ! open file with read-only acces
      call assert(nf_status == nf90_noerr, "nf90_open(self%file_name_, NF90_NOWRITE, ncid)", &
        trim(nf90_strerror(nf_status)) // self%file_name_)
    end associate

    associate( nf_status => nf90_inq_varid(ncid, varname, varid)) ! get variable's ID
      write(varid_string, *) varid
      call assert(nf_status == nf90_noerr, "Net_CDF_file_m(input_real): nf90_inq_varid " // trim(nf90_strerror(nf_status)), &
        diagnostic_data = "varname '" // varname // "', varid " // trim(adjustl(varid_string)))
    end associate

    select rank(values)
      rank(0)
        allocate(values)
        associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
          call assert(nf_status == nf90_noerr, "nf90_get_var(ncid, varid, values)", trim(nf90_strerror(nf_status)))
        end associate
      rank(1)
        associate(array_shape => get_shape(ncid, varname))
          call assert(size(array_shape)==rank(values), "netCDF_file_s(input_real): size(array_shape)==rank(values)", &
            intrinsic_array_t([size(array_shape),rank(values)]))
          allocate(values(array_shape(1)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call assert(nf_status == nf90_noerr, "nf90_get_var(ncid, varid, array)", trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank(2)
        associate(array_shape => get_shape(ncid, varname))
          call assert(size(array_shape)==rank(values), "netCDF_file_s(input_real): size(array_shape)==rank(values)", &
            intrinsic_array_t([size(array_shape),rank(values)]))
          allocate(values(array_shape(1), array_shape(2)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call assert(nf_status == nf90_noerr, "nf90_get_var(ncid, varid, array)", trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank(3)
        associate(array_shape => get_shape(ncid, varname))
          call assert(size(array_shape)==rank(values), "netCDF_file_s(input_real): size(array_shape)==rank(values)", &
            intrinsic_array_t([size(array_shape),rank(values)]))
          allocate(values(array_shape(1), array_shape(2), array_shape(3)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call assert(nf_status == nf90_noerr, "nf90_get_var(ncid, varid, array)", trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank(4)
        associate(array_shape => get_shape(ncid, varname))
          call assert(size(array_shape)==rank(values), "netCDF_file_s(input_real): size(array_shape)==rank(values)", &
            intrinsic_array_t([size(array_shape),rank(values)]))
          allocate(values(array_shape(1), array_shape(2), array_shape(3), array_shape(4)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call assert(nf_status == nf90_noerr, "nf90_get_var(ncid, varid, array)", trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank default
        error stop "NetCDF_file_s(input_real_array): unsupported rank"
    end select

  end procedure

end submodule netCDF_file_s