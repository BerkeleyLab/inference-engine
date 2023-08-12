! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
#ifndef __INTEL_FORTRAN
!! Due to a suspected bug in the Intel ifx compiler, the above C preprocessor macro
!! effectively eliminates this file's source code when building with an Intel compiler.
program read_training_data
  !! Read netCDF file data
  use sourcery_m, only : string_t, file_t, command_line_t
  use netCDF_file_m, only : netCDF_file_t
  implicit none

  type(string_t) file_name
  type(command_line_t) command_line

  file_name = string_t(command_line%flag_value("--input-file"))

  if (len(file_name%string())==0) then
    error stop new_line('a') // new_line('a') // &
      'Usage: ./build/run-fpm.sh run --example read-training-data -- --input-file "<file-name>"' 
  end if

  block
    real, allocatable :: pressure(:,:,:,:)

    associate(netCDF_file => netCDF_file_t(file_name%string()))
      call netCDF_file%input("pressure", pressure)
    end associate
  end block

end program read_training_data
#endif // __INTEL_FORTRAN
