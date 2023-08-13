! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
#ifndef __INTEL_FORTRAN
!! Due to a suspected bug in the Intel ifx compiler, the above C preprocessor macro
!! effectively eliminates this file's source code when building with an Intel compiler.
program train_cloud_microphysics
  !! Train a neural network to represent the simplest cloud microphysics model from
  !! the Intermediate Complexity Atmospheric Research Model (ICAR) at
  !! https://github.com/BerkeleyLab/icar.
  use sourcery_m, only : string_t, file_t, command_line_t
  use NetCDF_file_m, only : NetCDF_file_t
  implicit none

  type(command_line_t) command_line
  character(len=:), allocatable :: base

  base = command_line%flag_value("--base-name") ! gfortran 13 seg faults if this is an association

  if (len(base)==0) error stop new_line('a') // new_line('a') // &
    'Usage: ./build/run-fpm.sh run train-cloud-microphysics -- --base-name "<file-base-name>"'

  associate(network_input => base // "_input.nc", network_output => base // "_output.nc", network => base // "_network.json")

    read_and_train: &
    block
      real, allocatable, dimension(:,:,:,:) :: pressure_in, potential_temperature_in, temperature_in, &
                                               qv_in, qc_in, qi_in, qr_in, qs_in
      real, allocatable, dimension(:,:,:,:) :: pressure_out, potential_temperature_out, temperature_out, &
                                               qv_out, qc_out, qi_out, qr_out, qs_out
      real, allocatable, dimension(:,:,:) :: precipitation_in, snowfall_in
      real, allocatable, dimension(:,:,:) :: precipitation_out, snowfall_out
      real time_in, time_out

      associate(network_input_file => netCDF_file_t(network_input))
        call network_input_file%input("pressure", pressure_in)
        call network_input_file%input("potential_temperature", potential_temperature_in)
        call network_input_file%input("temperature", temperature_in)
        call network_input_file%input("precipitation", precipitation_in)
        call network_input_file%input("snowfall", snowfall_in)
        call network_input_file%input("qv", qv_in)
        call network_input_file%input("qc", qc_in)
        call network_input_file%input("qi", qi_in)
        call network_input_file%input("qr", qr_in)
        call network_input_file%input("qs", qs_in)
        call network_input_file%input("time", time_in)
      end associate

      associate(network_output_file => netCDF_file_t(network_output))
        call network_output_file%input("pressure", pressure_out)
        call network_output_file%input("potential_temperature", potential_temperature_out)
        call network_output_file%input("temperature", temperature_out)
        call network_output_file%input("precipitation", precipitation_out)
        call network_output_file%input("snowfall", snowfall_out)
        call network_output_file%input("qv", qv_out)
        call network_output_file%input("qc", qc_out)
        call network_output_file%input("qi", qi_out)
        call network_output_file%input("qr", qr_out)
        call network_output_file%input("qs", qs_out)
        call network_output_file%input("time", time_out)
      end associate

      associate(dt => time_out - time_in)
        associate( &
          dp_dt => (pressure_out - pressure_in)/dt, &
          dpt_dt => (potential_temperature_out - potential_temperature_in)/dt, &
          dtemp_dt => (temperature_out - temperature_in)/dt, &
          dprecip_dt => (precipitation_out - precipitation_in)/dt, &
          dsnow_dt => (snowfall_out - snowfall_in)/dt, &
          dqv_dt => (qv_out - qv_in)/dt, &
          dqc_dt => (qc_out - qc_in)/dt, &
          dqi_dt => (qi_out - qi_in)/dt, &
          dqr_dt => (qr_out - qr_in)/dt, &
          dqs_dt => (qs_out - qs_in)/dt  &
        )
        end associate
      end associate

    end block read_and_train

  end associate

  print *,new_line('a') // "______training_cloud_microhpysics done _______"

end program train_cloud_microphysics
#endif // __INTEL_FORTRAN
