! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module ubounds_m
  !! This module serves only to support array bounds checking in the main program below
  implicit none

  type ubounds_t
    integer, allocatable :: ubounds_(:)
  contains
    procedure equals
    generic :: operator(==) => equals
  end type
 
contains

  elemental function equals(lhs, rhs) result(lhs_equals_rhs)
    class(ubounds_t), intent(in) :: lhs, rhs
    logical lhs_equals_rhs
    lhs_equals_rhs = all(lhs%ubounds_ == rhs%ubounds_)
  end function

end module

program tensor_statistics
  !! This program
  !! 1. Computes the ranges and histograms of input and output tensors saved by
  !!    the neural-net branch of the Berkeley Lab fork of [ICAR](https://berkeleylab.github/icar).
  !! 2. Saves the resulting statistics to text files with space-separated columns and column labels.

  ! External dependencies:
  use julienne_m, only : command_line_t, file_t, string_t
  use assert_m, only : assert, intrinsic_array_t
  use ubounds_m, only : ubounds_t
  use ieee_arithmetic, only : ieee_is_nan
  use iso_fortran_env, only : int64, real64
    
  ! Internal dependencies:
  use NetCDF_file_m, only: NetCDF_file_t
  use histogram_m, only : histogram_t, to_file
  implicit none

  character(len=*), parameter :: usage = &
    new_line('a') // new_line('a') // &
    'Usage: ' // new_line('a') // new_line('a') // &
    './build/run-fpm.sh run tensor-statistics -- \' // new_line('a') // &
    '  --base <string> --bins <integer> \' // new_line('a') // &
    '  [--raw] [--start <integer>] [--end <integer>] [--stride <integer>]' // &
    new_line('a') // new_line('a') // &
    'where angular brackets denote user-provided values and square brackets denote optional arguments.' // new_line('a')

  integer(int64) t_start, t_finish, clock_rate
  integer num_bins, start_step, stride
  integer, allocatable :: end_step
  character(len=:), allocatable :: base_name
  logical raw

  call system_clock(t_start, clock_rate)
  call get_command_line_arguments(base_name, num_bins, start_step, end_step, stride, raw)
  call compute_histograms(base_name, raw)
  call system_clock(t_finish)

  print *,"System clock time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
  print *
  print *,"______________________________________________________"
  print *,"The *.plt files contain tensor ranges and histograms."
  print *,"If you have gnuplot installed, please execute the"
  print *,"following command to produce histogram visualizations:" 
  print *
  associate(file_name => trim(merge("plot-raw-histograms       ", "plot-normalized-histograms", raw)) // ".gnu")
   print*,"  gnuplot app/" // file_name
  end associate
  print *
  print *,"_______________ tensor_statistics done________________"

contains

  subroutine get_command_line_arguments(base_name, num_bins, start_step, end_step, stride, raw)
    character(len=:), allocatable, intent(out) :: base_name
    integer, intent(out) :: num_bins, start_step, stride
    integer, intent(out), allocatable :: end_step
    logical, intent(out) :: raw

    ! local variables
    type(command_line_t) command_line
    character(len=:), allocatable :: stride_string, bins_string, start_string, end_string

    base_name = command_line%flag_value("--base") ! gfortran 13 seg faults if this is an association
    bins_string = command_line%flag_value("--bins")
    start_string = command_line%flag_value("--start")
    end_string = command_line%flag_value("--end")
    stride_string = command_line%flag_value("--stride")
    raw = command_line%argument_present(["--raw"])

    associate(required_arguments => len(base_name)/=0 .and. len(bins_string)/=0)
       if (.not. required_arguments) error stop usage 
    end associate

    read(bins_string,*) num_bins

    if (len(stride_string)==0) then
      stride = 1
    else
      read(stride_string,*) stride
    end if

    if (len(start_string)==0) then
      start_step = 1
    else
      read(start_string,*) start_step
    end if

    if (len(end_string)/=0) then
      if (.not. allocated(end_step)) allocate(end_step)
      read(end_string,*) end_step
    end if
 
  end subroutine get_command_line_arguments

  subroutine compute_histograms(base_name, raw)
    character(len=*), intent(in) :: base_name
    logical, intent(in) :: raw
    integer(int64) t_histo_start, t_histo_finish 

    real, allocatable, dimension(:,:,:,:) :: &
      pressure_in , potential_temperature_in , temperature_in , &
      pressure_out, potential_temperature_out, temperature_out, &
      qv_out, qc_out, qr_out, qs_out, &
      qv_in , qc_in , qr_in , qs_in , &
      dpt_dt, dqv_dt, dqc_dt, dqr_dt, dqs_dt
    double precision, allocatable, dimension(:) :: time_in, time_out
    double precision, parameter :: tolerance = 1.E-07
    type(histogram_t), allocatable :: histograms(:)
    type(ubounds_t), allocatable :: ubounds(:)
    integer, allocatable :: lbounds(:)
    integer t, t_end

    associate( network_input_file_name => base_name // "_input.nc")

      print *,"Reading network inputs from " // network_input_file_name

      associate(network_input_file => netCDF_file_t(network_input_file_name))
        ! Skipping the following unnecessary inputs: precipitation, snowfall
        call network_input_file%input("pressure", pressure_in)
        call network_input_file%input("potential_temperature", potential_temperature_in)
        call network_input_file%input("temperature", temperature_in)
        call network_input_file%input("qv", qv_in)
        call network_input_file%input("qc", qc_in)
        call network_input_file%input("qr", qr_in)
        call network_input_file%input("qs", qs_in)
        call network_input_file%input("time", time_in)

        t_end = size(time_in)
        lbounds = [lbound(pressure_in), lbound(temperature_in), lbound(qv_in), lbound(qc_in), lbound(qr_in), lbound(qs_in)]
        ubounds = &
          [ubounds_t(ubound(qv_in)), ubounds_t(ubound(qc_in)), ubounds_t(ubound(qr_in)), ubounds_t(ubound(qs_in)), &
           ubounds_t(ubound(pressure_in)), ubounds_t(ubound(temperature_in)) &
          ]

        print *,"Calculating input tensor histograms."
        call system_clock(t_histo_start)
        histograms = [ &
          histogram_t(pressure_in, "pressure", num_bins, raw) &
         ,histogram_t(potential_temperature_in, 'potential-temperature', num_bins, raw) &
         ,histogram_t(temperature_in, "temperature", num_bins, raw) &
         ,histogram_t(qv_in, "qv", num_bins, raw) &
         ,histogram_t(qc_in, "qc", num_bins, raw) &
         ,histogram_t(qr_in, "qr", num_bins, raw) &
         ,histogram_t(qs_in, "qs", num_bins, raw) &
        ]
        call system_clock(t_histo_finish)
        print *,"Seven input histograms done in ", real(t_histo_finish - t_histo_start, real64)/real(clock_rate, real64), " sec."

        print *,"Writing input tensor histograms file"
        block
          type(file_t) histograms_file
          integer h

          if (raw) then
            do h = 1, size(histograms)
              histograms_file = to_file(histograms(h))
              call histograms_file%write_lines(string_t(histograms(h)%variable_name() // ".plt"))
            end do
          else
              histograms_file = to_file(histograms)
              call histograms_file%write_lines(string_t(base_name // "_inputs_stats.plt"))
          end if
        end block
      end associate
    end associate

    associate(network_output_file_name => base_name // "_output.nc")

      print *, "Reading network outputs from " // network_output_file_name

      associate(network_output_file => netCDF_file_t(network_output_file_name))
        ! Skipping the following unnecessary outputs: pressure, temperature, precipitation, snowfall
        call network_output_file%input("potential_temperature", potential_temperature_out)
        call network_output_file%input("qv", qv_out)
        call network_output_file%input("qc", qc_out)
        call network_output_file%input("qr", qr_out)
        call network_output_file%input("qs", qs_out)
        call network_output_file%input("time", time_out)
        lbounds = [lbounds, lbound(qv_out), lbound(qc_out), lbound(qr_out), lbound(qs_out)]
        ubounds = [ubounds, ubounds_t(ubound(qv_out)), ubounds_t(ubound(qc_out)), &
          ubounds_t(ubound(qr_out)), ubounds_t(ubound(qs_out))]
        call assert(all(lbounds == 1), "main: default input/output lower bounds", intrinsic_array_t(lbounds))
        call assert(all(ubounds == ubounds(1)), "main: matching input/output upper bounds")
        call assert(all(abs(time_in(2:t_end) - time_out(1:t_end-1))<tolerance), "main: matching time stamps")
      end associate

      print *,"Calculating time derivatives"
  
      allocate(dpt_dt, mold = potential_temperature_out)
      allocate(dqv_dt, mold = qv_out)
      allocate(dqc_dt, mold = qc_out)
      allocate(dqr_dt, mold = qr_out)
      allocate(dqs_dt, mold = qs_out)

      associate(dt => real(time_out - time_in))
        do concurrent(t = 1:t_end)
          dpt_dt(:,:,:,t) = (potential_temperature_out(:,:,:,t) - potential_temperature_in(:,:,:,t))/dt(t)
          dqv_dt(:,:,:,t) = (qv_out(:,:,:,t)- qv_in(:,:,:,t))/dt(t)
          dqc_dt(:,:,:,t) = (qc_out(:,:,:,t)- qc_in(:,:,:,t))/dt(t)
          dqr_dt(:,:,:,t) = (qr_out(:,:,:,t)- qr_in(:,:,:,t))/dt(t)
          dqs_dt(:,:,:,t) = (qs_out(:,:,:,t)- qs_in(:,:,:,t))/dt(t)
        end do
      end associate

      call assert(.not. any(ieee_is_nan(dpt_dt)), ".not. any(ieee_is_nan(dpt_dt)")
      call assert(.not. any(ieee_is_nan(dqv_dt)), ".not. any(ieee_is_nan(dqv_dt)")
      call assert(.not. any(ieee_is_nan(dqc_dt)), ".not. any(ieee_is_nan(dqc_dt)")
      call assert(.not. any(ieee_is_nan(dqr_dt)), ".not. any(ieee_is_nan(dqr_dt)")
      call assert(.not. any(ieee_is_nan(dqs_dt)), ".not. any(ieee_is_nan(dqs_dt)")

      print *,"Calculating output tensor histograms."
      call system_clock(t_histo_start, clock_rate)
      histograms = [ &
         histogram_t(dpt_dt, "dptdt", num_bins, raw) &
        ,histogram_t(dqv_dt, "dqvdt", num_bins, raw) &
        ,histogram_t(dqc_dt, "dqcdt", num_bins, raw) &
        ,histogram_t(dqr_dt, "dqrdt", num_bins, raw) &
        ,histogram_t(dqs_dt, "dqsdt", num_bins, raw) &
      ]
      call system_clock(t_histo_finish)
      print *,"Five output histograms done in ", real(t_histo_finish - t_histo_start, real64)/real(clock_rate, real64), " sec."
      block
        type(file_t) histograms_file
        integer h

        if (raw) then
          do h = 1, size(histograms)
            histograms_file = to_file(histograms(h))
            call histograms_file%write_lines(string_t(histograms(h)%variable_name() // ".plt"))
          end do
        else
            histograms_file = to_file(histograms)
            call histograms_file%write_lines(string_t(base_name // "_outputs_stats.plt"))
        end if
      end block
    end associate
  end subroutine

end program tensor_statistics
