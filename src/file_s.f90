submodule(file_m) file_s
  use iso_fortran_env, only : iostat_end, iostat_eor
  use assert_m, only : assert
  implicit none

contains
  
  module procedure read_lines

    integer io_status, line_num
    character(len=:), allocatable :: line
    integer, parameter :: max_message_length=128
    character(len=max_message_length) error_message
    integer, allocatable :: lengths(:)

    lengths = line_lengths(file_unit)

    associate(num_lines => size(lengths))

      allocate(file_object%lines_(num_lines))
  
      do line_num = 1, num_lines
        allocate(character(len=lengths(line_num)) :: line)
        read(file_unit, '(a)', iostat=io_status, iomsg=error_message) line
        call assert(io_status==0,"read_lines: io_status==0 after line read", error_message)
        file_object%lines_(line_num) = string_t(line)
        deallocate(line)
      end do

    end associate

  contains
   
    function line_count(file_unit) result(num_lines)
      integer, intent(in) :: file_unit
      integer num_lines
    
      rewind(file_unit)
      num_lines = 0 
      do  
        read(file_unit, *, iostat=io_status)
        if (io_status==iostat_end) exit
        num_lines = num_lines + 1 
      end do
      rewind(file_unit)
    end function

    function line_lengths(file_unit) result(lengths)
      integer, intent(in) :: file_unit
      integer, allocatable ::  lengths(:)
      integer io_status
      character(len=1) c

      associate(num_lines => line_count(file_unit))

        allocate(lengths(num_lines), source = 0)
        rewind(file_unit)

        do line_num = 1, num_lines
          do
            read(file_unit, '(a)', advance='no', iostat=io_status, iomsg=error_message) c
            if (io_status==iostat_eor) exit
            lengths(line_num) = lengths(line_num) + 1
          end do
        end do

        rewind(file_unit)
  
      end associate
    end function

  end procedure

  module procedure lines
    my_lines = self%lines_
  end procedure

end submodule file_s
