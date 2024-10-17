! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

program main
  use netCDF_file_test_m, only : netCDF_file_test_t
  implicit none

  real t_start, t_finish

  integer :: passes=0, tests=0

  call cpu_time(t_start)
  block 
    type(netCDF_file_test_t) netCDF_file_test
    call netCDF_file_test%report(passes, tests)
  end block
  call cpu_time(t_finish)

  print *
  print *,"Test suite execution time: ",t_finish - t_start
  print *
  print '(*(a,:,g0))',"_________ In total, ",passes," of ",tests, " tests pass. _________"
#if defined(MULTI_IMAGE_SUPPORT)
  sync all
#endif
  print *
  if (passes/=tests) error stop "-------- One or more tests failed. See the above report. ---------"
end program
