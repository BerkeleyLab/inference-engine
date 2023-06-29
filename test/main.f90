! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program main
  use inference_engine_test_m, only : inference_engine_test_t  
  use asymmetric_engine_test_m, only : asymmetric_engine_test_t  
  use skip_connections_test_m, only : skip_connections_test_t  
  use trainable_engine_test_m, only : trainable_engine_test_t  
  implicit none

  type(inference_engine_test_t) inference_engine_test
  type(asymmetric_engine_test_t) asymmetric_engine_test
  type(skip_connections_test_t) skip_connections_test
  type(trainable_engine_test_t) trainable_engine_test
  real t_start, t_finish

  integer :: passes=0, tests=0

  call cpu_time(t_start)
  call random_init(image_distinct=.true., repeatable=.true.)
  call inference_engine_test%report(passes, tests)
  call asymmetric_engine_test%report(passes, tests)
  call skip_connections_test%report(passes, tests)
  call trainable_engine_test%report(passes, tests)
  call cpu_time(t_finish)

  print *
  print *,"Test suite execution time: ",t_finish - t_start
  print *
  print '(*(a,:,g0))',"_________ In total, ",passes," of ",tests, " tests pass. _________"
  sync all
  print *
  if (passes/=tests) error stop "-------- One or more tests failed. See the above report. ---------"
end program
