program main
  use inference_engine_test_m, only : inference_engine_test_t  
  implicit none

  type(inference_engine_test_t) inference_engine_test

  integer :: passes=0, tests=0

  call inference_engine_test%report(passes, tests)

  print *
  print '(*(a,:,g0))',"_________ In total, ",passes," of ",tests, " tests pass. _________"
end program
