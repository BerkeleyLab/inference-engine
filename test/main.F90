! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program main
  use inference_engine_test_m, only : inference_engine_test_t
  use asymmetric_engine_test_m, only : asymmetric_engine_test_t
  use trainable_engine_test_m, only : trainable_engine_test_t
  use metadata_test_m, only : metadata_test_t
  use hyperparameters_test_m, only : hyperparameters_test_t
  use network_configuration_test_m, only : network_configuration_test_t
  use training_configuration_test_m, only : training_configuration_test_t
  use tensor_map_test_m, only : tensor_map_test_t
  use tensor_test_m, only : tensor_test_t
  use julienne_m, only : command_line_t
  implicit none

  type(inference_engine_test_t) inference_engine_test
  type(asymmetric_engine_test_t) asymmetric_engine_test
  type(trainable_engine_test_t) trainable_engine_test
  type(hyperparameters_test_t) hyperparameters_test
  type(metadata_test_t) metadata_test
  type(network_configuration_test_t) network_configuration_test
  type(training_configuration_test_t) training_configuration_test
  type(tensor_map_test_t) tensor_map_test
  type(tensor_test_t) tensor_test
  real t_start, t_finish

  integer :: passes=0, tests=0

  print_usage_if_help_requested: &
  block
    type(command_line_t) command_line
    character(len=*), parameter :: usage = &
      new_line('') // new_line('') // &
      'Usage: fpm test -- [--help] | [--contains <substring>]' // &
      new_line('') // new_line('') // &
      'where square brackets ([]) denote optional arguments, a pipe (|) separates alternative arguments,' // new_line('') // &
      'angular brackets (<>) denote a user-provided value, and passing a substring limits execution to' // new_line('') // &
      'the tests with test subjects or test descriptions containing the user-specified substring.' // new_line('')
    if (command_line%argument_present([character(len=len("--help"))::"--help","-h"])) stop usage
  end block print_usage_if_help_requested

  call cpu_time(t_start)
  call random_init(repeatable=.true.,image_distinct=.true.)
  call hyperparameters_test%report(passes, tests)
  call network_configuration_test%report(passes, tests)
  call metadata_test%report(passes, tests)
  call training_configuration_test%report(passes, tests)
  call tensor_map_test%report(passes, tests)
  call tensor_test%report(passes, tests)
  call asymmetric_engine_test%report(passes, tests)
  call inference_engine_test%report(passes, tests)
  call trainable_engine_test%report(passes, tests)
  call cpu_time(t_finish)

  print *
  print *,"Test suite execution time: ",t_finish - t_start
  print *
  print '(*(a,:,g0))',"_________ In total, ",passes," of ",tests, " tests pass. _________"
#ifndef __flang__
  sync all
#endif
  print *
  if (passes/=tests) error stop "-------- One or more tests failed. See the above report. ---------"
end program
