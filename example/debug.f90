! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program debug
  use inference_engine_m, only : inference_engine_t
  use string_m, only : string_t
  use file_m, only : file_t
  use kind_parameters_m, only : rkind
  implicit none

  type(inference_engine_t) random_engine, from_json
  type(file_t) to_json, round_trip

  random_engine = distinct_parameters()
  to_json = random_engine%to_json()
  from_json = inference_engine_t(to_json)
  round_trip = from_json%to_json()

  call to_json%write_lines(string_t("to.json"))
  call round_trip%write_lines(string_t("from.json"))

contains

  function distinct_parameters() result(inference_engine)
    type(inference_engine_t) inference_engine
    integer, parameter :: inputs = 2, hidden = 3, outputs = 1 ! number of neurons in input, output, and hidden layers
    integer, parameter :: n(*) = [inputs, hidden, hidden, outputs]    ! nodes per layer
    integer, parameter :: n_max = maxval(n), layers=size(n)   ! max layer width, number of layers
    integer, parameter :: w_shape(*) = [n_max, n_max, layers-1], b_shape(*) = [n_max, n_max]
    integer i
    real(rkind), allocatable :: w(:,:,:), b(:,:)

    w = reshape( [(i, i=1,product(w_shape))], w_shape)
    b = reshape( [(maxval(w) + i, i=1,product(b_shape))], b_shape)

    inference_engine = inference_engine_t( &
      metadata = [string_t("random"), string_t("Damian Rouson"), string_t("2023-07-15"), string_t("sigmoid"), string_t("false")], &
      weights = w, biases = b, nodes = n &
    )   
  end function

end program
