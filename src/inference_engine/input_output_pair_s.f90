! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(input_output_pair_m) input_output_pair_s
  use assert_m, only : assert
  implicit none

contains

  module procedure construct
    input_output_pair%inputs_ = inputs
    input_output_pair%expected_outputs_ = expected_outputs
  end procedure

  module procedure inputs
    my_inputs = self%inputs_
  end procedure

  module procedure expected_outputs
    my_expected_outputs = self%expected_outputs_
  end procedure

  module procedure shuffle
    type(input_output_pair_t) temp
    real harvest(2:size(pairs))
    integer i, j

    call random_number(harvest)

    durstenfeld_shuffle: &
    do i = size(pairs), 2, -1
      j = 1 + int(harvest(i)*i)
      temp     = pairs(i) 
      pairs(i) = pairs(j)
      pairs(j) = temp
    end do durstenfeld_shuffle

  end procedure

end submodule input_output_pair_s
