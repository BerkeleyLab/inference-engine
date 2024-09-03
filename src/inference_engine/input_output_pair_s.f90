! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(input_output_pair_m) input_output_pair_s
  implicit none

contains

  module procedure default_real_construct
    input_output_pair%inputs_ = inputs
    input_output_pair%expected_outputs_ = expected_outputs
  end procedure

  module procedure double_precision_construct
    input_output_pair%inputs_ = inputs
    input_output_pair%expected_outputs_ = expected_outputs
  end procedure

  module procedure default_real_inputs
    my_inputs = self%inputs_
  end procedure

  module procedure double_precision_inputs
    my_inputs = self%inputs_
  end procedure

  module procedure default_real_expected_outputs
    my_expected_outputs = self%expected_outputs_
  end procedure

  module procedure double_precision_expected_outputs
    my_expected_outputs = self%expected_outputs_
  end procedure

  module procedure default_real_shuffle
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

  module procedure double_precision_shuffle
    type(input_output_pair_t(double_precision)) temp
    double precision harvest(2:size(pairs))
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

  module procedure default_real_write_to_stdout
    integer i
    do i = 1, size(input_output_pairs)
      print *, input_output_pairs(i)%inputs_%values(), " | ", input_output_pairs(i)%expected_outputs_%values()
    end do
  end procedure

  module procedure double_precision_write_to_stdout
    integer i
    do i = 1, size(input_output_pairs)
      print *, input_output_pairs(i)%inputs_%values(), " | ", input_output_pairs(i)%expected_outputs_%values()
    end do
  end procedure

end submodule input_output_pair_s
