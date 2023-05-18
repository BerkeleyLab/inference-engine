submodule(input_output_pair_m) input_output_pair_s
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

end submodule input_output_pair_s
