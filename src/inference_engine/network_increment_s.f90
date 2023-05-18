submodule(network_increment_m) network_increment_s
  use kind_parameters_m, only : rkind
  implicit none

contains

  module procedure construct
    network_increment%delta_w_in_ =  delta_w_in
    network_increment%delta_w_hidden_ =  delta_w_hidden
    network_increment%delta_w_out_ = delta_w_out
    network_increment%delta_b_ =  delta_b
    network_increment%delta_b_out_ =  delta_b_out 
  end procedure

  module procedure add
    total%delta_w_in_     = lhs%delta_w_in_     + rhs%delta_w_in_
    total%delta_w_hidden_ = lhs%delta_w_hidden_ + rhs%delta_w_hidden_
    total%delta_w_out_    = lhs%delta_w_out_    + rhs%delta_w_out_
    total%delta_b_        = lhs%delta_b_       + rhs%delta_b_
    total%delta_b_out_    = lhs%delta_b_out_   + rhs%delta_b_out_
  end procedure

  module procedure divide
    ratio%delta_w_in_     = numerator%delta_w_in_     / denominator
    ratio%delta_w_hidden_ = numerator%delta_w_hidden_ / denominator
    ratio%delta_w_out_    = numerator%delta_w_out_    / denominator
    ratio%delta_b_        = numerator%delta_b_        / denominator
    ratio%delta_b_out_    = numerator%delta_b_out_    / denominator
  end procedure

end submodule network_increment_s
