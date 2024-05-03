! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(inference_engine_m_) inference_engine_s
  implicit none
contains
  module procedure infer
    integer i
    outputs = tensor_t([(0., i=1,self%nodes_(ubound(self%nodes_,1)))])
  end procedure

  module procedure construct_from_padded_arrays
    inference_engine%metadata_ = metadata
    inference_engine%weights_ = weights
    inference_engine%biases_ = biases
    inference_engine%nodes_ = nodes
  end procedure 
end submodule
