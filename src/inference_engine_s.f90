! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(inference_engine_m_) inference_engine_s
  use step_m, only : step_t
  implicit none
contains
  module procedure infer
    real(rkind), allocatable :: a(:,:)
    integer, parameter :: input_layer = 0
    integer k, l
    associate(w => self%weights_, b => self%biases_, n => self%nodes_, output_layer => ubound(self%nodes_,1))
      allocate(a(maxval(n), input_layer:output_layer))
      a(1:n(input_layer),input_layer) = inputs%values()
      feed_forward: &
      do l = input_layer+1, output_layer
        associate(z => matmul(w(1:n(l),1:n(l-1),l), a(1:n(l-1),l-1)) + b(1:n(l),l))
          a(1:n(l),l) = self%activation_strategy_%activation(z)
        end associate
      end do feed_forward
      outputs = tensor_t(a(1:n(output_layer), output_layer))
    end associate
  end procedure

  module procedure construct_from_padded_arrays
    inference_engine%metadata_ = metadata
    inference_engine%weights_ = weights
    inference_engine%biases_ = biases
    inference_engine%nodes_ = nodes
    inference_engine%activation_strategy_ = step_t()
  end procedure construct_from_padded_arrays

  module procedure num_inputs
    input_count = self%nodes_(lbound(self%nodes_,1))
  end procedure
end submodule inference_engine_s
