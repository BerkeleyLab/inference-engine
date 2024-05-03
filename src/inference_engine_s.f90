! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(inference_engine_m_) inference_engine_s
  use step_m, only : step_t
  use sourcery_formats_m, only : separated_values
  implicit none

contains

  module procedure map_to_input_range
    normalized_tensor = self%input_range_%map_to_training_range(tensor)
  end procedure

  module procedure map_from_output_range
    tensor = self%output_range_%map_from_training_range(normalized_tensor)
  end procedure

  module procedure infer

    real(rkind), allocatable :: a(:,:)
    integer, parameter :: input_layer = 0
    integer k, l

    associate(w => self%weights_, b => self%biases_, n => self%nodes_, output_layer => ubound(self%nodes_,1))

      allocate(a(maxval(n), input_layer:output_layer))

      associate(normalized_inputs => self%input_range_%map_to_training_range(inputs))
        a(1:n(input_layer),input_layer) = normalized_inputs%values()
      end associate

      feed_forward: &
      do l = input_layer+1, output_layer
        associate(z => matmul(w(1:n(l),1:n(l-1),l), a(1:n(l-1),l-1)) + b(1:n(l),l))
          a(1:n(l),l) = self%activation_strategy_%activation(z)
        end associate
      end do feed_forward

      associate(normalized_outputs => tensor_t(a(1:n(output_layer), output_layer)))
        outputs = self%output_range_%map_from_training_range(normalized_outputs)
      end associate

    end associate

  end procedure

  module procedure construct_from_padded_arrays

    inference_engine%metadata_ = metadata
    inference_engine%weights_ = weights
    inference_engine%biases_ = biases
    inference_engine%nodes_ = nodes

    block
      integer i

      if (present(input_range)) then
        inference_engine%input_range_ = input_range
      else
        associate(num_inputs => nodes(lbound(nodes,1)))
          associate(default_minima => [(0., i=1,num_inputs)], default_maxima => [(1., i=1,num_inputs)])
            inference_engine%input_range_ = tensor_range_t("inputs", default_minima, default_maxima)
          end associate
        end associate
      end if

      if (present(output_range)) then
        inference_engine%output_range_ = output_range
      else
        associate(num_outputs => nodes(ubound(nodes,1)))
          associate(default_minima => [(0., i=1,num_outputs)], default_maxima => [(1., i=1,num_outputs)])
            inference_engine%output_range_ = tensor_range_t("outputs", default_minima, default_maxima)
          end associate
        end associate
      end if
    end block

    inference_engine%activation_strategy_ = step_t()

  end procedure construct_from_padded_arrays

  module procedure num_inputs
    input_count = self%nodes_(lbound(self%nodes_,1))
  end procedure

end submodule inference_engine_s
