! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(inference_engine_m_) inference_engine_s
  use assert_m, only : assert, intrinsic_array_t
  use step_m, only : step_t
  use swish_m, only : swish_t
  use sigmoid_m, only : sigmoid_t
  use relu_m, only : relu_t
  use layer_m, only : layer_t
  use neuron_m, only : neuron_t
  use sourcery_formats_m, only : separated_values
  implicit none

  interface assert_consistency
    procedure inference_engine_consistency
  end interface

contains

  module procedure map_to_input_range
    normalized_tensor = self%input_range_%map_to_training_range(tensor)
  end procedure

  module procedure map_from_output_range
    tensor = self%output_range_%map_from_training_range(normalized_tensor)
  end procedure

  module procedure to_exchange
    exchange%input_range_ = self%input_range_
    exchange%output_range_ = self%output_range_
    exchange%metadata_ = self%metadata_
    exchange%weights_ = self%weights_
    exchange%biases_ = self%biases_
    exchange%nodes_ = self%nodes_
    exchange%activation_strategy_ = self%activation_strategy_ 
  end procedure

  module procedure infer

    real(rkind), allocatable :: a(:,:)
    integer, parameter :: input_layer = 0
    integer k, l

    call assert_consistency(self)

    associate(w => self%weights_, b => self%biases_, n => self%nodes_, output_layer => ubound(self%nodes_,1))

      allocate(a(maxval(n), input_layer:output_layer))

#ifndef _CRAYFTN
      associate(normalized_inputs => self%input_range_%map_to_training_range(inputs))
        a(1:n(input_layer),input_layer) = normalized_inputs%values()
      end associate
#else
      block
        type(tensor_t) normalized_inputs
        normalized_inputs = self%input_range_%map_to_training_range(inputs)
        a(1:n(input_layer),input_layer) = normalized_inputs%values()
      end block
#endif

      feed_forward: &
      do l = input_layer+1, output_layer
        associate(z => matmul(w(1:n(l),1:n(l-1),l), a(1:n(l-1),l-1)) + b(1:n(l),l))
          a(1:n(l),l) = self%activation_strategy_%activation(z)
        end associate
      end do feed_forward

#ifdef _CRAYFTN
      block
        type(tensor_t) :: normalized_outputs
        normalized_outputs = tensor_t(a(1:n(output_layer), output_layer))
#else
      associate(normalized_outputs => tensor_t(a(1:n(output_layer), output_layer)))
#endif
        outputs = self%output_range_%map_from_training_range(normalized_outputs)
#ifdef _CRAYFTN
      end block
#else
      end associate
#endif

    end associate

  end procedure

  pure subroutine inference_engine_consistency(self)

    type(inference_engine_t), intent(in) :: self

    integer, parameter :: input_layer=0

    associate( &
      all_allocated=>[allocated(self%weights_),allocated(self%biases_),allocated(self%nodes_),allocated(self%activation_strategy_)]&
    )   
      call assert(all(all_allocated),"inference_engine_s(inference_engine_consistency): fully_allocated", &
        intrinsic_array_t(all_allocated))
    end associate

    associate(max_width=>maxval(self%nodes_), component_dims=>[size(self%biases_,1), size(self%weights_,1), size(self%weights_,2)])
      call assert(all(component_dims == max_width), "inference_engine_s(inference_engine_consistency): conformable arrays", &
        intrinsic_array_t([max_width,component_dims]))
    end associate

    associate(input_subscript => lbound(self%nodes_,1))
      call assert(input_subscript == input_layer, "inference_engine_s(inference_engine_consistency): n base subsscript", &
        input_subscript)
    end associate

  end subroutine

  pure subroutine set_activation_strategy(inference_engine)
    type(inference_engine_t), intent(inout) :: inference_engine
    character(len=:), allocatable :: function_name
    ! This code is called in both constructors and and can't be refactored into a factory method
    ! pattern because the result would need to be allocatable and polymorphic, which would preclude
    ! the function being pure so it wouldn't be possible to call it from inside the pure constructor
    ! functions.
    function_name = inference_engine%metadata_(findloc(key, "activationFunction", dim=1))%string()
    select case(function_name)
      case("swish")
        inference_engine%activation_strategy_ = swish_t()
      case("sigmoid")
        inference_engine%activation_strategy_ = sigmoid_t()
      case("step")
        inference_engine%activation_strategy_ = step_t()
      case("relu")
        inference_engine%activation_strategy_ = relu_t()
      case default
        error stop "inference_engine_s(set_activation_strategy): unrecognized activation strategy '"//function_name//"'"
    end select
  end subroutine

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

    call set_activation_strategy(inference_engine)
    call assert_consistency(inference_engine)

  end procedure construct_from_padded_arrays

  module procedure assert_conformable_with

    call assert_consistency(self)
    call assert_consistency(inference_engine)

    associate(equal_shapes => [ &
      shape(self%weights_) == shape(inference_engine%weights_), &
      shape(self%biases_) == shape(inference_engine%biases_), &
      shape(self%nodes_) == shape(inference_engine%nodes_)  &
     ])
      call assert(all(equal_shapes), "assert_conformable_with: all(equal_shapes)", intrinsic_array_t(equal_shapes))
    end associate

    call assert(same_type_as(self%activation_strategy_, inference_engine%activation_strategy_), "assert_conformable_with: types)")
    
  end procedure

  module procedure num_outputs
    call assert_consistency(self)
    output_count = self%nodes_(ubound(self%nodes_,1))
  end procedure

  module procedure num_inputs
    call assert_consistency(self)
    input_count = self%nodes_(lbound(self%nodes_,1))
  end procedure

  module procedure nodes_per_layer
    call assert_consistency(self)
    node_count = self%nodes_
  end procedure

  module procedure skip
    use_skip_connections = self%metadata_(findloc(key, "usingSkipConnections", dim=1))%string() == "true"
  end procedure

  module procedure activation_function_name
    activation_name = self%metadata_(findloc(key, "activationFunction", dim=1))
  end procedure

end submodule inference_engine_s
