! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(neural_net_m) neural_net_s
  use assert_m, only : assert
  use step_m, only : step_t
  use swish_m, only : swish_t
  use sigmoid_m, only : sigmoid_t
  use relu_m, only : relu_t
  implicit none

  interface assert_consistency
    procedure neural_net_consistency
    procedure difference_consistency
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

  pure subroutine neural_net_consistency(self)

    type(neural_net_t), intent(in) :: self

    integer, parameter :: input_layer=0

    associate( &
      all_allocated=>[allocated(self%weights_),allocated(self%biases_),allocated(self%nodes_),allocated(self%activation_strategy_)]&
    )   
      call assert(all(all_allocated),"neural_net_s(neural_net_consistency): fully_allocated")
        
    end associate

    associate(max_width=>maxval(self%nodes_), component_dims=>[size(self%biases_,1), size(self%weights_,1), size(self%weights_,2)])
      call assert(all(component_dims == max_width), "neural_net_s(neural_net_consistency): conformable arrays")
    end associate

    associate(input_subscript => lbound(self%nodes_,1))
      call assert(input_subscript == input_layer, "neural_net_s(neural_net_consistency): n base subsscript")
    end associate

  end subroutine

  pure subroutine difference_consistency(self)

    type(difference_t), intent(in) :: self

    integer, parameter :: input_layer=0

    associate( &
      all_allocated=>[allocated(self%weights_difference_),allocated(self%biases_difference_),allocated(self%nodes_difference_)] &
    )   
      call assert(all(all_allocated),"neural_net_s(difference_consistency): fully_allocated")
    end associate

    call assert(all(size(self%biases_difference_,1)==[size(self%weights_difference_,1), size(self%weights_difference_,2)]), &
      "neural_net_s(difference_consistency): conformable arrays" &
    )

  end subroutine

  pure subroutine set_activation_strategy(neural_net)
    type(neural_net_t), intent(inout) :: neural_net
    character(len=:), allocatable :: function_name
    ! This code is called in both constructors and and can't be refactored into a factory method
    ! pattern because the result would need to be allocatable and polymorphic, which would preclude
    ! the function being pure so it wouldn't be possible to call it from inside the pure constructor
    ! functions.
    function_name = neural_net%metadata_(findloc(key, "activationFunction", dim=1))%string_
    select case(function_name)
      case("swish")
        neural_net%activation_strategy_ = swish_t()
      case("sigmoid")
        neural_net%activation_strategy_ = sigmoid_t()
      case("step")
        neural_net%activation_strategy_ = step_t()
      case("relu")
        neural_net%activation_strategy_ = relu_t()
      case default
        error stop "neural_net_s(set_activation_strategy): unrecognized activation strategy '"//function_name//"'"
    end select
  end subroutine

  module procedure construct_from_padded_arrays

    neural_net%metadata_ = metadata
    neural_net%weights_ = weights
    neural_net%biases_ = biases
    neural_net%nodes_ = nodes

    block
      integer i

      if (present(input_range)) then
        neural_net%input_range_ = input_range
      else
        associate(num_inputs => nodes(lbound(nodes,1)))
          associate(default_minima => [(0., i=1,num_inputs)], default_maxima => [(1., i=1,num_inputs)])
            neural_net%input_range_ = tensor_range_t("inputs", default_minima, default_maxima)
          end associate
        end associate
      end if

      if (present(output_range)) then
        neural_net%output_range_ = output_range
      else
        associate(num_outputs => nodes(ubound(nodes,1)))
          associate(default_minima => [(0., i=1,num_outputs)], default_maxima => [(1., i=1,num_outputs)])
            neural_net%output_range_ = tensor_range_t("outputs", default_minima, default_maxima)
          end associate
        end associate
      end if
    end block

    call set_activation_strategy(neural_net)
    call assert_consistency(neural_net)

  end procedure construct_from_padded_arrays

  module procedure assert_conformable_with

    call assert_consistency(self)
    call assert_consistency(neural_net)

    associate(equal_shapes => [ &
      shape(self%weights_) == shape(neural_net%weights_), &
      shape(self%biases_) == shape(neural_net%biases_), &
      shape(self%nodes_) == shape(neural_net%nodes_)  &
     ])
      call assert(all(equal_shapes), "assert_conformable_with: all(equal_shapes)")
    end associate

    call assert(same_type_as(self%activation_strategy_, neural_net%activation_strategy_), "assert_conformable_with: types)")
    
  end procedure

  module procedure subtract

    call assert_consistency(self)
    call assert_consistency(rhs)
    call self%assert_conformable_with(rhs)

    block
      integer l

      allocate(difference%weights_difference_, mold = self%weights_)
      allocate(difference%biases_difference_, mold = self%biases_)
      allocate(difference%nodes_difference_, mold = self%nodes_)

      difference%weights_difference_ = 0.
      difference%biases_difference_ = 0.
      difference%nodes_difference_ = 0.

      l = 0
      difference%nodes_difference_(l)  = self%nodes_(l) - rhs%nodes_(l)
     
      associate(n => self%nodes_)
#ifndef __INTEL_COMPILER
        do concurrent(l = 1:ubound(n,1))
          difference%weights_difference_(1:n(l),1:n(l-1),l) = self%weights_(1:n(l),1:n(l-1),l) - rhs%weights_(1:n(l),1:n(l-1),l)
          difference%biases_difference_(1:n(l),l) = self%biases_(1:n(l),l) - rhs%biases_(1:n(l),l)
          difference%nodes_difference_(l) = self%nodes_(l) - rhs%nodes_(l)
        end do
#else
        block
          integer j, k
          do l = 1, ubound(n,1)
            do j = 1, n(l)
              do k = 1, n(l-1)
                difference%weights_difference_(j,k,l) = self%weights_(j,k,l) - rhs%weights_(j,k,l)
                difference%biases_difference_(j,l) = self%biases_(j,l) - rhs%biases_(j,l)
                difference%nodes_difference_(l) = self%nodes_(l) - rhs%nodes_(l)
              end do
            end do
          end do
        end block
#endif
      end associate

    end block

    call assert_consistency(difference)
  end procedure

  module procedure norm 
    norm_of_self = maxval([abs(self%weights_difference_), abs(self%biases_difference_), real(abs(self%nodes_difference_))])
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
    use_skip_connections = self%metadata_(findloc(key, "usingSkipConnections", dim=1))%string_ == "true"
  end procedure

  module procedure activation_function_name
    activation_name = self%metadata_(findloc(key, "activationFunction", dim=1))
  end procedure

end submodule neural_net_s
