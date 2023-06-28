! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module trainable_engine_m
  !! Define an abstraction that supports training a neural network
  use inference_engine_m_, only : inference_engine_t
  use inference_strategy_m, only : inference_strategy_t
  use outputs_m, only : outputs_t
  use differentiable_activation_strategy_m, only : differentiable_activation_strategy_t
  use string_m, only : string_t
  use kind_parameters_m, only : rkind
  use inputs_m, only : inputs_t
  use expected_outputs_m, only : expected_outputs_t
  use mini_batch_m, only : mini_batch_t
  implicit none

  private
  public :: trainable_engine_t

  type, extends(inference_engine_t) :: trainable_engine_t
    !! Encapsulate the information needed to perform training
    private
    real(rkind), allocatable :: w(:,:,:) ! weights
    real(rkind), allocatable :: b(:,:) ! biases
    integer, allocatable :: n(:) ! nuerons per layer
    class(differentiable_activation_strategy_t), allocatable :: differentiable_activation_strategy_ 
  contains
    procedure :: train_single_hidden_layer
    procedure :: train_deep_network
    procedure :: infer_from_inputs_object_
    generic :: train => train_deep_network, train_single_hidden_layer
  end type

  interface trainable_engine_t

    pure module function construct_trainable_engine( &
      metadata, input_weights, hidden_weights, output_weights, biases, output_biases, differentiable_activation_strategy &
    ) &
    result(trainable_engine)
      implicit none
      type(string_t), intent(in) :: metadata(:)
      real(rkind), intent(in), dimension(:,:) :: input_weights, output_weights, biases
      real(rkind), intent(in) :: hidden_weights(:,:,:), output_biases(:)
      class(differentiable_activation_strategy_t), intent(in) :: differentiable_activation_strategy
      type(trainable_engine_t) trainable_engine
    end function

    pure module function construct_from_padded_arrays(nodes, weights, biases, differentiable_activation_strategy, metadata) &
    result(trainable_engine)
      implicit none
      integer, intent(in) :: nodes(0:)
      real(rkind), intent(in)  :: weights(:,:,:), biases(:,:)
      class(differentiable_activation_strategy_t), intent(in) :: differentiable_activation_strategy
      type(string_t), intent(in) :: metadata(:)
      type(trainable_engine_t) trainable_engine
 
    end function

  end interface

  interface

    pure module subroutine train_single_hidden_layer(self, mini_batch, inference_strategy)
      implicit none
      class(trainable_engine_t), intent(inout) :: self
      type(mini_batch_t), intent(in) :: mini_batch(:)
      class(inference_strategy_t), intent(in) :: inference_strategy
    end subroutine

    pure module subroutine train_deep_network(self, mini_batches)
      implicit none
      class(trainable_engine_t), intent(inout) :: self
      type(mini_batch_t), intent(in) :: mini_batches(:)
    end subroutine

    elemental module function infer_from_inputs_object_(self, inputs) result(outputs)
      implicit none
      class(trainable_engine_t), intent(in) :: self
      type(inputs_t), intent(in) :: inputs
      type(outputs_t) outputs
    end function
    
  end interface

end module trainable_engine_m
