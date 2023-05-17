! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module trainable_engine_m
  !! Define an abstraction that supports training a neural network
  use inference_engine_m_, only : inference_engine_t
  use inference_strategy_m, only : inference_strategy_t
  use differentiable_activation_strategy_m, only : differentiable_activation_strategy_t
  use string_m, only : string_t
  use kind_parameters_m, only : rkind
  use inputs_m, only : inputs_t
  use expected_outputs_m, only : expected_outputs_t
  implicit none

  private
  public :: trainable_engine_t

  type, extends(inference_engine_t) :: trainable_engine_t
    !! Encapsulate the information needed to perform training
    private
    class(differentiable_activation_strategy_t), allocatable :: differentiable_activation_strategy_ 
  contains
    procedure :: train
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

  end interface

  interface

    module subroutine train(self, inputs, inference_strategy, expected_outputs)
      implicit none
      class(trainable_engine_t), intent(inout) :: self
      type(inputs_t), intent(in) :: inputs(:)
      class(inference_strategy_t), intent(in) :: inference_strategy
      type(expected_outputs_t), intent(in) :: expected_outputs(:)
    end subroutine

  end interface

end module trainable_engine_m
