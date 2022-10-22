module inference_strategy_m
  use activation_strategy_m, only : activation_strategy_t
  implicit none

  private
  public :: inference_strategy_t

  type, abstract :: inference_strategy_t
  contains
     procedure(infer_interface), nopass, deferred :: infer
  end type

  abstract interface

    pure function infer_interface( &
      input, input_weights, hidden_weights, biases, output_biases, output_weights, activation_strategy &
    ) result(output)
      import activation_strategy_t
      implicit none
      real, intent(in)  :: input(:)
      real, intent(in), allocatable :: input_weights(:,:)    !! weights applied to go from the inputs to first hidden layer
      real, intent(in), allocatable :: hidden_weights(:,:,:) !! weights applied to go from one hidden layer to the next
      real, intent(in), allocatable :: output_weights(:,:)   !! weights applied to go from the final hidden layer to the outputs
      real, intent(in), allocatable :: output_biases(:)      !! neuronal offsets applied to outputs
      real, intent(in), allocatable :: biases(:,:)           !! neuronal offsets for each hidden layer
      class(activation_strategy_t), intent(in) :: activation_strategy
      real, allocatable :: output(:)
    end function

  end interface

end module inference_strategy_m
