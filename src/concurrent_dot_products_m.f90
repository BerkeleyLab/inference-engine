module concurrent_dot_products_m
  !! Perform inference using the dot_product intrinsic function inside `do concurrent` constructs
  !! to compute matrix-vector multiplies for forward information propagation from layer to layer
  use inference_strategy_m, only : inference_strategy_t
  use activation_strategy_m, only : activation_strategy_t
  implicit none

  private
  public :: concurrent_dot_products_t

  type, extends(inference_strategy_t) :: concurrent_dot_products_t
  contains
    procedure, nopass :: infer
  end type

  interface

    pure module function infer(neurons_per_layer, num_layers, num_outputs, &
      input, input_weights, hidden_weights, biases, output_biases, output_weights, activation_strategy) result(output)
      implicit none
      integer, intent(in) :: neurons_per_layer, num_layers, num_outputs
      real, intent(in)  :: input(:)
      real, intent(in), allocatable :: input_weights(:,:)    !! weights applied to go from the inputs to first hidden layer
      real, intent(in), allocatable :: hidden_weights(:,:,:) !! weights applied to go from one hidden layer to the next
      real, intent(in), allocatable :: biases(:,:)           !! neuronal offsets for each hidden layer
      real, intent(in), allocatable :: output_biases(:)      !! neuronal offsets applied to outputs
      real, intent(in), allocatable :: output_weights(:,:)   !! weights applied to go from the final hidden layer to the outputs
      class(activation_strategy_t), intent(in) :: activation_strategy
      real, allocatable :: output(:)
    end function

  end interface

end module concurrent_dot_products_m
