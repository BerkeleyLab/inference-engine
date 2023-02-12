! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module concurrent_dot_products_m
  !! Perform inference using the dot_product intrinsic function inside `do concurrent` constructs
  !! to compute matrix-vector multiplies for forward information propagation from layer to layer
  use inference_strategy_m, only : inference_strategy_t
  use activation_strategy_m, only : activation_strategy_t
  use kind_parameters_m, only : rkind
  implicit none

  private
  public :: concurrent_dot_products_t

  type, extends(inference_strategy_t) :: concurrent_dot_products_t
  contains
    procedure, nopass :: infer
  end type

  interface

    pure module function infer( &
      input, input_weights, hidden_weights, biases, output_biases, output_weights, activation_strategy &
    ) result(output)
      implicit none
      real(rkind), intent(in) :: input(:)
      real(rkind), intent(in) :: input_weights(:,:)    !! weights applied to go from the inputs to first hidden layer
      real(rkind), intent(in) :: hidden_weights(:,:,:) !! weights applied to go from one hidden layer to the next
      real(rkind), intent(in) :: biases(:,:)           !! neuronal offsets for each hidden layer
      real(rkind), intent(in) :: output_biases(:)      !! neuronal offsets applied to outputs
      real(rkind), intent(in) :: output_weights(:,:)   !! weights applied to go from the final hidden layer to the outputs
      class(activation_strategy_t), intent(in) :: activation_strategy
      real(rkind), allocatable :: output(:)
    end function

  end interface

end module concurrent_dot_products_m
