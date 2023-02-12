! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module inference_strategy_m
  use activation_strategy_m, only : activation_strategy_t
  use kind_parameters_m, only : rkind
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
      import activation_strategy_t, rkind
      implicit none
      real(rkind), intent(in)  :: input(:)
      real(rkind), intent(in) :: input_weights(:,:)    !! weights applied to go from the inputs to first hidden layer
      real(rkind), intent(in) :: hidden_weights(:,:,:) !! weights applied to go from one hidden layer to the next
      real(rkind), intent(in) :: output_weights(:,:)   !! weights applied to go from the final hidden layer to the outputs
      real(rkind), intent(in) :: output_biases(:)      !! neuronal offsets applied to outputs
      real(rkind), intent(in) :: biases(:,:)           !! neuronal offsets for each hidden layer
      class(activation_strategy_t), intent(in) :: activation_strategy
      real(rkind), allocatable :: output(:)
    end function

  end interface

end module inference_strategy_m
