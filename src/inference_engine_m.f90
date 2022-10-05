module inference_engine_m
  !! Define an abstraction that supports inference operationsn on a neural network
  implicit none

  private
  public :: inference_engine_t

  type inference_engine_t
    !! Encapsulate the minimal information needed to performance inference
    private
    real, allocatable :: input_weights_(:,:)
    real, allocatable :: hidden_weights_(:,:,:)
    real, allocatable :: output_weights_(:,:)
  contains
    generic :: inference_engine_t => read_weights
    procedure :: read_weights
  end type

  interface inference_engine_t

    pure module function construct(input_weights, output_weights, hidden_weights) result(inference_engine)
      implicit none
      real, intent(in), dimension(:,:) :: input_weights, output_weights
      real, intent(in) :: hidden_weights(:,:,:)
      type(inference_engine_t) inference_engine
    end function

  end interface

  interface

    module subroutine read_weights(self, file_name)
      implicit none
      class(inference_engine_t), intent(out) :: self
      character(len=*), intent(in) :: file_name
    end subroutine

    pure module function infer(self) result(output)
      implicit none
      class(inference_engine_t), intent(in) :: self
      real, allocatable :: output(:)
    end function

  end interface

end module inference_engine_m
