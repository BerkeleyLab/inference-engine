! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module inference_engine_m_
  !! Define an abstraction that supports inference operationsn on a neural network
  use sourcery_string_m, only : string_t
  use kind_parameters_m, only : rkind
  use tensor_m, only : tensor_t
  implicit none

  character(len=*), parameter :: key(*) = [character(len=len("usingSkipConnections")) :: &
    "modelName", "modelAuthor", "compilationDate", "activationFunction", "usingSkipConnections"]

  type inference_engine_t
    type(string_t) metadata_(size(key))
    real(rkind), allocatable :: weights_(:,:,:), biases_(:,:)
    integer, allocatable :: nodes_(:)
  contains
    procedure :: infer
  end type

  interface inference_engine_t
    pure module function construct_from_padded_arrays(metadata, weights, biases, nodes) &
      result(inference_engine)
      implicit none
      type(string_t), intent(in) :: metadata(:)
      real(rkind), intent(in) :: weights(:,:,:), biases(:,:)
      integer, intent(in) :: nodes(0:)
      type(inference_engine_t) inference_engine
    end function
  end interface

  interface
    elemental module function infer(self, inputs) result(outputs)
      implicit none
      class(inference_engine_t), intent(in) :: self
      type(tensor_t), intent(in) :: inputs
      type(tensor_t) outputs
    end function
  end interface

end module inference_engine_m_
