module inference_engine_m
  use sourcery_string_m, only : string_t
  implicit none

  character(len=*), parameter :: key(*) = [character(len=len("usingSkipConnections")) :: &
    "modelName", "modelAuthor", "compilationDate", "activationFunction", "usingSkipConnections"]

  type tensor_t
    real, allocatable :: values_(:)
  end type

  type inference_engine_t
    type(string_t) metadata_(size(key))
    real, allocatable :: weights_(:,:,:), biases_(:,:)
    integer, allocatable :: nodes_(:)
  contains
    procedure :: infer
  end type

  interface inference_engine_t
    module procedure construct_from_padded_arrays
  end interface
contains
  elemental function infer(self, inputs) result(outputs)
    class(inference_engine_t), intent(in) :: self
    type(tensor_t), intent(in) :: inputs
    type(tensor_t) outputs
    integer i
    outputs = tensor_t([(0., i=1,self%nodes_(ubound(self%nodes_,1)))])
  end function

  pure function construct_from_padded_arrays(metadata, weights, biases, nodes)   result(inference_engine)
    type(string_t), intent(in) :: metadata(:)
    real, intent(in) :: weights(:,:,:), biases(:,:)
    integer, intent(in) :: nodes(0:)
    type(inference_engine_t) inference_engine
    inference_engine%metadata_ = metadata
    inference_engine%weights_ = weights
    inference_engine%biases_ = biases
    inference_engine%nodes_ = nodes
  end function
end module
