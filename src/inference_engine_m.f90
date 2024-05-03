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
    integer, allocatable :: nodes_(:)
  contains
    procedure :: infer
  end type

contains
  elemental function infer(self, inputs) result(outputs)
    class(inference_engine_t), intent(in) :: self
    type(tensor_t), intent(in) :: inputs
    type(tensor_t) outputs
    integer i
    outputs = tensor_t([(0., i=1,self%nodes_(ubound(self%nodes_,1)))])
  end function
end module
