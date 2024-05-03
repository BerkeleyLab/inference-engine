module inference_engine_m
  implicit none

  type string_t
    character(len=:), allocatable :: string_
  contains
    generic :: array => real_array
    procedure real_array
  end type

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

  function real_array(self)
    class(string_t), intent(in) :: self
    real real_array(1)
    real_array = 0.
  end function

end module
