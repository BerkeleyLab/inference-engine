program concurrent_inferences
  use string_m
  implicit none

  type tensor_t
    real, allocatable :: values(:)
  end type

  type inference_engine_t
    type(string_t), allocatable :: metadata_(:)
  end type

  type(tensor_t) outputs(1)
  type(inference_engine_t) inference_engine
  integer i

  inference_engine = inference_engine_t([string_t(""), string_t("")])
  do concurrent(i=1:size(outputs))
    outputs(i)%values = infer(inference_engine)           
  end do

contains
  pure function infer(self) 
    type(inference_engine_t), intent(in) :: self
    real infer(1)
    infer = [0.]
  end function
end program
