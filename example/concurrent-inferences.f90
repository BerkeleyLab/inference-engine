program concurrent_inferences
  use inference_engine_m
  implicit none
  type tensor_t
    real, allocatable :: values(:)
  end type
  type(tensor_t) outputs(1)
  type(inference_engine_t) inference_engine
  integer i
  inference_engine = inference_engine_t([string_t(""), string_t("")])
  do concurrent(i=1:size(outputs))
    outputs(i)%values = infer(inference_engine)           
  end do
end program
