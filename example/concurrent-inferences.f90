program concurrent_inferences
  use inference_engine_m, only : inference_engine_t, infer, tensor_t, string_t
  implicit none
  type(inference_engine_t) inference_engine
  type(tensor_t) outputs(1)
  integer i
  inference_engine = inference_engine_t([string_t(""), string_t("")])
  do concurrent(i=1:size(outputs))
    outputs(i) = infer(inference_engine)           
  end do
end program
