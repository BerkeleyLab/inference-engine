program concurrent_inferences
  use inference_engine_m, only : inference_engine_t, infer, tensor_t, string_t
  implicit none

  type(inference_engine_t) inference_engine
  type(tensor_t) inputs(1)
  type(tensor_t) outputs(size(inputs))
  integer, parameter :: nodes(*) = [2, 3, 1]
  real input_components(size(inputs), nodes(size(nodes)))
  integer i

  inference_engine = inference_engine_t([string_t(""), string_t("")], nodes)
  call random_number(input_components)

  do concurrent(i=1:size(inputs))
    inputs(i) = tensor_t(input_components(i,:))
  end do

  do concurrent(i=1:size(inputs))
    outputs(i) = infer(inference_engine, inputs(i))           
  end do
end program
