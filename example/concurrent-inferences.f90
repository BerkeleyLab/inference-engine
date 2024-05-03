program concurrent_inferences
  use inference_engine_m, only : inference_engine_t, infer, tensor_t
  use sourcery_string_m, only : string_t
  implicit none

  type(inference_engine_t) network, inference_engine
  type(tensor_t), allocatable :: inputs(:), outputs(:)
  real, allocatable :: input_components(:,:)
  integer, parameter :: nodes_per_layer(*) = [2, 3, 1]
  integer i

  inference_engine = inference_engine_t( &
    metadata_ = [string_t("XOR"), string_t("Damian Rouson"), string_t("2023-07-02"), string_t("step"), string_t("false")], &
    nodes_ = nodes_per_layer &
  )

  allocate(inputs(1))
  allocate(outputs(size(inputs)))
  allocate(input_components(1, inference_engine%nodes_(lbound(inference_engine%nodes_,1))))
  call random_number(input_components)

  do concurrent(i=1:size(inputs))
    inputs(i) = tensor_t(input_components(i,:))
  end do

  do concurrent(i=1:size(inputs))
    outputs(i) = inference_engine%infer(inputs(i))           
  end do

  do concurrent(i=1:size(inputs))
    outputs(i) = infer(inference_engine, inputs(i))           
  end do
end program
