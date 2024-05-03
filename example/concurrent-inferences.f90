! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program concurrent_inferences
  !! This program demonstrates how to read a neural network from a JSON file
  !! and use the network to perform concurrent inferences.
  use inference_engine_m_, only : inference_engine_t, infer, tensor_t
  use sourcery_m, only : string_t
  implicit none

  type(inference_engine_t) network, inference_engine
  type(tensor_t), allocatable :: inputs(:), outputs(:)
  real, allocatable :: input_components(:,:)
  integer i
  integer, parameter :: nodes_per_layer(*) = [2, 3, 1]
  integer, parameter :: max_n = maxval(nodes_per_layer), layers = size(nodes_per_layer)

  inference_engine = inference_engine_t( &
    metadata = [string_t("XOR"), string_t("Damian Rouson"), string_t("2023-07-02"), string_t("step"), string_t("false")], &
    weights = reshape([real:: [1,1,0, 0,1,1, 0,0,0], [1,0,0, -2,0,0, 1,0,0]], [max_n, max_n, layers-1]), &
    biases = reshape([[0.,-1.99,0.], [0., 0., 0.]], [max_n, layers-1]), &
    nodes = nodes_per_layer &
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
