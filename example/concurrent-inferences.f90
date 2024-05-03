! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program concurrent_inferences
  !! This program demonstrates how to read a neural network from a JSON file
  !! and use the network to perform concurrent inferences.
  use inference_engine_m_, only : inference_engine_t, infer
  use tensor_m, only : tensor_t
  use sourcery_m, only : string_t
  implicit none

  type(inference_engine_t) network, inference_engine
  type(tensor_t), allocatable :: inputs(:,:,:), outputs(:,:,:)
  real, allocatable :: input_components(:,:,:,:)
  integer, parameter :: lat=263, lon=317, lev=15 ! latitudes, longitudes, levels (elevations)
  integer i, j, k
  integer, parameter :: nodes_per_layer(*) = [2, 3, 1]
 integer, parameter :: max_n = maxval(nodes_per_layer), layers = size(nodes_per_layer)

  inference_engine = inference_engine_t( &
    metadata = [string_t("XOR"), string_t("Damian Rouson"), string_t("2023-07-02"), string_t("step"), string_t("false")], &
    weights = reshape([real:: [1,1,0, 0,1,1, 0,0,0], [1,0,0, -2,0,0, 1,0,0]], [max_n, max_n, layers-1]), &
    biases = reshape([[0.,-1.99,0.], [0., 0., 0.]], [max_n, layers-1]), &
    nodes = nodes_per_layer &
  )

  allocate(inputs(lat,lon,lev))
  allocate(outputs(lat,lon,lev))
  allocate(input_components(lat,lon,lev,inference_engine%num_inputs()))
  call random_number(input_components)

  do concurrent(i=1:lat, j=1:lon, k=1:lev)
    inputs(i,j,k) = tensor_t(input_components(i,j,k,:))
  end do

  do concurrent(i=1:lat, j=1:lon, k=1:lev)
    outputs(i,j,k) = inference_engine%infer(inputs(i,j,k))           
  end do

  do concurrent(i=1:lat, j=1:lon, k=1:lev)
    outputs(i,j,k) = infer(inference_engine, inputs(i,j,k))           
  end do

end program
