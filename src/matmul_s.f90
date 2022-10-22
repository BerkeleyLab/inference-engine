submodule(matmul_m) matmul_s
  implicit none

contains

  module procedure infer
    
    integer n, layer, m
    integer, parameter :: input_layer = 1
    real, allocatable :: neuron(:,:)

    associate(neurons_per_layer => size(input_weights,2), num_layers => size(biases,2))
      allocate(neuron(neurons_per_layer, num_layers))
      do concurrent(n = 1:neurons_per_layer)
        neuron(n,input_layer) = activation_strategy%activation(dot_product(input_weights(:,n), input(:)) + biases(n,input_layer))
      end do
      do layer = 2, num_layers
        do concurrent(n = 1:neurons_per_layer)
          neuron(n,layer) = &
            activation_strategy%activation(dot_product(hidden_weights(:,n,layer-1), neuron(:,layer-1)) + biases(n,layer))
        end do
      end do

      output = activation_strategy%activation(matmul(output_weights(:,:), neuron(:,num_layers)) + output_biases(:))
    end associate

  end procedure

end submodule matmul_s
