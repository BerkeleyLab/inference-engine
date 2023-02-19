! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(matmul_m) matmul_s
  implicit none

contains

  module procedure infer
    
    real(rkind), allocatable :: neuron(:,:)

    associate(num_layers => size(biases,2))

      associate(neurons_per_layer => size(input_weights,1))
        allocate(neuron(neurons_per_layer, num_layers))
      end associate

      block
        integer, parameter :: input_layer = 1
        neuron(:,input_layer) = &
          activation_strategy%activation(matmul(input_weights(:,:), input(:)) + biases(:,input_layer))
      end block
      block
        integer layer
        if (skip) then
          do layer = 2, num_layers
            neuron(:,layer) = neuron(:,layer-1) + &
              activation_strategy%activation(matmul(hidden_weights(:,:,layer-1), neuron(:,layer-1)) + biases(:,layer))
          end do
        else
          do layer = 2, num_layers
            neuron(:,layer)= activation_strategy%activation(matmul(hidden_weights(:,:,layer-1), neuron(:,layer-1)) +biases(:,layer))
          end do
        end if
      end block
      output = activation_strategy%activation(matmul(output_weights(:,:), neuron(:,num_layers)) + output_biases(:))
    end associate

  end procedure

end submodule matmul_s
