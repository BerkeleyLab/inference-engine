! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(matmul_m) matmul_s
  implicit none

contains

  module procedure infer
    
    real(rkind), allocatable, dimension(:,:) :: neuron, pre_activation_in
    real(rkind), allocatable, dimension(:) :: pre_activation_out

    associate(num_layers => size(biases,2))

      associate(neurons_per_layer => size(input_weights,1))
        allocate(neuron(neurons_per_layer, num_layers))
        allocate(pre_activation_in, mold=neuron)
      end associate

      block
        integer, parameter :: input_layer = 1
        pre_activation_in(:,input_layer) = matmul(input_weights(:,:), input(:)) + biases(:,input_layer)
        neuron(:,input_layer) = activation_strategy%activation(pre_activation_in(:,input_layer))
      end block
      block
        integer layer
        if (skip) then
          do layer = 2, num_layers
            pre_activation_in(:,layer) = matmul(hidden_weights(:,:,layer-1), neuron(:,layer-1)) + biases(:,layer)
            neuron(:,layer) = neuron(:,layer-1) + activation_strategy%activation(pre_activation_in(:,layer))
          end do
        else
          do layer = 2, num_layers
            pre_activation_in(:,layer) = matmul(hidden_weights(:,:,layer-1), neuron(:,layer-1)) +biases(:,layer)
            neuron(:,layer)= activation_strategy%activation(pre_activation_in(:,layer))
          end do
        end if
      end block
      pre_activation_out = matmul(output_weights(:,:), neuron(:,num_layers)) + output_biases(:)
      outputs = outputs_t(activation_strategy%activation(pre_activation_out), pre_activation_in, pre_activation_out)
    end associate

  end procedure

end submodule matmul_s
