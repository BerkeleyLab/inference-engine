submodule(inference_engine_m) inference_engine_s
  use assert_m, only : assert
  implicit none

contains

  module procedure construct
    inference_engine%input_weights_ = input_weights
    inference_engine%hidden_weights_ = hidden_weights
    inference_engine%output_weights_ = output_weights
    inference_engine%biases_ = biases
    inference_engine%activation_ => activation
  end procedure

  module procedure read_weights

    integer file_unit, num_inputs, num_nodes, num_layers, num_outputs
    integer i, j, stat

    open(newunit=file_unit, file=file_name, form='formatted', status='old', iostat=stat, action='read')    
    call assert(stat==0,"stat==0 in open")

    read(file_unit,*,iostat=stat) num_inputs, num_nodes, num_layers, num_outputs
    call assert(stat==0,"stat==0 in read(...) num_inputs,...")

    allocate( &
       self%input_weights_(num_inputs, num_nodes), &
       self%hidden_weights_(num_nodes, num_nodes, num_layers), &
       self%output_weights_(num_outputs, num_nodes) &
    )

    do i = 1,size(self%input_weights_,1)
      read(file_unit, *, iostat=stat) self%input_weights_(i,:)
      call assert(stat==0,"stat==0 in input weights read")
    end do

    do i = 1,size(self%hidden_weights_,1)
      do j = 1,size(self%hidden_weights_,3)
        read(file_unit, *, iostat=stat) self%hidden_weights_(i,:,j)
        call assert(stat==0,"stat==0 in hidden weights read")
      end do
    end do

    do i = 1, size(self%output_weights_,2)
      read(file_unit, *, iostat=stat) self%output_weights_(:,i)
      call assert(stat==0,"stat==0 in output weights read")
    end do

    close (file_unit)

  end procedure

  module procedure infer
    
    integer n, layer, m
    real neuron(size(self%input_weights_,1), 1 + size(self%hidden_weights_,3))

    associate(neurons_per_layer => size(neuron,1), first_layer => 1)
      do concurrent(n = 1:neurons_per_layer)
        neuron(n,first_layer) = self%activation_(dot_product(self%input_weights_(n,:), input(:)))
      end do
      associate( num_layers => size(neuron,2))
        do layer = 2, num_layers
          do concurrent(n = 1:neurons_per_layer)
           neuron(n,layer) = self%activation_(dot_product(self%hidden_weights_(n,:,layer-1), neuron(:,layer-1)))
          end do
        end do
        associate( num_outputs => size(self%output_weights_,1))
          allocate(output(num_outputs))
          do concurrent(m = 1:num_outputs)
            output(m) = self%activation_(dot_product(self%output_weights_(m,:), neuron(:,num_layers)))
          end do
        end associate
      end associate
    end associate

  end procedure

end submodule inference_engine_s
