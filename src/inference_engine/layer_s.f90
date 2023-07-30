! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(layer_m) layer_s
  use assert_m, only : assert
  !use sourcery_m, only : string_t
  use intrinsic_array_m, only : intrinsic_array_t
  implicit none

contains

  module procedure construct
    type(neuron_t), pointer ::  neuron 
    integer num_inputs, neurons_in_layer
    character(len=:), allocatable :: line
    logical hidden_layers, output_layer

    line = adjustl(layer_lines(start)%string())
    hidden_layers = line == '['
    output_layer = line == '"output_layer": ['
    call assert(hidden_layers .or. output_layer, "layer_t construct: layer start", line)

    layer%neuron = neuron_t(layer_lines, start+1)
    num_inputs = size(layer%neuron%weights())

    neuron => layer%neuron
    neurons_in_layer = 1
    do 
      if (.not. neuron%next_allocated()) exit
      neuron => neuron%next_pointer()
      call assert(size(neuron%weights()) == num_inputs, "layer_t construct: constant number of inputs")
      neurons_in_layer = neurons_in_layer + 1
    end do

    line = trim(adjustl(layer_lines(start+4*neurons_in_layer+1)%string()))
    call assert(line(1:1)==']', "read_layer_list: hidden layer end")

    if (line(len(line):len(line)) == ",") layer%next = construct(layer_lines, start+4*neurons_in_layer+2)

  end procedure

  module procedure inference_engine

    associate( &
      num_inputs => hidden_layers%count_inputs(), &
      num_outputs => output_layer%count_neurons(), &
      neurons_per_hidden_layer => hidden_layers%count_neurons(), &
      num_hidden_layers =>  hidden_layers%count_layers(), &
      num_output_layers => output_layer%count_layers() &
    )   
      call assert(num_output_layers==1, "inference_engine_s(construct_from_json): 1 output layer", num_output_layers)

      associate(nodes => [num_inputs, neurons_per_hidden_layer, num_outputs])
        associate(n_max => maxval(nodes))
          block
            real(rkind), allocatable :: weights(:,:,:), biases(:,:)
            type(layer_t), pointer :: layer_ptr
            type(neuron_t), pointer :: neuron_ptr
            integer j, l

            allocate(weights(n_max, n_max, num_hidden_layers + num_output_layers))
            allocate(biases(n_max, num_hidden_layers + num_output_layers))

            layer_ptr => hidden_layers
            l = 0 
            loop_over_hidden_Layers: &
            do  
              l = l + 1
              neuron_ptr => layer_ptr%neuron
              j = 0
              loop_over_hidden_neurons: &
              do  
                j = j + 1
                associate(w => neuron_ptr%weights())
                  weights(j,1:size(w,1),l) = w
                end associate
                biases(j,l) = neuron_ptr%bias()

                if (.not. neuron_ptr%next_allocated()) exit
                neuron_ptr => neuron_ptr%next_pointer()

              end do loop_over_hidden_neurons

              if (.not. allocated(layer_ptr%next)) exit
              layer_ptr => layer_ptr%next_pointer()

            end do loop_over_hidden_Layers

            layer_ptr => output_layer
            l = l + 1
            neuron_ptr => layer_ptr%neuron
            j = 0
            loop_over_output_neurons: &
            do  
              j = j + 1
              associate(w => neuron_ptr%weights())
                weights(j,1:size(w,1),l) = w
              end associate
              biases(j,l) = neuron_ptr%bias()

              if (.not. neuron_ptr%next_allocated()) exit
              neuron_ptr => neuron_ptr%next_pointer()

            end do loop_over_output_neurons

            inference_engine_ = inference_engine_t(metadata, weights, biases, nodes)
          end block
        end associate
      end associate
    end associate
    
  end procedure

  module procedure count_layers

    type(layer_t), pointer :: layer_ptr

    layer_ptr => layer
    num_layers = 1 
    do  
      if (.not. allocated(layer_ptr%next)) exit
      layer_ptr => layer_ptr%next
      num_layers = num_layers + 1 
    end do

  end procedure

  module procedure count_neurons

    type(layer_t), pointer :: layer_ptr
    type(neuron_t), pointer :: neuron_ptr
    integer num_neurons

    layer_ptr => layer

    allocate(neurons_per_layer(0))

    do  
      num_neurons = 1 
      neuron_ptr => layer_ptr%neuron
      do  
        if (.not. neuron_ptr%next_allocated()) exit
        neuron_ptr => neuron_ptr%next_pointer()
        num_neurons = num_neurons + 1 
      end do
      neurons_per_layer = [neurons_per_layer, num_neurons]
      if (.not. allocated(layer_ptr%next)) exit
      layer_ptr => layer_ptr%next
    end do
 
  end procedure

  module procedure count_inputs
    num_inputs = layer%neuron%num_inputs() ! assume fully connected input layer
  end procedure

  module procedure neurons_per_layer

    type(neuron_t), pointer ::  neuron 

    neuron => self%neuron
    num_neurons = 1
    do 
      if (.not. neuron%next_allocated()) exit
      neuron => neuron%next_pointer()
      num_neurons = num_neurons + 1
    end do

  end procedure

  module procedure next_allocated
    next_is_allocated = allocated(self%next)
  end procedure

  module procedure next_pointer
    next_ptr => self%next
  end procedure

end submodule layer_s
