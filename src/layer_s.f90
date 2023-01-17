! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(layer_m) layer_s
  use assert_m, only : assert
  implicit none

contains

  module procedure construct

    type(neuron_t), pointer ::  neuron 
    integer num_inputs, neurons_in_layer
    character(len=:), allocatable :: line

    call assert(adjustl(layer_lines(start)%string())=='[', "layer_t construct: layer start", layer_lines(start)%string())

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

  module procedure count_layers

    type(layer_t), pointer :: layer_ptr

    layer_ptr => layer
    num_layers = 1 
    do  
      if (.not. allocated(layer_ptr%next)) exit
      layer_ptr => layer%next
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
      layer_ptr => layer%next
    end do
 
  end procedure

end submodule layer_s
