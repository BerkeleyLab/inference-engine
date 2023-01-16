! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(layer_m) layer_s
  use assert_m, only : assert
  implicit none

contains

  module procedure read_layer_list

    type(neuron_t), pointer ::  neuron 
    integer num_inputs, neurons_in_layer
    character(len=:), allocatable :: line

    call assert(adjustl(layer_lines(start)%string())=='[', "read_json: layer start", layer_lines(start)%string())

    layer%neuron = read_neuron_list(layer_lines, start+1)
    num_inputs = size(layer%neuron%weights())

    neuron => layer%neuron
    neurons_in_layer = 1
    do 
      if (.not. allocated(neuron%next)) exit
      neuron => neuron%next
      call assert(size(neuron%weights()) == num_inputs, "read_json: constant number of inputs") 
      neurons_in_layer = neurons_in_layer + 1
    end do

    line = trim(adjustl(layer_lines(start+4*neurons_in_layer+1)%string()))
    call assert(line(1:1)==']', "read_json: hidden layer end")

    if (line(len(line):len(line)) == ",") layer%next = read_layer_list(layer_lines, start+4*neurons_in_layer+2)

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
        if (.not. allocated(neuron_ptr%next)) exit
        neuron_ptr => neuron_ptr%next
        num_neurons = num_neurons + 1 
      end do
      neurons_per_layer = [neurons_per_layer, num_neurons]
      if (.not. allocated(layer_ptr%next)) exit
      layer_ptr => layer%next
    end do
 

  end procedure

  module procedure read_neuron_list

    character(len=:), allocatable :: line
    integer i

    call assert(adjustl(neuron_lines(start)%string())=='{', "read_json: neuron object start", neuron_lines(start)%string())

    line = neuron_lines(start+1)%string()
    associate(colon => index(line, ":"))
      call assert(adjustl(line(:colon-1))=='"weights"', "read_json: neuron weights", line)
      associate(opening_bracket => colon + index(line(colon+1:), "["))
        associate(closing_bracket => opening_bracket + index(line(opening_bracket+1:), "]"))
          associate(commas => count("," == [(line(i:i), i=opening_bracket+1,closing_bracket-1)]))
            associate(num_inputs => commas + 1)
              allocate(neuron%weights_(num_inputs))
              read(line(opening_bracket+1:closing_bracket-1), fmt=*) neuron%weights_
            end associate
          end associate
        end associate
      end associate
    end associate

    line = neuron_lines(start+2)%string()
    associate(colon => index(line, ":"))
      call assert(adjustl(line(:colon-1))=='"bias"', "read_json: neuron bias", line)
      read(line(colon+1:), fmt=*) neuron%bias_
    end associate

    line = adjustl(neuron_lines(start+3)%string())
    call assert(line(1:1)=='}', "read_json: neuron object end", line)
    line = adjustr(neuron_lines(start+3)%string())
    if (line(len(line):len(line)) == ",") neuron%next = read_neuron_list(neuron_lines, start+4)

  end procedure

  module procedure weights
    my_weights = self%weights_
  end procedure

  module procedure bias
    my_bias = self%bias_
  end procedure

end submodule layer_s
