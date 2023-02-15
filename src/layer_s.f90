! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(layer_m) layer_s
  use assert_m, only : assert
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

  module procedure input_weights

    type(neuron_t), pointer :: neuron
    integer i

    associate(num_inputs => self%neuron%num_inputs(), neurons_per_layer => self%neurons_per_layer())

      allocate(weights(num_inputs, neurons_per_layer))

      neuron => self%neuron
      weights(:,1) = neuron%weights()

      do i = 2, neurons_per_layer - 1
        call assert(neuron%next_allocated(), "layer_t%input_weights: neuron%next_allocated()")
        neuron => neuron%next_pointer()
        weights(:,i) = neuron%weights()
        call assert(neuron%num_inputs() == num_inputs, "layer_t%input_weights: constant number of inputs")
      end do
      neuron => neuron%next_pointer()
      call assert(.not. neuron%next_allocated(), "layer_t%input_weights: .not. neuron%next_allocated()")
      if (neurons_per_layer /= 1) weights(:,neurons_per_layer) = neuron%weights()

    end associate

  end procedure

  module procedure hidden_weights

    type(neuron_t), pointer :: neuron
    type(layer_t), pointer :: layer
    integer n, l

    associate( &
      num_inputs => self%neuron%num_inputs(), neurons_per_layer => self%neurons_per_layer(), num_layers => self%count_layers())

      allocate(weights(num_inputs, neurons_per_layer, num_layers))

      layer => self

      loop_over_layers: &
      do l = 1, num_layers

        neuron => layer%neuron
        weights(:,1,l) = neuron%weights()

        loop_over_neurons: &
        do n = 2, neurons_per_layer - 1
          call assert(neuron%next_allocated(), "layer_t%hidden_weights: neuron%next_allocated()")
          neuron => neuron%next_pointer()
          weights(:,n,l) = neuron%weights()
          call assert(neuron%num_inputs() == num_inputs, "layer_t%hidden_weights: constant number of inputs", &
            intrinsic_array_t([num_inputs, neuron%num_inputs(), l, n]))
        end do loop_over_neurons

        call assert(neuron%next_allocated(), "layer_t%hidden_weights: neuron%next_allocated()")
        neuron => neuron%next_pointer()
        if (neurons_per_layer /= 1) weights(:,neurons_per_layer,l) = neuron%weights() ! avoid redundant assignment

        if (l/=num_layers) then
          layer => layer%next
        else
          call assert(.not. layer%next_allocated(), "layer_t%hidden_weights: .not. layer%next_allocated()")
        end if

      end do loop_over_Layers

    end associate

  end procedure

  module procedure output_weights

    type(neuron_t), pointer :: neuron
    integer n

    associate(num_outputs => self%neurons_per_layer(), neurons_per_hidden_layer => self%neuron%num_inputs())

      neuron => self%neuron
      allocate(weights(num_outputs, neurons_per_hidden_layer))
      weights(1,:) = neuron%weights()

      loop_over_output_neurons: &
      do n = 2, num_outputs - 1
        call assert(neuron%next_allocated(), "layer_t%output_weights: neuron%next_allocated()")
        neuron => neuron%next_pointer()
        weights(n,:) = neuron%weights()
        call assert(neuron%num_inputs() == neurons_per_hidden_layer, "layer_t%output_weights: constant number of inputs")
      end do loop_over_output_neurons

      if (num_outputs > 1) then
        call assert(neuron%next_allocated(), "layer_t%output_weights: neuron%next_allocated()")
        neuron => neuron%next_pointer()
        weights(num_outputs,:) = neuron%weights() ! avoid redundant assignment
        call assert(.not. self%next_allocated(), "layer_t%output_weights: .not. layer%next_allocated()")
      end if

    end associate

  end procedure

  module procedure output_biases

    type(neuron_t), pointer :: neuron
    integer n

    associate(num_outputs => self%neurons_per_layer())

      neuron => self%neuron
      allocate(biases(num_outputs))
      biases(1) = neuron%bias()

      loop_over_output_neurons: &
      do n = 2, num_outputs - 1
        call assert(neuron%next_allocated(), "layer_t%output_biases: neuron%next_allocated()")
        neuron => neuron%next_pointer()
        biases(n) = neuron%bias()
      end do loop_over_output_neurons

      if (num_outputs > 1) then
        call assert(neuron%next_allocated(), "layer_t%output_biases: neuron%next_allocated()")
        neuron => neuron%next_pointer()
        biases(num_outputs) = neuron%bias() ! avoid redundant assignment
        call assert(.not. self%next_allocated(), "layer_t%output_biases: .not. layer%next_allocated()")
      end if

    end associate

  end procedure

  module procedure hidden_biases

    type(neuron_t), pointer :: neuron
    type(layer_t), pointer :: layer
    integer n, l

    associate(neurons_per_layer => self%neurons_per_layer(), num_layers => self%count_layers())

      allocate(biases(neurons_per_layer, num_layers))

      layer => self

      loop_over_layers: &
      do l = 1, num_layers

        neuron => layer%neuron
        biases(1,l) = neuron%bias()

        loop_over_neurons: &
        do n = 2, neurons_per_layer - 1
          call assert(neuron%next_allocated(), "layer_t%hidden_biases: neuron%next_allocated()", intrinsic_array_t([l,n]))
          neuron => neuron%next_pointer()
          biases(n,l) = neuron%bias()
        end do loop_over_neurons

        call assert(neuron%next_allocated(), "layer_t%hidden_biases: neuron%next_allocated()", &
          intrinsic_array_t([l,neurons_per_layer]))
        neuron => neuron%next_pointer()
        call assert(.not. neuron%next_allocated(), "layer_t%hidden_biases: .not. neuron%next_allocated()", &
          intrinsic_array_t([l,neurons_per_layer]))
        if (neurons_per_layer /= 1) biases(neurons_per_layer,l) = neuron%bias() ! avoid redundant assignment

        if (l/=num_layers) then
          layer => layer%next
        else
          call assert(.not. layer%next_allocated(), "layer_t%hidden_biases: .not. layer%next_allocated()")
        end if

      end do loop_over_layers

    end associate


  end procedure hidden_biases

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
