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

  module procedure infer
    
    integer n, layer, m
    real neuron(size(self%input_weights_,1), 1 + size(self%hidden_weights_,3))

    associate(neurons_per_layer => size(neuron,1), first_layer => 1)
      do concurrent(n = 1:neurons_per_layer)
        neuron(n,first_layer) = self%activation_(dot_product(self%input_weights_(n,:), input(:)) + self%biases_(n,1))
      end do
      associate( num_layers => size(neuron,2))
        do layer = 2, num_layers
          do concurrent(n = 1:neurons_per_layer)
            neuron(n,layer) = &
              self%activation_(dot_product(self%hidden_weights_(n,:,layer-1), neuron(:,layer-1)) + self%biases_(n,layer))
          end do
        end do
        associate(num_outputs => size(self%output_weights_,1))
          allocate(output(num_outputs))
          do concurrent(m = 1:num_outputs)
            output(m) = self%activation_(dot_product(self%output_weights_(m,:), neuron(:,num_layers)) + self%biases_(m,num_layers))
          end do
        end associate
      end associate
    end associate

  end procedure

  module procedure read_network

    integer i, file_unit, io_status, array_size, line_length, last_opening_bracket, first_closing_bracket, num_inputs, io_status
    real, allocatable :: array(:)
    character(len=1) c
    character(len=:), allocatable :: line, unbracketed_line

    open(newunit=file_unit, file=file_name, form='formatted', status='old', iostat=io_status, action='read')
    call assert(io_status==0,"stat==0 in open")

    io_status = 0
    line_length = 1
    get_line_length: &
    do 
      read(file_unit, '(a)',advance='no',iostat=io_status) c
      if (io_status/=0) exit
      line_length = line_length + 1
    end do get_line_length

    allocate(character(len=line_length):: line)

    rewind(file_unit)

    num_inputs = 0
    get_num_inputs: &
    do 
      read(file_unit,'(a)', iostat=io_status) line
      if (io_status/=0) exit
      num_inputs = num_inputs + 1
      if (index(line, "]]", back=.true.) /= 0) exit
    end do get_num_inputs

    last_opening_bracket = index(line, "[", back=.true.)
    first_closing_bracket = index(line, "]")
    unbracketed_line = line(last_opening_bracket+1:first_closing_bracket-1)

    rewind(file_unit)

    io_status = 0
    array_size = 1
    get_num_elements: &
    do while (io_status==0)
      if (allocated(array)) deallocate(array)
      allocate(array(array_size))
      read(unbracketed_line, *, iostat=io_status) array
      array_size = array_size + 1
    end do get_num_elements
    array = array(:size(array)-1)

    allocate(self%input_weights_(num_inputs, size(array)))

    rewind(file_unit)

    get_input_weights: &
    do i = 1, num_inputs
      read(file_unit,'(a)', iostat=io_status) line
      call assert(io_status==0, "read_network: io_status == 0", io_status)
      last_opening_bracket = index(line, "[", back=.true.)
      first_closing_bracket = index(line, "]")
      unbracketed_line = line(last_opening_bracket+1:first_closing_bracket-1)
      read(unbracketed_line,*) self%input_weights_(i,:)
    end do get_input_weights

    close(file_unit)
  end procedure

end submodule inference_engine_s
