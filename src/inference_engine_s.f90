submodule(inference_engine_m) inference_engine_s
  use assert_m, only : assert
  use intrinsic_array_m, only : intrinsic_array_t
  implicit none

contains

  module procedure construct
    inference_engine%input_weights_ = input_weights
    inference_engine%hidden_weights_ = hidden_weights
    inference_engine%output_weights_ = output_weights
    inference_engine%biases_ = biases
    inference_engine%activation_ => activation
  end procedure

  pure module subroutine assert_consistent(self)
    type(inference_engine_t), intent(in) :: self

    ! TODO: add allocated(self%output_weights_) to allocated_components array
    associate(allocated_components => [allocated(self%input_weights_), allocated(self%hidden_weights_), allocated(self%biases_)])
      call assert(all(allocated_components), "inference_engine_s(assert_consistent): fully allocated object", &
        intrinsic_array_t(allocated_components))
    end associate

    ! TODO: add {ubound,lbound}(self%output_weights_) differences
    associate(num_neurons => 1 + &
      [ ubound(self%biases_,         1) - lbound(self%biases_,         1), & 
        ubound(self%hidden_weights_, 1) - lbound(self%hidden_weights_, 1), &
        ubound(self%hidden_weights_, 2) - lbound(self%hidden_weights_, 2), &
        ubound(self%input_weights_,  2)  - lbound(self%input_weights_, 2)  &
    ] ) 
      call assert(all(self%neurons_per_layer() == num_neurons), "inference_engine_s(assert_consistent)", &
        intrinsic_array_t(num_neurons) &
      )
    end associate
  end subroutine

  module procedure num_inputs
    call assert(allocated(self%input_weights_), "inference_engine_t%num_inputs: allocated(self%input_weights_)")
    input_count = ubound(self%input_weights_,1) - ubound(self%input_weights_,1) + 1
  end procedure

  module procedure neurons_per_layer
    call assert(allocated(self%input_weights_), "inference_engine_t%neurons_per_layer: allocated(self%input_weights_)")
    neuron_count = ubound(self%input_weights_,2) - lbound(self%input_weights_,2) + 1
  end procedure

  module procedure num_hidden_layers
    call assert(allocated(self%hidden_weights_), "inference_engine_t%num_hidden_layers: allocated(self%hidden_weights_)")
    hidden_layer_count = ubound(self%hidden_weights_,2) - ubound(self%hidden_weights_,2) + 1
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

    integer file_unit, io_status, num_inputs, num_hidden_layers
    character(len=:), allocatable :: line

    open(newunit=file_unit, file=file_name, form='formatted', status='old', iostat=io_status, action='read')
    call assert(io_status==0,"read_network: io_status==0 after 'open' statement", io_status)

    call read_line_and_count_inputs(file_unit, line, num_inputs)
    call count_hidden_layers(file_unit, len(line), num_hidden_layers)

    associate(last_opening_bracket => index(line, "[", back=.true.), first_closing_bracket => index(line, "]"))
      associate(unbracketed_line => line(last_opening_bracket+1:first_closing_bracket-1))
        associate(neurons_per_layer=> num_array_elements_in(unbracketed_line))
          call read_weights_and_biases(file_unit, len(line), num_inputs, neurons_per_layer, num_hidden_layers, self)
        end associate
      end associate
    end associate

    close(file_unit)
    
    call assert_consistent(self)

  contains

    module function line_length(file_unit) result(length)
      integer, intent(in) :: file_unit
      integer length, io_status
      character(len=1) c

      rewind(file_unit)
      io_status = 0
      length = 1
      do 
        read(file_unit, '(a)',advance='no',iostat=io_status) c
        if (io_status/=0) exit
        length = length + 1
      end do
      rewind(file_unit)
    end function

    module subroutine read_line_and_count_inputs(file_unit, line, input_count)
      integer, intent(in) :: file_unit
      character(len=:), intent(out), allocatable :: line
      integer, intent(out) :: input_count
      integer io_status

      rewind(file_unit)
      allocate(character(len=line_length(file_unit)):: line)
      input_count = 0
      do 
        read(file_unit,'(a)', iostat=io_status) line
        call assert(io_status==0, "read_line_and_count_inputs: io_status==0", io_status ) 
        input_count = input_count + 1
        if (index(line, "]]", back=.true.) /= 0) exit
      end do
      rewind(file_unit)
    end subroutine

    pure module function num_array_elements_in(space_delimited_reals) result(array_size)
      character(len=*), intent(in) :: space_delimited_reals
      real, allocatable :: array(:)
      integer array_size, io_status
      
      io_status = 0
      array_size = 1
      do while (io_status==0)
        if (allocated(array)) deallocate(array)
        allocate(array(array_size))
        read(space_delimited_reals, *, iostat=io_status) array
        array_size = array_size + 1
      end do
      array_size = size(array)-1
    end function

    module subroutine read_weights_and_biases(file_unit, buffer_size, num_inputs, neurons_per_layer, num_hidden_layers, self)
      integer, intent(in) :: file_unit, buffer_size, num_inputs, neurons_per_layer, num_hidden_layers
      type(inference_engine_t), intent(out) :: self
      character(len=buffer_size) line_buffer
      integer input, io_status, layer, neuron
      integer, parameter :: input_layer = 1
      
      rewind(file_unit)

      allocate(self%input_weights_(num_inputs, neurons_per_layer))

      read_input_weights: &
      do input = 1, size(self%input_weights_,1)
        read(file_unit,'(a)', iostat=io_status) line_buffer
        call assert(io_status==0, "read_input_weights: io_status==0", io_status ) 
        associate(last_opening_bracket => index(line_buffer, "[", back=.true.), first_closing_bracket => index(line_buffer, "]"))
          associate(unbracketed_line => line_buffer(last_opening_bracket+1:first_closing_bracket-1))
            read(unbracketed_line,*) self%input_weights_(input,:)
          end associate
        end associate
      end do read_input_weights
     
      allocate(self%biases_(neurons_per_layer, num_hidden_layers+input_layer))
      allocate(self%hidden_weights_(neurons_per_layer, neurons_per_layer, num_hidden_layers))

      find_input_layer_biases: &
      do 
        read(file_unit,'(a)', iostat=io_status) line_buffer
        call assert(io_status==0, "find_input_layer_biases: io_status==0", io_status ) 
        if (index(line_buffer, "[")/=0) exit
      end do find_input_layer_biases

      read_input_layer_biases: &
      associate(last_opening_bracket => index(line_buffer, "[", back=.true.), first_closing_bracket => index(line_buffer, "]"))
        associate(unbracketed_line => line_buffer(last_opening_bracket+1:first_closing_bracket-1))
          read(unbracketed_line,*) self%biases_(:,input_layer)
        end associate
      end associate read_input_layer_biases

      read_hidden_layer_weights_biases: &
      do layer = 1, num_hidden_layers

        find_weights: &
        do 
          read(file_unit,'(a)', iostat=io_status) line_buffer
          call assert(io_status==0, "find_weights: io_status==0", io_status ) 
          if (index(line_buffer, "[[")/=0) exit
        end do find_weights

        read_weights: &
        do neuron = 1, size(self%hidden_weights_,2)
          if (neuron/=1) read(file_unit,'(a)', iostat=io_status) line_buffer
          associate(last_opening_bracket => index(line_buffer, "[", back=.true.), first_closing_bracket => index(line_buffer, "]"))
            associate(unbracketed_line => line_buffer(last_opening_bracket+1:first_closing_bracket-1))
              read(unbracketed_line,*) self%hidden_weights_(:,neuron,layer)
            end associate
          end associate
        end do read_weights

        find_biases: &
        do 
          read(file_unit,'(a)', iostat=io_status) line_buffer
          call assert(io_status==0, "read_biases: io_status==0", io_status ) 
          if (index(line_buffer, "[")/=0) exit
        end do find_biases

        read_biases: &
        associate(last_opening_bracket => index(line_buffer, "[", back=.true.), first_closing_bracket => index(line_buffer, "]"))
          associate(unbracketed_line => line_buffer(last_opening_bracket+1:first_closing_bracket-1))
            read(unbracketed_line,*) self%biases_(:,input_layer+layer)
          end associate
        end associate read_biases
        
      end do read_hidden_layer_weights_biases

      rewind(file_unit)
    end subroutine read_weights_and_biases

    subroutine count_hidden_layers(file_unit, buffer_size, hidden_layers)
      integer, intent(in) :: file_unit, buffer_size
      integer, intent(out) :: hidden_layers
      integer, parameter :: input_layer=1, output_layer=1
      integer layers, io_status
      character(len=buffer_size) line_buffer

      rewind(file_unit)
      layers = 0
      io_status=0
      do while(io_status==0)
        read(file_unit, '(a)', iostat=io_status) line_buffer
        if (index(line_buffer, "[[") /= 0) layers = layers +1
      end do
      hidden_layers = layers - (input_layer + output_layer)
      rewind(file_unit)
    end subroutine

  end procedure read_network

end submodule inference_engine_s
