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

    integer file_unit, io_status, last_opening_bracket, first_closing_bracket, num_hidden_layers
    real, allocatable :: array(:)
    character(len=:), allocatable :: line, unbracketed_line

    open(newunit=file_unit, file=file_name, form='formatted', status='old', iostat=io_status, action='read')
    call assert(io_status==0,"stat==0 in open")

    associate(line_len => line_length(file_unit))
      allocate(character(len=line_len):: line)

      associate( &
        num_inputs => get_num_inputs(file_unit, line), &
        last_opening_bracket => index(line, "[", back=.true.), &
        first_closing_bracket => index(line, "]") &
      )
        associate(unbracketed_line => line(last_opening_bracket+1:first_closing_bracket-1))
          array = read_arbitrary_length_array(unbracketed_line)
        end associate
        allocate(self%input_weights_(num_inputs, size(array)))
        call read_input_weights(file_unit, line_len, self%input_weights_)
        call count_hidden_layers(file_unit, line_len, num_hidden_layers)
      end associate
    end associate

    close(file_unit)

  contains

    module function line_length(file_unit) result(length)
      integer, intent(in) :: file_unit
      integer length, io_status
      character(len=1) c

      io_status = 0
      length = 1
      do 
        read(file_unit, '(a)',advance='no',iostat=io_status) c
        if (io_status/=0) exit
        length = length + 1
      end do
      rewind(file_unit)
    end function

    module function get_num_inputs(file_unit, line) result(n)
      integer, intent(in) :: file_unit
      character(len=*), intent(inout) :: line
      integer n, io_status

      n = 0
      do 
        read(file_unit,'(a)', iostat=io_status) line
        if (io_status/=0) exit
        n = n + 1
        if (index(line, "]]", back=.true.) /= 0) exit
      end do
      rewind(file_unit)
    end function

    pure module function read_arbitrary_length_array(space_delimited_reals) result(array)
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
      array = array(:size(array)-1)
    end function

    module subroutine read_input_weights(file_unit, buffer_size, weights)
      integer, intent(in) :: file_unit, buffer_size
      real, intent(inout) :: weights(:,:)
      character(len=buffer_size) line_buffer
      integer i, io_status
      
      do i = 1, size(weights,1)
        read(file_unit,'(a)', iostat=io_status) line_buffer
        call assert(io_status==0, "read_network: io_status == 0", io_status)
        associate(last_opening_bracket => index(line_buffer, "[", back=.true.), first_closing_bracket => index(line_buffer, "]"))
          associate(unbracketed_line => line_buffer(last_opening_bracket+1:first_closing_bracket-1))
            read(unbracketed_line,*) weights(i,:)
          end associate
        end associate
      end do
    end subroutine

    subroutine count_hidden_layers(file_unit, buffer_size, hidden_layers)
      integer, intent(in) :: file_unit, buffer_size
      integer, intent(out) :: hidden_layers
      integer, parameter :: output_layer=1
      integer layers, io_status
      character(len=buffer_size) line_buffer

      layers = 0
      io_status=0
      do while(io_status==0)
        read(file_unit, '(a)', iostat=io_status) line_buffer
        if (index(line_buffer, "[[") /= 0) layers = layers +1
      end do
      hidden_layers = layers - output_layer
      rewind(file_unit)
    end subroutine

  end procedure

end submodule inference_engine_s
