! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(inference_engine_m) inference_engine_s
  use assert_m, only : assert
  use intrinsic_array_m, only : intrinsic_array_t
  use matmul_m, only : matmul_t
  use step_m, only : step_t
  use layer_m, only : layer_t
  use neuron_m, only : neuron_t
  use file_m, only : file_t
  use iso_fortran_env, only : iostat_end
  implicit none

contains

  module procedure construct
    inference_engine%input_weights_ = input_weights
    inference_engine%hidden_weights_ = hidden_weights
    inference_engine%output_weights_ = output_weights
    inference_engine%biases_ = biases
    inference_engine%output_biases_ = output_biases
    if (present(activation_strategy)) then
      inference_engine%activation_strategy_ = activation_strategy
    else
      inference_engine%activation_strategy_ = step_t()
    end if
    if (present(inference_strategy)) then
      inference_engine%inference_strategy_ = inference_strategy
    else
      inference_engine%inference_strategy_ = matmul_t()
    end if
  end procedure

  module procedure conformable_with
    conformable = all( &
      [ shape(self%input_weights_ ) == shape(inference_engine%input_weights_ ) , &
       [shape(self%hidden_weights_) == shape(inference_engine%hidden_weights_)], &
       [shape(self%output_weights_) == shape(inference_engine%output_weights_)], &
       [shape(self%biases_        ) == shape(inference_engine%biases_        )], &
       [shape(self%output_biases_ ) == shape(inference_engine%output_biases_ )]  &
      ] &
    )
  end procedure

  module procedure subtract
    call assert(self%conformable_with(rhs), "inference_engine_t%subtract: conformable operands")
    
    difference%input_weights_  = self%input_weights_  - rhs%input_weights_ 
    difference%hidden_weights_ = self%hidden_weights_ - rhs%hidden_weights_
    difference%output_weights_ = self%output_weights_ - rhs%output_weights_
    difference%biases_         = self%biases_         - rhs%biases_         
    difference%output_biases_  = self%output_biases_  - rhs%output_biases_ 
  end procedure

  module procedure norm 
    norm_of_self = maxval(abs(self%input_weights_)) + maxval(abs(self%hidden_weights_)) + maxval(abs(self%output_weights_)) + & 
           maxval(abs(self%biases_)) + maxval(abs(self%output_biases_))
  end procedure

  pure subroutine assert_consistent(self)
    type(inference_engine_t), intent(in) :: self

    call assert(allocated(self%inference_strategy_), "inference_engine%assert_consistent: allocated(self%inference_strategy_)")
    call assert(allocated(self%activation_strategy_), "inference_engine%assert_consistent: allocated(self%activation_strategy_)")

    associate(allocated_components => &
      [allocated(self%input_weights_), allocated(self%hidden_weights_), allocated(self%output_weights_), &
       allocated(self%biases_), allocated(self%output_biases_)] &
    )
      call assert(all(allocated_components), "inference_engine_s(assert_consistent): fully allocated object", &
        intrinsic_array_t(allocated_components))
    end associate

    associate(num_neurons => 1 + &
      [ ubound(self%biases_,         1) - lbound(self%biases_,         1), & 
        ubound(self%hidden_weights_, 1) - lbound(self%hidden_weights_, 1), &
        ubound(self%hidden_weights_, 2) - lbound(self%hidden_weights_, 2), &
        ubound(self%input_weights_,  2) - lbound(self%input_weights_,  2), &
        ubound(self%output_weights_, 2) - lbound(self%output_weights_, 2)  &
    ] ) 
      call assert(all(self%neurons_per_layer() == num_neurons), "inference_engine_s(assert_consistent): num_neurons", &
        intrinsic_array_t(num_neurons) &
      )
    end associate

    associate(output_count => 1 + &
      [ ubound(self%output_weights_, 1) - lbound(self%output_weights_, 1), & 
        ubound(self%output_biases_,  1) - lbound(self%output_biases_,  1)  &
    ] )
      call assert(all(self%num_outputs() == output_count), "inference_engine_s(assert_consistent): num_outputs", &
        intrinsic_array_t(output_count) &
      )
    end associate
  end subroutine

  module procedure num_outputs
    call assert(allocated(self%output_weights_), "inference_engine_t%num_outputs: allocated(self%output_weights_)")
    output_count = ubound(self%output_weights_,1) - lbound(self%output_weights_,1) + 1
  end procedure

  module procedure num_inputs
    call assert(allocated(self%input_weights_), "inference_engine_t%num_inputs: allocated(self%input_weights_)")
    input_count = ubound(self%input_weights_,1) - lbound(self%input_weights_,1) + 1
  end procedure

  module procedure neurons_per_layer
    call assert(allocated(self%input_weights_), "inference_engine_t%neurons_per_layer: allocated(self%input_weights_)")
    neuron_count = ubound(self%input_weights_,2) - lbound(self%input_weights_,2) + 1
  end procedure

  module procedure num_hidden_layers
    call assert(allocated(self%hidden_weights_), "inference_engine_t%num_hidden_layers: allocated(self%hidden_weights_)")
    hidden_layer_count = ubound(self%hidden_weights_,3) - lbound(self%hidden_weights_,3) + 1
  end procedure

  module procedure infer
    
    integer, parameter :: input_layer = 1
  
    call assert_consistent(self)

    output = self%inference_strategy_%infer(input, &
      self%input_weights_, self%hidden_weights_, self%biases_, self%output_biases_, self%output_weights_, self%activation_strategy_&
    )
  end procedure

  module procedure write_network
    integer file_unit, io_status, input, layer, neuron

    open(newunit=file_unit, file=file_name%string(), form='formatted', status='unknown', iostat=io_status, action='write')
    call assert(io_status==0,"write_network: io_status==0 after 'open' statement", file_name%string())
 
    call assert_consistent(self)

    write_input_layer : &
    block
      input = 1
      write(file_unit,*) "[[", self%input_weights_(input,:), trim(merge("]]", "] ", self%num_inputs()==1))

      do input = 2, self%num_inputs() - 1
        write(file_unit,*) "[", self%input_weights_(input,:),"]"
      end do

      input = self%num_inputs()
      if (input>1) write(file_unit,*) "[",self%input_weights_(self%num_inputs(), :),"]]"

      write(file_unit,*)
      write(file_unit,*) "[",self%biases_(:,1),"]"
    end block write_input_layer

    write_hidden_layers: &
    do layer = 1, self%num_hidden_layers()

      write(file_unit,*)

      neuron = 1
      write(file_unit,*) "[[", self%hidden_weights_(:, neuron, layer), trim(merge("]]", "] ", self%neurons_per_layer()==1))

      do neuron = 2, self%neurons_per_layer()-1
        write(file_unit,*) "[",self%hidden_weights_(: , neuron, layer),"]"
      end do

      neuron = self%neurons_per_layer()
      if (neuron>1) write(file_unit,*) "[",self%hidden_weights_(:, neuron, layer),"]]"

      write(file_unit,*)
      write(file_unit,*) "[",self%biases_(:,layer+1),"]"

    end do write_hidden_layers
    
    write_output_layer: &
    block
      write(file_unit, *)

      neuron = 1
      write(file_unit,*) "[[", self%output_weights_(:, neuron), trim(merge("]]", "] ", self%neurons_per_layer()==1))

      do neuron = 2, self%neurons_per_layer()-1
        write(file_unit,*) "[",self%output_weights_(:, neuron),"]"
      end do

      neuron = self%neurons_per_layer()
      if (neuron>1) write(file_unit,*) "[",self%output_weights_(:, neuron),"]]"

      write(file_unit,*)
      write(file_unit,*) "[",self%output_biases_(:),"]"

    end block write_output_layer

    close(file_unit)
  end procedure

  module procedure read_network

    integer file_unit, io_status, num_inputs, num_hidden_layers, num_outputs
    character(len=:), allocatable :: line

    open(newunit=file_unit, file=file_name%string(), form='formatted', status='old', iostat=io_status, action='read')
    call assert(io_status==0,"read_network: io_status==0 after 'open' statement", file_name%string())

    call read_line_and_count_inputs(file_unit, line, num_inputs)
    call count_hidden_layers(file_unit, len(line), num_hidden_layers)
    call count_outputs(file_unit, len(line), num_hidden_layers, num_outputs)

    associate(last_opening_bracket => index(line, "[", back=.true.), first_closing_bracket => index(line, "]"))
      associate(unbracketed_line => line(last_opening_bracket+1:first_closing_bracket-1))
        associate(neurons_per_layer=> num_array_elements_in(unbracketed_line))
          call read_weights_and_biases(file_unit, len(line), num_inputs, neurons_per_layer, num_hidden_layers, num_outputs, self)
        end associate
      end associate
    end associate

    if (present(activation_strategy)) then
      self%activation_strategy_  = activation_strategy
    else
      self%activation_strategy_  = step_t()
    end if
 
    if (present(inference_strategy)) then
      self%inference_strategy_  = inference_strategy
    else
      self%inference_strategy_  = matmul_t()
    end if

    close(file_unit)

    call assert_consistent(self)

  contains

    function line_length(file_unit) result(length)
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
      backspace(file_unit)
    end function

    subroutine read_line_and_count_inputs(file_unit, line, input_count)
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

    pure function num_array_elements_in(space_delimited_reals) result(array_size)
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

    subroutine read_weights_and_biases( &
       file_unit, buffer_size, num_inputs, neurons_per_layer, num_hidden_layers, num_outputs, self &
    )
      integer, intent(in) :: file_unit, buffer_size, num_inputs, neurons_per_layer, num_hidden_layers, num_outputs
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

      allocate(self%output_weights_(num_outputs, neurons_per_layer))
      allocate(self%output_biases_(num_outputs))

      find_output_weights: &
      do 
        read(file_unit,'(a)', iostat=io_status) line_buffer
        call assert(io_status==0, "find_outut_layer_weights: io_status==0", io_status ) 
        if (index(line_buffer, "[[")/=0) exit
      end do find_output_weights

      read_output_weights: &
      do neuron = 1, size(self%hidden_weights_,2)
        if (neuron/=1) read(file_unit,'(a)', iostat=io_status) line_buffer
        associate(last_opening_bracket => index(line_buffer, "[", back=.true.), first_closing_bracket => index(line_buffer, "]"))
          associate(unbracketed_line => line_buffer(last_opening_bracket+1:first_closing_bracket-1))
            read(unbracketed_line,*) self%output_weights_(:,neuron)
          end associate
        end associate
      end do read_output_weights

      find_output_biases: &
      do 
        read(file_unit,'(a)', iostat=io_status) line_buffer
        call assert(io_status==0, "find_outut_layer_weights: io_status==0", io_status ) 
        if (index(line_buffer, "[")/=0) exit
      end do find_output_biases

      associate(last_opening_bracket => index(line_buffer, "[", back=.true.), first_closing_bracket => index(line_buffer, "]"))
        associate(unbracketed_line => line_buffer(last_opening_bracket+1:first_closing_bracket-1))
          read(unbracketed_line,*) self%output_biases_(:)
        end associate
      end associate

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

    subroutine count_outputs(file_unit, buffer_size, num_hidden_layers, output_count)
      integer, intent(in) :: file_unit, buffer_size, num_hidden_layers
      integer, intent(out) :: output_count
      character(len=buffer_size) line_buffer
      integer, parameter :: input_layer=1, output_layer=1
      integer layer

      rewind(file_unit)

      layer = 0

      find_end_of_hidden_layers: &
      do
        read(file_unit, '(a)', iostat=io_status) line_buffer
        call assert(io_status==0, "read_hidden_layers: io_status==0", io_status ) 
        if (index(line_buffer, "]]") /= 0) layer = layer + 1
        if (layer == input_layer  + num_hidden_layers + output_layer) exit
      end do find_end_of_hidden_layers

      find_and_read_output_biases: &
      do 
        read(file_unit,'(a)', iostat=io_status) line_buffer
        call assert(io_status==0, "find_output_biases: io_status==0", io_status ) 
        if (index(line_buffer, "[")/=0) exit
      end do find_and_read_output_biases

      associate(last_opening_bracket => index(line_buffer, "[", back=.true.), first_closing_bracket => index(line_buffer, "]"))
        associate(unbracketed_line => line_buffer(last_opening_bracket+1:first_closing_bracket-1))
          output_count = num_array_elements_in(unbracketed_line)
        end associate
      end associate

      rewind(file_unit)
    end subroutine

  end procedure read_network

  module procedure from_json

    type(string_t), allocatable :: lines(:)
    type(layer_t) hidden_layers
    type(neuron_t) output_neuron
    
    lines = file_%lines()

    call assert(adjustl(lines(1)%string())=="{", "from_json: outermost object start", lines(1)%string())
    call assert(adjustl(lines(2)%string())=='"hidden_layers": [', "from_json: hidden layers array start", lines(2)%string())

    block 
       integer, parameter :: first_layer_line=3, lines_per_neuron=4, bracket_lines_per_layer=2
       character(len=:), allocatable :: output_layer_line
       
       hidden_layers = layer_t(lines, start=first_layer_line)

       associate( output_layer_line_number => first_layer_line + lines_per_neuron*sum(hidden_layers%count_neurons()) &
         + bracket_lines_per_layer*hidden_layers%count_layers() + 1)

         output_layer_line = lines(output_layer_line_number)%string()
         call assert(adjustl(output_layer_line)=='"output_layer": [', "from_json: hidden layers array start", output_layer_line)

         output_neuron = neuron_t(lines, start=output_layer_line_number + 1)
       end associate
    end block

    self%input_weights_ = hidden_layers%input_weights()
    call assert(hidden_layers%next_allocated(), "inference_engine_t%from_json: next layer exists")

    block 
      type(layer_t), pointer :: next_layer

      next_layer => hidden_layers%next_pointer()
      self%hidden_weights_ = next_layer%hidden_weights()
    end block

    associate(output_weights => output_neuron%weights())
      self%output_weights_ = reshape(output_weights, [size(output_weights),1])
      self%output_biases_ = [output_neuron%bias()]
    end associate

    if (present(activation_strategy)) then
      self%activation_strategy_  = activation_strategy
    else
      self%activation_strategy_  = step_t()
    end if
 
    if (present(inference_strategy)) then
      self%inference_strategy_  = inference_strategy
    else
      self%inference_strategy_  = matmul_t()
    end if

    call assert_consistent(self)

  end procedure from_json

end submodule inference_engine_s
