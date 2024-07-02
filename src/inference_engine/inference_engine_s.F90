! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(inference_engine_m_) inference_engine_s
  use assert_m, only : assert, intrinsic_array_t
  use step_m, only : step_t
  use swish_m, only : swish_t
  use sigmoid_m, only : sigmoid_t
  use gelu_m, only : gelu_t
  use relu_m, only : relu_t
  use layer_m, only : layer_t
  use neuron_m, only : neuron_t
  use julienne_formats_m, only : separated_values
  implicit none

  interface assert_consistency
    procedure inference_engine_consistency
    procedure difference_consistency
  end interface

contains

  module procedure map_to_input_range
    normalized_tensor = self%input_range_%map_to_training_range(tensor)
  end procedure

  module procedure map_from_output_range
    tensor = self%output_range_%map_from_training_range(normalized_tensor)
  end procedure

  module procedure to_exchange
    exchange%input_range_ = self%input_range_
    exchange%output_range_ = self%output_range_
    associate(strings => self%metadata_%strings())
      exchange%metadata_ = metadata_t(strings(1),strings(2),strings(3),strings(4),strings(5))
    end associate
    exchange%weights_ = self%weights_
    exchange%biases_ = self%biases_
    exchange%nodes_ = self%nodes_
    exchange%activation_strategy_ = self%activation_strategy_ 
  end procedure

  module procedure infer

    real(rkind), allocatable :: a(:,:)
    integer, parameter :: input_layer = 0
    integer k, l

    call assert_consistency(self)

    associate(w => self%weights_, b => self%biases_, n => self%nodes_, output_layer => ubound(self%nodes_,1))

      allocate(a(maxval(n), input_layer:output_layer))

#ifndef _CRAYFTN
      associate(normalized_inputs => self%input_range_%map_to_training_range(inputs))
        a(1:n(input_layer),input_layer) = normalized_inputs%values()
      end associate
#else
      block
        type(tensor_t) normalized_inputs
        normalized_inputs = self%input_range_%map_to_training_range(inputs)
        a(1:n(input_layer),input_layer) = normalized_inputs%values()
      end block
#endif

      feed_forward: &
      do l = input_layer+1, output_layer
        associate(z => matmul(w(1:n(l),1:n(l-1),l), a(1:n(l-1),l-1)) + b(1:n(l),l))
          a(1:n(l),l) = self%activation_strategy_%activation(z)
        end associate
      end do feed_forward

#ifdef _CRAYFTN
      block
        type(tensor_t) :: normalized_outputs
        normalized_outputs = tensor_t(a(1:n(output_layer), output_layer))
#else
      associate(normalized_outputs => tensor_t(a(1:n(output_layer), output_layer)))
#endif
        outputs = self%output_range_%map_from_training_range(normalized_outputs)
#ifdef _CRAYFTN
      end block
#else
      end associate
#endif

    end associate

  end procedure

  pure subroutine inference_engine_consistency(self)

    type(inference_engine_t), intent(in) :: self

    integer, parameter :: input_layer=0

    associate( &
      all_allocated=>[allocated(self%weights_),allocated(self%biases_),allocated(self%nodes_),allocated(self%activation_strategy_)]&
    )   
      call assert(all(all_allocated),"inference_engine_s(inference_engine_consistency): fully_allocated", &
        intrinsic_array_t(all_allocated))
    end associate

    associate(max_width=>maxval(self%nodes_), component_dims=>[size(self%biases_,1), size(self%weights_,1), size(self%weights_,2)])
      call assert(all(component_dims == max_width), "inference_engine_s(inference_engine_consistency): conformable arrays", &
        intrinsic_array_t([max_width,component_dims]))
    end associate

    associate(input_subscript => lbound(self%nodes_,1))
      call assert(input_subscript == input_layer, "inference_engine_s(inference_engine_consistency): n base subsscript", &
        input_subscript)
    end associate

  end subroutine

  pure subroutine difference_consistency(self)

    type(difference_t), intent(in) :: self

    integer, parameter :: input_layer=0

    associate( &
      all_allocated=>[allocated(self%weights_difference_),allocated(self%biases_difference_),allocated(self%nodes_difference_)] &
    )   
      call assert(all(all_allocated),"inference_engine_s(difference_consistency): fully_allocated",intrinsic_array_t(all_allocated))
    end associate

    call assert(all(size(self%biases_difference_,1)==[size(self%weights_difference_,1), size(self%weights_difference_,2)]), &
      "inference_engine_s(difference_consistency): conformable arrays" &
    )

  end subroutine

  impure function activation_factory(activation_name) result(activation)
    character(len=*), intent(in) :: activation_name
    class(activation_strategy_t), allocatable :: activation

    select case(activation_name)
      case("swish")
        activation = swish_t()
      case("sigmoid")
        activation = sigmoid_t()
      case("step")
        activation = step_t()
      case("gelu")
        activation = gelu_t()
      case("relu")
        activation = relu_t()
      case default
        error stop "inference_engine_s(activation_factory): unrecognized activation strategy '"//activation_name//"'"
    end select
  end function

  module procedure construct_from_padded_arrays

    inference_engine%metadata_ = metadata_t(metadata(1),metadata(2),metadata(3),metadata(4),metadata(5))
    inference_engine%weights_ = weights
    inference_engine%biases_ = biases
    inference_engine%nodes_ = nodes

    block
      integer i

      if (present(input_range)) then
        inference_engine%input_range_ = input_range
      else
        associate(num_inputs => nodes(lbound(nodes,1)))
          associate(default_minima => [(0., i=1,num_inputs)], default_maxima => [(1., i=1,num_inputs)])
            inference_engine%input_range_ = tensor_range_t("inputs", default_minima, default_maxima)
          end associate
        end associate
      end if

      if (present(output_range)) then
        inference_engine%output_range_ = output_range
      else
        associate(num_outputs => nodes(ubound(nodes,1)))
          associate(default_minima => [(0., i=1,num_outputs)], default_maxima => [(1., i=1,num_outputs)])
            inference_engine%output_range_ = tensor_range_t("outputs", default_minima, default_maxima)
          end associate
        end associate
      end if
    end block

    associate(strings => inference_engine%metadata_%strings())
      inference_engine%activation_strategy_ = activation_factory(strings(4)%string())
    end associate

    call assert_consistency(inference_engine)

  end procedure construct_from_padded_arrays

  module procedure from_json

    type(string_t), allocatable :: lines(:), metadata(:)
    type(tensor_range_t) input_range, output_range
    type(layer_t) hidden_layers, output_layer
    real(rkind), allocatable :: hidden_weights(:,:,:)
    character(len=:), allocatable :: justified_line
    integer l
#ifdef _CRAYFTN
    type(tensor_range_t) proto_range
    type(metadata_t) proto_meta
    type(neuron_t) proto_neuron
    proto_range = tensor_range_t("",[0.],[1.])
    proto_meta = metadata_t(string_t(""),string_t(""),string_t(""),string_t(""),string_t(""))
    proto_neuron = neuron_t(weights=[0.], bias=0.)
#endif

    lines = file_%lines()
    call assert(adjustl(lines(1)%string())=="{", "inference_engine_s(from_json): expected outermost object '{'")

    associate(num_lines => size(lines))
      
#ifndef _CRAYFTN
      associate(proto_range => tensor_range_t("",[0.],[1.]))
#endif
        associate(range_lines => size(proto_range%to_json()))

          find_inputs_range: &
          do l = 1, num_lines
            justified_line = adjustl(lines(l)%string())
            if (justified_line == '"inputs_range": {') exit 
          end do find_inputs_range
          call assert(justified_line =='"inputs_range": {', 'from_json: expecting "inputs_range": {', justified_line)
          input_range = tensor_range_t(lines(l:l+range_lines-1))

          find_outputs_range: &
          do l = 1, num_lines
            justified_line = adjustl(lines(l)%string())
            if (justified_line == '"outputs_range": {') exit 
          end do find_outputs_range
          call assert(justified_line =='"outputs_range": {', 'from_json: expecting "outputs_range": {', justified_line)
          output_range = tensor_range_t(lines(l:l+range_lines-1))

        end associate
#ifndef _CRAYFTN
      end associate
#endif

      find_hidden_layers: &
      do l = 1, num_lines
        justified_line = adjustl(lines(l)%string())
        if (justified_line == '"hidden_layers": [') exit 
      end do find_hidden_layers
      call assert(justified_line=='"hidden_layers": [', 'from_json: expecting "hidden_layers": [', justified_line)

      read_hidden_layers: &
      block 
        integer, parameter :: bracket_lines_per_layer=2
        character(len=:), allocatable :: output_layer_line
               
        hidden_layers = layer_t(lines, start=l+1)

#ifndef _CRAYFTN
        associate(proto_neuron => neuron_t(weights=[0.], bias=0.))
#endif
          associate(num_neuron_lines => size(proto_neuron%to_json()))
            associate( output_layer_line_number => l + 1 + num_neuron_lines*sum(hidden_layers%count_neurons()) &
              + bracket_lines_per_layer*hidden_layers%count_layers() + 1)

              output_layer_line = lines(output_layer_line_number)%string()
              call assert(adjustl(output_layer_line)=='"output_layer": [', 'from_json: expecting "output_layer": [', &
                lines(output_layer_line_number)%string())

              output_layer = layer_t(lines, start=output_layer_line_number)
            end associate
          end associate
#ifndef _CRAYFTN
        end associate
#endif
      end block read_hidden_layers
    
      find_metadata: &
      do l = 1, num_lines
        justified_line = adjustl(lines(l)%string())
        if (justified_line == '"metadata": {') exit 
      end do find_metadata
      call assert(justified_line=='"metadata": {', 'from_json: expecting "metadata": {', justified_line)

#ifndef _CRAYFTN
      associate(proto_meta => metadata_t(string_t(""),string_t(""),string_t(""),string_t(""),string_t("")))
#endif
       associate(metadata_object => metadata_t(lines(l:l+size(proto_meta%to_json())-1)))
         inference_engine = hidden_layers%inference_engine(metadata_object%strings(), output_layer, input_range, output_range)
       end associate
#ifndef _CRAYFTN
      end associate
#endif
    end associate ! associate(num_lines ... )

    associate(strings => inference_engine%metadata_%strings())
      inference_engine%activation_strategy_ = activation_factory(strings(4)%string())
    end associate

    call assert_consistency(inference_engine)

  contains

    pure function get_key_string(line) result(unquoted_key)
      character(len=*), intent(in) :: line
      character(len=:), allocatable :: unquoted_key
    
      associate(opening_key_quotes => index(line, '"'), separator => index(line, ':'))
        associate(closing_key_quotes => opening_key_quotes + index(line(opening_key_quotes+1:), '"'))
          unquoted_key = trim(line(opening_key_quotes+1:closing_key_quotes-1))
        end associate
      end associate
    end function

    function get_key_value(line) result(value_)
      character(len=*), intent(in) :: line
      type(string_t) value_

#ifdef __INTEL_COMPILER
      character(len=:), allocatable :: text_after_colon
      integer :: opening_value_quotes, closing_value_quotes
      text_after_colon = line(index(line, ':')+1:)
      opening_value_quotes = index(text_after_colon, '"')
      closing_value_quotes = opening_value_quotes + index(text_after_colon(opening_value_quotes+1:), '"')
#endif
#ifndef __INTEL_COMPILER
      associate(text_after_colon => line(index(line, ':')+1:))
        associate(opening_value_quotes => index(text_after_colon, '"'))
          associate(closing_value_quotes => opening_value_quotes + index(text_after_colon(opening_value_quotes+1:), '"'))
#endif
            if (any([opening_value_quotes, closing_value_quotes] == 0)) then
              value_ = string_t(trim(adjustl((text_after_colon))))
            else
              value_ = string_t(text_after_colon(opening_value_quotes+1:closing_value_quotes-1))
            end if
#ifndef __INTEL_COMPILER
          end associate
        end associate
      end associate
#endif
    end function

  end procedure from_json

  module procedure assert_conformable_with

    call assert_consistency(self)
    call assert_consistency(inference_engine)

    associate(equal_shapes => [ &
      shape(self%weights_) == shape(inference_engine%weights_), &
      shape(self%biases_) == shape(inference_engine%biases_), &
      shape(self%nodes_) == shape(inference_engine%nodes_)  &
     ])
      call assert(all(equal_shapes), "assert_conformable_with: all(equal_shapes)", intrinsic_array_t(equal_shapes))
    end associate

    call assert(same_type_as(self%activation_strategy_, inference_engine%activation_strategy_), "assert_conformable_with: types)")
    
  end procedure

  module procedure subtract

    call assert_consistency(self)
    call assert_consistency(rhs)
    call self%assert_conformable_with(rhs)

    block
      integer l

      allocate(difference%weights_difference_, mold = self%weights_)
      allocate(difference%biases_difference_, mold = self%biases_)
      allocate(difference%nodes_difference_, mold = self%nodes_)

      difference%weights_difference_ = 0.
      difference%biases_difference_ = 0.
      difference%nodes_difference_ = 0.

      l = 0
      difference%nodes_difference_(l)  = self%nodes_(l) - rhs%nodes_(l)
     
      associate(n => self%nodes_)
#ifndef __INTEL_COMPILER
        do concurrent(l = 1:ubound(n,1))
          difference%weights_difference_(1:n(l),1:n(l-1),l) = self%weights_(1:n(l),1:n(l-1),l) - rhs%weights_(1:n(l),1:n(l-1),l)
          difference%biases_difference_(1:n(l),l) = self%biases_(1:n(l),l) - rhs%biases_(1:n(l),l)
          difference%nodes_difference_(l) = self%nodes_(l) - rhs%nodes_(l)
        end do
#else
        block
          integer j, k
          do l = 1, ubound(n,1)
            do j = 1, n(l)
              do k = 1, n(l-1)
                difference%weights_difference_(j,k,l) = self%weights_(j,k,l) - rhs%weights_(j,k,l)
                difference%biases_difference_(j,l) = self%biases_(j,l) - rhs%biases_(j,l)
                difference%nodes_difference_(l) = self%nodes_(l) - rhs%nodes_(l)
              end do
            end do
          end do
        end block
#endif
      end associate

    end block

    call assert_consistency(difference)
  end procedure

  module procedure norm 
    norm_of_self = maxval([abs(self%weights_difference_), abs(self%biases_difference_), real(abs(self%nodes_difference_))])
  end procedure

  module procedure num_outputs
    call assert_consistency(self)
    output_count = self%nodes_(ubound(self%nodes_,1))
  end procedure

  module procedure num_inputs
    call assert_consistency(self)
    input_count = self%nodes_(lbound(self%nodes_,1))
  end procedure

  module procedure nodes_per_layer
    call assert_consistency(self)
    node_count = self%nodes_
  end procedure

  module procedure to_json

    type(string_t), allocatable :: lines(:)
    integer layer, neuron, line
    integer, parameter :: characters_per_value=17
    character(len=:), allocatable :: comma_separated_values, csv_format
    character(len=17) :: single_value
    integer, parameter :: &
      outer_object_braces = 2, hidden_layer_outer_brackets = 2, inner_brackets_per_layer  = 2, &
      output_layer_brackets = 2
#ifdef _CRAYFTN
    type(tensor_range_t) proto_range
    type(metadata_t) proto_meta
    type(neuron_t) proto_neuron
    proto_range = tensor_range_t("",[0._rkind],[1._rkind])
    proto_meta = metadata_t(string_t(""),string_t(""),string_t(""),string_t(""),string_t(""))
    proto_neuron = neuron_t([0._rkind],0._rkind)
#endif

    call assert_consistency(self)

    csv_format = separated_values(separator=",", mold=[real(rkind)::])

    associate( &
      num_hidden_layers => size(self%nodes_)-2, &
      neurons_per_layer => self%nodes_(lbound(self%nodes_,1)+1), &
      num_outputs => self%num_outputs(), &
      num_inputs => self%num_inputs() &
    )
#ifndef _CRAYFTN
      associate( &
        proto_range => tensor_range_t("",[0._rkind],[1._rkind]), &
        proto_meta => metadata_t(string_t(""),string_t(""),string_t(""),string_t(""),string_t("")), &
        proto_neuron => neuron_t([0._rkind],0._rkind) &
      )
#endif
        associate( &
          num_tensor_range_lines => size(proto_range%to_json()), &
          num_metadata_lines => size(proto_meta%to_json()), &
          num_neuron_lines => size(proto_neuron%to_json()) &
        )
          call assert(all(neurons_per_layer==self%nodes_(lbound(self%nodes_,1)+1 : ubound(self%nodes_,1)-1)), &
            "to_json: uniform hidden layers")

          associate( &
            num_lines => outer_object_braces + num_metadata_lines + 2 * num_tensor_range_lines &
            + hidden_layer_outer_brackets + num_hidden_layers*(inner_brackets_per_layer + neurons_per_layer*num_neuron_lines) &
            + output_layer_brackets + num_outputs*num_neuron_lines&
          )
            allocate(lines(num_lines))

            line = 1
            lines(line) = string_t('{')
            line = line + 1

            lines(line:line+num_metadata_lines-1) = self%metadata_%to_json()
            line = line + num_metadata_lines

            lines(line:line+num_tensor_range_lines-1) =  self%input_range_%to_json()
            lines(line+num_tensor_range_lines-1) =  lines(line+num_tensor_range_lines-1)  // ","
            line = line + num_tensor_range_lines

            lines(line:line+num_tensor_range_lines-1) =  self%output_range_%to_json() 
            lines(line+num_tensor_range_lines-1) = lines(line+num_tensor_range_lines-1) // ","
            line = line + num_tensor_range_lines

            lines(line) = string_t('     "hidden_layers": [')
            line = line + 1

            layer = 1 
            lines(line) = string_t('         [')
            do neuron = 1, neurons_per_layer
              line = line + 1
              lines(line) = string_t('             {')
              line = line + 1
              if (allocated(comma_separated_values)) deallocate(comma_separated_values)
              allocate(character(len=num_inputs*(characters_per_value+1)-1)::comma_separated_values)
              block
                integer l          
                associate(n => self%nodes_)
                  l = 1
                  write(comma_separated_values, fmt = csv_format) self%weights_(neuron,1:n(l-1),l)
                end associate
              end block
              lines(line) = string_t('                "weights": [' // trim(comma_separated_values) // '],')
              line = line + 1
              write(single_value, fmt = csv_format) self%biases_(neuron,layer)
              lines(line) = string_t('                 "bias": ' // trim(single_value))
              line = line + 1
              lines(line) = string_t("             }" // trim(merge(' ',',',neuron==neurons_per_layer)))
            end do
            line = line + 1
            lines(line) = string_t(trim(merge("         ] ", "         ],", any(num_hidden_layers==[1,line]))))

            do layer = 1, num_hidden_layers-1
              line = line + 1
              lines(line) = string_t('         [')
              block
                real(rkind), allocatable :: hidden_layer_weights(:,:)
                integer j, l
              
                associate(n => self%nodes_, l => layer + 1)
                  allocate(hidden_layer_weights(n(l),n(l-1)))
                  do concurrent(j = 1:n(l))
                    hidden_layer_weights(j,1:n(l-1)) = self%weights_(j,1:n(l-1),l)
                  end do
                  hidden_layer_weights = transpose(hidden_layer_weights)
                end associate
              do neuron = 1, neurons_per_layer
                line = line + 1
                lines(line) = string_t('             {')
                line = line + 1
                if (allocated(comma_separated_values)) deallocate(comma_separated_values)
                allocate(character(len=neurons_per_layer*(characters_per_value+1)-1)::comma_separated_values)
                write(comma_separated_values, fmt = csv_format) hidden_layer_weights(:, neuron)
                lines(line) = string_t('                "weights": [' // trim(comma_separated_values) // '],')
                line = line + 1
                write(single_value, fmt = csv_format) self%biases_(neuron,layer+1)
                lines(line) = string_t('                 "bias": ' // trim(single_value))
                line = line + 1
                lines(line) = string_t("             }" // trim(merge(' ',',',neuron==neurons_per_layer)))
              end do
              end block
              line = line + 1
              lines(line) = string_t("         ]" // trim(merge(' ',',',layer==num_hidden_layers-1)))
            end do

            line = line + 1
            lines(line) = string_t("     ],")

            line = line + 1
            lines(line) = string_t('     "output_layer": [')

            do neuron = 1, num_outputs
              line = line + 1
              lines(line) = string_t('             {')
              line = line + 1
              if (allocated(comma_separated_values)) deallocate(comma_separated_values)
              allocate(character(len=neurons_per_layer*(characters_per_value+1)-1)::comma_separated_values)
              associate(n => self%nodes_, l => ubound(self%nodes_,1))
                write(comma_separated_values, fmt = csv_format) self%weights_(neuron,1:n(l-1),l)
              end associate
              lines(line) = string_t('                "weights": [' // trim(comma_separated_values) // '],')
              line = line + 1
              write(single_value, fmt = csv_format) self%biases_(neuron,ubound(self%biases_,2))
              lines(line) = string_t('                 "bias": ' // trim(single_value))
              line = line + 1
              lines(line) = string_t("             }" // trim(merge(' ',',',neuron==num_outputs)))
            end do

            line = line + 1
            lines(line) = string_t('     ]')

            line = line + 1
            lines(line) = string_t('}')

            call assert(line == num_lines, "inference_engine_t%to_json: all lines defined", intrinsic_array_t([num_lines, line]))
          end associate
        end associate
#ifndef _CRAYFTN
      end associate
#endif
    end associate
 
    json_file = file_t(lines)

  end procedure to_json

  module procedure skip
    associate(strings => self%metadata_%strings())
      use_skip_connections = merge(.true., .false.,  strings(5) == "true")
    end associate
  end procedure

  module procedure activation_function_name
    associate(strings => self%metadata_%strings())
      activation_name = strings(4)
    end associate
  end procedure

end submodule inference_engine_s
