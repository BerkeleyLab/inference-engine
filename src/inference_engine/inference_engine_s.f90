! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(inference_engine_m_) inference_engine_s
  use assert_m, only : assert
  use intrinsic_array_m, only : intrinsic_array_t
  use step_m, only : step_t
  use swish_m, only : swish_t
  use sigmoid_m, only : sigmoid_t
  use layer_m, only : layer_t
  use neuron_m, only : neuron_t
  use file_m, only : file_t
  use formats_m, only : separated_values
  implicit none

  interface assert_consistency
    module procedure inference_engine_consistency
    module procedure difference_consistency
  end interface

contains

  module procedure to_exchange
    inference_engine_exchange = inference_engine_exchange_t(self%metadata_, self%weights_, self%biases_, self%nodes_, self%activation_strategy_)
  end procedure

  module procedure infer

    real(rkind), allocatable :: z(:,:), a(:,:)
    integer, parameter :: input_layer = 0
    integer j, k, l

    call assert_consistency(self)

    associate(w => self%weights_, b => self%biases_, n => self%nodes_, output_layer => ubound(self%nodes_,1))

      allocate(a(maxval(n), input_layer:output_layer))

      a(1:n(input_layer),input_layer) = inputs%values()

      feed_forward: &
      do l = input_layer+1, output_layer
        associate(z => matmul(w(1:n(l),1:n(l-1),l), a(1:n(l-1),l-1)) + b(1:n(l),l))
          a(1:n(l),l) = self%activation_strategy_%activation(z)
        end associate
      end do feed_forward
 
      outputs = tensor_t(a(1:n(output_layer), output_layer))

    end associate

  end procedure

  pure module subroutine inference_engine_consistency(self)

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

  pure module subroutine difference_consistency(self)

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

  pure subroutine set_activation_strategy(inference_engine)
    type(inference_engine_t), intent(inout) :: inference_engine
    ! This code is called in both constructors and and can't be refactored into a factory method
    ! pattern because the result would need to be allocatable and polymorphic, which would preclude
    ! the function being pure so it wouldn't be possible to call it from inside the pure constructor
    ! functions.
    select case(inference_engine%metadata_(findloc(key, "activationFunction", dim=1))%string())
      case("swish")
        inference_engine%activation_strategy_ = swish_t()
      case("sigmoid")
        inference_engine%activation_strategy_ = sigmoid_t()
      case("step")
        inference_engine%activation_strategy_ = step_t()
      case default
        error stop "inference_engine_s(set_activation_strategy): unrecognized activation strategy"
    end select
  end subroutine

  module procedure construct_from_padded_arrays

    inference_engine%metadata_ = metadata
    inference_engine%weights_ = weights
    inference_engine%biases_ = biases
    inference_engine%nodes_ = nodes
    call set_activation_strategy(inference_engine)
    call assert_consistency(inference_engine)

  end procedure construct_from_padded_arrays

  module procedure construct_from_json

    type(string_t), allocatable :: lines(:), metadata(:)
    type(layer_t) hidden_layers, output_layer
    type(neuron_t) output_neuron
    real(rkind), allocatable :: hidden_weights(:,:,:)
    integer l

    lines = file_%lines()

    l = 1
    call assert(adjustl(lines(l)%string())=="{", "construct_from_json: expecting '{' to start outermost object", lines(l)%string())

    l = 2
    metadata = [string_t(""),string_t(""),string_t(""),string_t(""),string_t("false")]
    if (adjustl(lines(l)%string()) == '"metadata": {') then
      block
        character(len=:), allocatable :: justified_line
        do 
          l = l + 1
          justified_line = adjustl(lines(l)%string())
          if (justified_line == "},") exit
          metadata(findloc(key, trim(get_key_string(justified_line)), dim=1)) = get_key_value(justified_line)
        end do
        l = l + 1
      end block
    end if

    call assert(adjustl(lines(l)%string())=='"hidden_layers": [', 'from_json: expecting "hidden_layers": [', lines(l)%string())
    l = l + 1

    block 
       integer, parameter :: lines_per_neuron=4, bracket_lines_per_layer=2
       character(len=:), allocatable :: output_layer_line
             
       hidden_layers = layer_t(lines, start=l)

       associate( output_layer_line_number => l + lines_per_neuron*sum(hidden_layers%count_neurons()) &
         + bracket_lines_per_layer*hidden_layers%count_layers() + 1)

         output_layer_line = lines(output_layer_line_number)%string()
         call assert(adjustl(output_layer_line)=='"output_layer": [', 'from_json: expecting "output_layer": [', &
           lines(output_layer_line_number)%string())

         output_layer = layer_t(lines, start=output_layer_line_number)
       end associate
    end block

    inference_engine = hidden_layers%inference_engine(metadata, output_layer) 

    call set_activation_strategy(inference_engine)
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

      associate(text_after_colon => line(index(line, ':')+1:))
        associate(opening_value_quotes => index(text_after_colon, '"'))
          associate(closing_value_quotes => opening_value_quotes + index(text_after_colon(opening_value_quotes+1:), '"'))
            if (any([opening_value_quotes, closing_value_quotes] == 0)) then
              value_ = string_t(trim(adjustl((text_after_colon))))
            else
              value_ = string_t(text_after_colon(opening_value_quotes+1:closing_value_quotes-1))
            end if
          end associate
        end associate
      end associate
    end function

  end procedure construct_from_json

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
        do concurrent(l = 1:ubound(n,1))
          difference%weights_difference_(1:n(l),1:n(l-1),l) = self%weights_(1:n(l),1:n(l-1),l) - rhs%weights_(1:n(l),1:n(l-1),l)
          difference%biases_difference_(1:n(l),l) = self%biases_(1:n(l),l) - rhs%biases_(1:n(l),l)
          difference%nodes_difference_(l) = self%nodes_(l) - rhs%nodes_(l)
        end do
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
      outer_object_braces = 2, hidden_layer_outer_brackets = 2, lines_per_neuron = 4, inner_brackets_per_layer  = 2, &
      output_layer_brackets = 2, metadata_outer_braces = 2

    call assert_consistency(self)

    csv_format = separated_values(separator=",", mold=[real(rkind)::])

    associate(num_hidden_layers => size(self%nodes_)-2, &
      neurons_per_layer => self%nodes_(lbound(self%nodes_,1)+1), &
      num_outputs => self%num_outputs(), &
      num_inputs => self%num_inputs() &
    )

      call assert(all(neurons_per_layer==self%nodes_(lbound(self%nodes_,1)+1 : ubound(self%nodes_,1)-1)), &
        "to_json: uniform hidden layers")

      associate(num_lines => &
        outer_object_braces &
        + metadata_outer_braces + size(key) &
        + hidden_layer_outer_brackets + (num_hidden_layers)*(inner_brackets_per_layer + neurons_per_layer*lines_per_neuron) &
        + output_layer_brackets + num_outputs*lines_per_neuron &
      )
        allocate(lines(num_lines))

        line = 1
        lines(line) = string_t('{')

        line = line + 1
        lines(line) = string_t('    "metadata": {')

        line = line + 1
        lines(line) = string_t('        "modelName": "' // &
                                                       self%metadata_(findloc(key, "modelName", dim=1))%string() // '",')
        line = line + 1
        lines(line) = string_t('        "modelAuthor": "' // &
                                                       self%metadata_(findloc(key, "modelAuthor", dim=1))%string() // '",')
        line = line + 1
        lines(line) = string_t('        "compilationDate": "' // &
                                                       self%metadata_(findloc(key, "compilationDate", dim=1))%string() // '",')
        line = line + 1
        lines(line) = string_t('        "activationFunction": "' // &
                                                       self%metadata_(findloc(key, "activationFunction", dim=1))%string() // '",')
        line = line + 1
        lines(line) = string_t('        "usingSkipConnections": ' // &
                                                       self%metadata_(findloc(key, "usingSkipConnections", dim=1))%string())

        line = line + 1
        lines(line) = string_t('    },')

        line = line + 1
        lines(line) = string_t('     "hidden_layers": [')

        layer = 1 
        line = line + 1
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
 
    json_file = file_t(lines)

  end procedure to_json

  module procedure skip
    use_skip_connections = self%metadata_(findloc(key, "usingSkipConnections", dim=1))%string() == "true"
  end procedure

  module procedure activation_function_name
    activation_name = self%metadata_(findloc(key, "activationFunction", dim=1))
  end procedure

end submodule inference_engine_s
