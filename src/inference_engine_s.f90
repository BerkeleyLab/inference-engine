! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(inference_engine_m) inference_engine_s
  use assert_m, only : assert
  use intrinsic_array_m, only : intrinsic_array_t
  use matmul_m, only : matmul_t
  use step_m, only : step_t
  use swish_m, only : swish_t
  use sigmoid_m, only : sigmoid_t
  use layer_m, only : layer_t
  use neuron_m, only : neuron_t
  use file_m, only : file_t
  use formats_m, only : separated_values
  use iso_fortran_env, only : iostat_end
  implicit none

contains

  module procedure construct_from_components

    real(rkind), allocatable :: transposed(:,:,:)
    integer layer

    allocate(transposed(size(hidden_weights,2), size(hidden_weights,1), size(hidden_weights,3)))
    do concurrent(layer = 1:size(hidden_weights,3))
      transposed(:,:,layer) = transpose(hidden_weights(:,:,layer))
    end do

    inference_engine%metadata_ = metadata
    inference_engine%input_weights_ = transpose(input_weights)
    inference_engine%hidden_weights_ = transposed
    inference_engine%output_weights_ = output_weights
    inference_engine%biases_ = biases
    inference_engine%output_biases_ = output_biases

    ! This code is repeated in both constructors and needs to be maintained consistently in both
    ! places.  It can't be factored into one factory method pattern because the result would need
    ! to be allocatable and polymorphic, which would preclude the function being pure so it 
    ! wouldn't be possible to call it from inside this pure constructor function.
    select case(inference_engine%metadata_(findloc(key, "activationFunction", dim=1))%string())
      case("swish")
        inference_engine%activation_strategy_ = swish_t()
      case("sigmoid")
        inference_engine%activation_strategy_ = sigmoid_t()
      case("step")
        inference_engine%activation_strategy_ = step_t()
      case default
        error stop "inference_engine_t construct_from_json: unrecognized activation strategy"
    end select

    call assert_consistent(inference_engine)

  end procedure

  module procedure construct_from_json

    type(string_t), allocatable :: lines(:)
    type(layer_t) hidden_layers, output_layer
    type(neuron_t) output_neuron
    real(rkind), allocatable :: hidden_weights(:,:,:)
    integer l

    lines = file_%lines()

    l = 1
    call assert(adjustl(lines(l)%string())=="{", "construct_from_json: expecting '{' to start outermost object", lines(l)%string())

    l = 2
    inference_engine%metadata_ = [string_t(""),string_t(""),string_t(""),string_t(""),string_t("false")]
    if (adjustl(lines(l)%string()) == '"metadata": {') then
      block
        character(len=:), allocatable :: justified_line
        do 
          l = l + 1
          justified_line = adjustl(lines(l)%string())
          if (justified_line == "},") exit
          inference_engine%metadata_(findloc(key, trim(get_key_string(justified_line)), dim=1)) = get_key_value(justified_line)
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

    inference_engine%input_weights_ = transpose(hidden_layers%input_weights())
    call assert(hidden_layers%next_allocated(), "inference_engine_t%from_json: next layer exists")

    block 
      type(layer_t), pointer :: next_layer
      real(rkind), allocatable :: transposed(:,:,:)
      integer layer

      next_layer => hidden_layers%next_pointer()
      hidden_weights = next_layer%hidden_weights()
      inference_engine%biases_ = hidden_layers%hidden_biases()

      allocate(transposed(size(hidden_weights,2), size(hidden_weights,1), size(hidden_weights,3)))
      do concurrent(layer = 1:size(hidden_weights,3)) 
        transposed(:,:,layer) = transpose(hidden_weights(:,:,layer))
      end do
      inference_engine%hidden_weights_ = transposed
    end block

    inference_engine%output_weights_ = output_layer%output_weights()
    inference_engine%output_biases_ = output_layer%output_biases()

    select case(inference_engine%metadata_(findloc(key, "activationFunction", dim=1))%string())
      case("swish")
        inference_engine%activation_strategy_ = swish_t()
      case("sigmoid")
        inference_engine%activation_strategy_ = sigmoid_t()
      case("step")
        inference_engine%activation_strategy_ = step_t()
      case default
        error stop "inference_engine_t construct_from_json: unrecognized activation strategy"
    end select

    call assert_consistent(inference_engine)

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
            end if
            value_ = string_t(text_after_colon(opening_value_quotes+1:closing_value_quotes-1))
          end associate
        end associate
      end associate
    end function

  end procedure construct_from_json

  module procedure assert_conformable_with

    call assert_consistent(self)
    call assert_consistent(inference_engine)

    associate(equal_shapes => [ &
      shape(self%input_weights_ ) == shape(inference_engine%input_weights_ ), &
      shape(self%hidden_weights_) == shape(inference_engine%hidden_weights_), &
      shape(self%output_weights_) == shape(inference_engine%output_weights_), &
      shape(self%biases_        ) == shape(inference_engine%biases_        ), &
      shape(self%output_biases_ ) == shape(inference_engine%output_biases_ )  &
     ])
      call assert(all(equal_shapes), "assert_conformable_with: all(equal_shapes)", intrinsic_array_t(equal_shapes))
    end associate

    call assert(same_type_as(self%activation_strategy_, inference_engine%activation_strategy_), "assert_conformable_with: types)")
    
  end procedure

  module procedure subtract
    call self%assert_conformable_with(rhs)

    difference%metadata_       = self%metadata_
    difference%input_weights_  = self%input_weights_  - rhs%input_weights_ 
    difference%hidden_weights_ = self%hidden_weights_ - rhs%hidden_weights_
    difference%output_weights_ = self%output_weights_ - rhs%output_weights_
    difference%biases_         = self%biases_         - rhs%biases_         
    difference%output_biases_  = self%output_biases_  - rhs%output_biases_ 
    difference%activation_strategy_ = self%activation_strategy_

    call assert_consistent(difference)
  end procedure

  module procedure norm 
    call assert_consistent(self)
    norm_of_self = maxval(abs(self%input_weights_)) + maxval(abs(self%hidden_weights_)) + maxval(abs(self%output_weights_)) + & 
           maxval(abs(self%biases_)) + maxval(abs(self%output_biases_))
  end procedure

  pure subroutine assert_consistent(self)
    type(inference_engine_t), intent(in) :: self

    call assert(all(self%metadata_%is_allocated()), "inference_engine_t%assert_consistent: self%metadata_s%is_allocated()")
    call assert(allocated(self%activation_strategy_), "inference_engine_t%assert_consistent: allocated(self%activation_strategy_)")

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
        ubound(self%input_weights_,  1) - lbound(self%input_weights_,  1), &
        ubound(self%output_weights_, 2) - lbound(self%output_weights_, 2)  &
    ] ) 
      call assert(all(num_neurons == num_neurons(1)), "inference_engine_s(assert_consistent): num_neurons", &
        intrinsic_array_t(num_neurons) &
      )
    end associate

    associate(output_count => 1 + &
      [ ubound(self%output_weights_, 1) - lbound(self%output_weights_, 1), & 
        ubound(self%output_biases_,  1) - lbound(self%output_biases_,  1)  &
    ] )
      call assert(all(output_count == output_count(1)), "inference_engine_s(assert_consistent): output_count", &
        intrinsic_array_t(output_count) &
      )
    end associate
  end subroutine

  module procedure num_outputs
    call assert_consistent(self)
    output_count = ubound(self%output_weights_,1) - lbound(self%output_weights_,1) + 1
  end procedure

  module procedure num_inputs
    call assert_consistent(self)
    input_count = ubound(self%input_weights_,2) - lbound(self%input_weights_,2) + 1
  end procedure

  module procedure neurons_per_layer
    call assert_consistent(self)
    neuron_count = ubound(self%input_weights_,1) - lbound(self%input_weights_,1) + 1
  end procedure

  module procedure num_hidden_layers
    call assert_consistent(self)
    hidden_layer_count = ubound(self%hidden_weights_,3) - lbound(self%hidden_weights_,3) + 1
  end procedure

  module procedure infer_from_array_of_inputs
    integer layer

    call assert_consistent(self)

    output = inference_strategy%infer( &
      input = input, &
      input_weights = self%input_weights_, &
      hidden_weights = self%hidden_weights_ , &
      biases = self%biases_, &
      output_biases = self%output_biases_, &
      output_weights = self%output_weights_, &
      activation_strategy = self%activation_strategy_, &
      skip = self%skip() &
    )
  end procedure

  module procedure infer_from_inputs_object
    integer layer

    call assert_consistent(self)

    outputs%outputs_ = inference_strategy%infer( &
      input = inputs%inputs_, &
      input_weights = self%input_weights_, &
      hidden_weights = self%hidden_weights_ , &
      biases = self%biases_, &
      output_biases = self%output_biases_, &
      output_weights = self%output_weights_, &
      activation_strategy = self%activation_strategy_, &
      skip = self%skip() &
    )
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

    call assert_consistent(self)

    csv_format = separated_values(separator=",", mold=[real(rkind)::])

    associate(num_hidden_layers => self%num_hidden_layers(),  neurons_per_layer => self%neurons_per_layer(), &
      num_outputs => self%num_outputs(), num_inputs => self%num_inputs())
      associate(num_lines => &
        outer_object_braces &
        + metadata_outer_braces + size(key) &
        + hidden_layer_outer_brackets + (num_hidden_layers + 1)*(inner_brackets_per_layer + neurons_per_layer*lines_per_neuron) &
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
        lines(line) = string_t('        "usingSkipConnections": "' // &
                                                       self%metadata_(findloc(key, "usingSkipConnections", dim=1))%string() // '",')

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
          allocate(character(len=num_inputs*(characters_per_value+1)-1)::comma_separated_values)
          write(comma_separated_values, fmt = csv_format) self%input_weights_(neuron,:)
          lines(line) = string_t('                "weights": [' // trim(comma_separated_values) // '],')
          deallocate(comma_separated_values)
          line = line + 1
          write(single_value, fmt = csv_format) self%biases_(neuron,layer)
          lines(line) = string_t('                 "bias": ' // trim(single_value))
          line = line + 1
          lines(line) = string_t("             }" // trim(merge(' ',',',neuron==neurons_per_layer)))
        end do
        line = line + 1
        lines(line) = string_t(trim(merge("         ],", "         ] ", line/=num_hidden_layers + 1)))

        do layer = 1, num_hidden_layers
          line = line + 1
          lines(line) = string_t('         [')
          do neuron = 1, neurons_per_layer
            line = line + 1
            lines(line) = string_t('             {')
            line = line + 1
            allocate(character(len=neurons_per_layer*(characters_per_value+1)-1)::comma_separated_values)
            write(comma_separated_values, fmt = csv_format) self%hidden_weights_(:, neuron, layer)
            lines(line) = string_t('                "weights": [' // trim(comma_separated_values) // '],')
            deallocate(comma_separated_values)
            line = line + 1
            write(single_value, fmt = csv_format) self%biases_(neuron,layer+1)
            lines(line) = string_t('                 "bias": ' // trim(single_value))
            line = line + 1
            lines(line) = string_t("             }" // trim(merge(' ',',',neuron==neurons_per_layer)))
          end do
          line = line + 1
          lines(line) = string_t("         ]" // trim(merge(' ',',',layer==num_hidden_layers)))
        end do

        line = line + 1
        lines(line) = string_t("     ],")

        line = line + 1
        lines(line) = string_t('     "output_layer": [')

        do neuron = 1, num_outputs
          line = line + 1
          lines(line) = string_t('             {')
          line = line + 1
          allocate(character(len=neurons_per_layer*(characters_per_value+1)-1)::comma_separated_values)
          write(comma_separated_values, fmt = csv_format) self%output_weights_(neuron,:)
          lines(line) = string_t('                "weights": [' // trim(comma_separated_values) // '],')
          deallocate(comma_separated_values)
          line = line + 1
          write(single_value, fmt = csv_format) self%output_biases_(neuron)
          lines(line) = string_t('                 "bias": ' // trim(single_value))
          line = line + 1
          lines(line) = string_t("             }")
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

end submodule inference_engine_s
