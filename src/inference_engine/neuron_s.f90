! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(neuron_m) neuron_s
  use assert_m, only : assert
  use julienne_formats_m, only : separated_values
  implicit none

contains

  module procedure default_real_to_json
    integer, parameter :: characters_per_value=17
    character(len=*), parameter :: indent = repeat(" ",ncopies=12)
    character(len=:), allocatable :: csv_format, weights_string, bias_string

    call assert(allocated(self%weights_), "neuron_s(to_json): allocated weights_")

    csv_format = separated_values(separator=",", mold=[real::])
    allocate(character(len=size(self%weights_)*(characters_per_value+1)-1)::weights_string)
    allocate(character(len=characters_per_value)::bias_string)
    write(weights_string, fmt = csv_format) self%weights_
    write(bias_string,*) self%bias_
    lines = [ &
      string_t(indent // '{'), &
      string_t(indent // '  "weights": [' // trim(adjustl(weights_string)) // '],'), &
      string_t(indent // '  "bias": ' // trim(adjustl(bias_string))), &
      string_t(indent // '}') &
    ]
  end procedure

  module procedure double_precision_to_json
    integer, parameter :: characters_per_value=34
    character(len=*), parameter :: indent = repeat(" ",ncopies=12)
    character(len=:), allocatable :: csv_format, weights_string, bias_string

    call assert(allocated(self%weights_), "neuron_s(to_json): allocated weights_")

    csv_format = separated_values(separator=",", mold=[double precision::])
    allocate(character(len=size(self%weights_)*(characters_per_value+1)-1)::weights_string)
    allocate(character(len=characters_per_value)::bias_string)
    write(weights_string, fmt = csv_format) self%weights_
    write(bias_string,*) self%bias_
    lines = [ &
      string_t(indent // '{'), &
      string_t(indent // '  "weights": [' // trim(adjustl(weights_string)) // '],'), &
      string_t(indent // '  "bias": ' // trim(adjustl(bias_string))), &
      string_t(indent // '}') &
    ]
  end procedure

  module procedure from_json

    character(len=:), allocatable :: line
    integer i

    call assert(adjustl(neuron_lines(start)%string())=='{', "neuron_s(construct): neuron object start",neuron_lines(start)%string())

    line = neuron_lines(start+1)%string()
    associate(colon => index(line, ":"))
      call assert(adjustl(line(:colon-1))=='"weights"', "neuron_s(construct): neuron weights", line)
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
      call assert(adjustl(line(:colon-1))=='"bias"', "neuron_s(construct): neuron bias", line)
      read(line(colon+1:), fmt=*) neuron%bias_
    end associate

    line = adjustl(neuron_lines(start+3)%string())
    call assert(line(1:1)=='}', "neuron_s(construct): neuron object end", line)
    line = adjustr(neuron_lines(start+3)%string())
    if (line(len(line):len(line)) == ",") neuron%next = from_json(neuron_lines, start+4)

  end procedure

  module procedure double_precision_from_json

    character(len=:), allocatable :: line
    integer i

    call assert(adjustl(neuron_lines(start)%string())=='{', &
      "neuron_s(double_precison_from_json): neuron object start",neuron_lines(start)%string())

    line = neuron_lines(start+1)%string()
    associate(colon => index(line, ":"))
      call assert(adjustl(line(:colon-1))=='"weights"', "neuron_s(double_precision_from_json): neuron weights", line)
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
      call assert(adjustl(line(:colon-1))=='"bias"', "neuron_s(double_precision_from_json): neuron bias", line)
      read(line(colon+1:), fmt=*) neuron%bias_
    end associate

    line = adjustl(neuron_lines(start+3)%string())
    call assert(line(1:1)=='}', "neuron_s(double_precision_from_json): neuron object end", line)
    line = adjustr(neuron_lines(start+3)%string())
    if (line(len(line):len(line)) == ",") neuron%next = double_precision_from_json(neuron_lines, start+4)

  end procedure

  module procedure default_real_from_components
    neuron%weights_ = weights
    neuron%bias_ = bias
  end procedure

  module procedure double_precision_from_components
    neuron%weights_ = weights
    neuron%bias_ = bias
  end procedure

  module procedure default_real_weights
    my_weights = self%weights_
  end procedure

  module procedure double_precision_weights
    my_weights = self%weights_
  end procedure

  module procedure default_real_bias
    my_bias = self%bias_
  end procedure

  module procedure double_precision_bias
    my_bias = self%bias_
  end procedure

  module procedure default_real_next_allocated
    next_is_allocated = allocated(self%next)
  end procedure

  module procedure double_precision_next_allocated
    next_is_allocated = allocated(self%next)
  end procedure

  module procedure default_real_next_pointer
    if (allocated(self%next)) then
      next_ptr => self%next
    else
      next_ptr => null()
    end if
  end procedure

  module procedure double_precision_next_pointer
    if (allocated(self%next)) then
      next_ptr => self%next
    else
      next_ptr => null()
    end if
  end procedure

  module procedure default_real_num_inputs
    size_weights = size(self%weights_)
  end procedure

  module procedure double_precision_num_inputs
    size_weights = size(self%weights_)
  end procedure

end submodule neuron_s
