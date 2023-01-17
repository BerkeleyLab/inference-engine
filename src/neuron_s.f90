! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(neuron_m) neuron_s
  use assert_m, only : assert
  implicit none

contains

  module procedure construct

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
    if (line(len(line):len(line)) == ",") neuron%next = construct(neuron_lines, start+4)

  end procedure

  module procedure weights
    my_weights = self%weights_
  end procedure

  module procedure bias
    my_bias = self%bias_
  end procedure

  module procedure next_allocated
    next_is_allocated = allocated(self%next)
  end procedure

  module procedure next_pointer
    next_ptr => self%next
  end procedure

end submodule neuron_s
