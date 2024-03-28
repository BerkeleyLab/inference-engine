submodule(tensor_range_m) tensor_range_s
  implicit none
  
contains

  module procedure construct_from_components
    tensor_range%minima_ = minima
    tensor_range%maxima_ = maxima 
  end procedure

  module procedure construct_from_json
    integer i, start
    start = 1

    associate(opening_line => range_lines(start)%string())
      call assert(adjustl(opening_line)=='{', "tensor_range_s(construct_from_components): start", opening_line)
    end associate

    line = lines(start)%string()
    associate(colon => index(line, ":"))
      !call assert(adjustl(line(:colon-1))=='"weights"', "neuron_s(construct): neuron weights", line)
      !associate(opening_bracket => colon + index(line(colon+1:), "["))
      !  associate(closing_bracket => opening_bracket + index(line(opening_bracket+1:), "]"))
      !    associate(commas => count("," == [(line(i:i), i=opening_bracket+1,closing_bracket-1)]))
      !      associate(num_inputs => commas + 1)
      !        allocate(neuron%weights_(num_inputs))
      !        read(line(opening_bracket+1:closing_bracket-1), fmt=*) neuron%weights_
      !      end associate
      !    end associate
      !  end associate
      !end associate
    end associate

    !line = neuron_lines(start+2)%string()
    !associate(colon => index(line, ":"))
    !  call assert(adjustl(line(:colon-1))=='"bias"', "neuron_s(construct): neuron bias", line)
    !  read(line(colon+1:), fmt=*) neuron%bias_
    !end associate

    !line = adjustl(neuron_lines(start+3)%string())
    !call assert(line(1:1)=='}', "neuron_s(construct): neuron object end", line)
    !line = adjustr(neuron_lines(start+3)%string())
    !if (line(len(line):len(line)) == ",") neuron%next = construct(neuron_lines, start+4)
  end procedure

  module procedure map_to_unit_range
    normalized_tensor%values_ = (tensor%values() - self%minima_)/(self%maxima_ - self%minima_)
  end procedure

  module procedure map_from_unit_range
    unnormalized_tensor%values_ = self%minima_ + (tensor%values() - self%minima_)*(self%maxima_ - self%minima_)
  end procedure

end submodule tensor_range_s
