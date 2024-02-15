! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module string_m
  implicit none

  type string_t
    character(len=:), allocatable :: string_
  contains
    procedure :: string
  end type

contains

  pure function string(self)
    class(string_t), intent(in) :: self
    character(len=:), allocatable :: string
    string = self%string_
  end function

end module
! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module neuron_m
  use string_m, only : string_t
  implicit none

  private
  public :: neuron_t

  type neuron_t
    !! linked list of neurons
    private
    real, allocatable :: weights_(:)
    real bias_
    type(neuron_t), allocatable :: next
  contains
    procedure :: weights
    procedure :: bias
    procedure :: next_allocated
    procedure :: next_pointer
    procedure :: num_inputs
  end type

  interface neuron_t

    pure recursive module function construct(neuron_lines, start) result(neuron)
      !! construct linked list of neuron_t objects from an array of JSON-formatted text lines
      implicit none
      type(string_t), intent(in) :: neuron_lines(:)
      integer, intent(in) :: start
      type(neuron_t) neuron
    end function

  end interface

  interface

    module function weights(self) result(my_weights)
      implicit none
      class(neuron_t), intent(in) :: self
      real, allocatable :: my_weights(:)
    end function

    module function bias(self) result(my_bias)
      implicit none
      class(neuron_t), intent(in) :: self
      real my_bias
    end function

    module function next_allocated(self) result(next_is_allocated)
      implicit none
      class(neuron_t), intent(in) :: self
      logical next_is_allocated
    end function

    module function next_pointer(self) result(next_ptr)
      implicit none
      class(neuron_t), intent(in), target :: self
      type(neuron_t), pointer :: next_ptr
    end function

    pure module function num_inputs(self) result(size_weights)
      implicit none
      class(neuron_t), intent(in) :: self
      integer size_weights
    end function

  end interface

end module
! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(neuron_m) neuron_s
  implicit none

contains

  module procedure construct

    character(len=:), allocatable :: line
    integer i

    line = neuron_lines(start+1)%string()
    associate(colon => index(line, ":"))
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
      read(line(colon+1:), fmt=*) neuron%bias_
    end associate

    line = adjustl(neuron_lines(start+3)%string())
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

  module procedure num_inputs
    size_weights = size(self%weights_)
  end procedure

end submodule neuron_s
! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module layer_m
  use neuron_m, only : neuron_t
  use string_m, only : string_t
  implicit none

  private
  public :: layer_t

  type layer_t
    !! linked list of layers, each comprised of a linked list of neurons
    private
    type(neuron_t) neuron !! linked list of this layer's neurons 
    type(layer_t), allocatable :: next !! next layer
#ifdef MAYBEBUG
  contains
    procedure :: count_neurons
#endif
  end type

  interface layer_t

    recursive module function construct(layer_lines, start) result(layer)
      !! construct a linked list of layer_t objects from an array of JSON-formatted text lines
      implicit none
      type(string_t), intent(in) :: layer_lines(:)
      integer, intent(in) :: start
      type(layer_t), target :: layer
    end function

  end interface

  interface
#ifdef MAYBEBUG
    module function count_neurons(layer) result(neurons_per_layer)
      implicit none
      class(layer_t), intent(in), target :: layer
      integer, allocatable :: neurons_per_layer(:)
    end function
#endif
  end interface

end module
! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(layer_m) layer_s
  implicit none

contains

  module procedure construct

    type(neuron_t), pointer ::  neuron 
    integer num_inputs, neurons_in_layer
    character(len=:), allocatable :: line
    logical hidden_layers, output_layer

    line = adjustl(layer_lines(start)%string())
    hidden_layers = line == '['
    output_layer = line == '"output_layer": ['

    layer%neuron = neuron_t(layer_lines, start+1)
    num_inputs = size(layer%neuron%weights())
  end procedure
#ifdef MAYBEBUG
  module procedure count_neurons

    type(layer_t), pointer :: layer_ptr
    type(neuron_t), pointer :: neuron_ptr
    integer num_neurons

    layer_ptr => layer

    allocate(neurons_per_layer(0))

    do  
      num_neurons = 1 
      neuron_ptr => layer_ptr%neuron
      do  
        if (.not. neuron_ptr%next_allocated()) exit
        neuron_ptr => neuron_ptr%next_pointer()
        num_neurons = num_neurons + 1 
      end do
      neurons_per_layer = [neurons_per_layer, num_neurons]
      if (.not. allocated(layer_ptr%next)) exit
      layer_ptr => layer_ptr%next
    end do
 
  end procedure
#endif
end submodule layer_s
