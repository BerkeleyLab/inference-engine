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
  contains
    procedure :: count_layers
    procedure :: count_neurons
    procedure :: input_weights
    procedure :: hidden_weights
    procedure :: hidden_biases
    procedure :: neurons_per_layer
    procedure :: next_allocated
    procedure :: next_pointer
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

    module function count_layers(layer) result(num_layers)
      implicit none
      class(layer_t), intent(in), target :: layer
      integer num_layers
    end function

    module function count_neurons(layer) result(neurons_per_layer)
      implicit none
      class(layer_t), intent(in), target :: layer
      integer, allocatable :: neurons_per_layer(:)
    end function

    module function input_weights(self) result(weights)
      implicit none
      class(layer_t), intent(in), target :: self
      real, allocatable :: weights(:,:)
    end function

    module function hidden_weights(self) result(weights)
      implicit none
      class(layer_t), intent(in), target :: self
      real, allocatable :: weights(:,:,:)
    end function

    module function hidden_biases(self) result(biases)
      implicit none
      class(layer_t), intent(in), target :: self
      real, allocatable :: biases(:,:)
    end function

    module function neurons_per_layer(self) result(num_neurons)
      implicit none
      class(layer_t), intent(in), target :: self
      integer num_neurons
    end function

    module function next_allocated(self) result(next_is_allocated)
      implicit none
      class(layer_t), intent(in) :: self
      logical next_is_allocated
    end function

    module function next_pointer(self) result(next_ptr)
      implicit none
      class(layer_t), intent(in), target :: self
      type(layer_t), pointer :: next_ptr
    end function

  end interface

end module
