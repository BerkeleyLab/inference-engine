! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module layer_m
  use string_m, only : string_t
  implicit none

  private
  public :: layer_t, neuron_t

  type neuron_t
    !! linked list of neurons
    private
    real, allocatable :: weights_(:)
    real bias_
    type(neuron_t), allocatable :: next
  contains
    procedure :: weights
    procedure :: bias
  end type

  type layer_t
    !! linked list of layers, each comprised of a linked list of neurons
    private
    type(neuron_t) neuron
    type(layer_t), allocatable :: next
  contains
    procedure :: count_layers
    procedure :: count_neurons
  end type

  interface layer_t

    recursive module function read_layer_list(layer_lines, start) result(layer)
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

  end interface

  interface neuron_t

    pure recursive module function read_neuron_list(neuron_lines, start) result(neuron)
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

  end interface

end module
