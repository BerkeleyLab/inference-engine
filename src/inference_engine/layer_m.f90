! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module layer_m
  use neuron_m, only : neuron_t
  use sourcery_string_m, only : string_t
  use kind_parameters_m, only : rkind
  use inference_engine_m_, only : inference_engine_t
  implicit none

  private
  public :: layer_t

  type layer_t
    !! linked list of layers, each comprised of a linked list of neurons
    private
    type(neuron_t) neuron !! linked list of this layer's neurons 
    type(layer_t), allocatable :: next !! next layer
  contains
    procedure :: inference_engine
    procedure :: count_layers
    procedure :: count_neurons
    procedure :: count_inputs
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

    module function inference_engine(hidden_layers, metadata, output_layer) result(inference_engine_)
      implicit none
      class(layer_t), intent(in), target :: hidden_layers
      type(layer_t), intent(in), target :: output_layer
      type(string_t), intent(in) :: metadata(:)
      type(inference_engine_t) inference_engine_
    end function

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

    module function count_inputs(layer) result(num_inputs)
      implicit none
      class(layer_t), intent(in) :: layer
      integer num_inputs
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
