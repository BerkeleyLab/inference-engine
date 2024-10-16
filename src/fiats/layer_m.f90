! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module layer_m
  use double_precision_string_m, only : double_precision_string_t
  use kind_parameters_m, only : default_real, double_precision
  use julienne_string_m, only : string_t
  use neural_network_m, only : neural_network_t
  use metadata_m, only : metadata_t
  use neuron_m, only : neuron_t
  use tensor_map_m, only : tensor_map_t
  implicit none

  private
  public :: layer_t

  type layer_t(k)
    !! linked list of layers, each comprised of a linked list of neurons
    integer, kind :: k = default_real
    type(neuron_t(k)),             private :: neuron !! linked list of this layer's neurons 
    type(layer_t(k)), allocatable, private :: next !! next layer
  contains
    generic :: neural_network    => default_real_neural_network   , double_precision_neural_network
    procedure, private           :: default_real_neural_network   , double_precision_neural_network
    generic :: count_layers      => default_real_count_layers     , double_precision_count_layers
    procedure, private           :: default_real_count_layers     , double_precision_count_layers
    generic :: count_neurons     => default_real_count_neurons    , double_precision_count_neurons
    procedure, private           :: default_real_count_neurons    , double_precision_count_neurons
    generic :: count_inputs      => default_real_count_inputs     , double_precision_count_inputs
    procedure, private           :: default_real_count_inputs     , double_precision_count_inputs
    generic :: neurons_per_layer => default_real_neurons_per_layer, double_precision_neurons_per_layer
    procedure, private           :: default_real_neurons_per_layer, double_precision_neurons_per_layer
    generic :: next_allocated    => default_real_next_allocated   , double_precision_next_allocated
    procedure, private           :: default_real_next_allocated   , double_precision_next_allocated
    generic :: next_pointer      => default_real_next_pointer     , double_precision_next_pointer 
    procedure, private           :: default_real_next_pointer     , double_precision_next_pointer
  end type

  interface layer_t

    recursive module function default_real_construct_layer(layer_lines, start) result(layer)
      !! construct a linked list of layer_t objects from an array of JSON-formatted text lines
      implicit none
      type(string_t), intent(in) :: layer_lines(:)
      integer, intent(in) :: start
      type(layer_t), target :: layer
    end function

    recursive module function double_precision_construct_layer(layer_lines, start) result(layer)
      !! construct a linked list of layer_t objects from an array of JSON-formatted text lines
      implicit none
      type(double_precision_string_t), intent(in) :: layer_lines(:)
      integer, intent(in) :: start
      type(layer_t(double_precision)), target :: layer
    end function

  end interface

  interface

    module function default_real_neural_network(hidden_layers, metadata, output_layer, input_map, output_map) result(neural_network_)
      implicit none
      class(layer_t), intent(in), target :: hidden_layers
      type(layer_t), intent(in), target :: output_layer
      type(string_t), intent(in) :: metadata(:)
      type(tensor_map_t), intent(in) :: input_map, output_map
      type(neural_network_t) neural_network_
    end function

    module function double_precision_neural_network(hidden_layers, metadata, output_layer, input_map, output_map) result(neural_network_)
      implicit none
      class(layer_t(double_precision)), intent(in), target :: hidden_layers
      type(layer_t(double_precision)), intent(in), target :: output_layer
      type(metadata_t), intent(in) :: metadata
      type(tensor_map_t(double_precision)), intent(in) :: input_map, output_map
      type(neural_network_t(double_precision)) neural_network_
    end function

    module function default_real_count_layers(layer) result(num_layers)
      implicit none
      class(layer_t), intent(in), target :: layer
      integer num_layers
    end function

    module function double_precision_count_layers(layer) result(num_layers)
      implicit none
      class(layer_t(double_precision)), intent(in), target :: layer
      integer num_layers
    end function

    module function default_real_count_neurons(layer) result(neurons_per_layer_result)
      implicit none
      class(layer_t), intent(in), target :: layer
      integer, allocatable :: neurons_per_layer_result(:)
    end function

    module function double_precision_count_neurons(layer) result(neurons_per_layer_result)
      implicit none
      class(layer_t(double_precision)), intent(in), target :: layer
      integer, allocatable :: neurons_per_layer_result(:)
    end function

    module function default_real_count_inputs(layer) result(num_inputs)
      implicit none
      class(layer_t), intent(in) :: layer
      integer num_inputs
    end function

    module function double_precision_count_inputs(layer) result(num_inputs)
      implicit none
      class(layer_t(double_precision)), intent(in) :: layer
      integer num_inputs
    end function

    module function default_real_neurons_per_layer(self) result(num_neurons)
      implicit none
      class(layer_t), intent(in), target :: self
      integer num_neurons
    end function

    module function double_precision_neurons_per_layer(self) result(num_neurons)
      implicit none
      class(layer_t(double_precision)), intent(in), target :: self
      integer num_neurons
    end function

    module function default_real_next_allocated(self) result(next_is_allocated)
      implicit none
      class(layer_t), intent(in) :: self
      logical next_is_allocated
    end function

    module function double_precision_next_allocated(self) result(next_is_allocated)
      implicit none
      class(layer_t(double_precision)), intent(in) :: self
      logical next_is_allocated
    end function

    module function default_real_next_pointer(self) result(next_ptr)
      implicit none
      class(layer_t), intent(in), target :: self
      type(layer_t), pointer :: next_ptr
    end function

    module function double_precision_next_pointer(self) result(next_ptr)
      implicit none
      class(layer_t(double_precision)), intent(in), target :: self
      type(layer_t(double_precision)), pointer :: next_ptr
    end function

  end interface

end module
