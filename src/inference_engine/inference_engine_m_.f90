! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module inference_engine_m_
  !! Define an abstraction that supports inference operationsn on a neural network
  use activation_strategy_m, only : activation_strategy_t
  use double_precision_file_m, only : double_precision_file_t
  use kind_parameters_m, only : default_real, double_precision
  use julienne_m, only : file_t, string_t
  use metadata_m, only : metadata_t
  use tensor_m, only : tensor_t
  use tensor_map_m, only : tensor_map_t
  implicit none

  private
  public :: inference_engine_t
  public :: exchange_t

  type inference_engine_t(k)
    !! Encapsulate the minimal information needed to perform inference
    integer, kind :: k = default_real 
    type(tensor_map_t(k)), private :: input_map_, output_map_
    type(metadata_t), private :: metadata_
    real(k), allocatable, private :: weights_(:,:,:), biases_(:,:)
    integer, allocatable, private :: nodes_(:)
    class(activation_strategy_t), allocatable, private :: activation_strategy_ ! Strategy Pattern facilitates elemental activation
  contains
    generic :: operator(==)             => default_real_approximately_equal,     double_precision_approximately_equal
    generic :: infer                    => default_real_infer,                   double_precision_infer
    generic :: to_json                  => default_real_to_json,                 double_precision_to_json
    generic :: map_to_input_range       => default_real_map_to_input_range,      double_precision_map_to_input_range
    generic :: map_from_output_range    => default_real_map_from_output_range,   double_precision_map_from_output_range
    generic :: num_hidden_layers        => default_real_num_hidden_layers,       double_precision_num_hidden_layers
    generic :: num_inputs               => default_real_num_inputs,              double_precision_num_inputs
    generic :: num_outputs              => default_real_num_outputs,             double_precision_num_outputs
    generic :: nodes_per_layer          => default_real_nodes_per_layer,         double_precision_nodes_per_layer
    generic :: assert_conformable_with  => default_real_assert_conformable_with, double_precision_assert_conformable_with
    generic :: skip                     => default_real_skip,                    double_precision_skip
    generic :: activation_function_name => default_real_activation_name,         double_precision_activation_name
    generic :: to_exchange              => default_real_to_exchange,             double_precision_to_exchange
    procedure, private :: default_real_approximately_equal,     double_precision_approximately_equal
    procedure, private :: default_real_infer,                   double_precision_infer
    procedure, private :: default_real_to_json,                 double_precision_to_json
    procedure, private :: default_real_map_to_input_range,      double_precision_map_to_input_range
    procedure, private :: default_real_map_from_output_range,   double_precision_map_from_output_range
    procedure, private :: default_real_num_hidden_layers,       double_precision_num_hidden_layers
    procedure, private :: default_real_num_inputs,              double_precision_num_inputs
    procedure, private :: default_real_num_outputs,             double_precision_num_outputs
    procedure, private :: default_real_nodes_per_layer,         double_precision_nodes_per_layer
    procedure, private :: default_real_assert_conformable_with, double_precision_assert_conformable_with
    procedure, private :: default_real_skip,                    double_precision_skip
    procedure, private :: default_real_activation_name,         double_precision_activation_name
    procedure, private :: default_real_to_exchange,             double_precision_to_exchange
  end type

  type, extends(inference_engine_t) :: unnormalized_engine_t
  contains
    generic :: infer =>   default_real_infer_unnormalized, double_precision_infer_unnormalized
    procedure, private :: default_real_infer_unnormalized, double_precision_infer_unnormalized
  end type

  type exchange_t(k)
    integer, kind :: k = default_real
    type(tensor_map_t(k)) input_map_, output_map_
    type(metadata_t) metadata_
    real(k), allocatable :: weights_(:,:,:), biases_(:,:)
    integer, allocatable :: nodes_(:)
    class(activation_strategy_t), allocatable :: activation_strategy_ ! Strategy Pattern facilitates elemental activation
  end type

  interface inference_engine_t

    impure module function default_real_construct_from_components(metadata, weights, biases, nodes, input_map, output_map) &
      result(inference_engine)
      implicit none
      type(string_t), intent(in) :: metadata(:)
      real, intent(in) :: weights(:,:,:), biases(:,:)
      integer, intent(in) :: nodes(0:)
      type(tensor_map_t), intent(in), optional :: input_map, output_map
      type(inference_engine_t) inference_engine
    end function

    impure module function double_precision_construct_from_components(metadata, weights, biases, nodes, input_map, output_map) &
      result(inference_engine)
      implicit none
      type(metadata_t), intent(in) :: metadata
      double precision, intent(in) :: weights(:,:,:), biases(:,:)
      integer, intent(in) :: nodes(0:)
      type(tensor_map_t(double_precision)), intent(in), optional :: input_map, output_map
      type(inference_engine_t(double_precision)) inference_engine
    end function

    impure elemental module function default_real_from_json(file_) result(inference_engine)
      implicit none
      type(file_t), intent(in) :: file_
      type(inference_engine_t) inference_engine
    end function

    impure elemental module function double_precision_from_json(file) result(inference_engine)
      implicit none
      type(double_precision_file_t), intent(in) :: file
      type(inference_engine_t(double_precision)) inference_engine
    end function

  end interface

  interface

    elemental module function default_real_approximately_equal(lhs, rhs) result(lhs_eq_rhs)
      !! The result is true if lhs and rhs are the same to within a tolerance
      implicit none
      class(inference_engine_t), intent(in) :: lhs, rhs
      logical lhs_eq_rhs
    end function

    elemental module function double_precision_approximately_equal(lhs, rhs) result(lhs_eq_rhs)
      !! The result is true if lhs and rhs are the same to within a tolerance
      implicit none
      class(inference_engine_t(double_precision)), intent(in) :: lhs, rhs
      logical lhs_eq_rhs
    end function

    elemental module function default_real_map_to_input_range(self, tensor) result(normalized_tensor)
      !! The result contains the input tensor values normalized to fall on the range used during training
      implicit none
      class(inference_engine_t), intent(in) :: self
      type(tensor_t), intent(in) :: tensor
      type(tensor_t) normalized_tensor
    end function

    elemental module function double_precision_map_to_input_range(self, tensor) result(normalized_tensor)
      !! The result contains the input tensor values normalized to fall on the range used during training
      implicit none
      class(inference_engine_t(double_precision)), intent(in) :: self
      type(tensor_t(double_precision)), intent(in) :: tensor
      type(tensor_t(double_precision)) normalized_tensor
    end function

    elemental module function default_real_map_from_output_range(self, normalized_tensor) result(tensor)
      !! The result contains the output tensor values unnormalized via the inverse of the mapping used in training
      implicit none
      class(inference_engine_t), intent(in) :: self
      type(tensor_t), intent(in) :: normalized_tensor
      type(tensor_t) tensor
    end function

    elemental module function double_precision_map_from_output_range(self, normalized_tensor) result(tensor)
      !! The result contains the output tensor values unnormalized via the inverse of the mapping used in training
      implicit none
      class(inference_engine_t(double_precision)), intent(in) :: self
      type(tensor_t(double_precision)), intent(in) :: normalized_tensor
      type(tensor_t(double_precision)) tensor
    end function

    impure module function default_real_to_exchange(self) result(exchange)
      implicit none
      class(inference_engine_t), intent(in) :: self
      type(exchange_t) exchange
    end function

    impure module function double_precision_to_exchange(self) result(exchange)
      implicit none
      class(inference_engine_t(double_precision)), intent(in) :: self
      type(exchange_t(double_precision)) exchange
    end function

    impure elemental module function default_real_to_json(self) result(json_file)
      implicit none
      class(inference_engine_t), intent(in) :: self
      type(file_t) json_file
    end function

    impure elemental module function double_precision_to_json(self) result(json_file)
      implicit none
      class(inference_engine_t(double_precision)), intent(in) :: self
      type(file_t) json_file
    end function

    elemental module subroutine default_real_assert_conformable_with(self, inference_engine)
      implicit none
      class(inference_engine_t), intent(in) :: self
      type(inference_engine_t), intent(in) :: inference_engine
    end subroutine

    elemental module subroutine double_precision_assert_conformable_with(self, inference_engine)
      implicit none
      class(inference_engine_t(double_precision)), intent(in) :: self
      type(inference_engine_t(double_precision)), intent(in) :: inference_engine
    end subroutine

    elemental module function default_real_infer(self, inputs) result(outputs)
      implicit none
      class(inference_engine_t), intent(in) :: self
      type(tensor_t), intent(in) :: inputs
      type(tensor_t) outputs
    end function

    elemental module function default_real_infer_unnormalized(self, inputs) result(outputs)
      implicit none
      class(unnormalized_engine_t), intent(in) :: self
      type(tensor_t), intent(in) :: inputs
      type(tensor_t) outputs
    end function

    elemental module function double_precision_infer_unnormalized(self, inputs) result(outputs)
      implicit none
      class(unnormalized_engine_t(double_precision)), intent(in) :: self
      type(tensor_t(double_precision)), intent(in) :: inputs
      type(tensor_t(double_precision)) outputs
    end function

    elemental module function double_precision_infer(self, inputs) result(outputs)
      implicit none
      class(inference_engine_t(double_precision)), intent(in) :: self
      type(tensor_t(double_precision)), intent(in) :: inputs
      type(tensor_t(double_precision)) outputs
    end function

    elemental module function default_real_num_outputs(self) result(output_count)
      implicit none
      class(inference_engine_t), intent(in) :: self
      integer output_count
    end function

    elemental module function double_precision_num_outputs(self) result(output_count)
      implicit none
      class(inference_engine_t(double_precision)), intent(in) :: self
      integer output_count
    end function

    elemental module function default_real_num_hidden_layers(self) result(hidden_layer_count)
      implicit none
      class(inference_engine_t), intent(in) :: self
      integer hidden_layer_count
    end function

    elemental module function double_precision_num_hidden_layers(self) result(hidden_layer_count)
      implicit none
      class(inference_engine_t(double_precision)), intent(in) :: self
      integer hidden_layer_count
    end function

    elemental module function default_real_num_inputs(self) result(input_count)
      implicit none
      class(inference_engine_t), intent(in) :: self
      integer input_count
    end function

    elemental module function double_precision_num_inputs(self) result(input_count)
      implicit none
      class(inference_engine_t(double_precision)), intent(in) :: self
      integer input_count
    end function

    pure module function default_real_nodes_per_layer(self) result(node_count)
      implicit none
      class(inference_engine_t), intent(in) :: self
      integer, allocatable :: node_count(:)
    end function

    pure module function double_precision_nodes_per_layer(self) result(node_count)
      implicit none
      class(inference_engine_t(double_precision)), intent(in) :: self
      integer, allocatable :: node_count(:)
    end function

    elemental module function default_real_activation_name(self) result(activation_name)
      implicit none
      class(inference_engine_t), intent(in) :: self
      type(string_t) activation_name
    end function

    elemental module function double_precision_activation_name(self) result(activation_name)
      implicit none
      class(inference_engine_t(double_precision)), intent(in) :: self
      type(string_t) activation_name
    end function

    pure module function default_real_skip(self) result(use_skip_connections)
      implicit none
      class(inference_engine_t), intent(in) :: self
      logical use_skip_connections
    end function

    pure module function double_precision_skip(self) result(use_skip_connections)
      implicit none
      class(inference_engine_t(double_precision)), intent(in) :: self
      logical use_skip_connections
    end function

  end interface

end module inference_engine_m_
