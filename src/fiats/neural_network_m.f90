! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module neural_network_m
  !! Define an abstraction that supports inference operations on a neural network
  use activation_m, only : activation_t
  use double_precision_file_m, only : double_precision_file_t
  use kind_parameters_m, only : default_real, double_precision
  use julienne_m, only : file_t, string_t
  use metadata_m, only : metadata_t
  use mini_batch_m, only : mini_batch_t
  use tensor_m, only : tensor_t
  use tensor_map_m, only : tensor_map_t
  implicit none

  private
  public :: neural_network_t
  public :: unmapped_network_t
  public :: workspace_t

  type neural_network_t(k)
    !! Encapsulate the information needed to perform inference
    integer, kind :: k = default_real 
    type(tensor_map_t(k)), private :: input_map_, output_map_
    type(metadata_t), private :: metadata_
    real(k), allocatable, private :: weights_(:,:,:), biases_(:,:)
    integer, allocatable, private :: nodes_(:)
    type(activation_t), private :: activation_
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
    generic :: learn                    => default_real_learn
    generic :: assert_consistency       => default_real_consistency,             double_precision_consistency
    procedure, private, non_overridable :: default_real_consistency,             double_precision_consistency
    procedure, private, non_overridable :: default_real_approximately_equal,     double_precision_approximately_equal
    procedure, private, non_overridable :: default_real_infer,                   double_precision_infer
    procedure, private, non_overridable :: default_real_learn
    procedure, private, non_overridable :: default_real_to_json,                 double_precision_to_json
    procedure, private, non_overridable :: default_real_map_to_input_range,      double_precision_map_to_input_range
    procedure, private, non_overridable :: default_real_map_from_output_range,   double_precision_map_from_output_range
    procedure, private, non_overridable :: default_real_num_hidden_layers,       double_precision_num_hidden_layers
    procedure, private, non_overridable :: default_real_num_inputs,              double_precision_num_inputs
    procedure, private, non_overridable :: default_real_num_outputs,             double_precision_num_outputs
    procedure, private, non_overridable :: default_real_nodes_per_layer,         double_precision_nodes_per_layer
    procedure, private, non_overridable :: default_real_assert_conformable_with, double_precision_assert_conformable_with
    procedure, private, non_overridable :: default_real_skip,                    double_precision_skip
    procedure, private, non_overridable :: default_real_activation_name,         double_precision_activation_name
  end type

  type workspace_t(k)
    integer, kind :: k = default_real
    real(k), allocatable, dimension(:,:) :: a
    real(k), allocatable, dimension(:,:,:) :: dcdw, vdw, sdw, vdwc, sdwc
    real(k), allocatable, dimension(:,:) :: z, delta, dcdb, vdb, sdb, vdbc, sdbc
  contains
    generic :: fully_allocated          => default_real_allocated
    generic :: allocate_if_necessary    => default_real_allocate
    procedure, non_overridable, private :: default_real_allocated
    procedure, non_overridable, private :: default_real_allocate
  end type

  interface workspace_t

    pure module function default_real_workspace(neural_network) result(workspace)
      implicit none
      type(neural_network_t), intent(in) :: neural_network
      type(workspace_t) workspace
    end function

  end interface

  interface

    module subroutine default_real_allocate(self, neural_network)
      implicit none
      class(workspace_t), intent(inout) :: self
      type(neural_network_t), intent(in) :: neural_network
    end subroutine

    pure module function default_real_allocated(self) result(all_allocated)
      implicit none
      class(workspace_t), intent(in) :: self
      logical all_allocated
    end function

  end interface

  interface neural_network_t

    module function default_real_construct_from_components(metadata, weights, biases, nodes, input_map, output_map) &
      result(neural_network)
      implicit none
      type(string_t), intent(in) :: metadata(:)
      real, intent(in) :: weights(:,:,:), biases(:,:)
      integer, intent(in) :: nodes(0:)
      type(tensor_map_t), intent(in), optional :: input_map, output_map
      type(neural_network_t) neural_network
    end function

    module function double_precision_construct_from_components(metadata, weights, biases, nodes, input_map, output_map) &
      result(neural_network)
      implicit none
      type(metadata_t), intent(in) :: metadata
      double precision, intent(in) :: weights(:,:,:), biases(:,:)
      integer, intent(in) :: nodes(0:)
      type(tensor_map_t(double_precision)), intent(in), optional :: input_map, output_map
      type(neural_network_t(double_precision)) neural_network
    end function

    impure elemental module function default_real_from_json(file_) result(neural_network)
      implicit none
      type(file_t), intent(in) :: file_
      type(neural_network_t) neural_network
    end function

    impure elemental module function double_precision_from_json(file) result(neural_network)
      implicit none
      type(double_precision_file_t), intent(in) :: file
      type(neural_network_t(double_precision)) neural_network
    end function

  end interface




  interface ! neural_network_t type-bound procedures

    elemental module function default_real_approximately_equal(lhs, rhs) result(lhs_eq_rhs)
      !! The result is true if lhs and rhs are the same to within a tolerance
      implicit none
      class(neural_network_t), intent(in) :: lhs, rhs
      logical lhs_eq_rhs
    end function

    elemental module function double_precision_approximately_equal(lhs, rhs) result(lhs_eq_rhs)
      !! The result is true if lhs and rhs are the same to within a tolerance
      implicit none
      class(neural_network_t(double_precision)), intent(in) :: lhs, rhs
      logical lhs_eq_rhs
    end function

    elemental module function default_real_map_to_input_range(self, tensor) result(normalized_tensor)
      !! The result contains the input tensor values normalized to fall on the range used during training
      implicit none
      class(neural_network_t), intent(in) :: self
      type(tensor_t), intent(in) :: tensor
      type(tensor_t) normalized_tensor
    end function

    elemental module function double_precision_map_to_input_range(self, tensor) result(normalized_tensor)
      !! The result contains the input tensor values normalized to fall on the range used during training
      implicit none
      class(neural_network_t(double_precision)), intent(in) :: self
      type(tensor_t(double_precision)), intent(in) :: tensor
      type(tensor_t(double_precision)) normalized_tensor
    end function

    elemental module function default_real_map_from_output_range(self, normalized_tensor) result(tensor)
      !! The result contains the output tensor values unmapped via the inverse of the mapping used in training
      implicit none
      class(neural_network_t), intent(in) :: self
      type(tensor_t), intent(in) :: normalized_tensor
      type(tensor_t) tensor
    end function

    elemental module function double_precision_map_from_output_range(self, normalized_tensor) result(tensor)
      !! The result contains the output tensor values unmapped via the inverse of the mapping used in training
      implicit none
      class(neural_network_t(double_precision)), intent(in) :: self
      type(tensor_t(double_precision)), intent(in) :: normalized_tensor
      type(tensor_t(double_precision)) tensor
    end function

    impure elemental module function default_real_to_json(self) result(json_file)
      implicit none
      class(neural_network_t), intent(in) :: self
      type(file_t) json_file
    end function

    impure elemental module function double_precision_to_json(self) result(json_file)
      implicit none
      class(neural_network_t(double_precision)), intent(in) :: self
      type(file_t) json_file
    end function

    elemental module subroutine default_real_assert_conformable_with(self, neural_network)
      implicit none
      class(neural_network_t), intent(in) :: self
      type(neural_network_t), intent(in) :: neural_network
    end subroutine

    elemental module subroutine double_precision_assert_conformable_with(self, neural_network)
      implicit none
      class(neural_network_t(double_precision)), intent(in) :: self
      type(neural_network_t(double_precision)), intent(in) :: neural_network
    end subroutine

    elemental module function default_real_infer(self, inputs) result(outputs)
      implicit none
      class(neural_network_t), intent(in) :: self
      type(tensor_t), intent(in) :: inputs
      type(tensor_t) outputs
    end function

    elemental module function double_precision_infer(self, inputs) result(outputs)
      implicit none
      class(neural_network_t(double_precision)), intent(in) :: self
      type(tensor_t(double_precision)), intent(in) :: inputs
      type(tensor_t(double_precision)) outputs
    end function

    elemental module function default_real_num_outputs(self) result(output_count)
      implicit none
      class(neural_network_t), intent(in) :: self
      integer output_count
    end function

    elemental module function double_precision_num_outputs(self) result(output_count)
      implicit none
      class(neural_network_t(double_precision)), intent(in) :: self
      integer output_count
    end function

    elemental module function default_real_num_hidden_layers(self) result(hidden_layer_count)
      implicit none
      class(neural_network_t), intent(in) :: self
      integer hidden_layer_count
    end function

    elemental module function double_precision_num_hidden_layers(self) result(hidden_layer_count)
      implicit none
      class(neural_network_t(double_precision)), intent(in) :: self
      integer hidden_layer_count
    end function

    elemental module function default_real_num_inputs(self) result(input_count)
      implicit none
      class(neural_network_t), intent(in) :: self
      integer input_count
    end function

    elemental module function double_precision_num_inputs(self) result(input_count)
      implicit none
      class(neural_network_t(double_precision)), intent(in) :: self
      integer input_count
    end function

    pure module function default_real_nodes_per_layer(self) result(node_count)
      implicit none
      class(neural_network_t), intent(in) :: self
      integer, allocatable :: node_count(:)
    end function

    pure module function double_precision_nodes_per_layer(self) result(node_count)
      implicit none
      class(neural_network_t(double_precision)), intent(in) :: self
      integer, allocatable :: node_count(:)
    end function

    elemental module function default_real_activation_name(self) result(activation_name)
      implicit none
      class(neural_network_t), intent(in) :: self
      type(string_t) activation_name
    end function

    elemental module function double_precision_activation_name(self) result(activation_name)
      implicit none
      class(neural_network_t(double_precision)), intent(in) :: self
      type(string_t) activation_name
    end function

    pure module function default_real_skip(self) result(use_skip_connections)
      implicit none
      class(neural_network_t), intent(in) :: self
      logical use_skip_connections
    end function

    pure module function double_precision_skip(self) result(use_skip_connections)
      implicit none
      class(neural_network_t(double_precision)), intent(in) :: self
      logical use_skip_connections
    end function

    pure module subroutine default_real_learn(self, mini_batches_arr, cost, adam, learning_rate, workspace)
      implicit none
      class(neural_network_t), intent(inout) :: self
      type(mini_batch_t), intent(in) :: mini_batches_arr(:)
      real, intent(out), allocatable, optional :: cost(:)
      logical, intent(in) :: adam
      real, intent(in) :: learning_rate
      type(workspace_t), intent(inout) :: workspace
    end subroutine

    pure module subroutine default_real_consistency(self)
      implicit none
      class(neural_network_t), intent(in) :: self
    end subroutine

    pure module subroutine double_precision_consistency(self)
      implicit none
      class(neural_network_t(double_precision)), intent(in) :: self
    end subroutine

  end interface

  type unmapped_network_t(k)
    integer, kind :: k = default_real
    private
    type(neural_network_t(k)) neural_network_
  contains
    generic :: infer                    => default_real_infer_unmapped, double_precision_infer_unmapped
    procedure, private, non_overridable :: default_real_infer_unmapped, double_precision_infer_unmapped
  end type

  interface unmapped_network_t

    impure elemental module function double_precision_unmapped_from_json(file) result(unmapped_network)
      implicit none
      type(double_precision_file_t), intent(in) :: file
      type(unmapped_network_t(double_precision)) unmapped_network
    end function

  end interface

  interface

    elemental module function default_real_infer_unmapped(self, inputs) result(outputs)
      implicit none
      class(unmapped_network_t), intent(in) :: self
      type(tensor_t), intent(in) :: inputs
      type(tensor_t) outputs
    end function

    elemental module function double_precision_infer_unmapped(self, inputs) result(outputs)
      implicit none
      class(unmapped_network_t(double_precision)), intent(in) :: self
      type(tensor_t(double_precision)), intent(in) :: inputs
      type(tensor_t(double_precision)) outputs
    end function

  end interface

end module neural_network_m
