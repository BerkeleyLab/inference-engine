! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module trainable_engine_m
  !! Define an abstraction that supports training a neural network
  use julienne_string_m, only : string_t
  use inference_engine_m_, only : inference_engine_t
  use differentiable_activation_strategy_m, only : differentiable_activation_strategy_t
  use kind_parameters_m, only : rkind
  use metadata_m, only : metadata_t
  use tensor_m, only :  tensor_t
  use tensor_map_m, only :  tensor_map_t
  use mini_batch_m, only : mini_batch_t
  use training_configuration_m, only : training_configuration_t
  use input_output_pair_m, only : input_output_pair_t
  implicit none

  private
  public :: trainable_engine_t

  type trainable_engine_t
    !! Encapsulate the information needed to perform training
    private
    type(tensor_map_t) input_map_, output_map_
    type(metadata_t) metadata_
    real(rkind), allocatable :: w(:,:,:) ! weights
    real(rkind), allocatable :: b(:,:) ! biases
    integer, allocatable :: n(:) ! nodes per layer
    class(differentiable_activation_strategy_t), allocatable :: differentiable_activation_strategy_ 
    real(rkind), allocatable, dimension(:,:) :: a
    real(rkind), allocatable, dimension(:,:,:) :: dcdw, vdw, sdw, vdwc, sdwc
    real(rkind), allocatable, dimension(:,:) :: z, delta, dcdb, vdb, sdb, vdbc, sdbc
  contains
    procedure :: assert_consistent
    procedure :: train
    procedure :: infer
    procedure :: num_layers
    procedure :: num_inputs
    procedure :: num_outputs
    procedure :: to_inference_engine
    procedure :: map_to_input_training_range
    procedure :: map_from_input_training_range
    procedure :: map_to_output_training_range
    procedure :: map_from_output_training_range
    procedure :: map_to_training_ranges
  end type

  integer, parameter :: input_layer = 0

  interface trainable_engine_t
#ifdef __INTEL_COMPILER
     impure module function construct_trainable_engine_from_padded_arrays( &
       nodes, weights, biases, differentiable_activation_strategy, metadata, input_map, output_map &
     ) &
#else
     impure module function construct_from_padded_arrays( &
       nodes, weights, biases, differentiable_activation_strategy, metadata, input_map, output_map &
     ) &
#endif
      result(trainable_engine)
      implicit none
      integer, intent(in) :: nodes(input_layer:)
      real(rkind), intent(in)  :: weights(:,:,:), biases(:,:)
      class(differentiable_activation_strategy_t), intent(in) :: differentiable_activation_strategy
      type(string_t), intent(in) :: metadata(:)
      type(tensor_map_t), intent(in), optional :: input_map, output_map
      type(trainable_engine_t) trainable_engine
    end function

    impure module function construct_from_inference_engine(inference_engine) result(trainable_engine)
      implicit none
      type(inference_engine_t), intent(in) :: inference_engine
      type(trainable_engine_t) trainable_engine
    end function

    module function perturbed_identity_network(training_configuration, perturbation_magnitude, metadata, input_map, output_map)&
      result(trainable_engine)
      implicit none
      type(training_configuration_t), intent(in) :: training_configuration
      type(string_t), intent(in) :: metadata(:)
      real(rkind), intent(in) :: perturbation_magnitude
      type(tensor_map_t) input_map, output_map
      type(trainable_engine_t) trainable_engine
    end function

  end interface

  interface

    pure module subroutine assert_consistent(self)
      implicit none
      class(trainable_engine_t), intent(in) :: self
    end subroutine

    pure module subroutine train(self, mini_batches_arr, cost, adam, learning_rate)
      implicit none
      class(trainable_engine_t), intent(inout) :: self
      type(mini_batch_t), intent(in) :: mini_batches_arr(:)
      real(rkind), intent(out), allocatable, optional :: cost(:)
      logical, intent(in) :: adam
      real(rkind), intent(in) :: learning_rate
    end subroutine

    elemental module function infer(self, inputs) result(outputs)
      implicit none
      class(trainable_engine_t), intent(in) :: self
      type(tensor_t), intent(in) :: inputs
      type(tensor_t) outputs
    end function
    
    elemental module function num_inputs(self) result(n_in)
      implicit none
      class(trainable_engine_t), intent(in) :: self
      integer n_in
    end function

    elemental module function num_outputs(self) result(n_out)
      implicit none
      class(trainable_engine_t), intent(in) :: self
      integer n_out
    end function

    elemental module function num_layers(self) result(n_layers)
      implicit none
      class(trainable_engine_t), intent(in) :: self
      integer n_layers
    end function

    module function to_inference_engine(self) result(inference_engine)
      implicit none
      class(trainable_engine_t), intent(in) :: self
      type(inference_engine_t) :: inference_engine
    end function

    elemental module function map_to_input_training_range(self, tensor) result(normalized_tensor)
      implicit none
      class(trainable_engine_t), intent(in) :: self
      type(tensor_t), intent(in) :: tensor
      type(tensor_t) normalized_tensor
    end function

    elemental module function map_from_input_training_range(self, tensor) result(unnormalized_tensor)
      implicit none
      class(trainable_engine_t), intent(in) :: self
      type(tensor_t), intent(in) :: tensor
      type(tensor_t) unnormalized_tensor
    end function

    elemental module function map_to_output_training_range(self, tensor) result(normalized_tensor)
      implicit none
      class(trainable_engine_t), intent(in) :: self
      type(tensor_t), intent(in) :: tensor
      type(tensor_t) normalized_tensor
    end function

    elemental module function map_from_output_training_range(self, tensor) result(unnormalized_tensor)
      implicit none
      class(trainable_engine_t), intent(in) :: self
      type(tensor_t), intent(in) :: tensor
      type(tensor_t) unnormalized_tensor
    end function

    elemental module function map_to_training_ranges(self, input_output_pair) result(normalized_input_output_pair)
      implicit none
      class(trainable_engine_t), intent(in) :: self
      type(input_output_pair_t), intent(in) :: input_output_pair
      type(input_output_pair_t) normalized_input_output_pair
    end function

  end interface

end module trainable_engine_m
