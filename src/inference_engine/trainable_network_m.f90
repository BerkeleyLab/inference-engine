module trainable_network_m
  use inference_engine_m_, only : inference_engine_t, workspace_t
  use input_output_pair_m, only : input_output_pair_t
  use julienne_m, only : string_t
  use kind_parameters_m, only : default_real
  use mini_batch_m, only : mini_batch_t
  use training_configuration_m, only : training_configuration_t
  use tensor_map_m, only : tensor_map_t
  implicit none

  private
  public :: trainable_network_t 

  type, extends(inference_engine_t) ::  trainable_network_t(k)
    integer, kind :: k = default_real 
    private
    type(workspace_t), private :: workspace_
  contains
    generic :: train           => default_real_train                   
    procedure, private, non_overridable :: default_real_train
    generic ::   map_to_training_ranges => default_real_map_to_training_ranges
    procedure, private, non_overridable :: default_real_map_to_training_ranges
  end type

  interface trainable_network_t 

    pure module function default_real_network(inference_engine) result(trainable_network)
      implicit none
      type(inference_engine_t), intent(in) :: inference_engine
      type(trainable_network_t) trainable_network
    end function 

    module function perturbed_identity_network(training_configuration, perturbation_magnitude, metadata, input_map, output_map) &
      result(trainable_network)
      implicit none
      type(training_configuration_t), intent(in) :: training_configuration
      type(string_t), intent(in) :: metadata(:)
      real, intent(in) :: perturbation_magnitude
      type(tensor_map_t) input_map, output_map
      type(trainable_network_t) trainable_network
    end function

  end interface

  interface

    pure module subroutine default_real_train(self, mini_batches_arr, cost, adam, learning_rate)
      implicit none
      class(trainable_network_t), intent(inout) :: self
      type(mini_batch_t), intent(in) :: mini_batches_arr(:)
      real, intent(out), allocatable, optional :: cost(:)
      logical, intent(in) :: adam
      real, intent(in) :: learning_rate
    end subroutine

    elemental module function default_real_map_to_training_ranges(self, input_output_pair) result(normalized_input_output_pair)
      implicit none
      class(trainable_network_t), intent(in) :: self
      type(input_output_pair_t), intent(in) :: input_output_pair
      type(input_output_pair_t) normalized_input_output_pair
    end function

  end interface

end module trainable_network_m
