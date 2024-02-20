! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module trainable_engine_m
  !! Define an abstraction that supports training a neural network
  use string_m, only : string_t
  use kind_parameters_m, only : rkind
  use tensor_m, only :  tensor_t
  use mini_batch_m, only : mini_batch_t
  use training_configuration_m, only : training_configuration_t
  implicit none

  private
  public :: trainable_engine_t

  type trainable_engine_t
    !! Encapsulate the information needed to perform training
    private
    type(string_t), allocatable :: metadata_(:)
    real(rkind), allocatable :: w(:,:,:) ! weights
    real(rkind), allocatable :: b(:,:) ! biases
    integer, allocatable :: n(:) ! nodes per layer
  contains
    procedure :: train
    procedure :: infer
    procedure :: num_layers
    procedure :: num_inputs
    procedure :: num_outputs
  end type

  integer, parameter :: input_layer = 0

  interface trainable_engine_t

    pure module function construct_from_padded_arrays(nodes, weights, biases, metadata) &
    result(trainable_engine)
      implicit none
      integer, intent(in) :: nodes(input_layer:)
      real(rkind), intent(in)  :: weights(:,:,:), biases(:,:)
      type(string_t), intent(in) :: metadata(:)
      type(trainable_engine_t) trainable_engine
    end function

    module function perturbed_identity_network(training_configuration, perturbation_magnitude, metadata) result(trainable_engine)
      implicit none
      type(training_configuration_t), intent(in) :: training_configuration
      type(string_t), intent(in) :: metadata(:)
      real(rkind), intent(in) :: perturbation_magnitude
      type(trainable_engine_t) trainable_engine
    end function

  end interface

  interface

    pure module subroutine train(self, mini_batches, cost, adam, learning_rate)
      implicit none
      class(trainable_engine_t), intent(inout) :: self
      type(mini_batch_t), intent(in) :: mini_batches(:)
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

  end interface

end module trainable_engine_m
