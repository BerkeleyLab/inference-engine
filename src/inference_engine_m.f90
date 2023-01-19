! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module inference_engine_m
  !! Define an abstraction that supports inference operationsn on a neural network
  use string_m, only : string_t
  use inference_strategy_m, only : inference_strategy_t
  use activation_strategy_m, only : activation_strategy_t
  use file_m, only : file_t
  implicit none

  private
  public :: inference_engine_t

  type inference_engine_t
    !! Encapsulate the minimal information needed to performance inference
    private
    real, allocatable :: input_weights_(:,:)    ! weights applied to go from the inputs to first hidden layer
    real, allocatable :: hidden_weights_(:,:,:) ! weights applied to go from one hidden layer to the next
    real, allocatable :: output_weights_(:,:)   ! weights applied to go from the final hidden layer to the outputs
    real, allocatable :: biases_(:,:)           ! neuronal offsets for each hidden layer
    real, allocatable :: output_biases_(:)      ! neuronal offsets applied to outputs
    class(activation_strategy_t), allocatable :: activation_strategy_
    class(inference_strategy_t), allocatable :: inference_strategy_
  contains
    procedure :: read_network
    procedure :: from_json
    procedure :: write_network
    procedure :: infer
    procedure :: num_inputs
    procedure :: num_outputs
    procedure :: neurons_per_layer
    procedure :: num_hidden_layers
    procedure :: norm
    procedure :: conformable_with
    procedure :: subtract
    generic :: operator(-) => subtract
  end type

  interface inference_engine_t

    pure module function construct &
      (input_weights, hidden_weights, output_weights, biases, output_biases, inference_strategy, activation_strategy) &
      result(inference_engine)
      implicit none
      real, intent(in), dimension(:,:) :: input_weights, output_weights, biases
      real, intent(in) :: hidden_weights(:,:,:), output_biases(:)
      class(inference_strategy_t), intent(in), optional :: inference_strategy
      class(activation_strategy_t), intent(in), optional :: activation_strategy
      type(inference_engine_t) inference_engine
    end function

  end interface

  interface

    impure elemental module subroutine read_network(self, file_name, activation_strategy, inference_strategy)
      implicit none
      class(inference_engine_t), intent(out) :: self
      type(string_t), intent(in) :: file_name
      class(activation_strategy_t), intent(in), optional :: activation_strategy
      class(inference_strategy_t), intent(in), optional :: inference_strategy
    end subroutine

    impure elemental module subroutine from_json(self, file_, activation_strategy, inference_strategy)
      implicit none
      class(inference_engine_t), intent(out) :: self
      type(file_t), intent(in) :: file_
      class(activation_strategy_t), intent(in), optional :: activation_strategy
      class(inference_strategy_t), intent(in), optional :: inference_strategy
    end subroutine

    impure elemental module subroutine write_network(self, file_name)
      implicit none
      class(inference_engine_t), intent(in) :: self
      type(string_t), intent(in) :: file_name
    end subroutine

    elemental module function norm(self) result(norm_of_self)
      implicit none
      class(inference_engine_t), intent(in) :: self
      real  norm_of_self
    end function

    elemental module function subtract(self, rhs) result(difference)
      implicit none
      class(inference_engine_t), intent(in) :: self
      type(inference_engine_t), intent(in) :: rhs
      type(inference_engine_t)  difference
    end function

    elemental module function conformable_with(self, inference_engine) result(conformable)
      implicit none
      class(inference_engine_t), intent(in) :: self
      type(inference_engine_t), intent(in) :: inference_engine
      logical conformable
    end function

    pure module function infer(self, input) result(output)
      implicit none
      class(inference_engine_t), intent(in) :: self
      real, intent(in) :: input(:)
      real, allocatable :: output(:)
    end function

    elemental module function num_outputs(self) result(output_count)
      implicit none
      class(inference_engine_t), intent(in) :: self
      integer output_count
    end function

    elemental module function num_inputs(self) result(input_count)
      implicit none
      class(inference_engine_t), intent(in) :: self
      integer input_count
    end function

    elemental module function neurons_per_layer(self) result(neuron_count)
      implicit none
      class(inference_engine_t), intent(in) :: self
      integer neuron_count
    end function

    elemental module function num_hidden_layers(self) result(hidden_layer_count)
      implicit none
      class(inference_engine_t), intent(in) :: self
      integer hidden_layer_count
    end function

  end interface

end module inference_engine_m
