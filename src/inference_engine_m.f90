! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module inference_engine_m
  !! Define an abstraction that supports inference operationsn on a neural network
  use string_m, only : string_t
  use inference_strategy_m, only : inference_strategy_t
  use activation_strategy_m, only : activation_strategy_t
  use file_m, only : file_t
  use kind_parameters_m, only : rkind
  use inputs_m, only : inputs_t
  use outputs_m, only : outputs_t
  use differentiable_activation_strategy_m, only :differentiable_activation_strategy_t
  implicit none

  private
  public :: inference_engine_t

  character(len=*), parameter :: key(*) = [character(len=len("usingSkipConnections")) :: &
    "modelName", "modelAuthor", "compilationDate", "activationFunction", "usingSkipConnections"]

  type inference_engine_t
    !! Encapsulate the minimal information needed to perform inference
    private
    type(string_t) metadata_(size(key))
    real(rkind), allocatable :: input_weights_(:,:)    ! weights applied to go from the inputs to first hidden layer
    real(rkind), allocatable :: hidden_weights_(:,:,:) ! weights applied to go from one hidden layer to the next
    real(rkind), allocatable :: output_weights_(:,:)   ! weights applied to go from the final hidden layer to the outputs
    real(rkind), allocatable :: biases_(:,:)           ! neuronal offsets for each hidden layer
    real(rkind), allocatable :: output_biases_(:)      ! neuronal offsets applied to outputs
    class(activation_strategy_t), allocatable :: activation_strategy_ ! Strategy Pattern facilitates elemental activation
  contains
    procedure :: to_json
    procedure, private :: infer_from_array_of_inputs
    procedure, private :: infer_from_inputs_object
    generic :: infer => infer_from_array_of_inputs, infer_from_inputs_object
    procedure :: train
    procedure :: num_inputs
    procedure :: num_outputs
    procedure :: neurons_per_layer
    procedure :: num_hidden_layers
    procedure :: norm
    procedure :: assert_conformable_with
    procedure, private :: subtract
    generic :: operator(-) => subtract
    procedure :: skip
    procedure :: activation_function_name
  end type

  interface inference_engine_t

    pure module function construct_from_components(metadata, input_weights, hidden_weights, output_weights, biases, output_biases) &
      result(inference_engine)
      implicit none
      type(string_t), intent(in) :: metadata(:)
      real(rkind), intent(in), dimension(:,:) :: input_weights, output_weights, biases
      real(rkind), intent(in) :: hidden_weights(:,:,:), output_biases(:)
      type(inference_engine_t) inference_engine
    end function

    pure module function construct_trainable_engine( &
      metadata, input_weights, hidden_weights, output_weights, biases, output_biases, differentiable_activation_strategy &
    ) &
    result(inference_engine)
    implicit none
    type(string_t), intent(in) :: metadata(:)
    real(rkind), intent(in), dimension(:,:) :: input_weights, output_weights, biases
    real(rkind), intent(in) :: hidden_weights(:,:,:), output_biases(:)
    type(inference_engine_t) inference_engine
    class(differentiable_activation_strategy_t), intent(in) :: differentiable_activation_strategy
  end function

    impure elemental module function construct_from_json(file_) result(inference_engine)
      implicit none
      type(file_t), intent(in) :: file_
      type(inference_engine_t) inference_engine
    end function

  end interface

  interface

    impure elemental module function to_json(self) result(json_file)
      implicit none
      class(inference_engine_t), intent(in) :: self
      type(file_t) json_file
    end function

    elemental module function norm(self) result(norm_of_self)
      implicit none
      class(inference_engine_t), intent(in) :: self
      real(rkind)  norm_of_self
    end function

    elemental module function subtract(self, rhs) result(difference)
      implicit none
      class(inference_engine_t), intent(in) :: self
      type(inference_engine_t), intent(in) :: rhs
      type(inference_engine_t)  difference
    end function

    elemental module subroutine assert_conformable_with(self, inference_engine)
      implicit none
      class(inference_engine_t), intent(in) :: self
      type(inference_engine_t), intent(in) :: inference_engine
    end subroutine

    pure module function infer_from_array_of_inputs(self, input, inference_strategy) result(outputs)
      implicit none
      class(inference_engine_t), intent(in) :: self
      real(rkind), intent(in) :: input(:)
      class(inference_strategy_t), intent(in) :: inference_strategy
      type(outputs_t) outputs
    end function

    elemental module function infer_from_inputs_object(self, inputs, inference_strategy) result(outputs)
      implicit none
      class(inference_engine_t), intent(in) :: self
      type(inputs_t), intent(in) :: inputs
      class(inference_strategy_t), intent(in) :: inference_strategy
      type(outputs_t) outputs
    end function

    pure module subroutine train(self, inputs, inference_strategy, expected_outputs)
      implicit none
      class(inference_engine_t), intent(inout) :: self
      type(inputs_t), intent(in) :: inputs(:)
      class(inference_strategy_t), intent(in) :: inference_strategy
      type(outputs_t), intent(in) :: expected_outputs(:)
    end subroutine

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

    elemental module function skip(self) result(use_skip_connections)
      implicit none
      class(inference_engine_t), intent(in) :: self
      logical use_skip_connections
    end function

    elemental module function activation_function_name(self) result(activation_name)
      implicit none
      class(inference_engine_t), intent(in) :: self
      type(string_t) activation_name
    end function

  end interface

end module inference_engine_m
