! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module inference_engine_m
  !! Define an abstraction that supports inference operationsn on a neural network
  use string_m, only : string_t
  use inference_strategy_m, only : inference_strategy_t
  use activation_strategy_m, only : activation_strategy_t
  use file_m, only : file_t
  use kind_parameters_m, only : rkind
  implicit none

  private
  public :: inference_engine_t
  public :: inputs_t
  public :: outputs_t

  type inputs_t
    real(rkind), allocatable :: inputs_(:)
  end type

  type outputs_t
    real(rkind), allocatable :: outputs_(:)
  end type

  character(len=*), parameter :: key(*) = [character(len=len("usingSkipConnections")) :: &
    "modelName", "modelAuthor", "compilationDate", "activationFunction", "usingSkipConnections"]

  type metadata_t
    type(string_t) key_value(size(key))
  end type

  type inference_engine_t
    !! Encapsulate the minimal information needed to performance inference
    private
    type(metadata_t) metadata_
    real(rkind), allocatable :: input_weights_(:,:)    ! weights applied to go from the inputs to first hidden layer
    real(rkind), allocatable :: hidden_weights_(:,:,:) ! weights applied to go from one hidden layer to the next
    real(rkind), allocatable :: output_weights_(:,:)   ! weights applied to go from the final hidden layer to the outputs
    real(rkind), allocatable :: biases_(:,:)           ! neuronal offsets for each hidden layer
    real(rkind), allocatable :: output_biases_(:)      ! neuronal offsets applied to outputs
    class(activation_strategy_t), allocatable :: activation_strategy_
    class(inference_strategy_t), allocatable :: inference_strategy_
  contains
    procedure :: read_network
    procedure :: to_json
    procedure :: write_network
    procedure, private :: infer_from_array_of_inputs
    procedure, private :: infer_from_inputs_object
    generic :: infer => infer_from_array_of_inputs, infer_from_inputs_object
    procedure :: num_inputs
    procedure :: num_outputs
    procedure :: neurons_per_layer
    procedure :: num_hidden_layers
    procedure :: norm
    procedure :: conformable_with
    procedure, private :: subtract
    generic :: operator(-) => subtract
  end type

  interface inference_engine_t

    pure module function construct_from_components &
      (input_weights, hidden_weights, output_weights, biases, output_biases, inference_strategy, activation_strategy) &
      result(inference_engine)
      implicit none
      real(rkind), intent(in), dimension(:,:) :: input_weights, output_weights, biases
      real(rkind), intent(in) :: hidden_weights(:,:,:), output_biases(:)
      class(inference_strategy_t), intent(in), optional :: inference_strategy
      class(activation_strategy_t), intent(in), optional :: activation_strategy
      type(inference_engine_t) inference_engine
    end function

    impure elemental module function construct_from_json(file_, inference_strategy) result(inference_engine)
      implicit none
      type(file_t), intent(in) :: file_
      class(inference_strategy_t), intent(in), optional :: inference_strategy
      type(inference_engine_t) inference_engine
    end function

  end interface

  interface

    impure elemental module function to_json(self) result(json_file)
      implicit none
      class(inference_engine_t), intent(in) :: self
      type(file_t) json_file
    end function

    impure elemental module subroutine read_network(self, file_name, activation_strategy, inference_strategy)
      implicit none
      class(inference_engine_t), intent(out) :: self
      type(string_t), intent(in) :: file_name
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
      real(rkind)  norm_of_self
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

    pure module function infer_from_array_of_inputs(self, input) result(output)
      implicit none
      class(inference_engine_t), intent(in) :: self
      real(rkind), intent(in) :: input(:)
      real(rkind), allocatable :: output(:)
    end function

    elemental module function infer_from_inputs_object(self, inputs) result(outputs)
      implicit none
      class(inference_engine_t), intent(in) :: self
      type(inputs_t), intent(in) :: inputs
      type(outputs_t) outputs
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
