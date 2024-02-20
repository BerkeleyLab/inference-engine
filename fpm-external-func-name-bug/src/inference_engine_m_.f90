! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module inference_engine_m_
  !! Define an abstraction that supports inference operationsn on a neural network
  use activation_strategy_m, only : activation_strategy_t
  use string_m, only : string_t
  use kind_parameters_m, only : rkind
  use tensor_m, only : tensor_t
  use differentiable_activation_strategy_m, only :differentiable_activation_strategy_t
  implicit none

  private
  public :: inference_engine_t
  public :: difference_t
  public :: exchange_t
  public :: infer

  character(len=*), parameter :: key(*) = [character(len=len("usingSkipConnections")) :: &
    "modelName", "modelAuthor", "compilationDate", "activationFunction", "usingSkipConnections"]

  type inference_engine_t
    !! Encapsulate the minimal information needed to perform inference
    private
    type(string_t) metadata_(size(key))
    real(rkind), allocatable :: weights_(:,:,:), biases_(:,:)
    integer, allocatable :: nodes_(:)
    class(activation_strategy_t), allocatable :: activation_strategy_ ! Strategy Pattern facilitates elemental activation
  contains
    procedure :: infer
    procedure :: num_inputs
    procedure :: num_outputs
    procedure :: nodes_per_layer
    procedure :: assert_conformable_with
    procedure :: skip
    procedure, private :: subtract
    generic :: operator(-) => subtract
    procedure :: activation_function_name
    procedure :: to_exchange
  end type

  type exchange_t
    type(string_t) metadata_(size(key))
    real(rkind), allocatable :: weights_(:,:,:), biases_(:,:)
    integer, allocatable :: nodes_(:)
    class(activation_strategy_t), allocatable :: activation_strategy_ ! Strategy Pattern facilitates elemental activation
  end type

  type difference_t
    private
    real(rkind), allocatable :: weights_difference_(:,:,:), biases_difference_(:,:)
    integer, allocatable :: nodes_difference_(:)
  contains
    procedure :: norm
  end type

  interface inference_engine_t

    pure module function construct_from_padded_arrays(metadata, weights, biases, nodes) result(inference_engine)
      implicit none
      type(string_t), intent(in) :: metadata(:)
      real(rkind), intent(in) :: weights(:,:,:), biases(:,:)
      integer, intent(in) :: nodes(0:)
      type(inference_engine_t) inference_engine
    end function

  end interface

  interface

    pure module function to_exchange(self) result(exchange)
      implicit none
      class(inference_engine_t), intent(in) :: self
      type(exchange_t) exchange
    end function

    elemental module function norm(self) result(norm_of_self)
      implicit none
      class(difference_t), intent(in) :: self
      real(rkind)  norm_of_self
    end function

    elemental module function subtract(self, rhs) result(difference)
      implicit none
      class(inference_engine_t), intent(in) :: self
      type(inference_engine_t), intent(in) :: rhs
      type(difference_t)  difference
    end function

    elemental module subroutine assert_conformable_with(self, inference_engine)
      implicit none
      class(inference_engine_t), intent(in) :: self
      type(inference_engine_t), intent(in) :: inference_engine
    end subroutine

    elemental module function infer(self, inputs) result(outputs)
      implicit none
      class(inference_engine_t), intent(in) :: self
      type(tensor_t), intent(in) :: inputs
      type(tensor_t) outputs
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

    pure module function nodes_per_layer(self) result(node_count)
      implicit none
      class(inference_engine_t), intent(in) :: self
      integer, allocatable :: node_count(:)
    end function

    elemental module function activation_function_name(self) result(activation_name)
      implicit none
      class(inference_engine_t), intent(in) :: self
      type(string_t) activation_name
    end function

    pure module function skip(self) result(use_skip_connections)
      implicit none
      class(inference_engine_t), intent(in) :: self
      logical use_skip_connections
    end function

  end interface

end module inference_engine_m_
