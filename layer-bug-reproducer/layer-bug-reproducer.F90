! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module neuron_m
  implicit none

  private
  public :: neuron_t

  type neuron_t
    !! linked list of neurons
    integer bias_
    type(neuron_t), allocatable :: next
  end type

  interface neuron_t

    pure recursive module function construct(start) result(neuron)
      !! construct linked list of neuron_t objects from an array of JSON-formatted text lines
      implicit none
      integer, intent(in) :: start
      type(neuron_t) neuron
    end function

  end interface

contains

  module procedure construct
    neuron%bias_ = start
    if (start .eq. 20) neuron%next = construct(start+4)
  end procedure

end module


! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module layer_m
  use neuron_m, only : neuron_t
  implicit none

  private
  public :: layer_t

  type layer_t
    !! linked list of layers, each comprised of a linked list of neurons
    type(neuron_t) neuron !! linked list of this layer's neurons 
    type(layer_t), allocatable :: next !! next layer
  contains
    procedure :: count_neurons
  end type

  interface layer_t

    recursive module function construct(start) result(layer)
      !! construct a linked list of layer_t objects from an array of JSON-formatted text lines
      implicit none
      integer, intent(in) :: start
      type(layer_t), target :: layer
    end function

  end interface

  interface
    module function count_neurons(layer) result(neurons_per_layer)
      implicit none
      class(layer_t), intent(in), target :: layer
      integer, allocatable :: neurons_per_layer(:)
    end function
  end interface

end module
! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(layer_m) layer_s
  implicit none

contains

  module procedure construct
    layer%neuron = neuron_t(start+1)
  end procedure

  module procedure count_neurons
  ! BUG: If next line of exectuable code is commented out, compiles with ifx
  ! If code is not commented out, ifx reports a compiler error for line 137
  ! type(layer_t), pointer :: layer_ptr
  end procedure

end submodule layer_s
