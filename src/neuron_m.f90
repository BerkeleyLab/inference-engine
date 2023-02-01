! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module neuron_m
  use string_m, only : string_t
  use kind_parameters_m, only : rkind
  implicit none

  private
  public :: neuron_t

  type neuron_t
    !! linked list of neurons
    private
    real(rkind), allocatable :: weights_(:)
    real(rkind) bias_
    type(neuron_t), allocatable :: next
  contains
    procedure :: weights
    procedure :: bias
    procedure :: next_allocated
    procedure :: next_pointer
    procedure :: num_inputs
  end type

  interface neuron_t

    pure recursive module function construct(neuron_lines, start) result(neuron)
      !! construct linked list of neuron_t objects from an array of JSON-formatted text lines
      implicit none
      type(string_t), intent(in) :: neuron_lines(:)
      integer, intent(in) :: start
      type(neuron_t) neuron
    end function

  end interface

  interface

    module function weights(self) result(my_weights)
      implicit none
      class(neuron_t), intent(in) :: self
      real(rkind), allocatable :: my_weights(:)
    end function

    module function bias(self) result(my_bias)
      implicit none
      class(neuron_t), intent(in) :: self
      real(rkind) my_bias
    end function

    module function next_allocated(self) result(next_is_allocated)
      implicit none
      class(neuron_t), intent(in) :: self
      logical next_is_allocated
    end function

    module function next_pointer(self) result(next_ptr)
      implicit none
      class(neuron_t), intent(in), target :: self
      type(neuron_t), pointer :: next_ptr
    end function

    pure module function num_inputs(self) result(size_weights)
      implicit none
      class(neuron_t), intent(in) :: self
      integer size_weights
    end function

  end interface

end module
