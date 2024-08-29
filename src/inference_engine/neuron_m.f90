! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module neuron_m
  use julienne_string_m, only : string_t
  use kind_parameters_m, only : default_real
  implicit none

  private
  public :: neuron_t

  type neuron_t(k)
    !! linked list of neurons
    integer, kind :: k = default_real
    real(k),        allocatable, private :: weights_(:)
    real(k),                     private :: bias_
    type(neuron_t(k)), allocatable, private :: next
  contains
    generic :: to_json => default_real_to_json
    procedure, private :: default_real_to_json
    generic :: weights => default_real_weights
    procedure, private :: default_real_weights
    generic :: bias =>    default_real_bias
    procedure, private :: default_real_bias
    generic :: next_allocated => default_real_next_allocated
    procedure, private ::        default_real_next_allocated
    generic :: next_pointer => default_real_next_pointer
    procedure, private ::      default_real_next_pointer
    generic :: num_inputs => default_real_num_inputs
    procedure, private ::    default_real_num_inputs
  end type

  interface neuron_t

    pure recursive module function from_json(neuron_lines, start) result(neuron)
      !! construct linked list of neuron_t objects from an array of JSON-formatted text lines
      implicit none
      type(string_t), intent(in) :: neuron_lines(:)
      integer, intent(in) :: start
      type(neuron_t) neuron
    end function

    pure module function from_components(weights, bias) result(neuron)
      !! construct single neuron_t object from an array of weights and a bias
      real, intent(in) :: weights(:)
      real, intent(in) :: bias
      type(neuron_t) neuron
    end function

  end interface

  interface

    pure module function default_real_to_json(self) result(lines)
      implicit none
      class(neuron_t), intent(in) :: self
      type(string_t), allocatable :: lines(:)
    end function

    module function default_real_weights(self) result(my_weights)
      implicit none
      class(neuron_t), intent(in) :: self
      real, allocatable :: my_weights(:)
    end function

    module function default_real_bias(self) result(my_bias)
      implicit none
      class(neuron_t), intent(in) :: self
      real my_bias
    end function

    module function default_real_next_allocated(self) result(next_is_allocated)
      implicit none
      class(neuron_t), intent(in) :: self
      logical next_is_allocated
    end function

    module function default_real_next_pointer(self) result(next_ptr)
      implicit none
      class(neuron_t), intent(in), target :: self
      type(neuron_t), pointer :: next_ptr
    end function

    pure module function default_real_num_inputs(self) result(size_weights)
      implicit none
      class(neuron_t), intent(in) :: self
      integer size_weights
    end function

  end interface

end module
