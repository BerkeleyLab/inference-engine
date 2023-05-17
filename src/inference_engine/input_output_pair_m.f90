module input_output_pair_m
  use expected_outputs_m, only : expected_outputs_t
  use kind_parameters_m, only : rkind
  use inputs_m, only : inputs_t
  implicit none

  private
  public :: input_output_pair_t

  type input_output_pair_t
    private
    type(inputs_t) inputs_
    type(expected_outputs_t) expected_outputs_
  contains
    procedure :: inputs
    procedure :: expected_outputs
  end type

  type input_output_pair_t
    private
    type(input_output_pair_t), allocatable :: input_output_pairs_(:)
  end type

  interface input_output_pair_t

    elemental module function construct(inputs, expected_outputs) result(input_output_pair)
      implicit none
      type(inputs_t), intent(in) :: inputs
      type(expected_outputs_t), intent(in) :: expected_outputs
      type(input_output_pair_t) input_output_pair
    end function

  end interface

  interface

    elemental module function inputs(self) result(my_inputs)
      implicit none
      class(input_output_pair_t), intent(in) :: self
      type(inputs_t) :: my_inputs
    end function

    elemental module function expected_outputs(self) result(my_expected_outputs)
      implicit none
      class(input_output_pair_t), intent(in) :: self
      type(expected_outputs_t) :: my_expected_outputs
    end function

  end interface

end module input_output_pair_m
