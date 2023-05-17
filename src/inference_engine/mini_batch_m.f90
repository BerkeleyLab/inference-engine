module mini_batch_m
  use expected_outputs_m, only : expected_outputs_t
  use kind_parameters_m, only : rkind
  use inputs_m, only : inputs_t
  implicit none

  private
  public :: input_output_pair_t
  public :: mini_batch_t

  type input_output_pair_t
    private
    type(inputs_t) inputs_
    type(expected_outputs_t) expected_outputs_
  end type

  type mini_batch_t
    private
    type(input_output_pair_t), allocatable :: input_output_pairs_(:)
  end type

  interface

    elemental module function inputs(self) result(my_inputs)
      implicit none
      class(input_output_pair_t), intent(in) :: self
      type(inputs_t) :: my_inputs
    end function

  end interface

end module mini_batch_m
