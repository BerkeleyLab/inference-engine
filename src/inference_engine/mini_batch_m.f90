module mini_batch_m
  use kind_parameters_m, only : rkind
  use inputs_m, only : inputs_t
  implicit none

  private
  public :: expected_outputs_t
  public :: input_output_pair_t
  public :: mini_batch_t

  type expected_outputs_t
    real(rkind), allocatable :: outputs_(:)
  end type

  type input_output_pair_t
    type(inputs_t) inputs_
    type(expected_outputs_t) expected_outputs_
  end type

  type mini_batch_t
    type(input_output_pair_t), allocatable :: input_output_pairs_(:)
  end type

end module mini_batch_m
