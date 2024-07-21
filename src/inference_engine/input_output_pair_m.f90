! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module input_output_pair_m
  use tensor_m, only : tensor_t
  implicit none

  private
  public :: input_output_pair_t
  public :: shuffle
  public :: write_to_stdout

  type input_output_pair_t
    private
    type(tensor_t) inputs_, expected_outputs_
  contains
    procedure :: inputs
    procedure :: expected_outputs
  end type

  interface input_output_pair_t

    elemental module function construct(inputs, expected_outputs) result(input_output_pair)
      implicit none
      type(tensor_t), intent(in) :: inputs, expected_outputs
      type(input_output_pair_t) input_output_pair
    end function

  end interface

  interface

    elemental module function inputs(self) result(my_inputs)
      implicit none
      class(input_output_pair_t), intent(in) :: self
      type(tensor_t) :: my_inputs
    end function

    elemental module function expected_outputs(self) result(my_expected_outputs)
      implicit none
      class(input_output_pair_t), intent(in) :: self
      type(tensor_t) :: my_expected_outputs
    end function

    module subroutine shuffle(pairs)
      implicit none
      type(input_output_pair_t), intent(inout) :: pairs(:)
    end subroutine

    module subroutine write_to_stdout(input_output_pairs)
      implicit none
      type(input_output_pair_t), intent(in) :: input_output_pairs(:)
    end subroutine

  end interface

end module input_output_pair_m
