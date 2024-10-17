! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module input_output_pair_m
  use tensor_m, only : tensor_t
  use kind_parameters_m, only : default_real, double_precision
  implicit none

  private
  public :: input_output_pair_t
  public :: shuffle
  public :: write_to_stdout

  type input_output_pair_t(k)
    integer, kind :: k = default_real
    type(tensor_t(k)), private :: inputs_, expected_outputs_
  contains
    generic :: inputs                   => default_real_inputs, double_precision_inputs
    procedure, private, non_overridable :: default_real_inputs, double_precision_inputs
    generic :: expected_outputs         => default_real_expected_outputs, double_precision_expected_outputs
    procedure, private, non_overridable :: default_real_expected_outputs, double_precision_expected_outputs
  end type

  interface input_output_pair_t

    elemental module function default_real_construct(inputs, expected_outputs) result(input_output_pair)
      implicit none
      type(tensor_t), intent(in) :: inputs, expected_outputs
      type(input_output_pair_t) input_output_pair
    end function

    elemental module function double_precision_construct(inputs, expected_outputs) result(input_output_pair)
      implicit none
      type(tensor_t(double_precision)), intent(in) :: inputs, expected_outputs
      type(input_output_pair_t(double_precision)) input_output_pair
    end function

  end interface

  interface

    elemental module function default_real_inputs(self) result(my_inputs)
      implicit none
      class(input_output_pair_t), intent(in) :: self
      type(tensor_t) :: my_inputs
    end function

    elemental module function double_precision_inputs(self) result(my_inputs)
      implicit none
      class(input_output_pair_t(double_precision)), intent(in) :: self
      type(tensor_t(double_precision)) :: my_inputs
    end function

    elemental module function default_real_expected_outputs(self) result(my_expected_outputs)
      implicit none
      class(input_output_pair_t), intent(in) :: self
      type(tensor_t) :: my_expected_outputs
    end function

    elemental module function double_precision_expected_outputs(self) result(my_expected_outputs)
      implicit none
      class(input_output_pair_t(double_precision)), intent(in) :: self
      type(tensor_t(double_precision)) :: my_expected_outputs
    end function

  end interface

  interface shuffle

    module subroutine default_real_shuffle(pairs)
      implicit none
      type(input_output_pair_t), intent(inout) :: pairs(:)
    end subroutine

    module subroutine double_precision_shuffle(pairs)
      implicit none
      type(input_output_pair_t(double_precision)), intent(inout) :: pairs(:)
    end subroutine

  end interface

  interface write_to_stdout

    module subroutine default_real_write_to_stdout(input_output_pairs)
      implicit none
      type(input_output_pair_t), intent(in) :: input_output_pairs(:)
    end subroutine

    module subroutine double_precision_write_to_stdout(input_output_pairs)
      implicit none
      type(input_output_pair_t(double_precision)), intent(in) :: input_output_pairs(:)
    end subroutine

  end interface

end module input_output_pair_m
