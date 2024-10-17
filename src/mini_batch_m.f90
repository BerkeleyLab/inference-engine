! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module mini_batch_m
  use input_output_pair_m, only : input_output_pair_t
  use kind_parameters_m, only : default_real, double_precision
  implicit none

  private
  public :: mini_batch_t

  type mini_batch_t(k)
    integer, kind :: k = default_real
    type(input_output_pair_t(k)), private, allocatable :: input_output_pairs_(:)
  contains
    generic :: input_output_pairs => default_real_input_output_pairs, double_precision_input_output_pairs
    procedure, non_overridable    :: default_real_input_output_pairs, double_precision_input_output_pairs
  end type

  interface mini_batch_t

    pure module function default_real_construct(input_output_pairs) result(mini_batch)
      implicit none
      type(input_output_pair_t), intent(in) :: input_output_pairs(:)
      type(mini_batch_t) mini_batch
    end function

    pure module function double_precision_construct(input_output_pairs) result(mini_batch)
      implicit none
      type(input_output_pair_t(double_precision)), intent(in) :: input_output_pairs(:)
      type(mini_batch_t(double_precision)) mini_batch
    end function

  end interface

  interface

    pure module function default_real_input_output_pairs(self) result(my_input_output_pairs)
      implicit none
      class(mini_batch_t), intent(in) :: self
      type(input_output_pair_t), allocatable :: my_input_output_pairs(:)
    end function

    pure module function double_precision_input_output_pairs(self) result(my_input_output_pairs)
      implicit none
      class(mini_batch_t(double_precision)), intent(in) :: self
      type(input_output_pair_t(double_precision)), allocatable :: my_input_output_pairs(:)
    end function

  end interface

end module mini_batch_m
