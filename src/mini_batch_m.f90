! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module mini_batch_m
  use input_output_pair_m, only : input_output_pair_t
  use kind_parameters_m, only : rkind
  implicit none

  private
  public :: mini_batch_t

  type mini_batch_t
    private
    type(input_output_pair_t), allocatable :: input_output_pairs_(:)
  contains
    procedure :: input_output_pairs
  end type

  interface mini_batch_t

    pure module function construct(input_output_pairs) result(mini_batch)
      implicit none
      type(input_output_pair_t), intent(in) :: input_output_pairs(:)
      type(mini_batch_t) mini_batch
    end function

  end interface

  interface

    pure module function input_output_pairs(self) result(my_input_output_pairs)
      implicit none
      class(mini_batch_t), intent(in) :: self
      type(input_output_pair_t), allocatable :: my_input_output_pairs(:)
    end function

  end interface

end module mini_batch_m
