module mini_batch_m
  use input_output_pair_m, only : input_output_pair_t
  use network_increment_m, only : network_increment_t
  use kind_parameters_m, only : rkind
  implicit none

  private
  public :: mini_batch_t

  type mini_batch_t
    private
    type(input_output_pair_t), allocatable :: input_output_pairs_(:)
    type(network_increment_t), allocatable :: network_increments_(:)
  contains
    procedure :: average_increment
  end type

  interface mini_batch_t

    pure module function construct(input_output_pairs, network_increments) result(mini_batch)
      implicit none
      type(input_output_pair_t), intent(in) :: input_output_pairs(:)
      type(network_increment_t), intent(in) :: network_increments(:)
      type(mini_batch_t) mini_batch
    end function

  end interface

  interface

    pure module function average_increment(self) result(average)
      implicit none
      class(mini_batch_t), intent(in) :: self
      type(network_increment_t) average
    end function

  end interface

end module mini_batch_m
