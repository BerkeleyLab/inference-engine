module trainable_network_m
  use inference_engine_m_, only : inference_engine_t, workspace_t
  use mini_batch_m, only : mini_batch_t
  use kind_parameters_m, only : default_real
  implicit none

  private
  public :: trainable_network_t 

  type, extends(inference_engine_t) ::  trainable_network_t(k)
    integer, kind :: k = default_real 
    private
    type(workspace_t), private :: workspace_
  contains
    generic :: train           => default_real_train                   
    procedure, non_overridable :: default_real_train
  end type

  interface trainable_network_t 

    pure module function default_real_network(inference_engine) result(trainable_network)
      implicit none
      type(inference_engine_t), intent(in) :: inference_engine
      type(trainable_network_t) trainable_network
    end function 
 
  end interface

  interface

    pure module subroutine default_real_train(self, mini_batches_arr, cost, adam, learning_rate)
      implicit none
      class(trainable_network_t), intent(inout) :: self
      type(mini_batch_t), intent(in) :: mini_batches_arr(:)
      real, intent(out), allocatable, optional :: cost(:)
      logical, intent(in) :: adam
      real, intent(in) :: learning_rate
    end subroutine

  end interface

end module trainable_network_m
