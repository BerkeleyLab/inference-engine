module neural_net_m
  use tensor_m, only : tensor_t
  implicit none
  private

  type neural_net_t
    private
  contains
    procedure infer
  end type

contains

  pure function infer(self, inputs) result(outputs)
    class(neural_net_t), intent(in) :: self
    type(tensor_t), intent(in) :: inputs
    type(tensor_t) outputs
    error stop "inference not yet implemented"
  end function

end module neural_net_m
