program concurrent_inferences
  use string_m
  implicit none

  type inference_engine_t
    type(string_t), allocatable :: metadata(:)
  end type

  real inferences(1)
  type(inference_engine_t) inference_engine
  integer i

  inference_engine%metadata = [string_t("")]

  do concurrent(i=1:size(inferences))
    inferences(i) = infer(inference_engine)           
  end do

contains
  pure function infer(self) 
    type(inference_engine_t), intent(in) :: self
    real infer
    infer = 0.
  end function
end program
