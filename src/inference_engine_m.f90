module inference_engine_m
  implicit none

  type string_t
    character(len=:), allocatable :: string_
  contains
    generic :: array => real_array
    procedure real_array
  end type

  type inference_engine_t
    type(string_t), allocatable :: metadata_(:)
  end type

contains

  pure function infer(self) 
    type(inference_engine_t), intent(in) :: self
    real infer(1)
    infer = [0.]
  end function

  function real_array(self)
    class(string_t), intent(in) :: self
    real real_array(1)
    real_array = 0.
  end function

end module
