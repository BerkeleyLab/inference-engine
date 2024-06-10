module assert_m
  implicit none
contains
  pure subroutine assert(assertion, description)
    logical, intent(in) :: assertion
    character(len=*), intent(in) :: description
#ifndef NO_ASSERTIONS
    if (.not. assertion) error stop "Assertion '" // description // "' failed."
#endif
  end subroutine
end module assert_m
