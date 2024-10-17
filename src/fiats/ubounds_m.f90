! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module ubounds_m
  !! This module serves only to support array bounds checking in the main program below
  implicit none

  type ubounds_t
    integer, allocatable :: ubounds_(:)
  contains
    procedure equals
    generic :: operator(==) => equals
  end type
 
contains

  elemental function equals(lhs, rhs) result(lhs_equals_rhs)
    class(ubounds_t), intent(in) :: lhs, rhs
    logical lhs_equals_rhs
    lhs_equals_rhs = all(lhs%ubounds_ == rhs%ubounds_)
  end function

end module
