! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module tensor_m
  implicit none
  type tensor_t
    real, allocatable :: values_(:)
  end type
end module tensor_m
