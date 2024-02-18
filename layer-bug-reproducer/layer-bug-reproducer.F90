module type1_m
  implicit none

  type type1
    integer num
    type(type1), allocatable :: next
  end type

  interface type1

    pure recursive module function construct(start) result(object)
      implicit none
      integer, intent(in) :: start
      type(type1) object
    end function

  end interface

contains

  module procedure construct
    object%num = start
    if (start .eq. 20) object%next = construct(start+4)
  end procedure

end module


module type2_m
  use type1_m, only : type1
  implicit none

  type type2
    type(type1) object
    type(type2), allocatable :: next
  end type

  interface type2

    recursive module function construct(start) result(group)
      implicit none
      integer, intent(in) :: start
      type(type2), target :: group
    end function

  end interface

  interface
    module function count_objects(group) result(objects_per_group)
      implicit none
      type(type2), intent(in), target :: group
      integer, allocatable :: objects_per_group(:)
    end function
  end interface

end module

submodule(type2_m) type2_s
  implicit none

contains

  module procedure construct
    group%object = type1(start+1)
  end procedure

  module procedure count_objects
  ! BUG: If next line of executable code is commented out, compiles with ifx
  ! If code is not commented out, ifx reports a compiler error for line 137
  type(type2), pointer :: group_ptr
  end procedure

end submodule type2_s
