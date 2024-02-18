module component_m
  implicit none

  type component
    type(component), allocatable :: next
  end type

  interface component

    recursive module function construct(items) result(object)
      implicit none
      integer, intent(in) :: items
      type(component) object
    end function

  end interface

contains

  module procedure construct
    if (items < 0) error stop "negative count"
    if (items > 0) object%next = construct(items-1)
  end procedure

end module


module container_m
  use component_m, only : component
  implicit none

  type container
    type(component) object
    type(container), allocatable :: next
  end type

  interface container

    recursive module function construct(items) result(group)
      implicit none
      integer items
      type(container), target :: group
    end function

  end interface

  interface
    module function count_objects(group) result(objects_per_group)
      implicit none
      type(container), target :: group
      integer, allocatable :: objects_per_group(:)
    end function
  end interface

end module

submodule(container_m) container_s
  implicit none

contains

  module procedure construct
    group%object = component(items+1)
  end procedure

  module procedure count_objects
  ! BUG: If next line of executable code is commented out, compiles with ifx
  ! If code is not commented out, ifx reports a compiler error for line 137
  type(container), pointer :: group_ptr
  end procedure

end submodule container_s
