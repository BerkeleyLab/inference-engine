module component_m
  implicit none

  type component
    type(component), allocatable :: next
  end type

  interface component

    recursive module function construct(items)
      implicit none
      integer items
      type(component) construct
    end function

  end interface

contains

  module procedure construct
    if (items < 0) error stop "negative count"
    if (items > 0) construct%next = construct(items-1)
  end procedure

end module


module container_m
  use component_m, only : component
  implicit none

  type container_t
    type(component) object
    type(container_t), allocatable :: next
  end type

  interface container_t

    recursive module function construct(items)
      implicit none
      integer items
      type(container_t), target :: construct
    end function

  end interface

  interface
    module function count_objects(group)
      implicit none
      type(container_t), target :: group
      integer, allocatable :: count_objects(:)
    end function
  end interface

end module

submodule(container_m) container_s
  implicit none

contains

  module procedure construct
    construct%object = component(items+1)
  end procedure

  module procedure count_objects
  type(container_t), pointer :: group_ptr
  end procedure

end submodule container_s
