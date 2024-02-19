module component_m
  implicit none

  type component_t
    type(component_t), allocatable :: next
  end type

  interface component_t
    recursive module function construct(items)
      implicit none
      integer items
      type(component_t) construct
    end function
  end interface

contains
  module procedure construct
    if (items < 0) error stop "negative count"
    if (items > 0) construct%next = component_t(items-1)
  end procedure
end module

module container_m
  use component_m, only : component_t
  implicit none

  type container_t
    type(component_t) component
    type(container_t), allocatable :: next
  end type

  interface container_t
    recursive module function construct(num_components)
      implicit none
      integer num_components
      type(container_t) construct
    end function
  end interface

  interface
    module function count_components(container)
      implicit none
      type(container_t) container
      integer count_components
    end function
  end interface

end module

submodule(container_m) container_s
  implicit none

contains

  module procedure construct
    construct%component = component_t(num_components)
  end procedure

  module procedure count_components
    type(container_t) local_container
  end procedure

end submodule container_s
