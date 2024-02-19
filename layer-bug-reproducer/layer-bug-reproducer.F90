module component_m
  implicit none

  type component_t
  end type

  interface component_t
    module function construct()
      implicit none
      type(component_t) construct
    end function
  end interface

contains
  module procedure construct
  end procedure
end module

module container_m
  use component_m, only : component_t
  implicit none

  type container_t
    type(component_t) component
  end type

  interface container_t
    module function construct()
      implicit none
      type(container_t) construct
    end function
  end interface

end module

submodule(container_m) container_s
  implicit none
contains
  module procedure construct
    type(container_t) local_container
    construct%component = component_t()
  end procedure
end submodule
