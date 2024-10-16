submodule(neural_network_m) workspace_s
  use assert_m, only : assert
  implicit none

  integer, parameter :: input_layer = 0

contains

  module procedure default_real_workspace

    allocate(workspace%dcdw, mold=neural_network%weights_) ! Gradient of cost function with respect to weights
    allocate(workspace%vdw , mold=neural_network%weights_)
    allocate(workspace%sdw , mold=neural_network%weights_)
    allocate(workspace%vdwc, mold=neural_network%weights_)
    allocate(workspace%sdwc, mold=neural_network%weights_)

    allocate(workspace%dcdb, mold=neural_network%biases_ ) ! Gradient of cost function with respect with biases
    allocate(workspace%vdb , mold=neural_network%biases_ )
    allocate(workspace%sdb , mold=neural_network%biases_ )
    allocate(workspace%vdbc, mold=neural_network%biases_ )
    allocate(workspace%sdbc, mold=neural_network%biases_ )

    ! TODO: #if ! (F2023_LOCALITY || F2018_LOCALITY)
    !          then don't allocate a, z, and delta

    allocate(workspace%z    , mold=neural_network%biases_)
    allocate(workspace%delta, mold=neural_network%biases_)

    associate(output_layer => ubound(neural_network%nodes_,1))
      allocate(workspace%a(maxval(neural_network%nodes_), input_layer:output_layer)) ! Activations
    end associate

    call assert(workspace%fully_allocated(), "workspace_s(defalt_real_workspace): workspace allocated")

  end procedure

  module procedure default_real_allocated

    ! TODO: #if ! (F2023_LOCALITY || F2018_LOCALITY)
    !          then don't check a, z, and delta allocations

    all_allocated = all( [ & 
      allocated(self%a), allocated(self%dcdw), allocated(self%vdw), allocated(self%sdw), allocated(self%vdwc), allocated(self%sdwc)&
     ,allocated(self%z), allocated(self%dcdb), allocated(self%vdb), allocated(self%sdb), allocated(self%vdbc), allocated(self%sdbc)&
     ,allocated(self%delta) &
    ])
  end procedure


  module procedure default_real_allocate

    call allocate_if_necessary(self%dcdw,  mold=neural_network%weights_) ! Gradient of cost function with respect to weights
    call allocate_if_necessary(self%vdw ,  mold=neural_network%weights_) 
    call allocate_if_necessary(self%sdw ,  mold=neural_network%weights_) 
    call allocate_if_necessary(self%vdwc,  mold=neural_network%weights_) 
    call allocate_if_necessary(self%sdwc,  mold=neural_network%weights_) 

    call allocate_if_necessary(self%dcdb , mold=neural_network%biases_) ! Gradient of cost function with respect with biases
    call allocate_if_necessary(self%vdb  , mold=neural_network%biases_) 
    call allocate_if_necessary(self%sdb  , mold=neural_network%biases_) 
    call allocate_if_necessary(self%vdbc , mold=neural_network%biases_) 
    call allocate_if_necessary(self%sdbc , mold=neural_network%biases_) 

    ! TODO: #if ! (F2023_LOCALITY || F2018_LOCALITY)
    !          then don't allocate a, z, and delta

    call allocate_if_necessary(self%z    , mold=neural_network%biases_) 
    call allocate_if_necessary(self%delta, mold=neural_network%biases_) 

    associate(output_layer => ubound(neural_network%nodes_,1))
      allocate(self%a(maxval(neural_network%nodes_), input_layer:output_layer)) ! Activations
    end associate

  contains

    subroutine allocate_if_necessary(array, mold)
      real, allocatable, intent(inout) :: array(..)
      real, intent(in) :: mold(..)

      select rank(array)
        rank(2)
          select rank(mold)
            rank(2)
              if (.not. allocated(array)) then
                allocate(array, mold=mold)
              else
                if (any(shape(array) /= shape(mold))) then
                  deallocate(array)
                  allocate(array, mold=mold)
                end if
              end if
            rank default
              error stop "workspace_s(allocate_if_necessary): mold-rank mismatch with rank-2 'array'"
          end select
        rank(3)
          select rank(mold)
            rank(3)
              if (.not. allocated(array)) then
                allocate(array, mold=mold)
              else
                if (any(shape(array) /= shape(mold))) then
                  deallocate(array)
                  allocate(array, mold=mold)
                end if
              end if
            rank default
              error stop "workspace_s(allocate_if_necessary): mold-rank mismatch with rank-3 'array'"
          end select
        rank default
          error stop "workspace_s(allocate_if_necessary): unsupported 'array' rank"
      end select

    end subroutine allocate_if_necessary

  end procedure default_real_allocate

end submodule workspace_s
