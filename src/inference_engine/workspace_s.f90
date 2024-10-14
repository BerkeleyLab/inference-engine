submodule(inference_engine_m_) workspace_s
  use assert_m, only : assert
  implicit none

  integer, parameter :: input_layer = 0

contains

  module procedure default_real_workspace

    allocate(workspace%dcdw, mold=inference_engine%weights_) ! Gradient of cost function with respect to weights
    allocate(workspace%vdw , mold=inference_engine%weights_)
    allocate(workspace%sdw , mold=inference_engine%weights_)
    allocate(workspace%vdwc, mold=inference_engine%weights_)
    allocate(workspace%sdwc, mold=inference_engine%weights_)

    allocate(workspace%dcdb, mold=inference_engine%biases_ ) ! Gradient of cost function with respect with biases
    allocate(workspace%vdb , mold=inference_engine%biases_ )
    allocate(workspace%sdb , mold=inference_engine%biases_ )
    allocate(workspace%vdbc, mold=inference_engine%biases_ )
    allocate(workspace%sdbc, mold=inference_engine%biases_ )

    ! TODO: #if ! (F2023_LOCALITY || F2018_LOCALITY)
    !          then don't allocate a, z, and delta

    allocate(workspace%z    , mold=inference_engine%biases_)
    allocate(workspace%delta, mold=inference_engine%biases_)

    associate(output_layer => ubound(inference_engine%nodes_,1))
      allocate(workspace%a(maxval(inference_engine%nodes_), input_layer:output_layer)) ! Activations
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

    call allocate_if_necessary(self%dcdw,  mold=inference_engine%weights_) ! Gradient of cost function with respect to weights
    call allocate_if_necessary(self%vdw ,  mold=inference_engine%weights_) 
    call allocate_if_necessary(self%sdw ,  mold=inference_engine%weights_) 
    call allocate_if_necessary(self%vdwc,  mold=inference_engine%weights_) 
    call allocate_if_necessary(self%sdwc,  mold=inference_engine%weights_) 

    call allocate_if_necessary(self%dcdb , mold=inference_engine%biases_) ! Gradient of cost function with respect with biases
    call allocate_if_necessary(self%vdb  , mold=inference_engine%biases_) 
    call allocate_if_necessary(self%sdb  , mold=inference_engine%biases_) 
    call allocate_if_necessary(self%vdbc , mold=inference_engine%biases_) 
    call allocate_if_necessary(self%sdbc , mold=inference_engine%biases_) 

    ! TODO: #if ! (F2023_LOCALITY || F2018_LOCALITY)
    !          then don't allocate a, z, and delta

    call allocate_if_necessary(self%z    , mold=inference_engine%biases_) 
    call allocate_if_necessary(self%delta, mold=inference_engine%biases_) 

    associate(output_layer => ubound(inference_engine%nodes_,1))
      allocate(self%a(maxval(inference_engine%nodes_), input_layer:output_layer)) ! Activations
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
