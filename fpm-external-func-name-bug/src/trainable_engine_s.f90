! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(trainable_engine_m) trainable_engine_s
  use tensor_m, only : tensor_t
  implicit none

  integer, parameter :: input_layer = 0

contains

  module procedure train
    integer l, batch, mini_batch_size, pair
    type(tensor_t), allocatable :: inputs(:), expected_outputs(:)

    associate(output_layer => ubound(self%n,1))
      
      associate(w => self%w, b => self%b, n => self%n, num_mini_batches => size(mini_batches))

        iterate_across_batches: &
        do batch = 1, num_mini_batches

          associate(input_output_pairs => mini_batches(batch)%input_output_pairs())
            inputs = input_output_pairs%inputs()
            expected_outputs = input_output_pairs%expected_outputs()
            mini_batch_size = size(input_output_pairs)
          end associate

        end do iterate_across_batches

      end associate
    end associate
    
  end procedure

  module procedure construct_from_padded_arrays

     trainable_engine%n = nodes
     trainable_engine%w = weights
     trainable_engine%b = biases
  end procedure

  module procedure perturbed_identity_network

    integer k, l
    integer :: nodes_junk(10)
    real, allocatable :: identity(:,:,:), w_harvest(:,:,:), b_harvest(:,:)

    associate(n=>nodes_junk)
      associate(n_max => maxval(n), layers => size(n))

        identity = reshape( [( [(e(k,n_max), k=1,n_max)], l = 1, layers-1 )], [n_max, n_max, layers-1])
        allocate(w_harvest, mold = identity)
        allocate(b_harvest(size(identity,1), size(identity,3)))
        call random_number(w_harvest)
        call random_number(b_harvest)

        associate( &
          w => identity + perturbation_magnitude*(w_harvest-0.5)/0.5, &
          b => perturbation_magnitude*(b_harvest-0.5)/0.5 &
        )
          trainable_engine = trainable_engine_t( &
            nodes = n, weights = w, biases = b &
          )
        end associate
      end associate
    end associate

  contains

    pure function e(j,n) result(unit_vector)
      integer, intent(in) :: j, n
      integer k
      real, allocatable :: unit_vector(:)
      unit_vector = real([(merge(1,0,j==k),k=1,n)])
    end function

  end procedure

end submodule trainable_engine_s
