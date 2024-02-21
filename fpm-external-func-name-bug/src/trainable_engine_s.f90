! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(trainable_engine_m) trainable_engine_s
  use tensor_m, only : tensor_t
  implicit none

  integer, parameter :: input_layer = 0

contains

  module procedure train
    integer l, batch, mini_batch_size, pair
    real(rkind), allocatable :: &
      z(:,:), a(:,:), delta(:,:), dcdw(:,:,:), dcdb(:,:), vdw(:,:,:), sdw(:,:,:), vdb(:,:), sdb(:,:), vdwc(:,:,:), sdwc(:,:,:), &
      vdbc(:,:), sdbc(:,:)
    type(tensor_t), allocatable :: inputs(:), expected_outputs(:)
    real(rkind) eta, alpha

    eta = learning_rate
    alpha = learning_rate

    associate(output_layer => ubound(self%n,1))
      
      allocate(a(maxval(self%n), input_layer:output_layer)) ! Activations

      allocate(dcdw,  mold=self%w) ! Gradient of cost function with respect to weights
      allocate(vdw,   mold=self%w) 
      allocate(sdw,   mold=self%w) 
      allocate(vdwc,  mold=self%w) 
      allocate(sdwc,  mold=self%w) 

      allocate(z,     mold=self%b) ! z-values: Sum z_j^l = w_jk^{l} a_k^{l-1} + b_j^l
      allocate(delta, mold=self%b)
      allocate(dcdb,  mold=self%b) ! Gradient of cost function with respect with biases
      allocate(vdb,   mold=self%b) 
      allocate(sdb,   mold=self%b) 
      allocate(vdbc,  mold=self%b) 
      allocate(sdbc,  mold=self%b) 

      vdw = 0.d0
      sdw = 1.d0
      vdb = 0.d0
      sdb = 1.d0

      associate(w => self%w, b => self%b, n => self%n, num_mini_batches => size(mini_batches))

        if (present(cost)) allocate(cost(num_mini_batches))
      
        iterate_across_batches: &
        do batch = 1, num_mini_batches

          if (present(cost)) cost(batch) = 0.
          dcdw = 0.; dcdb = 0.
          
          associate(input_output_pairs => mini_batches(batch)%input_output_pairs())
            inputs = input_output_pairs%inputs()
            expected_outputs = input_output_pairs%expected_outputs()
            mini_batch_size = size(input_output_pairs)
          end associate

          iterate_through_batch: &
          do pair = 1, mini_batch_size

            a(1:10, input_layer) = inputs(pair)%values()

            feed_forward: &
            do l = 1,output_layer
              z(1:n(l),l) = matmul(w(1:n(l),1:n(l-1),l), a(1:n(l-1),l-1)) + b(1:n(l),l)
            end do feed_forward

            associate(y => expected_outputs(pair)%values())
              if (present(cost)) &
                cost(batch) = cost(batch) + sum((y(1:n(output_layer))-a(1:n(output_layer),output_layer))**2)/(2.e0*mini_batch_size)
          
            end associate
            
            block
              integer j

              sum_gradients: &
              do l = 1,output_layer
                dcdb(1:n(l),l) = dcdb(1:n(l),l) + delta(1:n(l),l)
                do concurrent(j = 1:n(l))
                  dcdw(j,1:n(l-1),l) = dcdw(j,1:n(l-1),l) + a(1:n(l-1),l-1)*delta(j,l)
                end do
              end do sum_gradients
            end block
    
          end do iterate_through_batch
        
          if (adam) then
            block
              ! Adam parameters  
              real, parameter :: beta(*) = [.9_rkind, .999_rkind]
              real, parameter :: obeta(*) = [1._rkind - beta(1), 1._rkind - beta(2)]
              real, parameter :: epsilon = real(1.D-08,rkind)

              adam_adjust_weights_and_biases: &
              do concurrent(l = 1:output_layer)
                dcdw(1:n(l),1:n(l-1),l) = dcdw(1:n(l),1:n(l-1),l)/(mini_batch_size)
                vdw(1:n(l),1:n(l-1),l)  = beta(1)*vdw(1:n(l),1:n(l-1),l) + obeta(1)*dcdw(1:n(l),1:n(l-1),l)
                sdw (1:n(l),1:n(l-1),l) = beta(2)*sdw(1:n(l),1:n(l-1),l) + obeta(2)*(dcdw(1:n(l),1:n(l-1),l)**2)
                vdwc(1:n(l),1:n(l-1),l) = vdw(1:n(l),1:n(l-1),l)/(1._rkind - beta(1)**num_mini_batches)
                sdwc(1:n(l),1:n(l-1),l) = sdw(1:n(l),1:n(l-1),l)/(1._rkind - beta(2)**num_mini_batches)
                w(1:n(l),1:n(l-1),l) = w(1:n(l),1:n(l-1),l) &
                  - alpha*vdwc(1:n(l),1:n(l-1),l)/(sqrt(sdwc(1:n(l),1:n(l-1),l))+epsilon) ! Adjust weights

                dcdb(1:n(l),l) = dcdb(1:n(l),l)/mini_batch_size
                vdb(1:n(l),l) = beta(1)*vdb(1:n(l),l) + obeta(1)*dcdb(1:n(l),l)
                sdb(1:n(l),l) = beta(2)*sdb(1:n(l),l) + obeta(2)*(dcdb(1:n(l),l)**2)
                vdbc(1:n(l),l) = vdb(1:n(l),l)/(1._rkind - beta(1)**num_mini_batches)
                sdbc(1:n(l),l) = sdb(1:n(l),l)/(1._rkind - beta(2)**num_mini_batches)
                b(1:n(l),l) = b(1:n(l),l) - alpha*vdbc(1:n(l),l)/(sqrt(sdbc(1:n(l),l))+epsilon) ! Adjust weights
              end do adam_adjust_weights_and_biases
            end block
          else
            adjust_weights_and_biases: &
            do concurrent(l = 1:output_layer)
              dcdb(1:n(l),l) = dcdb(1:n(l),l)/mini_batch_size
              b(1:n(l),l) = b(1:n(l),l) - eta*dcdb(1:n(l),l) ! Adjust biases
              dcdw(1:n(l),1:n(l-1),l) = dcdw(1:n(l),1:n(l-1),l)/mini_batch_size
              w(1:n(l),1:n(l-1),l) = w(1:n(l),1:n(l-1),l) - eta*dcdw(1:n(l),1:n(l-1),l) ! Adjust weights
            end do adjust_weights_and_biases
          end if

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
