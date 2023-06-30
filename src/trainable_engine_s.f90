! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(trainable_engine_m) trainable_engine_s
  use assert_m, only : assert
  use intrinsic_array_m, only : intrinsic_array_t
  use network_increment_m, only : network_increment_t, operator(.average.)
  use input_output_pair_m, only : input_output_pair_t
  use sigmoid_m, only : sigmoid_t
  implicit none

  integer, parameter :: input_layer = 0

contains

  module procedure num_inputs
    n_in = self%n(input_layer)
  end procedure

  module procedure num_layers
    n_layers = size(self%n,1)
  end procedure

  module procedure assert_consistent

    associate( &
      fully_allocated=>[allocated(self%w),allocated(self%b),allocated(self%n),allocated(self%differentiable_activation_strategy_)] &
    )
      call assert(all(fully_allocated),"trainable_engine_s(assert_consistent): fully_allocated",intrinsic_array_t(fully_allocated))
    end associate

    associate(max_width => maxval(self%n), component_dims => [size(self%b,1), size(self%w,1), size(self%w,2)])
      call assert(all(component_dims == max_width), "trainable_engine_s(assert_consistent): conformable arrays", &
        intrinsic_array_t([max_width,component_dims]))
    end associate

    call assert(lbound(self%n,1)==input_layer, "trainable_engine_s(assert_consistent): n base subsscript", lbound(self%n,1))

  end procedure

  module procedure infer

    real(rkind), allocatable :: z(:,:), a(:,:)
    integer l

    call self%assert_consistent

    associate(w => self%w, b => self%b, n => self%n, output_layer => ubound(self%b,2))

      allocate(z, mold=b)
      allocate(a(maxval(n), input_layer:output_layer)) ! Activations

      a(1:n(input_layer),input_layer) = inputs%values()

      feed_forward: &
      do l = 1,output_layer
        z(1:n(l),l) = matmul(w(1:n(l),1:n(l-1),l), a(1:n(l-1),l-1)) + b(1:n(l),l)
        a(1:n(l),l) = self%differentiable_activation_strategy_%activation(z(1:n(l),l))
      end do feed_forward
 
      associate(n_hidden => output_layer-1)
      outputs = outputs_t( &
        outputs = a(1:n(output_layer),output_layer), &
        pre_activation_in = z(:,1:n_hidden), &
        pre_activation_out = z(1:n(output_layer),output_layer) &
      )
      end associate

    end associate

  end procedure

  module procedure train
    integer i, j, k, l, batch, iter, mini_batch_size, pair
    real(rkind), parameter :: eta = 1.5e0 ! Learning parameter
    real(rkind), allocatable :: z(:,:), a(:,:), y(:), delta(:,:), dcdw(:,:,:), dcdb(:,:)
    real(rkind) cost
    type(inputs_t), allocatable :: inputs(:)
    type(expected_outputs_t), allocatable :: expected_outputs(:)

    call self%assert_consistent

    associate(output_layer => ubound(self%n,1))
      
      allocate(a(maxval(self%n), input_layer:output_layer)) ! Activations
      allocate(dcdw,  mold=self%w) ! Gradient of cost function with respect to weights
      allocate(z,     mold=self%b) ! z-values: Sum z_j^l = w_jk^{l} a_k^{l-1} + b_j^l
      allocate(delta, mold=self%b)
      allocate(dcdb,  mold=self%b) ! Gradient of cost function with respect with biases

      associate(w => self%w, b => self%b, n => self%n)
      
        iterate_across_batches: &
        do iter = 1, size(mini_batches)

          cost = 0.; dcdw = 0.; dcdb = 0.
          
          associate(input_output_pairs => mini_batches(iter)%input_output_pairs())
            inputs = input_output_pairs%inputs()
            expected_outputs = input_output_pairs%expected_outputs()
            mini_batch_size = size(input_output_pairs)
          end associate

          iterate_through_batch: &
          do pair = 1, mini_batch_size

            a(1:self%num_inputs(), input_layer) = inputs(pair)%values()
            y = expected_outputs(pair)%outputs()

            feed_forward: &
            do l = 1,output_layer
              z(1:n(l),l) = matmul(w(1:n(l),1:n(l-1),l), a(1:n(l-1),l-1)) + b(1:n(l),l)
              a(1:n(l),l) = self%differentiable_activation_strategy_%activation(z(1:n(l),l))
            end do feed_forward

            cost = cost + sum((y(1:n(output_layer))-a(1:n(output_layer),output_layer))**2)/(2.e0*mini_batch_size)
          
            delta(1:n(output_layer),output_layer) = &
              (a(1:n(output_layer),output_layer) - y(1:n(output_layer))) &
              * self%differentiable_activation_strategy_%activation_derivative(z(1:n(output_layer),output_layer))
            
            associate(n_hidden => self%num_layers()-2)
              back_propagate_error: &
              do l = n_hidden,1,-1
                delta(1:n(l),l) = matmul(transpose(w(1:n(l+1),1:n(l),l+1)), delta(1:n(l+1),l+1)) &
                  * self%differentiable_activation_strategy_%activation_derivative(z(1:n(l),l))
              end do back_propagate_error
            end associate

            sum_gradients: &
            do l = 1,output_layer
              dcdb(1:n(l),l) = dcdb(1:n(l),l) + delta(1:n(l),l)
              do concurrent(j = 1:n(l))
                dcdw(j,1:n(l-1),l) = dcdw(j,1:n(l-1),l) + a(1:n(l-1),l-1)*delta(j,l)
              end do
            end do sum_gradients
    
          end do iterate_through_batch
        
          adjust_weights_and_biases: &
          do l = 1,output_layer
            dcdb(1:n(l),l) = dcdb(1:n(l),l)/mini_batch_size
            b(1:n(l),l) = b(1:n(l),l) - eta*dcdb(1:n(l),l) ! Adjust biases
            dcdw(1:n(l),1:n(l-1),l) = dcdw(1:n(l),1:n(l-1),l)/mini_batch_size
            w(1:n(l),1:n(l-1),l) = w(1:n(l),1:n(l-1),l) - eta*dcdw(1:n(l),1:n(l-1),l) ! Adjust weights
          end do adjust_weights_and_biases
        end do iterate_across_batches

      end associate
    end associate
    
  end procedure

  module procedure construct_from_padded_arrays

     trainable_engine%n = nodes
     trainable_engine%w = weights
     trainable_engine%b = biases
     trainable_engine%differentiable_activation_strategy_ = differentiable_activation_strategy

     call trainable_engine%assert_consistent
  end procedure

end submodule trainable_engine_s
