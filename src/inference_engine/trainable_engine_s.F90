! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(trainable_engine_m) trainable_engine_s
  use assert_m, only : assert
  use intrinsic_array_m, only : intrinsic_array_t
  use tensor_m, only : tensor_t
#ifdef _CRAYFTN
  use input_output_pair_m, only : input_output_pair_t
#endif
  implicit none

  integer, parameter :: input_layer = 0

contains

  module procedure num_inputs
    n_in = self%n(input_layer)
  end procedure

  module procedure num_layers
    n_layers = size(self%n,1)
  end procedure

  module procedure num_outputs
    n_out = self%n(ubound(self%n,1))
  end procedure

  module procedure construct_from_inference_engine

#ifndef _CRAYFTN
    associate(exchange => inference_engine%to_exchange())
#else
    use inference_engine_m_, only: exchange_t
    type(exchange_t) exchange
    exchange = inference_engine%to_exchange()
#endif
      trainable_engine%input_range_ = exchange%input_range_
      trainable_engine%output_range_ = exchange%output_range_
      trainable_engine%metadata_ = exchange%metadata_
      trainable_engine%w = exchange%weights_
      trainable_engine%b = exchange%biases_
      trainable_engine%n = exchange%nodes_
      select type(activation => exchange%activation_strategy_)
        class is(differentiable_activation_strategy_t)
           trainable_engine%differentiable_activation_strategy_ = activation
        class default
           error stop &
           "trainable_engine_s(from_inference_engine): activation strategy must be a differentiable_activation_stragegy_t"
      end select
#ifndef _CRAYFTN
    end associate
#endif

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

    real(rkind), allocatable :: a(:,:)
    integer l

    call self%assert_consistent

    associate(w => self%w, b => self%b, n => self%n, output_layer => ubound(self%n,1))

      allocate(a(maxval(n), input_layer:output_layer)) ! Activations

#ifndef _CRAYFTN
      associate(normalized_inputs => self%input_range_%map_to_training_range(inputs))
#else
      block
        type(tensor_t) normalized_inputs
        normalized_inputs = self%input_range_%map_to_training_range(inputs)
#endif
        a(1:n(input_layer),input_layer) = normalized_inputs%values()
#ifndef _CRAYFTN
      end associate
#else
      end block
#endif

      feed_forward: &
      do l = 1,output_layer
        a(1:n(l),l) = self%differentiable_activation_strategy_%activation( &
          matmul(w(1:n(l),1:n(l-1),l), a(1:n(l-1),l-1)) + b(1:n(l),l) &
        )
      end do feed_forward
 
      associate(normalized_outputs => tensor_t(a(1:n(output_layer), output_layer)))
        outputs = self%output_range_%map_from_training_range(normalized_outputs)
      end associate

    end associate

  end procedure

  module procedure train
    integer l, batch, mini_batch_size, pair
    type(tensor_t), allocatable :: inputs(:), expected_outputs(:)

    call self%assert_consistent

    if (.not. allocated(self%dcdw)) allocate(self%dcdw, mold=self%w) ! Gradient of cost function with respect to weights
    if (.not. allocated(self%vdw)) allocate(self%vdw, mold=self%w) 
    if (.not. allocated(self%sdw)) allocate(self%sdw, mold=self%w) 
    if (.not. allocated(self%vdwc)) allocate(self%vdwc,  mold=self%w) 
    if (.not. allocated(self%sdwc)) allocate(self%sdwc,  mold=self%w) 

    if (.not. allocated(self%z)) allocate(self%z,  mold=self%b) ! z-values: Sum z_j^l = w_jk^{l} a_k^{l-1} + b_j^l
    if (.not. allocated(self%delta)) allocate(self%delta, mold=self%b)
    if (.not. allocated(self%dcdb)) allocate(self%dcdb,  mold=self%b) ! Gradient of cost function with respect with biases
    if (.not. allocated(self%vdb)) allocate(self%vdb,   mold=self%b) 
    if (.not. allocated(self%sdb)) allocate(self%sdb,   mold=self%b) 
    if (.not. allocated(self%vdbc)) allocate(self%vdbc,  mold=self%b) 
    if (.not. allocated(self%sdbc)) allocate(self%sdbc,  mold=self%b) 

    associate(output_layer => ubound(self%n,1))
      
      if (.not. allocated(self%a)) allocate(self%a(maxval(self%n), input_layer:output_layer)) ! Activations

      associate( &
        a => self%a, dcdw => self%dcdw, vdw => self%vdw, sdw => self%sdw, vdwc => self%vdwc, sdwc => self%sdwc, &
        z => self%z, delta => self%delta, dcdb => self%dcdb, vdb => self%vdb, sdb => self%sdb, vdbc => self%vdbc, sdbc=> self%sdbc &
      )
        vdw = 0.d0
        sdw = 1.d0
        vdb = 0.d0
        sdb = 1.d0

        associate(w => self%w, b => self%b, n => self%n, num_mini_batches => size(mini_batches_arr))

          if (present(cost)) allocate(cost(num_mini_batches))
        
          iterate_across_batches: &
          do batch = 1, num_mini_batches

            if (present(cost)) cost(batch) = 0.
            dcdw = 0.; dcdb = 0.

#ifndef _CRAYFTN
            associate(input_output_pairs => mini_batches_arr(batch)%input_output_pairs())
#else
            block
              type(input_output_pair_t), allocatable :: input_output_pairs(:)
              input_output_pairs = mini_batches_arr(batch)%input_output_pairs()
#endif  
              inputs = input_output_pairs%inputs()
              expected_outputs = input_output_pairs%expected_outputs()
              mini_batch_size = size(input_output_pairs)
#ifndef _CRAYFTN
            end associate
#else
            end block
#endif  

            iterate_through_batch: &
            do pair = 1, mini_batch_size

              a(1:self%num_inputs(), input_layer) = inputs(pair)%values()

              feed_forward: &
              do l = 1,output_layer
                z(1:n(l),l) = matmul(w(1:n(l),1:n(l-1),l), a(1:n(l-1),l-1)) + b(1:n(l),l)
                a(1:n(l),l) = self%differentiable_activation_strategy_%activation(z(1:n(l),l))
              end do feed_forward

              associate(y => expected_outputs(pair)%values())
                if (present(cost)) &
                  cost(batch)= cost(batch) + sum((y(1:n(output_layer))-a(1:n(output_layer),output_layer))**2)/(2.e0*mini_batch_size)
            
                delta(1:n(output_layer),output_layer) = &
                  (a(1:n(output_layer),output_layer) - y(1:n(output_layer))) &
                  * self%differentiable_activation_strategy_%activation_derivative(z(1:n(output_layer),output_layer))
              end associate
              
              associate(n_hidden => self%num_layers()-2)
                back_propagate_error: &
                do l = n_hidden,1,-1
                  delta(1:n(l),l) = matmul(transpose(w(1:n(l+1),1:n(l),l+1)), delta(1:n(l+1),l+1)) &
                    * self%differentiable_activation_strategy_%activation_derivative(z(1:n(l),l))
                end do back_propagate_error
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

                associate(alpha => learning_rate)
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
                end associate
              end block
            else
              associate(eta => learning_rate)
                adjust_weights_and_biases: &
                do concurrent(l = 1:output_layer)
                  dcdb(1:n(l),l) = dcdb(1:n(l),l)/mini_batch_size
                  b(1:n(l),l) = b(1:n(l),l) - eta*dcdb(1:n(l),l) ! Adjust biases
                  dcdw(1:n(l),1:n(l-1),l) = dcdw(1:n(l),1:n(l-1),l)/mini_batch_size
                  w(1:n(l),1:n(l-1),l) = w(1:n(l),1:n(l-1),l) - eta*dcdw(1:n(l),1:n(l-1),l) ! Adjust weights
                end do adjust_weights_and_biases
              end associate
            end if
          end do iterate_across_batches
        end associate
      end associate
    end associate
  end procedure

#ifdef __INTEL_COMPILER
  module procedure construct_trainable_engine_from_padded_arrays
#else
  module procedure construct_from_padded_arrays
#endif

    trainable_engine%metadata_ = metadata_t(metadata(1),metadata(2),metadata(3),metadata(4),metadata(5))
    trainable_engine%n = nodes
    trainable_engine%w = weights
    trainable_engine%b = biases
    trainable_engine%differentiable_activation_strategy_ = differentiable_activation_strategy

    block 
      integer i

      if (present(input_range)) then
         trainable_engine%input_range_ = input_range
      else
        associate(num_inputs => nodes(lbound(nodes,1)))
          trainable_engine%input_range_ = tensor_range_t("inputs", minima=[(0., i=1,num_inputs)], maxima=[(1., i=1,num_inputs)])
        end associate
      end if

      if (present(output_range)) then
         trainable_engine%output_range_ = output_range
      else
        associate(num_outputs => nodes(ubound(nodes,1)))
          trainable_engine%output_range_ = tensor_range_t("outputs", minima=[(0., i=1,num_outputs)], maxima=[(1., i=1,num_outputs)])
        end associate
      end if
    end block

    call trainable_engine%assert_consistent
  end procedure

  module procedure to_inference_engine
    inference_engine = inference_engine_t(self%metadata_%strings(), self%w, self%b, self%n, self%input_range_, self%output_range_)
  end procedure

  module procedure perturbed_identity_network

    integer k, l
    real, allocatable :: identity(:,:,:), w_harvest(:,:,:), b_harvest(:,:)

    associate(n=>training_configuration%nodes_per_layer())
      associate(n_max => maxval(n), layers => size(n))

        identity = reshape( [( [(e(k,n_max), k=1,n_max)], l = 1, layers-1 )], [n_max, n_max, layers-1])
        allocate(w_harvest, mold = identity)
        allocate(b_harvest(size(identity,1), size(identity,3)))
        call random_number(w_harvest)
        call random_number(b_harvest)

        associate( &
          w => identity + perturbation_magnitude*(w_harvest-0.5)/0.5, &
          b => perturbation_magnitude*(b_harvest-0.5)/0.5, &
          activation => training_configuration%differentiable_activation_strategy() &
        )
          trainable_engine = trainable_engine_t( &
            nodes = n, weights = w, biases = b, differentiable_activation_strategy = activation, metadata = metadata, &
            input_range = input_range, output_range = output_range &
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

  module procedure map_to_training_ranges
    associate( &
      inputs => input_output_pair%inputs(), &
      expected_outputs => input_output_pair%expected_outputs() &
    )
      associate( &
         normalized_inputs => self%input_range_%map_to_training_range(inputs), &
         normalized_outputs => self%output_range_%map_to_training_range(expected_outputs) &
      )
        normalized_input_output_pair = input_output_pair_t(normalized_inputs, normalized_outputs)
      end associate
    end associate
  end procedure

  module procedure map_to_input_training_range
    normalized_tensor = self%input_range_%map_to_training_range(tensor)
  end procedure

  module procedure map_from_input_training_range
    unnormalized_tensor = self%input_range_%map_from_training_range(tensor)
  end procedure
  
  module procedure map_to_output_training_range
    normalized_tensor = self%output_range_%map_to_training_range(tensor)
  end procedure

  module procedure map_from_output_training_range
    unnormalized_tensor = self%output_range_%map_from_training_range(tensor)
  end procedure
  

end submodule trainable_engine_s
