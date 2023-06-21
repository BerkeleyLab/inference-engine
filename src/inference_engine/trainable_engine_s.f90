! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(trainable_engine_m) trainable_engine_s
  use assert_m, only : assert
  use outputs_m, only : outputs_t
  use network_increment_m, only : network_increment_t, operator(.average.)
  use sigmoid_m, only : sigmoid_t
  use input_output_pair_m, only : input_output_pair_t
  implicit none

contains

  module procedure train_single_hidden_layer

    integer pair, l, batch
    real(rkind), allocatable  :: delta(:,:), delta_in(:), delta_w(:,:,:)
    type(outputs_t), allocatable :: actual_outputs(:)
    type(network_increment_t), allocatable :: network_increments(:)

    call self%assert_consistent

    associate( &
      num_hidden_layers => self%num_hidden_layers(), &
      neurons_per_layer => self%neurons_per_layer() &
    )
      call assert(num_hidden_layers==1, "trainable_engine_s(train_single_hidden_layer) not validated for deep networks")
      allocate(delta(neurons_per_layer, num_hidden_layers))
      allocate(delta_w(neurons_per_layer, neurons_per_layer, num_hidden_layers-1))

      loop_over_mini_batches: &
      do batch = 1, size(mini_batch)

        associate(input_output_pairs => mini_batch(batch)%input_output_pairs())
          associate( &
            expected_outputs => input_output_pairs%expected_outputs(), &
            inputs => input_output_pairs%inputs() &
          )
            if (allocated(network_increments)) deallocate(network_increments)
            allocate(network_increments(size(inputs)))
            if (allocated(actual_outputs)) deallocate(actual_outputs)
            allocate(actual_outputs(size(inputs)))

            loop_over_input_ouput_pairs: &
            do concurrent(pair = 1:size(inputs))

              actual_outputs(pair) = self%infer(inputs(pair), inference_strategy) 

              associate( &
                a_L => actual_outputs(pair)%outputs(), &
                y_L => expected_outputs(pair)%outputs(), &
                z_L => actual_outputs(pair)%pre_activation_out(), &
                z => actual_outputs(pair)%pre_activation_in(), &
                w_L => self%output_weights(), &
                w => self%hidden_weights(), &
                w_in => self%input_weights() &
              )
                associate( &
                    a => self%differentiable_activation_strategy_%activation(z), &
                    sigma_prime_of_z_L => self%differentiable_activation_strategy_%activation_derivative(z_L), &
                    sigma_prime_of_z => self%differentiable_activation_strategy_%activation_derivative(z) &
                )
                  associate(delta_L => (a_L - y_L)*sigma_prime_of_z_L)

                    delta(:,num_hidden_layers) = matmul(transpose(w_L), delta_L) * sigma_prime_of_z(:,num_hidden_layers)

                    do l = num_hidden_layers-1 , 1, -1
                      delta(:,l) = matmul(transpose(w(:,:,l+1)), delta(:,l+1)) * sigma_prime_of_z(:,l)
                    end do

                    block
                      real(rkind), parameter :: eta = 1. ! training rate

                      do concurrent(l = 1:size(delta_w,3))
                        delta_w(:,:,l) = -eta*outer_product(delta(:,l+1), a(:,l))
                      end do

                      network_increments(pair) = network_increment_t( &
                        delta_w_in =  -eta*outer_product(delta(:,1), inputs(pair)%values()), &
                        delta_w_hidden = delta_w, &
                        delta_w_out = -eta*outer_product(delta_L, a(:,num_hidden_layers)), &
                        delta_b_hidden = -eta*delta, &
                        delta_b_out = -eta*delta_L &
                      )
                    end block
                  end associate
                end associate
              end associate
            end do loop_over_input_ouput_pairs

            call self%increment( .average. network_increments)

          end associate
        end associate
      end do loop_over_mini_batches
    end associate

  contains

    pure function outer_product(u, v) result(u_v_T)
      real(rkind), intent(in), dimension(:) :: u, v
      real(rkind), allocatable, dimension(:,:) :: u_v_T
      integer i, j
      associate(rows => size(u), columns => size(v))
        allocate(u_v_T(rows, columns))
        do concurrent(i = 1:rows, j = 1:columns)
          u_v_T = u(i)*v(j)
        end do
      end associate
    end function

  end procedure

  module procedure train_deep_network
    integer i,j,k,l,nodes_max, batch, iter, mini_batch_size, pair
    integer, parameter ::  nhidden=2
    real(rkind) :: r,eta,ir,rr
    real(rkind) :: cost
    integer, allocatable :: nodes(:)
    real(rkind), allocatable :: w(:,:,:),z(:,:),b(:,:),a(:,:),y(:),delta(:,:)
    real(rkind), allocatable :: dcdw(:,:,:),dcdb(:,:)
    type(sigmoid_t) sigmoid
    type(inputs_t), allocatable :: batch_inputs(:)
    type(expected_outputs_t), allocatable :: batch_expected_outputs(:)
    type(input_output_pair_t), allocatable :: input_output_pairs(:)
    
    allocate(nodes(0:nhidden+1))
    ! Number of nodes in each layes
    nodes(0) = 2 ! Number of inputs
    nodes(1) = 3
    nodes(2) = 3
    nodes(3) = 1 ! Number of nodes in the output layer

    nodes_max = maxval(nodes)

    eta = 1.5e0 ! Learning parameter
    
    allocate(a(nodes_max,0:nhidden+1)) ! Activations, Layer 0: Inputs, Layer nhidden+1: Outputs
    allocate(z(nodes_max,nhidden+1)) ! z-values: Sum z_j^l = w_jk^{l} a_k^{l-1} + b_j^l
    allocate(w(nodes_max,nodes_max,nhidden+1)) ! Weights w_{jk}^l is the weight from the k'th neuron in the (l-1)'th layer to the j'th neuron in the l'th layer
    allocate(b(nodes_max,nhidden+1)) ! Bias b_j^l is the bias in j'th neuron of the l'th layer
    allocate(delta(nodes_max,nhidden+1))
    allocate(dcdw(nodes_max,nodes_max,nhidden+1)) ! Gradient of cost function with respect to weights
    allocate(dcdb(nodes_max,nhidden+1)) ! Gradient of cost function with respect with biases
    allocate(y(nodes(nhidden+1))) ! Desired output

    w = 0.e0 ! Initialize weights
    b = 0.e0 ! Initialize biases

    do iter = 1, size(mini_batches)

       cost = 0.e0
       dcdw = 0.e0
       dcdb = 0.e0
       
       input_output_pairs = mini_batches(iter)%input_output_pairs()
       mini_batch_size = size(input_output_pairs )
       batch_inputs = input_output_pairs%inputs()
       batch_expected_outputs = input_output_pairs%expected_outputs()

       do pair = 1, mini_batch_size

          ! Create an AND gate
          a(1:nodes(0),0) = batch_inputs(pair)%values()
          y = batch_expected_outputs(pair)%outputs()

          ! Feedforward
          do l = 1,nhidden+1
             do j = 1,nodes(l)
                z(j,l) = 0.e0
                do k = 1,nodes(l-1)
                   z(j,l) = z(j,l) + w(j,k,l)*a(k,l-1)
                end do
                z(j,l) = z(j,l) + b(j,l)
                a(j,l) = sigmoid%activation(real(z(j,l), kind(1.)))
             end do
          end do

          do k = 1,nodes(nhidden+1)
             cost = cost + (y(k)-a(k,nhidden+1))**2
          end do
       
          do k = 1,nodes(nhidden+1)
             delta(k,nhidden+1) = (a(k,nhidden+1)-y(k))*sigmoid%activation_derivative(real(z(k,nhidden+1), kind(1.)))
          end do

          ! Backpropagate the error
          do l = nhidden,1,-1
             do j = 1,nodes(l)
                delta(j,l) = 0.e0
                do k = 1,nodes(l+1)
                   delta(j,l) = delta(j,l) + w(k,j,l+1)*delta(k,l+1)
                end do
                delta(j,l) = delta(j,l)*sigmoid%activation_derivative(real(z(j,l), kind(1.)))
             end do
          end do

          ! Sum up gradients in the inner iteration
          do l = 1,nhidden+1
              do j = 1,nodes(l)
                do k = 1,nodes(l-1)
                   dcdw(j,k,l) = dcdw(j,k,l) + a(k,l-1)*delta(j,l)
                end do
                dcdb(j,l) = dcdb(j,l) + delta(j,l)
             end do
           end do
       
       end do
    
       cost = cost/(2.e0*mini_batch_size)

       do l = 1,nhidden+1
          do j = 1,nodes(l)
             do k = 1,nodes(l-1)
                dcdw(j,k,l) = dcdw(j,k,l)/mini_batch_size
                w(j,k,l) = w(j,k,l) - eta*dcdw(j,k,l) ! Adjust weights
             end do
             dcdb(j,l) = dcdb(j,l)/mini_batch_size
             b(j,l) = b(j,l) - eta*dcdb(j,l) ! Adjust biases
          end do
       end do

    end do

    block
      type(trainable_engine_t) trainable_engine

      trainable_engine = trainable_engine_t(nodes, w, b, sigmoid_t(), &
        [string_t("deep network"), string_t("Damian Rouson"), string_t("2023-06-18"), string_t("sigmoid"), string_t("false")])
      self%inference_engine_t = trainable_engine%inference_engine_t
      self%differentiable_activation_strategy_ = trainable_engine%differentiable_activation_strategy_
    end block
    
  end procedure

  module procedure construct_trainable_engine

    trainable_engine%inference_engine_t = inference_engine_t( &
      metadata, input_weights, hidden_weights, output_weights, biases, output_biases &
    )
    trainable_engine%differentiable_activation_strategy_ = differentiable_activation_strategy
    
  end procedure


  module procedure construct_from_padded_arrays

    integer l

    associate( &
      n_in => nodes(0), & ! number of inputs
      n_out => nodes(size(nodes)-1), & ! number of outputs
      neurons => nodes(1), & ! number of neurons per layer
      n_hidden => size(nodes) - 2 & ! number of hidden layers 
    )
      trainable_engine = construct_trainable_engine( &
        metadata = metadata, &
        input_weights = real(reshape([(weights(1:nodes(l), 1:nodes(l-1),l), l=1,1)], [n_in, neurons]), rkind), &
        hidden_weights = real(reshape([(weights(1:nodes(l), 1:nodes(l-1),l), l=2,n_hidden)], [neurons,neurons,n_hidden-1]), rkind),&
        output_weights = real(reshape([(weights(1:nodes(l), 1:nodes(l-1),l), l=n_hidden+1,n_hidden+1)], [n_out,neurons]), rkind), &
        biases = real(reshape([([(biases(1:nodes(l),l))], l=1,n_hidden)], [neurons, n_hidden]), rkind), &
        output_biases = real([(biases(1:nodes(l),l), l=n_hidden+1,n_hidden+1)], rkind), &
        differentiable_activation_strategy = differentiable_activation_strategy &
      )   
    end associate

  end procedure

end submodule trainable_engine_s
