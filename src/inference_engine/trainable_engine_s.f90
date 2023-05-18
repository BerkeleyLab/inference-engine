! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(trainable_engine_m) trainable_engine_s
  use outputs_m, only : outputs_t
  implicit none

contains

  module procedure train

    type(outputs_t) actual_outputs ! compiler issue: gfortran won't let this be `associate`
    integer i, l
    real(rkind), allocatable  :: delta(:,:), delta_in(:), delta_w(:,:,:)

    call self%assert_consistent

    associate(input_output_pairs => mini_batch%input_output_pairs())

      associate( &
        num_hidden_layers => self%num_hidden_layers(), &
        neurons_per_layer => self%neurons_per_layer(), &
        expected_outputs => input_output_pairs%expected_outputs(), &
        inputs => input_output_pairs%inputs() &
      )
        allocate(delta(neurons_per_layer, num_hidden_layers))
        allocate(delta_w(neurons_per_layer, neurons_per_layer, num_hidden_layers-1))

        do i = 1, size(inputs)

          actual_outputs = self%infer(inputs(i), inference_strategy) 

          associate( &
            a_L => actual_outputs%outputs(), &
            y_L => expected_outputs(i)%outputs(), &
            z_L => actual_outputs%pre_activation_out(), &
            z => actual_outputs%pre_activation_in(), &
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

                  call self%increment( &
                    delta_w_in =  -eta*outer_product(delta(:,1), inputs(i)%inputs()), &
                    delta_w_hidden = delta_w, &
                    delta_w_out = -eta*outer_product(delta_L, a(:,num_hidden_layers)), &
                    delta_b_hidden = -eta*delta, &
                    delta_b_out = -eta*delta_L &
                  )
                end block
              end associate
            end associate
          end associate
        end do
      end associate
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

  module procedure construct_trainable_engine

    trainable_engine%inference_engine_t = inference_engine_t( &
      metadata, input_weights, hidden_weights, output_weights, biases, output_biases &
    )
    trainable_engine%differentiable_activation_strategy_ = differentiable_activation_strategy
    
  end procedure

end submodule trainable_engine_s
