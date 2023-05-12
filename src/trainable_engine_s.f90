! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(trainable_engine_m) trainable_engine_s
  use assert_m, only : assert
  use intrinsic_array_m, only : intrinsic_array_t
  use outputs_m, only : outputs_t
  implicit none

contains

  module procedure train

    type(outputs_t) actual_outputs
    integer i, l
    real(rkind), allocatable :: sigma_prime_of_z_l(:,:), sigma_prime_of_z_out(:)

    call self%assert_consistent
    call assert(size(inputs)==size(expected_outputs), "train: size(inputs)==size(expected_outputs)")

    do i = 1, size(inputs)
      actual_outputs = self%infer(inputs(i)%inputs(), inference_strategy)
      sigma_prime_of_z_out = self%differentiable_activation_strategy_%activation_derivative(actual_outputs%pre_activation_out())
      associate(a_L => actual_outputs%outputs(), y_L => expected_outputs(i)%outputs())
        call assert(all(size(a_L)==[size(y_L),size(sigma_prime_of_z_out)]), &
          "trainable_engine_t%train: all(size(a_L)==[size(y_L),size(sigma_prime_of_z_out)])", &
          intrinsic_array_t([size(a_L), size(y_L), size(sigma_prime_of_z_out)]) &
        )
        associate(delta_L => (a_L - y_L)*sigma_prime_of_z_out)
          sigma_prime_of_z_l = self%differentiable_activation_strategy_%activation_derivative(actual_outputs%pre_activation_in())
          do l = self%num_hidden_layers() - 1, 2, -1
         
          end do
        end associate
      end associate
    end do


  end procedure

  module procedure construct_trainable_engine

    trainable_engine%inference_engine_t = inference_engine_t( &
      metadata, input_weights, hidden_weights, output_weights, biases, output_biases &
    )
    trainable_engine%differentiable_activation_strategy_ = differentiable_activation_strategy
    
  end procedure

end submodule trainable_engine_s
