! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module inference_engine_m
 !! Specify the user-facing modules, derived types, and type parameters
 use activation_strategy_m, only : activation_strategy_t
 use differentiable_activation_strategy_m, only : differentiable_activation_strategy_t
 use hyperparameters_m, only : hyperparameters_t
 use input_output_pair_m, only : input_output_pair_t, shuffle, write_to_stdout
 use inference_engine_m_, only : inference_engine_t, infer
 use metadata_m, only : metadata_t
 use mini_batch_m, only : mini_batch_t
 use network_configuration_m, only : network_configuration_t
 use gelu_m, only : gelu_t
 use relu_m, only : relu_t
 use sigmoid_m, only : sigmoid_t
 use step_m, only : step_t
 use swish_m, only : swish_t
 use tensor_m, only : tensor_t
 use tensor_map_m, only : tensor_map_t
 use trainable_engine_m, only : trainable_engine_t
 use training_configuration_m, only : training_configuration_t
 use ubounds_m, only : ubounds_t
 implicit none
end module
