! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module inference_engine_m
 !! Specify the user-facing modules, derived types, and type parameters
 use activation_strategy_m, only : activation_strategy_t
 use differentiable_activation_strategy_m, only : differentiable_activation_strategy_t
 use input_output_pair_m, only : input_output_pair_t
 use inference_engine_m_, only : inference_engine_t, difference_t
 use kind_parameters_m, only : rkind
 use mini_batch_m, only : mini_batch_t
 use NetCDF_file_m, only : NetCDF_file_t 
 use sigmoid_m, only : sigmoid_t
 use step_m, only : step_t
 use swish_m, only : swish_t
 use tensor_m, only : tensor_t
 use trainable_engine_m, only : trainable_engine_t
 implicit none
end module
