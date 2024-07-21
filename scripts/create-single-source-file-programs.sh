#!/bin/bash
# Copyright (c), The Regents of the University of California
# Terms of use are as specified in LICENSE.txt
# ----
# This script concatenates the Inference-Engine software stack into
# single-source-file programs.

if [ -d ../build/dependencies/assert ] || [ -d ../build/dependencies/julienne ]; then
  echo "Dependencies assert and julienne were found in ../build/dependencies as expected." 
else
  echo ""
  echo ""
  echo "-------------------- create-single-source-file.sh -----------------------"
  echo "Dependencies assert and julienne were not found in ../build/dependencies." 
  echo "Running fpm build to download the dependencies." 
  echo "This unavoidably builds inference-engine too." 
  echo "-------------------------------------------------------------------------"
  echo ""
  echo ""
  fpm build
fi

echo "Concatenating assert, julienne, and inference-engine." 

cat \
  ../build/dependencies/assert/src/assert/characterizable_m.f90 \
  ../build/dependencies/assert/src/assert/intrinsic_array_m.F90 \
  ../build/dependencies/assert/src/assert/assert_subroutine_m.F90 \
  ../build/dependencies/assert/src/assert/intrinsic_array_s.F90 \
  ../build/dependencies/assert/src/assert/assert_subroutine_s.F90 \
  ../build/dependencies/assert/src/assert_m.f90 \
> single-file.F90

cat \
  ../build/dependencies/julienne/src/julienne/julienne_bin_m.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_command_line_m.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_formats_m.F90 \
  ../build/dependencies/julienne/src/julienne/julienne_user_defined_collectives_m.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_string_m.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_file_m.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_test_result_m.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_test_description_m.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_test_m.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_vector_test_description_m.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_bin_s.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_command_line_s.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_formats_s.F90 \
  ../build/dependencies/julienne/src/julienne/julienne_user_defined_collectives_s.F90 \
  ../build/dependencies/julienne/src/julienne/julienne_string_s.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_file_s.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_test_result_s.F90 \
  ../build/dependencies/julienne/src/julienne/julienne_test_description_s.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_test_s.F90 \
  ../build/dependencies/julienne/src/julienne/julienne_vector_test_description_s.f90 \
  ../build/dependencies/julienne/src/julienne_m.f90 \
>> single-file.F90
  
cat \
  ../src/inference_engine/kind_parameters_m.f90 \
  ../src/inference_engine/activation_strategy_m.f90 \
  ../src/inference_engine/differentiable_activation_strategy_m.f90 \
  ../src/inference_engine/gelu_m.f90 \
  ../src/inference_engine/relu_m.f90 \
  ../src/inference_engine/sigmoid_m.f90 \
  ../src/inference_engine/step_m.f90 \
  ../src/inference_engine/swish_m.f90 \
  ../src/inference_engine/hyperparameters_m.f90 \
  ../src/inference_engine/network_configuration_m.f90 \
  ../src/inference_engine/training_configuration_m.f90 \
  ../src/inference_engine/neuron_m.f90 \
  ../src/inference_engine/metadata_m.f90 \
  ../src/inference_engine/tensor_m.f90 \
  ../src/inference_engine/input_output_pair_m.f90 \
  ../src/inference_engine/mini_batch_m.f90 \
  ../src/inference_engine/tensor_range_m.f90 \
  ../src/inference_engine/ubounds_m.f90 \
  ../src/inference_engine/inference_engine_m_.f90 \
  ../src/inference_engine/layer_m.f90 \
  ../src/inference_engine/trainable_engine_m.F90 \
  ../src/inference_engine_m.f90 \
  ../src/inference_engine/gelu_s.f90 \
  ../src/inference_engine/relu_s.f90 \
  ../src/inference_engine/step_s.f90 \
  ../src/inference_engine/sigmoid_s.f90 \
  ../src/inference_engine/swish_s.f90 \
  ../src/inference_engine/training_configuration_s.F90 \
  ../src/inference_engine/network_configuration_s.F90 \
  ../src/inference_engine/hyperparameters_s.f90 \
  ../src/inference_engine/input_output_pair_s.f90 \
  ../src/inference_engine/neuron_s.f90 \
  ../src/inference_engine/layer_s.f90 \
  ../src/inference_engine/metadata_s.f90 \
  ../src/inference_engine/mini_batch_s.f90 \
  ../src/inference_engine/tensor_s.f90 \
  ../src/inference_engine/tensor_range_s.f90 \
  ../src/inference_engine/inference_engine_s.F90 \
  ../src/inference_engine/trainable_engine_s.F90 \
>> single-file.F90

echo ""
echo ""
echo "-------------------- create-single-source-file.sh -----------------------"

echo "Creating concurrent-inferences.F90"
cat single-file.F90 \
  ../example/concurrent-inferences.f90 \
> concurrent-inferences.F90

if [ -f concurrent-inferences.F90 ]; then
  echo "concurrent-inferences.F90 created."
else
  echo "concurrent-inferences.F90 wasn't created -- something went wrong"
fi

echo "Creating saturated-mixing-ratio.F90"
cat single-file.F90 \
  ../example/supporting-modules/saturated_mixing_ratio_m.f90 \
  ../example/learn-saturated-mixing-ratio.f90 \
> saturated-mixing-ratio.F90

if [ -f saturated-mixing-ratio.F90 ]; then
  echo "saturated-mixing-ratio.F90 created."
else
  echo "saturated-mixing-ratio.F90 wasn't created -- something went wrong"
fi

echo "Example compile commands:"
echo ""
echo 'gfortran -fcoarray=single -O3 -o saturated-mixing-ratio saturated-mixing-ratio.F90'
echo 'flang-new -O3 -mmlir -allow-assumed-rank -o saturated-mixing-ratio saturated-mixing-ratio.F90'
echo 'nagfor -fpp -O4 -o saturated-mixing-ratio saturated-mixing-ratio.F90'
echo 'ifx -O3 -o saturated-mixing-ratio saturated-mixing-ratio.F90'

rm single-file.F90

echo "-------------------------------------------------------------------------"
