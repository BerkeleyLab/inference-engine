inference engine
---
```mermaid
classDiagram

class activation_strategy_t

activation_strategy_t <|-- differentiable_activation_strategy_t

class expected_outputs_t

class inference_engine_t

inference_engine_t o-- string_t

input_output_pair_t o-- inputs_t
input_output_pair_t o-- expected_outputs_t

layer_t o-- neuron_t
layer_t o--"0..*" layer_t

mini_batch_t o--"0..*" input_output_pair_t

class netCDF_file_t

neuron_t o--"0..*" neuron_t

class outputs_t


differentiable_activation_strategy_t <|--sigmoid_t
<<abstract>> differentiable_activation_strategy_t
activation_strategy_t <|-- step_t

differentiable_activation_strategy_t <|-- swish_t

class trainable_engine_t{
    -metadata_:string
    -w : real
    -b : real
    -n : integer

    +assert_consistent()
    +train()
    +infer()
    +num_layers()
    +num_inputs()
    +to_inference_engine()
    differentiable_activation_strategy_t()

}
trainable_engine_t o--"0..*" string_t
