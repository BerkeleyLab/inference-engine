inference engine
---
```mermaid
classDiagram

class neural_network_t
class trainable_network_t
class tensor_map_t
class activation_t
class metadata_t
class mini_batch_t
class tensor_t

neural_network_t <|-- trainable_network_t
neural_network_t o-- metadata_t
neural_network_t o-- tensor_map_t
neural_network_t o-- activation_t


class neuron_t
class layer_t
class hyperparameters_t
class network_configuration_t
class file_t
class double_precision_file_t

mini_batch_t o--"1..*" input_output_pair_t
input_output_pair_t o-- "2" tensor_t
file_t <|-- double_precision_file_t

layer_t o-- neuron_t
layer_t o--"0..*" layer_t
neuron_t o--"0..*" neuron_t

class trainable_network_t{
    -neural_network_t : neural_network_t
    -metadata_t : metadata
    -w : real
    -b : real
    -n : integer

    +assert_consistent()
    +train()
    +infer()
    +num_layers()
    +num_inputs()
}
