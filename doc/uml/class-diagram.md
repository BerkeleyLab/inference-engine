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
class workspace_t

neural_network_t <|-- trainable_network_t
neural_network_t o-- metadata_t
neural_network_t o-- tensor_map_t
neural_network_t o-- activation_t
trainable_network_t o-- workspace_t

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


class neural_network_t{
    -input_map_ : tensor_map_t
    -output_map_ : tensor_map_t
    -activation_ : activation_t
    -metadata_ : metadata_t
    -weights_ : real
    -bisases_ : real
    -nodes_ : integer
+operator(==)
+infer()
+to_json()
+map_to_input_range()
+map_from_output_range()
+num_hidden_layers()
+num_inputs()
+num_outputs()
+nodes_per_layer()       
+assert_conformable_with ()
+skip()                  
+activation_function_name()
+learn()                 
+assert_consistency() 
}
class trainable_network_t{
    -neural_network_t : neural_network_t
    -workspace_ : workspace_t
    +train()
    +map_to_training_ranges()
}
