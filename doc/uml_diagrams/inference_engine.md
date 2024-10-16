Fiats Class Diagram
-------------------
```mermaid
classDiagram

class activation_t
class double_precision_string_t
class metadata_t
class mini_batch_t
class neural_network_t
class string_t
class tensor_t
class tensor_map_t
class trainable_network_t
class workspace_t

neural_network_t    <|-- trainable_network_t
neural_network_t     o-- metadata_t
neural_network_t     o-- tensor_map_t
neural_network_t     o-- activation_t
trainable_network_t  o-- workspace_t
string_t            <|-- double_precision_string_t

class double_precision_file_t
class file_t
class hyperparameters_t
class layer_t
class network_configuration_t
class neuron_t

mini_batch_t         o--"1..*" input_output_pair_t
input_output_pair_t  o-- "2"   tensor_t
file_t              <|--       double_precision_file_t

layer_t                  o-- neuron_t
layer_t                  o--"0..*" layer_t
neuron_t                 o--"0..*" neuron_t
training_configuration_t o-- hyperparameters_t
training_configuration_t o-- network_configuration_t

class neural_network_t{
    - input_map_ : tensor_map_t
    - output_map_ : tensor_map_t
    - activation_ : activation_t
    - metadata_ : metadata_t
    - weights_ : real
    - bisases_ : real
    - nodes_ : integer
    + operator(==) logical
    + infer(tensor_t) tensor_t
    + to_json() file_t
    + map_to_input_range(tensor_t) tensor_t
    + map_from_output_range(tensor_t) tensor_t
    + num_hidden_layers() integer
    + num_inputs() integer
    + num_outputs() integer
    + nodes_per_layer() integer
    + assert_conformable_with()
    + skip() logical
    + activation_function_name() string_t
    + learn(mini_batch_t, real, logical, real, workspace_t)
    + assert_consistency()
}
class trainable_network_t{
    - workspace_ : workspace_t
    + train(mini_batch_t, real, logical, real)
    + map_to_training_ranges() : input_output_pair_t
}
class tensor_map_t{
  - layer_ : character
  - intercept_ : real
  - slope_ : real
  + to_json() : file_t
  + operator(==) logical
}
class tensor_t{
  - values_ : real
  + values() real
  - num_components() integer
}
class metadata_t{
  - modelName_ : string_t
  - modelAuthor_ : string_t
  - compilationDate_ : string_t
  - activationFunction_: string_t
  - usingSkipConnections_ : string_t
  + strings() string_t
  + to_json() file_t
  + activation_name() string_t
  + operator(==) logical
}
class mini_batch_t{
  - input_output_pairs_ : input_output_pair_t
  + input_output_pairs() input_output_pair_t
}
class activation_t{
  - selection_ : integer
  + function_name() string_t
  + evaluate() real
  + differentiate() real
}
class input_output_pair_t{
  - inputs_ : tensor_t
  - expected_outputs_ : tensor_t
  + inputs() tensor_t
  + expected_outputs() tensor_t
  + shuffle()
  + write_to_stdout()
}
class file_t{
  - lines_ : string_t
  + lines() : string_t
  + write_lines(string_t)
}
class double_precision_file_t{
  + double_precision_lines() double_precision_string_t
}
class hyperparameters_t{
  - mini_batches_ : integer
  - learning_rate_ : real
  - optimizer_ : character
  + to_json() file_t
  + mini_batches() : integer
  + learning_rate() : real
  + optimizer_name() : character
}
class network_configuration_t{
  - skip_connections_ : logical
  - nodes_per_layer_: integer
  - activation_name_ : character
  + to_json() file_t
  + operator(==) logical
  + activation_name() string_t
  + nodes_per_layer() integer
  + skip_connections() logical
}
class layer_t{
  - neuron_ : neuron_t
  - next_ : layer_t
  + neural_network() : neural_network_t
  + count_layers() integer
  + count_neurons() integer
  + count_inputs() integer
  + neurons_per_layer() integer
  + next_allocated() logical
  + next_pointer() layer_t
}
class neuron_t{
  - weights_ : real
  - bias_ : next
  + to_json() file_t
  + weights() real
  + bias() real
  + next_allocated() logical
  + next_pointer() neuron_t
  + num_inputs() integer
}
class training_configuration_t{
  - hyperparameters_ : hyperparameters_t
  - network_configuration : nework_configuration_t
  + operator(==) logical
  + to_json() file_t
  + mini_batches() integer
  + optimizer_name() string_t
  + nodes_per_layer() integer
  + skip_connections() logical
}
