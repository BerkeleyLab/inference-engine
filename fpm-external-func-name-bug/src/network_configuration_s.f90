submodule(network_configuration_m) network_configuration_s
  implicit none

  character(len=*), parameter :: skip_connections_key  = "skip connections"
  character(len=*), parameter :: nodes_per_layer_key = "nodes per layer"
  character(len=*), parameter :: activation_name_key     = "activation function"

contains

  module procedure from_components
    network_configuration%skip_connections_ = skip_connections
    network_configuration%nodes_per_layer_ = nodes_per_layer
    network_configuration%activation_name_ = activation_name
  end procedure 

  module procedure equals

    lhs_equals_rhs = &
      lhs%skip_connections_ .eqv. rhs%skip_connections_ .and. &
      lhs%activation_name_ == rhs%activation_name_ .and. &
      all(lhs%nodes_per_layer_ == rhs%nodes_per_layer_)
     
  end procedure 

  module procedure activation_name
  end procedure

  module procedure nodes_per_layer
    nodes = self%nodes_per_layer_
  end procedure

  module procedure skip_connections
    using_skip = self%skip_connections_
  end procedure

end submodule network_configuration_s
