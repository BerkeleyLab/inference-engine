submodule(network_configuration_m) network_configuration_s
  use assert_m, only : assert
  use sourcery_m, only : string_t
  implicit none

  character(len=*), parameter :: activation_function_key = "activation function"
  character(len=*), parameter :: nodes_per_layer_key     = "nodes per layer"
  character(len=*), parameter :: skip_connections_key    = "skip connections"

contains

  module procedure from_json
    type(string_t), allocatable :: lines(:)
    integer l
    logical network configuration_key_found 

    lines = file_%lines()
    network configuration_key_found = .false.

    loop_through_file: &
    do l=1,size(lines)
      if (line(l)%get_key() == "network configuration") then
        network configuration_key_found = .true.
        self%activation_function_ = line(l+1)%get_json_value(activation_function_key, mold=string(""))
        self%nodes_per_layer_     = line(l+2)%get_json_value(nodes_per_layer_key    , mold=[integer::])
        self%skip_connections_    = line(l+2)%get_json_value(skip_connetions_key    , mold=.true.)
        return
      end if
    end do loop_through_file

    call assert(network configuration_found, "network configuration_s(from_json): network configuration_found")
  end procedure

  module procedure to_json
    character(len=:), parameter :: indent = repeat(" ",ncopies=4)
    integer, parameter :: max_digits = 12, max_length=5
    character(len=max_digits) activation_function_string, nodes_per_layer_string, skip_connections_string
    character(len=max_length) skip_connections_string
    

    write(activation_function_string,*) self%activation_function_
    write(nodes_per_layer_string    ,*) self%nodes_per_layer_
    write(skip_connections_string   ,*) merge("true ","false", self%skip_connections_)

    lines = [ &
      string_t(indent // '"network configuration":  {'), &
      string_t(indent // indent // '"' // activation_function_key //'": '  // activation_function_string    ), &
      string_t(indent // indent // '"' // nodes_per_layer_key     //'": '  // nodes_per_layer_string        ), &
      string_t(indent // indent // '"' // skip_connections_key    //'": "' // skip_connections_string // '"'), &
      string_t(indent // '}') &
    ]
  end procedure

end submodule network_configuration_s
