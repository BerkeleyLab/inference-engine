submodule(network_configuration_m) network_configuration_s
  use assert_m, only : assert
  use sourcery_m, only : csv
  implicit none

  character(len=*), parameter :: skip_connections_key  = "skip connections"
  character(len=*), parameter :: nodes_per_layer_key = "nodes per layer"
  character(len=*), parameter :: activation_function_key     = "activation function"

contains

  module procedure from_components
    network_configuration%skip_connections_ = skip_connections
    network_configuration%nodes_per_layer_ = nodes_per_layer
    network_configuration%activation_function_ = activation_function
  end procedure 

  module procedure equals

    call assert(allocated(lhs%activation_function_) .and. allocated(rhs%activation_function_), "network_configuration_s(equals): allocated activation_functions")

    lhs_equals_rhs = &
      lhs%skip_connections_ .eqv. rhs%skip_connections_ .and. &
      lhs%activation_function_ == rhs%activation_function_ .and. &
      all(lhs%nodes_per_layer_ == rhs%nodes_per_layer_)
     
  end procedure 

  module procedure from_json
    integer l
    logical network_configuration_key_found 

    network_configuration_key_found = .false.

    do l=1,size(lines)
      if (lines(l)%get_json_key() == "network configuration") then
        network_configuration_key_found = .true.
        network_configuration%skip_connections_  = lines(l+1)%get_json_value(string_t(skip_connections_key), mold=.true.)
        network_configuration%nodes_per_layer_ = lines(l+2)%get_json_value(string_t(nodes_per_layer_key), mold=[integer::])
        network_configuration%activation_function_ = lines(l+3)%get_json_value(string_t(activation_function_key), mold=string_t(""))
        return
      end if
    end do

    call assert(network_configuration_key_found, "network_configuration_s(from_json): network_configuration_found")
  end procedure

  module procedure to_json
    character(len=*), parameter :: indent = repeat(" ",ncopies=4)
    integer, parameter :: max_logical_width= 6, char_per_elem = 10, brackets = 2
    character(len=max_logical_width) skip_connections_string
    character(len=:), allocatable :: nodes_per_layer_string

    allocate(character(len=size(self%nodes_per_layer_)*char_per_elem + brackets) :: nodes_per_layer_string)

    write(skip_connections_string,*) trim(merge("true ","false",self%skip_connections_))
    write(nodes_per_layer_string, csv) self%nodes_per_layer_

    lines = [ &
      string_t(indent // '"network configuration": {'), &
      string_t(indent // indent // '"' // skip_connections_key    // '" : '  // trim(adjustl(skip_connections_string  )) //  ','), &
      string_t(indent // indent // '"' // nodes_per_layer_key     // '" : [' // trim(adjustl(nodes_per_layer_string   )) // '],'), &
      string_t(indent // indent // '"' // activation_function_key // '" : "' // trim(adjustl(self%activation_function_)) // '"' ), &
      string_t(indent // '}') &
    ]
  end procedure

end submodule network_configuration_s
