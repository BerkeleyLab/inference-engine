! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(inference_engine_m_) inference_engine_s
  use assert_m, only : assert, intrinsic_array_t
  use double_precision_string_m, only : double_precision_string_t
  use gelu_m, only : gelu_t
  use kind_parameters_m, only : double_precision
  use layer_m, only : layer_t
  use neuron_m, only : neuron_t
  use relu_m, only : relu_t
  use step_m, only : step_t
  use swish_m, only : swish_t
  use sigmoid_m, only : sigmoid_t
  implicit none

  interface assert_consistency
    procedure default_real_consistency
    procedure double_precision_consistency
  end interface

  character(len=*), parameter :: acceptable_engine_tag = "0.13.0" ! git tag capable of reading the current json file format

contains

  module procedure default_real_map_to_input_range
    normalized_tensor = self%input_map_%map_to_training_range(tensor)
  end procedure

  module procedure double_precision_map_to_input_range
    normalized_tensor = self%input_map_%map_to_training_range(tensor)
  end procedure

  module procedure default_real_map_from_output_range
    tensor = self%output_map_%map_from_training_range(normalized_tensor)
  end procedure

  module procedure double_precision_map_from_output_range
    tensor = self%output_map_%map_from_training_range(normalized_tensor)
  end procedure

  module procedure default_real_to_exchange
    exchange%input_map_ = self%input_map_
    exchange%output_map_ = self%output_map_
    associate(strings => self%metadata_%strings())
      exchange%metadata_ = metadata_t(strings(1),strings(2),strings(3),strings(4),strings(5))
    end associate
    exchange%weights_ = self%weights_
    exchange%biases_ = self%biases_
    exchange%nodes_ = self%nodes_
    exchange%activation_strategy_ = self%activation_strategy_ 
  end procedure

  module procedure double_precision_to_exchange
    exchange%input_map_ = self%input_map_
    exchange%output_map_ = self%output_map_
    associate(strings => self%metadata_%strings())
      exchange%metadata_ = metadata_t(strings(1),strings(2),strings(3),strings(4),strings(5))
    end associate
    exchange%weights_ = self%weights_
    exchange%biases_ = self%biases_
    exchange%nodes_ = self%nodes_
    exchange%activation_strategy_ = self%activation_strategy_ 
  end procedure

  module procedure default_real_infer_unmapped

    real, allocatable :: a(:,:)
    integer, parameter :: input_layer = 0 
    integer l

    call assert_consistency(self)

    associate(w => self%weights_, b => self%biases_, n => self%nodes_, output_layer => ubound(self%nodes_,1))

      allocate(a(maxval(n), input_layer:output_layer))

      a(1:n(input_layer),input_layer) = inputs%values()

      feed_forward: &
      do l = input_layer+1, output_layer
      associate(z => matmul(w(1:n(l),1:n(l-1),l), a(1:n(l-1),l-1)) + b(1:n(l),l))
          if (l .lt. output_layer) then
             a(1:n(l),l) = self%activation_strategy_%activation(z)
          else
             a(1:n(l),l) = z(1:n(l))
          end if
        end associate
      end do feed_forward

      outputs = tensor_t(a(1:n(output_layer), output_layer))

    end associate

  end procedure

  module procedure double_precision_infer_unmapped

    double precision, allocatable :: a(:,:)
    integer, parameter :: input_layer = 0 
    integer l

    call assert_consistency(self)

    associate(w => self%weights_, b => self%biases_, n => self%nodes_, output_layer => ubound(self%nodes_,1))

      allocate(a(maxval(n), input_layer:output_layer))

      a(1:n(input_layer),input_layer) = inputs%values()

      feed_forward: &
      do l = input_layer+1, output_layer
      associate(z => matmul(w(1:n(l),1:n(l-1),l), a(1:n(l-1),l-1)) + b(1:n(l),l))
          if (l .lt. output_layer) then
             a(1:n(l),l) = self%activation_strategy_%activation(z)
          else
             a(1:n(l),l) = z(1:n(l))
          end if
        end associate
      end do feed_forward

      outputs = tensor_t(a(1:n(output_layer), output_layer))

    end associate

  end procedure

  module procedure default_real_infer

    real, allocatable :: a(:,:)
    integer, parameter :: input_layer = 0
    integer l

    call assert_consistency(self)

    associate(w => self%weights_, b => self%biases_, n => self%nodes_, output_layer => ubound(self%nodes_,1))

      allocate(a(maxval(n), input_layer:output_layer))

#ifndef _CRAYFTN
      associate(normalized_inputs => self%input_map_%map_to_training_range(inputs))
        a(1:n(input_layer),input_layer) = normalized_inputs%values()
      end associate
#else
      block
        type(tensor_t) normalized_inputs
        normalized_inputs = self%input_map_%map_to_training_range(inputs)
        a(1:n(input_layer),input_layer) = normalized_inputs%values()
      end block
#endif

      feed_forward: &
      do l = input_layer+1, output_layer
        associate(z => matmul(w(1:n(l),1:n(l-1),l), a(1:n(l-1),l-1)) + b(1:n(l),l))
          a(1:n(l),l) = self%activation_strategy_%activation(z)
        end associate
      end do feed_forward

#ifndef _CRAYFTN
      associate(normalized_outputs => tensor_t(a(1:n(output_layer), output_layer)))
        outputs = self%output_map_%map_from_training_range(normalized_outputs)
      end associate
#else
      block
        type(tensor_t) :: normalized_outputs
        normalized_outputs = tensor_t(a(1:n(output_layer), output_layer))
        outputs = self%output_map_%map_from_training_range(normalized_outputs)
      end block
#endif

    end associate

  end procedure

  module procedure double_precision_infer

    double precision, allocatable :: a(:,:)
    integer, parameter :: input_layer = 0
    integer l

    call assert_consistency(self)

    associate(w => self%weights_, b => self%biases_, n => self%nodes_, output_layer => ubound(self%nodes_,1))

      allocate(a(maxval(n), input_layer:output_layer))

#ifndef _CRAYFTN
      associate(normalized_inputs => self%input_map_%map_to_training_range(inputs))
        a(1:n(input_layer),input_layer) = normalized_inputs%values()
      end associate
#else
      block
        type(tensor_t) normalized_inputs
        normalized_inputs = self%input_map_%map_to_training_range(inputs)
        a(1:n(input_layer),input_layer) = normalized_inputs%values()
      end block
#endif

      feed_forward: &
      do l = input_layer+1, output_layer
        associate(z => matmul(w(1:n(l),1:n(l-1),l), a(1:n(l-1),l-1)) + b(1:n(l),l))
          a(1:n(l),l) = self%activation_strategy_%activation(z)
        end associate
      end do feed_forward

#ifdef _CRAYFTN
      block
        type(tensor_t) :: normalized_outputs
        normalized_outputs = tensor_t(a(1:n(output_layer), output_layer))
#else
      associate(normalized_outputs => tensor_t(a(1:n(output_layer), output_layer)))
#endif
        outputs = self%output_map_%map_from_training_range(normalized_outputs)
#ifdef _CRAYFTN
      end block
#else
      end associate
#endif

    end associate

  end procedure

  pure subroutine default_real_consistency(self)

    class(inference_engine_t), intent(in) :: self

    integer, parameter :: input_layer=0

    associate( &
      all_allocated=>[allocated(self%weights_),allocated(self%biases_),allocated(self%nodes_),allocated(self%activation_strategy_)]&
    )   
      call assert(all(all_allocated),"inference_engine_s(inference_engine_consistency): fully_allocated", &
        intrinsic_array_t(all_allocated))
    end associate

    associate(max_width=>maxval(self%nodes_), component_dims=>[size(self%biases_,1), size(self%weights_,1), size(self%weights_,2)])
      call assert(all(component_dims == max_width), "inference_engine_s(inference_engine_consistency): conformable arrays", &
        intrinsic_array_t([max_width,component_dims]))
    end associate

    associate(input_subscript => lbound(self%nodes_,1))
      call assert(input_subscript == input_layer, "inference_engine_s(inference_engine_consistency): n base subsscript", &
        input_subscript)
    end associate

  end subroutine

  pure subroutine double_precision_consistency(self)

    class(inference_engine_t(double_precision)), intent(in) :: self

    integer, parameter :: input_layer=0

    associate( &
      all_allocated=>[allocated(self%weights_),allocated(self%biases_),allocated(self%nodes_),allocated(self%activation_strategy_)]&
    )   
      call assert(all(all_allocated),"inference_engine_s(inference_engine_consistency): fully_allocated", &
        intrinsic_array_t(all_allocated))
    end associate

    associate(max_width=>maxval(self%nodes_), component_dims=>[size(self%biases_,1), size(self%weights_,1), size(self%weights_,2)])
      call assert(all(component_dims == max_width), "inference_engine_s(inference_engine_consistency): conformable arrays", &
        intrinsic_array_t([max_width,component_dims]))
    end associate

    associate(input_subscript => lbound(self%nodes_,1))
      call assert(input_subscript == input_layer, "inference_engine_s(inference_engine_consistency): n base subsscript", &
        input_subscript)
    end associate

  end subroutine

  impure function activation_factory_method(activation_name) result(activation)
    character(len=*), intent(in) :: activation_name
    class(activation_strategy_t), allocatable :: activation

    select case(activation_name)
      case("swish")
        activation = swish_t()
      case("sigmoid")
        activation = sigmoid_t()
      case("step")
        activation = step_t()
      case("gelu")
        activation = gelu_t()
      case("relu")
        activation = relu_t()
      case default
        error stop "inference_engine_s(activation_factory_method): unrecognized activation strategy '"//activation_name//"'"
    end select
  end function

  module procedure default_real_construct_from_components

    inference_engine%metadata_ = metadata_t(metadata(1),metadata(2),metadata(3),metadata(4),metadata(5))
    inference_engine%weights_ = weights
    inference_engine%biases_ = biases
    inference_engine%nodes_ = nodes

    block
      integer i

      if (present(input_map)) then
        inference_engine%input_map_ = input_map
      else
        associate(num_inputs => nodes(lbound(nodes,1)))
          associate(default_minima => [(0., i=1,num_inputs)], default_maxima => [(1., i=1,num_inputs)])
            inference_engine%input_map_ = tensor_map_t("inputs", default_minima, default_maxima)
          end associate
        end associate
      end if

      if (present(output_map)) then
        inference_engine%output_map_ = output_map
      else
        associate(num_outputs => nodes(ubound(nodes,1)))
          associate(default_minima => [(0., i=1,num_outputs)], default_maxima => [(1., i=1,num_outputs)])
            inference_engine%output_map_ = tensor_map_t("outputs", default_minima, default_maxima)
          end associate
        end associate
      end if
    end block

    if (allocated(inference_engine%activation_strategy_)) deallocate(inference_engine%activation_strategy_)
    allocate(inference_engine%activation_strategy_, source = activation_factory_method(metadata(4)%string()))
    inference_engine%activation_ = activation_t(metadata(4)%string())

    call assert_consistency(inference_engine)

  end procedure default_real_construct_from_components

  module procedure double_precision_construct_from_components

    inference_engine%metadata_ = metadata
    inference_engine%weights_ = weights
    inference_engine%biases_ = biases
    inference_engine%nodes_ = nodes

    block
      integer i

      if (present(input_map)) then
        inference_engine%input_map_ = input_map
      else
        associate(num_inputs => nodes(lbound(nodes,1)))
          associate(default_intercept => [(0D0, i=1,num_inputs)], default_slope => [(1D0, i=1,num_inputs)])
            inference_engine%input_map_ = tensor_map_t("inputs", default_intercept, default_slope)
          end associate
        end associate
      end if

      if (present(output_map)) then
        inference_engine%output_map_ = output_map
      else
        associate(num_outputs => nodes(ubound(nodes,1)))
          associate(default_intercept => [(0D0, i=1,num_outputs)], default_slope => [(1D0, i=1,num_outputs)])
            inference_engine%output_map_ = tensor_map_t("outputs", default_intercept, default_slope)
          end associate
        end associate
      end if
    end block

    if (allocated(inference_engine%activation_strategy_)) deallocate(inference_engine%activation_strategy_)
    associate(function_name => metadata%activation_name())
      allocate(inference_engine%activation_strategy_, source = activation_factory_method(function_name%string()))
    end associate

    call assert_consistency(inference_engine)

  end procedure double_precision_construct_from_components

  module procedure default_real_to_json

#ifdef _CRAYFTN
    type(tensor_map_t) proto_map
    type(metadata_t) proto_meta
    type(neuron_t) proto_neuron
    proto_map = tensor_map_t("",[0.],[1.])
    proto_meta = metadata_t(string_t(""),string_t(""),string_t(""),string_t(""),string_t(""))
    proto_neuron = neuron_t([0.],1.)
#endif

    call assert_consistency(self)

    associate( &
       num_hidden_layers => self%num_hidden_layers() &
      ,num_outputs => self%num_outputs() &
      ,num_inputs => self%num_inputs() &
      ,first_hidden => lbound(self%nodes_,1) + 1 &
      ,last_hidden => ubound(self%nodes_,1) - 1 &
#ifndef _CRAYFTN
      ,proto_map => tensor_map_t("",[0.],[1.]) &
      ,proto_meta => metadata_t(string_t(""),string_t(""),string_t(""),string_t(""),string_t("")) &
      ,proto_neuron => neuron_t([0.],0.) &
#endif
    )
      associate( &
        metadata_lines => size(proto_meta%to_json()), &
        tensor_map_lines => size(proto_map%to_json()), &
        neuron_lines => size(proto_neuron%to_json()) &
      )
        block
          type(string_t), allocatable :: lines(:)
          integer layer, n, line
          integer, parameter :: &
            brace = 1, bracket_hidden_layers_array = 1, bracket_layer = 1, bracket_output_layer = 1, file_version_lines = 1
               
          associate( json_lines => &
            brace + &                                                          ! { 
              file_version_lines + &                                           !   "acceptable_engine_tag": ...
              metadata_lines + &                                               !   "metadata": ...
              tensor_map_lines + &                                             !   "inputs_tensor_map": ...
              tensor_map_lines + &                                             !   "outputs_tensor_map": ...
                bracket_hidden_layers_array + &                                !   "hidden_layers": [
                  bracket_layer*num_hidden_layers + &                          !      [
                    neuron_lines*sum(self%nodes_(first_hidden:last_hidden))+ & !        neuron ...
                  bracket_layer*num_hidden_layers + &                          !      ] ...
                bracket_hidden_layers_array + &                                !   ],
                bracket_output_layer + &                                       !   "output_layer": [
                  neuron_lines*num_outputs + &                                 !        neurons
                bracket_output_layer + &                                       !    ]
            brace &                                                            ! }
          )
            allocate(lines(json_lines))
            lines(brace) = string_t('{')
            lines(brace+1:brace+file_version_lines)= string_t('    "acceptable_engine_tag": "')//acceptable_engine_tag//'",'
            associate(meta_start => brace + file_version_lines + 1)
              associate(meta_end => meta_start + metadata_lines - 1)
              lines(meta_start:meta_end) = self%metadata_%to_json()
              lines(meta_end) = lines(meta_end) // ","
              associate(input_map_start => meta_end + 1,  input_map_end => meta_end + tensor_map_lines)
                lines(input_map_start:input_map_end) =  self%input_map_%to_json()
                lines(input_map_end) = lines(input_map_end) // ","
                associate(output_map_start => input_map_end + 1,  output_map_end => input_map_end + tensor_map_lines)
                  lines(output_map_start:output_map_end) =  self%output_map_%to_json()
                  lines(output_map_end) = lines(output_map_end) // ","
                  lines(output_map_end + 1) = string_t('     "hidden_layers": [')
                  line= output_map_end + 1
                end associate
              end associate
              end associate
            end associate
            do layer = first_hidden, last_hidden
              line = line + 1
              lines(line) = string_t('         [')
              do n = 1, self%nodes_(layer)
                associate( &
                  neuron => neuron_t(weights=self%weights_(n,1:self%nodes_(layer-1),layer), bias=self%biases_(n,layer)), &
                  neuron_start => line + 1, &
                  neuron_end => line + neuron_lines &
                )
                  lines(neuron_start:neuron_end) = neuron%to_json()
                  lines(neuron_end) = lines(neuron_end) // trim(merge(" ", ",", n==self%nodes_(layer)))
                end associate
                line = line + neuron_lines
              end do
              line = line + 1
              lines(line) = string_t('         ]') // trim(merge(" ", ",", layer==last_hidden))
            end do
            line = line + 1
            lines(line) = string_t('    ],')
            line = line + 1
            lines(line) = string_t('     "output_layer": [')
            layer = last_hidden + 1
            do n = 1, self%nodes_(layer)
              associate( &
                  neuron => neuron_t(weights=self%weights_(n,1:self%nodes_(layer-1),layer), bias=self%biases_(n,layer)), &
                  neuron_start=>line+1, &
                  neuron_end=>line+neuron_lines &
              )
                lines(neuron_start:neuron_end) = neuron%to_json()
                lines(neuron_end) = lines(neuron_end) // trim(merge(" ", ",", n==self%nodes_(layer)))
              end associate
              line = line + neuron_lines
            end do
            line = line + 1
            lines(line) = string_t('         ]')
            line = line + 1
            lines(line) = string_t('}')
            call assert(line == json_lines, "inference_engine_t%to_json: all lines defined", intrinsic_array_t([json_lines, line]))
          end associate
          json_file = file_t(lines)
        end block
      end associate
    end associate
  end procedure default_real_to_json

  module procedure double_precision_to_json

#ifdef _CRAYFTN
    type(tensor_map_t) proto_map
    type(metadata_t) proto_meta
    type(neuron_t) proto_neuron
    proto_map = tensor_map_t("",[0D0],[1D0])
    proto_meta = metadata_t(string_t(""),string_t(""),string_t(""),string_t(""),string_t(""))
    proto_neuron = neuron_t([0D0],1D0)
#endif

    call assert_consistency(self)

    associate( &
       num_hidden_layers => self%num_hidden_layers() &
      ,num_outputs => self%num_outputs() &
      ,num_inputs => self%num_inputs() &
      ,first_hidden => lbound(self%nodes_,1) + 1 &
      ,last_hidden => ubound(self%nodes_,1) - 1 &
#ifndef _CRAYFTN
      ,proto_map => tensor_map_t("",[0D0],[0D0]) &
      ,proto_meta => metadata_t(string_t(""),string_t(""),string_t(""),string_t(""),string_t("")) &
      ,proto_neuron => neuron_t([0D0],0D0) &
#endif
    )
      associate( &
        metadata_lines => size(proto_meta%to_json()), &
        tensor_map_lines => size(proto_map%to_json()), &
        neuron_lines => size(proto_neuron%to_json()) &
      )
        block
          type(string_t), allocatable :: lines(:)
          integer layer, n, line
          integer, parameter :: &
            brace = 1, bracket_hidden_layers_array = 1, bracket_layer = 1, bracket_output_layer = 1, file_version_lines = 1
               
          associate( json_lines => &
            brace + &                                                          ! { 
              file_version_lines + &                                           !   "acceptable_engine_tag": ...
              metadata_lines + &                                               !   "metadata": ...
              tensor_map_lines + &                                             !   "inputs_tensor_map": ...
              tensor_map_lines + &                                             !   "outputs_tensor_map": ...
                bracket_hidden_layers_array + &                                !   "hidden_layers": [
                  bracket_layer*num_hidden_layers + &                          !      [
                    neuron_lines*sum(self%nodes_(first_hidden:last_hidden))+ & !        neuron ...
                  bracket_layer*num_hidden_layers + &                          !      ] ...
                bracket_hidden_layers_array + &                                !   ],
                bracket_output_layer + &                                       !   "output_layer": [
                  neuron_lines*num_outputs + &                                 !        neurons
                bracket_output_layer + &                                       !    ]
            brace &                                                            ! }
          )
            allocate(lines(json_lines))
            lines(brace) = string_t('{')
            lines(brace+1:brace+file_version_lines)= string_t('    "acceptable_engine_tag": "')//acceptable_engine_tag//'",'
            associate(meta_start => brace + file_version_lines + 1)
              associate(meta_end => meta_start + metadata_lines - 1)
              lines(meta_start:meta_end) = self%metadata_%to_json()
              lines(meta_end) = lines(meta_end) // ","
                associate(input_map_start => meta_end + 1,  input_map_end => meta_end + tensor_map_lines)
                  lines(input_map_start:input_map_end) =  self%input_map_%to_json()
                  lines(input_map_end) = lines(input_map_end) // ","
                  associate(output_map_start => input_map_end + 1,  output_map_end => input_map_end + tensor_map_lines)
                    lines(output_map_start:output_map_end) =  self%output_map_%to_json()
                    lines(output_map_end) = lines(output_map_end) // ","
                    lines(output_map_end + 1) = string_t('     "hidden_layers": [')
                    line= output_map_end + 1
                  end associate
                end associate
              end associate
            end associate
            do layer = first_hidden, last_hidden
              line = line + 1
              lines(line) = string_t('         [')
              do n = 1, self%nodes_(layer)
                associate( &
                  neuron => neuron_t(weights=self%weights_(n,1:self%nodes_(layer-1),layer), bias=self%biases_(n,layer)), &
                  neuron_start => line + 1, &
                  neuron_end => line + neuron_lines &
                )
                  lines(neuron_start:neuron_end) = neuron%to_json()
                  lines(neuron_end) = lines(neuron_end) // trim(merge(" ", ",", n==self%nodes_(layer)))
                end associate
                line = line + neuron_lines
              end do
              line = line + 1
              lines(line) = string_t('         ]') // trim(merge(" ", ",", layer==last_hidden))
            end do
            line = line + 1
            lines(line) = string_t('    ],')
            line = line + 1
            lines(line) = string_t('     "output_layer": [')
            layer = last_hidden + 1
            do n = 1, self%nodes_(layer)
              associate( &
                  neuron => neuron_t(weights=self%weights_(n,1:self%nodes_(layer-1),layer), bias=self%biases_(n,layer)), &
                  neuron_start=>line+1, &
                  neuron_end=>line+neuron_lines &
              )
                lines(neuron_start:neuron_end) = neuron%to_json()
                lines(neuron_end) = lines(neuron_end) // trim(merge(" ", ",", n==self%nodes_(layer)))
              end associate
              line = line + neuron_lines
            end do
            line = line + 1
            lines(line) = string_t('         ]')
            line = line + 1
            lines(line) = string_t('}')
            call assert(line == json_lines, "inference_engine_t%to_json: all lines defined", intrinsic_array_t([json_lines, line]))
          end associate
          json_file = file_t(lines)
        end block
      end associate
    end associate
  end procedure double_precision_to_json

  module procedure default_real_from_json

    character(len=:), allocatable :: justified_line
    integer l, num_file_lines
    type(string_t), allocatable :: lines(:)
    type(tensor_map_t) input_map, output_map
    type(layer_t) hidden_layers, output_layer

    lines = file_%lines()
    call assert(adjustl(lines(1)%string())=="{", "inference_engine_s(default_real_from_json): expected outermost object '{'")
 
    check_git_tag: &
    block 
      character(len=:), allocatable :: tag

      tag = lines(2)%get_json_value("acceptable_engine_tag", mold="")
      call assert( &
        tag == acceptable_engine_tag &
        ,"inference_engine_s(default_real_from_json): acceptable_engine_tag" &
        ,tag //"(expected " //acceptable_engine_tag // ")" &
      )
    end block check_git_tag
      
    num_file_lines = size(lines)

    read_tensor_maps: &
    associate(proto_map => tensor_map_t("",[0.],[1.]))
      associate(num_map_lines => size(proto_map%to_json()))

         find_inputs_map: &
         do l = 1, num_file_lines
           justified_line = adjustl(lines(l)%string())
           if (justified_line == '"inputs_map": {') exit
         end do find_inputs_map

         call assert(justified_line =='"inputs_map": {', 'default_real_from_json: expecting "inputs_map": {', justified_line)
         input_map = tensor_map_t(lines(l:l+num_map_lines-1))

         find_outputs_map: &
         do l = 1, num_file_lines
           justified_line = adjustl(lines(l)%string())
           if (justified_line == '"outputs_map": {') exit
         end do find_outputs_map

         call assert(justified_line =='"outputs_map": {', 'default_real_from_json: expecting "outputs_map": {', justified_line)
         output_map = tensor_map_t(lines(l:l+num_map_lines-1))

      end associate
    end associate read_tensor_maps

    find_hidden_layers: &
    do l = 1, num_file_lines
      justified_line = adjustl(lines(l)%string())
      if (justified_line == '"hidden_layers": [') exit
    end do find_hidden_layers
    call assert(justified_line=='"hidden_layers": [', 'default_real_from_json: expecting "hidden_layers": [', justified_line)

    read_hidden_layers: &
    block
      integer, parameter :: bracket_lines_per_layer=2
      character(len=:), allocatable :: output_layer_line

      hidden_layers = layer_t(lines, start=l+1)

      read_layers_of_neurons: &
      associate(proto_neuron => neuron_t(weights=[0.], bias=0.))
        associate(output_layer_line_number => l + 1 + size(proto_neuron%to_json())*sum(hidden_layers%count_neurons()) &
          + bracket_lines_per_layer*hidden_layers%count_layers() + 1)

          output_layer_line = lines(output_layer_line_number)%string()

          call assert(adjustl(output_layer_line)=='"output_layer": [', 'default_real_from_json: expecting "output_layer": [', &
            lines(output_layer_line_number)%string())

          output_layer = layer_t(lines, start=output_layer_line_number)
        end associate
      end associate read_layers_of_neurons
    end block read_hidden_layers

    find_metadata: &
    do l = 1, num_file_lines
      justified_line = adjustl(lines(l)%string())
      if (justified_line == '"metadata": {') exit
    end do find_metadata
    call assert(justified_line=='"metadata": {', 'default_real_from_json: expecting "metadata": {', justified_line)

    read_metadata: &
    associate(proto_meta => metadata_t(string_t(""),string_t(""),string_t(""),string_t(""),string_t("")))
      associate(metadata => metadata_t(lines(l : l + size(proto_meta%to_json()) - 1)))
        associate(metadata_strings => metadata%strings())
          inference_engine = hidden_layers%inference_engine(metadata_strings, output_layer, input_map, output_map)
          if (allocated(inference_engine%activation_strategy_)) deallocate(inference_engine%activation_strategy_)
          associate(function_name => metadata%activation_name())
            allocate(inference_engine%activation_strategy_, source = activation_factory_method(function_name%string()))
          end associate
        end associate
      end associate
    end associate read_metadata

    call assert_consistency(inference_engine)

  end procedure default_real_from_json

  module procedure double_precision_unmapped_from_json
    unmapped_engine%inference_engine_t = double_precision_from_json(file)
  end procedure

  module procedure double_precision_from_json

    character(len=:), allocatable :: justified_line
    integer l, num_file_lines
    type(double_precision_string_t), allocatable :: lines(:)
    type(tensor_map_t(double_precision)) input_map, output_map
    type(layer_t(double_precision)) hidden_layers, output_layer

    lines = file%double_precision_lines()
    call assert(adjustl(lines(1)%string())=="{", "inference_engine_s(double_precision_from_json): expected outermost object '{'")

    check_git_tag: &
    block
      character(len=:), allocatable :: tag

      tag = lines(2)%get_json_value("acceptable_engine_tag", mold="")
      call assert( &
        tag == acceptable_engine_tag &
        ,"inference_engine_s(double_precision_from_json): acceptable_engine_tag" &
        ,tag //"(expected " //acceptable_engine_tag // ")" &
      )
    end block check_git_tag

    num_file_lines = size(lines)

    read_tensor_maps: &
    associate(proto_map => tensor_map_t("",[0D0],[1D0]))
      associate(num_map_lines => size(proto_map%to_json()))

         find_inputs_map: &
         do l = 1, num_file_lines
           justified_line = adjustl(lines(l)%string())
           if (justified_line == '"inputs_map": {') exit
         end do find_inputs_map

         call assert(justified_line =='"inputs_map": {', 'double_precision_from_json: expecting "inputs_map": {', justified_line)
         input_map = tensor_map_t(lines(l:l+num_map_lines-1))

         find_outputs_map: &
         do l = 1, num_file_lines
           justified_line = adjustl(lines(l)%string())
           if (justified_line == '"outputs_map": {') exit
         end do find_outputs_map

         call assert(justified_line =='"outputs_map": {', 'double_precision_from_json: expecting "outputs_map": {', justified_line)
         output_map = tensor_map_t(lines(l:l+num_map_lines-1))

      end associate
    end associate read_tensor_maps

    find_hidden_layers: &
    do l = 1, num_file_lines
      justified_line = adjustl(lines(l)%string())
      if (justified_line == '"hidden_layers": [') exit
    end do find_hidden_layers
    call assert(justified_line=='"hidden_layers": [', 'double_precision_from_json: expecting "hidden_layers": [', justified_line)

    read_hidden_layers: &
    block
      integer, parameter :: bracket_lines_per_layer=2
      character(len=:), allocatable :: output_layer_line

      hidden_layers = layer_t(lines, start=l+1)

      read_layers_of_neurons: &
      associate(proto_neuron => neuron_t(weights=[0D0], bias=0D0))
        associate(output_layer_line_number => l + 1 + size(proto_neuron%to_json())*sum(hidden_layers%count_neurons()) &
          + bracket_lines_per_layer*hidden_layers%count_layers() + 1)

          output_layer_line = lines(output_layer_line_number)%string()

          call assert(adjustl(output_layer_line)=='"output_layer": [', 'double_precision_from_json: expecting "output_layer": [', &
            lines(output_layer_line_number)%string())

          output_layer = layer_t(lines, start=output_layer_line_number)
        end associate
      end associate read_layers_of_neurons
    end block read_hidden_layers

    find_metadata: &
    do l = 1, num_file_lines
      justified_line = adjustl(lines(l)%string())
      if (justified_line == '"metadata": {') exit
    end do find_metadata
    call assert(justified_line=='"metadata": {', 'double_precision_from_json: expecting "metadata": {', justified_line)

    read_metadata: &
    associate(proto_meta => metadata_t(string_t(""),string_t(""),string_t(""),string_t(""),string_t("")))
      associate(metadata => metadata_t(lines(l : l + size(proto_meta%to_json()) - 1)))
        inference_engine = hidden_layers%inference_engine(metadata, output_layer, input_map, output_map)
        if (allocated(inference_engine%activation_strategy_)) deallocate(inference_engine%activation_strategy_)
        associate(function_name => metadata%activation_name())
          allocate(inference_engine%activation_strategy_, source = activation_factory_method(function_name%string()))
        end associate
      end associate
    end associate read_metadata

    call assert_consistency(inference_engine)

  end procedure double_precision_from_json

  module procedure default_real_assert_conformable_with

    call assert_consistency(self)
    call assert_consistency(inference_engine)

    associate(equal_shapes => [ &
      shape(self%weights_) == shape(inference_engine%weights_), &
      shape(self%biases_) == shape(inference_engine%biases_), &
      shape(self%nodes_) == shape(inference_engine%nodes_)  &
     ])
      call assert(all(equal_shapes), "assert_conformable_with: all(equal_shapes)", intrinsic_array_t(equal_shapes))
    end associate

    call assert(same_type_as(self%activation_strategy_, inference_engine%activation_strategy_), "assert_conformable_with: types)")
    
  end procedure

  module procedure double_precision_assert_conformable_with

    call assert_consistency(self)
    call assert_consistency(inference_engine)

    associate(equal_shapes => [ &
      shape(self%weights_) == shape(inference_engine%weights_), &
      shape(self%biases_) == shape(inference_engine%biases_), &
      shape(self%nodes_) == shape(inference_engine%nodes_)  &
     ])
      call assert(all(equal_shapes), "assert_conformable_with: all(equal_shapes)", intrinsic_array_t(equal_shapes))
    end associate

    call assert(same_type_as(self%activation_strategy_, inference_engine%activation_strategy_), "assert_conformable_with: types)")
    
  end procedure

  module procedure default_real_approximately_equal

    logical nodes_eq

    nodes_eq = all(lhs%nodes_ == rhs%nodes_)

    call assert_consistency(lhs)
    call assert_consistency(rhs)
    call lhs%assert_conformable_with(rhs)

    block
      integer l
      logical layer_eq(ubound(lhs%nodes_,1))
      real, parameter :: tolerance = 1.E-06

      associate(n => lhs%nodes_)
#ifndef __INTEL_COMPILER
        do concurrent(l = 1:ubound(n,1))
            layer_eq(l) = all(abs(lhs%weights_(1:n(l),1:n(l-1),l) - rhs%weights_(1:n(l),1:n(l-1),l)) < tolerance) .and. &
                          all(abs(lhs%biases_(1:n(l),l)           - rhs%biases_(1:n(l),l)) < tolerance)
        end do
#else
        block
          integer j, k
          do l = 1, ubound(n,1)
            do j = 1, n(l)
              do k = 1, n(l-1)
                layer_eq(l) = all(abs(lhs%weights_(j,k,l) - rhs%weights_(j,k,l)) < tolerance) .and. &
                              all(abs(lhs%biases_(j,l)    - rhs%biases_(j,l)) < tolerance)
              end do
            end do
          end do
        end block
#endif
      end associate

      lhs_eq_rhs = nodes_eq .and. all(layer_eq)
    end block

  end procedure

  module procedure double_precision_approximately_equal

    logical nodes_eq

    nodes_eq = all(lhs%nodes_ == rhs%nodes_)

    call assert_consistency(lhs)
    call assert_consistency(rhs)
    call lhs%assert_conformable_with(rhs)

    block
      integer l
      logical layer_eq(ubound(lhs%nodes_,1))
      real, parameter :: tolerance = 1.D-12

      associate(n => lhs%nodes_)
#ifndef __INTEL_COMPILER
        do concurrent(l = 1:ubound(n,1))
            layer_eq(l) = all(abs(lhs%weights_(1:n(l),1:n(l-1),l) - rhs%weights_(1:n(l),1:n(l-1),l)) < tolerance) .and. &
                          all(abs(lhs%biases_(1:n(l),l)           - rhs%biases_(1:n(l),l)) < tolerance)
        end do
#else
        block
          integer j, k
          do l = 1, ubound(n,1)
            do j = 1, n(l)
              do k = 1, n(l-1)
                layer_eq(l) = all(abs(lhs%weights_(j,k,l) - rhs%weights_(j,k,l)) < tolerance) .and. &
                              all(abs(lhs%biases_(j,l)    - rhs%biases_(j,l)) < tolerance)
              end do
            end do
          end do
        end block
#endif
      end associate

      lhs_eq_rhs = nodes_eq .and. all(layer_eq)
    end block

  end procedure

  module procedure default_real_num_outputs
    call assert_consistency(self)
    output_count = self%nodes_(ubound(self%nodes_,1))
  end procedure

  module procedure double_precision_num_outputs
    call assert_consistency(self)
    output_count = self%nodes_(ubound(self%nodes_,1))
  end procedure

  module procedure default_real_num_hidden_layers
    integer, parameter :: input_layer = 1, output_layer = 1
    call assert_consistency(self)
    associate(num_layers => size(self%nodes_))
      hidden_layer_count =  num_layers - (input_layer + output_layer)
    end associate
  end procedure

  module procedure double_precision_num_hidden_layers
    integer, parameter :: input_layer = 1, output_layer = 1
    call assert_consistency(self)
    associate(num_layers => size(self%nodes_))
      hidden_layer_count =  num_layers - (input_layer + output_layer)
    end associate
  end procedure

  module procedure default_real_num_inputs
    call assert_consistency(self)
    input_count = self%nodes_(lbound(self%nodes_,1))
  end procedure

  module procedure double_precision_num_inputs
    call assert_consistency(self)
    input_count = self%nodes_(lbound(self%nodes_,1))
  end procedure

  module procedure default_real_nodes_per_layer
    call assert_consistency(self)
    node_count = self%nodes_
  end procedure

  module procedure double_precision_nodes_per_layer
    call assert_consistency(self)
    node_count = self%nodes_
  end procedure

  module procedure default_real_skip
    associate(strings => self%metadata_%strings())
      use_skip_connections = merge(.true., .false.,  strings(5) == "true")
    end associate
  end procedure

  module procedure double_precision_skip
    associate(strings => self%metadata_%strings())
      use_skip_connections = merge(.true., .false.,  strings(5) == "true")
    end associate
  end procedure

  module procedure default_real_activation_name
    associate(strings => self%metadata_%strings())
      activation_name = strings(4)
    end associate
  end procedure

  module procedure double_precision_activation_name
    associate(strings => self%metadata_%strings())
      activation_name = strings(4)
    end associate
  end procedure

end submodule inference_engine_s
