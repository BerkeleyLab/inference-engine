! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(training_configuration_m) training_configuration_s
  use assert_m, only : assert
  use double_precision_string_m, only : double_precision_string_t
  use activation_m, only : activation_t, gelu, relu, sigmoid, swish
  implicit none

  character(len=*), parameter :: header="{", footer="}", separator = ","

contains

  module procedure default_real_from_components

    training_configuration%hyperparameters_ = hyperparameters
    training_configuration%network_configuration_ = network_configuration
    training_configuration%file_t = file_t([ &
      string_t(header), &
      training_configuration%hyperparameters_%to_json(), &
      string_t(separator), &
      training_configuration%network_configuration_%to_json(), &
      string_t(footer) &
    ])
  end procedure

  module procedure double_precision_from_components

    training_configuration%hyperparameters_ = hyperparameters
    training_configuration%network_configuration_ = network_configuration
    training_configuration%file_t = file_t([ &
      string_t(header), &
      training_configuration%hyperparameters_%to_json(), &
      string_t(separator), &
      training_configuration%network_configuration_%to_json(), &
      string_t(footer) &
    ])
  end procedure

  module procedure default_real_from_file
    integer, parameter :: hyperparameters_start=2, hyperparameters_end=6, separator_line=7   ! line numbers
    integer, parameter :: net_config_start=8, net_config_end=12                         ! line numbers
    integer, parameter :: file_start=hyperparameters_start-1, file_end=net_config_end+1 ! line numbers
#if defined __INTEL_COMPILER || _CRAYFTN
    type(string_t), allocatable :: lines(:)
#endif

    training_configuration%file_t = file_object

#if defined __INTEL_COMPILER || _CRAYFTN
    lines = training_configuration%file_t%lines()
    call assert(trim(adjustl(lines(file_start)%string()))==header, &
      "training_configuration_s(default_precision_from_file): header",lines(file_start))
    training_configuration%hyperparameters_ = hyperparameters_t(lines(hyperparameters_start:hyperparameters_end))
    call assert(trim(adjustl(lines(separator_line)%string()))==separator, &
      "training_configuration_s(default_precision_from_file): separator", &
      lines(file_start))
    training_configuration%network_configuration_= network_configuration_t(lines(net_config_start:net_config_end))
    call assert(trim(adjustl(lines(file_end)%string()))==footer, &
      "training_configuration_s(default_precision_from_file): footer", lines(file_end))
#else
    associate(lines => training_configuration%file_t%lines())
      call assert(trim(adjustl(lines(file_start)%string()))==header, &
        "training_configuration_s(default_precision_from_file): header",lines(file_start))
      training_configuration%hyperparameters_ = hyperparameters_t(lines(hyperparameters_start:hyperparameters_end))
      call assert(trim(adjustl(lines(separator_line)%string()))==separator, &
        "training_configuration_s(default_precision_from_file): separator", &
        lines(file_start))
      training_configuration%network_configuration_= network_configuration_t(lines(net_config_start:net_config_end))
      call assert(trim(adjustl(lines(file_end)%string()))==footer, &
        "training_configuration_s(default_precision_from_file): footer", lines(file_end))
    end associate
#endif

  end procedure

  module procedure double_precision_from_file
    integer, parameter :: hyperparameters_start=2, hyperparameters_end=6, separator_line=7   ! line numbers
    integer, parameter :: net_config_start=8, net_config_end=12                         ! line numbers
    integer, parameter :: file_start=hyperparameters_start-1, file_end=net_config_end+1 ! line numbers
#if defined __INTEL_COMPILER || _CRAYFTN
    type(double_precision_string_t), allocatable :: lines(:)
#endif

    training_configuration%double_precision_file_t = file_object

#if defined __INTEL_COMPILER || _CRAYFTN
    lines = training_configuration%double_precision_file_t%double_precision_lines()

    call assert(adjustl(lines(file_start)%string()) == header, &
      "training_configuration_s(double_precision_from_file): header",lines(file_start))

    training_configuration%hyperparameters_ = hyperparameters_t(lines(hyperparameters_start:hyperparameters_end))

    call assert(adjustl(lines(separator_line)%string()) == separator, &
      "training_configuration_s(double_precision_from_file): separator", lines(file_start))

    training_configuration%network_configuration_= network_configuration_t(lines(net_config_start:net_config_end))

    call assert(adjustl(lines(file_end)%string()) == footer, &
      "training_configuration_s(double_precision_from_file): footer", lines(file_end))
#else

    associate(lines => training_configuration%double_precision_file_t%double_precision_lines())

      call assert(adjustl(lines(file_start)%string()) == header, &
        "training_configuration_s(double_precision_from_file): header", lines(file_start))

      training_configuration%hyperparameters_ = hyperparameters_t(lines(hyperparameters_start:hyperparameters_end))

      call assert(adjustl(lines(separator_line)%string()) == separator, &
        "training_configuration_s(double_precision_from_file): separator", lines(file_start))

      training_configuration%network_configuration_= network_configuration_t(lines(net_config_start:net_config_end))

      call assert(adjustl(lines(file_end)%string()) == footer, &
        "training_configuration_s(double_precision_from_file): footer", lines(file_end))

    end associate

#endif

  end procedure

  module procedure default_real_to_json
    json_lines = self%lines()
  end procedure

  module procedure double_precision_to_json
    json_lines = self%lines()
  end procedure

  module procedure default_real_equals
    lhs_eq_rhs = &
      lhs%hyperparameters_ == rhs%hyperparameters_ .and. &
      lhs%network_configuration_ == rhs%network_configuration_
  end procedure

  module procedure double_precision_equals
    lhs_eq_rhs = &
      lhs%hyperparameters_ == rhs%hyperparameters_ .and. &
      lhs%network_configuration_ == rhs%network_configuration_
  end procedure

  module procedure default_real_mini_batches
    num_mini_batches = self%hyperparameters_%mini_batches()
  end procedure

  module procedure double_precision_mini_batches
    num_mini_batches = self%hyperparameters_%mini_batches()
  end procedure

  module procedure default_real_optimizer_name
    identifier = self%hyperparameters_%optimizer_name()
  end procedure

  module procedure double_precision_optimizer_name
    identifier = self%hyperparameters_%optimizer_name()
  end procedure

  module procedure default_real_learning_rate
    rate = self%hyperparameters_%learning_rate()
  end procedure

  module procedure double_precision_learning_rate
    rate = self%hyperparameters_%learning_rate()
  end procedure

  module procedure default_real_nodes_per_layer
    nodes = self%network_configuration_%nodes_per_layer()
  end procedure

  module procedure double_precision_nodes_per_layer
    nodes = self%network_configuration_%nodes_per_layer()
  end procedure

  module procedure default_real_skip_connections
    using_skip = self%network_configuration_%skip_connections()
  end procedure

  module procedure double_precision_skip_connections
    using_skip = self%network_configuration_%skip_connections()
  end procedure

  module procedure default_real_differentiable_activation
#if defined __INTEL_COMPILER || _CRAYFTN
    type(string_t) :: activation_name
    activation_name = self%network_configuration_%activation_name()
#else
    associate(activation_name => self%network_configuration_%activation_name())
#endif
      select case(activation_name%string())
        case ("gelu")
          activation = activation_t(gelu)
        case ("relu")
          activation = activation_t(relu)
        case ("sigmoid")
          activation = activation_t(sigmoid)
        case ("swish")
          activation = activation_t(swish)
        case default
          error stop 'activation_factory_s(factory): unrecognized activation name "' // activation_name%string() // '"' 
      end select
#if defined __INTEL_COMPILER || _CRAYFTN
#else
    end associate
#endif
  end procedure

  module procedure double_precision_differentiable_activation
#if defined __INTEL_COMPILER || _CRAYFTN
    type(string_t) :: activation_name
    activation_name = self%network_configuration_%activation_name()
#else
    associate(activation_name => self%network_configuration_%activation_name())
#endif
      select case(activation_name%string())
        case ("gelu")
          activation = activation_t(gelu)
        case ("relu")
          activation = activation_t(relu)
        case ("sigmoid")
          activation = activation_t(sigmoid)
        case ("swish")
          activation = activation_t(swish)
        case default
          error stop 'activation_factory_s(factory): unrecognized activation name "' // activation_name%string() // '"' 
      end select
#if defined __INTEL_COMPILER || _CRAYFTN
#else
    end associate
#endif
  end procedure

end submodule training_configuration_s
