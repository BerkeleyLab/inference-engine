submodule(training_configuration_m) training_configuration_s
  use assert_m, only : assert
  implicit none

  character(len=*), parameter :: header="{", footer="}", separator = ","

contains

  module procedure from_components

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

  module procedure from_file
    integer, parameter :: hyperparameters_start=2, hyperparameters_end=6, separator_line=7   ! line numbers
    integer, parameter :: net_config_start=8, net_config_end=12                         ! line numbers
    integer, parameter :: file_start=hyperparameters_start-1, file_end=net_config_end+1 ! line numbers

    training_configuration%file_t = file_object

    associate(lines => training_configuration%file_t%lines())
      call assert(trim(adjustl(lines(file_start)%string()))==header,"training_configuration_s(from_file): header",lines(file_start))
      training_configuration%hyperparameters_ = hyperparameters_t(lines(hyperparameters_start:hyperparameters_end))
      call assert(trim(adjustl(lines(separator_line)%string()))==separator,"training_configuration_s(from_file): separator", &
        lines(file_start))
      training_configuration%network_configuration_= network_configuration_t(lines(net_config_start:net_config_end))
      call assert(trim(adjustl(lines(file_end)%string()))==footer, "training_configuration_s(from_file): footer", lines(file_end))
    end associate
  end procedure

  module procedure to_json
    json_lines = self%lines()
  end procedure

  module procedure equals
    lhs_eq_rhs = &
      lhs%hyperparameters_ == rhs%hyperparameters_ .and. &
      lhs%network_configuration_ == rhs%network_configuration_
  end procedure

end submodule training_configuration_s
