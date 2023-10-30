submodule(hyperparameters_m) hyperparameters_s
  use assert_m, only : assert
  implicit none

  character(len=*), parameter :: mini_batches_key  = "mini-batches"
  character(len=*), parameter :: learning_rate_key = "learning rate"
  character(len=*), parameter :: optimizer_key     = "optimizer"

contains

  module procedure from_json
    type(string_t), allocatable :: lines(:)
    integer l
    logical hyperparameters_key_found 

    lines = file_%lines()
    hyperparameters_key_found = .false.

    loop_through_file: &
    do l=1,size(lines)
      if (lines(l)%get_json_key() == "hyperparameters") then
        hyperparameters_key_found = .true.
        hyperparameters%mini_batches_  = lines(l+1)%get_json_value(string_t(mini_batches_key), mold=0)
        hyperparameters%learning_rate_ = lines(l+2)%get_json_value(string_t(learning_rate_key), mold=0.)
        hyperparameters%optimizer_ = lines(l+3)%get_json_value(string_t(optimizer_key), mold=string_t(""))
        return
      end if
    end do loop_through_file

    call assert(hyperparameters_key_found, "hyperparameters_s(from_json): hyperparameters_found")
  end procedure

  module procedure to_json
    character(len=*), parameter :: indent = repeat(" ",ncopies=4)
    integer, parameter :: max_digits = 12
    character(len=max_digits) mini_batches_string, learning_rate_string

    write(mini_batches_string,*) self%mini_batches_
    write(learning_rate_string,*) self%learning_rate_

    lines = [ &
      string_t(indent // '"hyperparameters": {'), &
      string_t(indent // indent // '"' // mini_batches_key  // '": '  // mini_batches_string   ), &
      string_t(indent // indent // '"' // learning_rate_key // '": '  // learning_rate_string  ), &
      string_t(indent // indent // '"' // optimizer_key     // '": "' // self%optimizer_ // '"'), &
      string_t(indent // '}') &
    ]
  end procedure

end submodule hyperparameters_s
