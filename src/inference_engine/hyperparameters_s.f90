submodule(hyperparameters_m) hyperparameters_s
  use assert_m, only : assert, intrinsic_array_t
  use sourcery_m, only : string_t
  implicit none

contains

  module procedure construct_from_json_file
    integer l
    type(string_t), allocatable :: lines(:)

    lines = file_%lines()

    l = 1
    call assert(adjustl(lines(l)%string())=="{", 'construct_from_json_file: adjustl(lines(l)%string())=="{"', lines(l)%string())

 !{
 !     "activation" : "sigmoid",
 !     "num_mini_batches" : 10,
 !     "nodes per layer" : [2, 72, 2],
 !     "initialization" : {
 !         "type" : "perturbed identity",
 !         "parameters" : [ { "spread" :  0.05 } ]
 !      }
 !}

       
    l = l + 1
    call assert(adjustl(lines(l)%string())=="}", 'construct_from_json_file: adjustl(lines(l)%string())=="}"', lines(l)%string())
  end procedure

  module procedure to_json
    type(string_t), allocatable :: lines(:)
    integer, parameter :: outer_object_braces = 2
    integer, parameter :: num_lines = outer_object_braces
    integer l

    allocate(lines(num_lines))

    l = 1
    lines(l) = string_t('{')

    l = l + 1
    !lines(line) = string_t('        "modelName": "' // &
                                                   !self%metadata_(findloc(key, "modelName", dim=1))%string() // '",')



    l = l + 1
    call assert(l == num_lines, "hyperparameters_s(to_json): l == num_lines", intrinsic_array_t([l,num_lines]))
    lines(l) = string_t('}')
  end procedure

end submodule hyperparameters_s
