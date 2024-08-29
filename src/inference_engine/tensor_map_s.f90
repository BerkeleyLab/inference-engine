! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(tensor_map_m) tensor_map_s
  use assert_m, only : assert
  use julienne_m, only : separated_values
  use kind_parameters_m, only : default_real
  implicit none
  
contains

  module procedure construct_default_real
    call assert(size(minima)==size(maxima),"tensor_map_s(construct_default_real): size(minima)==size(maxima)")
    tensor_map%layer_ = layer
    tensor_map%intercept_ = minima
    tensor_map%slope_ = maxima - minima
  end procedure

  module procedure construct_double_precision
    call assert(size(minima)==size(maxima),"tensor_map_s(construct_double_precision): size(minima)==size(maxima)")
    tensor_map%layer_ = layer
    tensor_map%intercept_ = minima
    tensor_map%slope_ = maxima - minima
  end procedure

  module procedure from_json
    logical tensor_map_key_found
    integer l 

    tensor_map_key_found = .false.

    do l=1,size(lines)
      if (lines(l)%get_json_key() == "inputs_map" .or. lines(l)%get_json_key() == "outputs_map") then
        tensor_map_key_found = .true.
        tensor_map%layer_  = lines(l+1)%get_json_value(key=string_t("layer"), mold=string_t(""))
        tensor_map%intercept_ = lines(l+2)%get_json_value(key=string_t("intercept"), mold=[0.])
        tensor_map%slope_ = lines(l+3)%get_json_value(key=string_t("slope"), mold=[0.])
        return
      end if
    end do 

    call assert(tensor_map_key_found, "tensor_map_s(from_json): 'tensor_map' key found")
  end procedure

  module procedure default_real_equals
    real, parameter :: tolerance = 1.E-08

    call assert(allocated(lhs%layer_) .and. allocated(rhs%layer_), &
      "tensor_map_s(default_real_equals): allocated layer_ components")
    call assert(allocated(lhs%intercept_) .and. allocated(rhs%intercept_), &
      "tensor_map_s(default_real_equals): allocated intercept_ components)")
    call assert(allocated(lhs%slope_) .and.  allocated(rhs%slope_), &
      "tensor_map_s(default_real_equals): allocated slope_ components)")
    call assert(size(lhs%intercept_) == size(rhs%intercept_), &
       "tensor_map_s(default_real_equals): size(lhs%intercept_) == size(rhs%intercept_)")
    call assert(size(lhs%slope_) == size(rhs%slope_), &
       "tensor_map_s(default_real_equals): size(lhs%slope_) == size(rhs%slope_)")

    lhs_equals_rhs = &
      lhs%layer_ == rhs%layer_ .and. &
      all(abs(lhs%intercept_ - rhs%intercept_) <= tolerance).and. &
      all(abs(lhs%slope_ - rhs%slope_) <= tolerance)
  end procedure 

  module procedure double_precision_equals
    double precision, parameter :: tolerance = 1.D-015

    call assert(allocated(lhs%layer_) .and. allocated(rhs%layer_), &
      "tensor_map_s(double_precision_equals): allocated layer_ components")
    call assert(allocated(lhs%intercept_) .and. allocated(rhs%intercept_), &
      "tensor_map_s(double_precision_equals): allocated intercept_ components)")
    call assert(allocated(lhs%slope_) .and.  allocated(rhs%slope_), &
      "tensor_map_s(double_precision_equals): allocated slope_ components)")
    call assert(size(lhs%intercept_) == size(rhs%intercept_), &
      "tensor_map_s(double_precision_equals): size(lhs%intercept_) == size(rhs%intercept_)")
    call assert(size(lhs%slope_) == size(rhs%slope_), &
      "tensor_map_s(double_precision_equals): size(lhs%slope_) == size(rhs%slope_)")

    lhs_equals_rhs = &
      lhs%layer_ == rhs%layer_ .and. &
      all(abs(lhs%intercept_ - rhs%intercept_) <= tolerance).and. &
      all(abs(lhs%slope_ - rhs%slope_) <= tolerance)
  end procedure 

  module procedure default_real_to_json
    integer, parameter :: characters_per_value=17
    character(len=*), parameter :: indent = repeat(" ",ncopies=4)
    character(len=:), allocatable :: csv_format, intercept_string, slope_string

    call assert(allocated(self%layer_), &
      "tensor_map_s(default_real_to_json): allocated layer_")
    call assert(allocated(self%intercept_) .and. allocated(self%slope_), &
      "tensor_map_s(default_real_to_json): allocated intercept_/slope_")

    csv_format = separated_values(separator=",", mold=[real(default_real)::])
    allocate(character(len=size(self%intercept_)*(characters_per_value+1)-1)::intercept_string)
    allocate(character(len=size(self%slope_)*(characters_per_value+1)-1)::slope_string)
    write(intercept_string, fmt = csv_format) self%intercept_
    write(slope_string, fmt = csv_format) self%slope_

    block 
      character(len=:), allocatable :: layer
      layer = trim(adjustl(self%layer_))
      lines = [ &
        string_t(indent // '"'//layer//'_map": {'), &
        string_t(indent // '  "layer": "' // layer // '",'), &
        string_t(indent // '  "intercept": [' // trim(adjustl(intercept_string)) // '],'), & 
        string_t(indent // '  "slope": [' // trim(adjustl(slope_string)) // ']'), &
        string_t(indent // '}') &
      ]
    end block
  end procedure

  module procedure double_precision_to_json
    integer, parameter :: characters_per_value=34
    character(len=*), parameter :: indent = repeat(" ",ncopies=4)
    character(len=:), allocatable :: csv_format, intercept_string, slope_string

    call assert(allocated(self%layer_), &
      "tensor_map_s(default_real_to_json): allocated layer_")
    call assert(allocated(self%intercept_) .and. allocated(self%slope_), &
      "tensor_map_s(default_real_to_json): allocated intercept_/slope_")

    csv_format = separated_values(separator=",", mold=[double precision::])
    allocate(character(len=size(self%intercept_)*(characters_per_value+1)-1)::intercept_string)
    allocate(character(len=size(self%slope_)*(characters_per_value+1)-1)::slope_string)
    write(intercept_string, fmt = csv_format) self%intercept_
    write(slope_string, fmt = csv_format) self%slope_

    block 
      character(len=:), allocatable :: layer
      layer = trim(adjustl(self%layer_))
      lines = [ &
        string_t(indent // '"'//layer//'_map": {'), &
        string_t(indent // '  "layer": "' // layer // '",'), &
        string_t(indent // '  "intercept": [' // trim(adjustl(intercept_string)) // '],'), & 
        string_t(indent // '  "slope": [' // trim(adjustl(slope_string)) // ']'), &
        string_t(indent // '}') &
      ]
    end block
  end procedure

  module procedure default_real_map_to_training_range
    associate(tensor_values => tensor%values())
      associate(normalized_values => (tensor_values - self%intercept_)/self%slope_)
        normalized_tensor = tensor_t(normalized_values)
      end associate
    end associate
  end procedure

  module procedure double_precision_map_to_training_range
    associate(tensor_values => tensor%values())
      associate(normalized_values => (tensor_values - self%intercept_)/self%slope_)
        normalized_tensor = tensor_t(normalized_values)
      end associate
    end associate
  end procedure

  module procedure default_real_map_from_training_range
    associate(tensor_values => tensor%values())
      associate(unnormalized_values => self%intercept_ + tensor_values*self%slope_)
        unnormalized_tensor = tensor_t(unnormalized_values)
      end associate
    end associate
  end procedure

  module procedure double_precision_map_from_training_range
    associate(tensor_values => tensor%values())
      associate(unnormalized_values => self%intercept_ + tensor_values*self%slope_)
        unnormalized_tensor = tensor_t(unnormalized_values)
      end associate
    end associate
  end procedure

end submodule tensor_map_s
