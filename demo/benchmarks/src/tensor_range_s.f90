! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(range_m) range_s
  use assert_m, only : assert
  use kind_parameters_m, only : rkind
  implicit none
  
contains

  module procedure from_components
    call assert(size(minima)==size(maxima),"range_s(from_components): size(minima)==size(maxima)")
    range%layer_ = layer
    range%minima_ = minima
    range%maxima_ = maxima 
  end procedure

  module procedure equals
    real, parameter :: tolerance = 1.E-08

    call assert(allocated(lhs%layer_) .and. allocated(rhs%layer_), "range_s(equals): allocated layer_ components")
    call assert(allocated(lhs%minima_) .and. allocated(rhs%minima_), "range_s(equals): allocated minima_ components)")
    call assert(allocated(lhs%maxima_) .and.  allocated(rhs%maxima_), "range_s(equals): allocated maxima_ components)")
    call assert(size(lhs%minima_) == size(rhs%minima_), "range_s(equals): size(lhs%minima_) == size(rhs%minima_)")
    call assert(size(lhs%maxima_) == size(rhs%maxima_), "range_s(equals): size(lhs%maxima_) == size(rhs%maxima_)")

    lhs_equals_rhs = &
      lhs%layer_ == rhs%layer_ .and. &
      all(abs(lhs%minima_ - rhs%minima_) <= tolerance).and. &
      all(abs(lhs%maxima_ - rhs%maxima_) <= tolerance)
  end procedure 

  module procedure map_to_training_range
    associate(tensor_values => tensor%values())
      associate(normalized_values => (tensor_values - self%minima_)/(self%maxima_ - self%minima_))
        normalized_tensor = tensor_t(normalized_values)
      end associate
    end associate
  end procedure

  module procedure map_from_training_range
    associate(tensor_values => tensor%values())
      associate(unnormalized_values => self%minima_ + tensor_values*(self%maxima_ - self%minima_))
        unnormalized_tensor = tensor_t(unnormalized_values)
      end associate
    end associate
  end procedure

  module procedure in_range
    is_in_range = all(tensor%values() >= self%minima_) .and. all(tensor%values() <= self%maxima_)
  end procedure

end submodule range_s
