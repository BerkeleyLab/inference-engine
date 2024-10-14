! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module tensor_test_m
  !! Define inference tests and procedures required for reporting results

  ! External dependencies
  use kind_parameters_m, only : double_precision 
  use julienne_m, only : test_t, test_result_t, test_description_t, test_description_substring, string_t, file_t
#ifdef __GFORTRAN__
  use julienne_m, only : test_function_i
#endif

  ! Internal dependencies
  use fiats_m, only : tensor_t

  implicit none

  private
  public :: tensor_test_t

  type, extends(test_t) :: tensor_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "An tensor_t object" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#ifndef __GFORTRAN__
    test_descriptions = [ &
      test_description_t("double-precision construction and value extraction", double_precision_construction) &
    ]
#else
    procedure(test_function_i), pointer :: double_precision_construction_ptr
      
    double_precision_construction_ptr => double_precision_construction

    test_descriptions = [ &
      test_description_t("double-precision construction and value extraction", double_precision_construction_ptr) &
    ]
#endif
    associate( &
      substring_in_subject => index(subject(), test_description_substring) /= 0, &
      substring_in_description => test_descriptions%contains_text(string_t(test_description_substring)) &
    )
      test_descriptions = pack(test_descriptions, substring_in_subject .or. substring_in_description)
    end associate
    test_results = test_descriptions%run()
  end function

  function double_precision_construction() result(test_passes)
    logical test_passes
    type(tensor_t(double_precision)) tensor
    double precision, parameter :: tolerance = 1.0D-12
    double precision, parameter :: values(*) = [1.000000000001D-12]

    tensor = tensor_t(values) ! this will fail to compile if no double_precision constructor exists and the 
    associate(tensor_values => tensor%values())
      test_passes = kind(tensor_values) == double_precision .and. all(abs(tensor_values - values) < tolerance)
    end associate
  end function

end module tensor_test_m
