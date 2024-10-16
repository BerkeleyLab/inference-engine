! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module metadata_test_m
  !! Test metadata_t object I/O and construction

  ! External dependencies
  use fiats_m, only : metadata_t
  use julienne_m, only : test_t, test_result_t, test_description_t, test_description_substring, string_t
#ifdef __GFORTRAN__
  use julienne_m, only : test_function_i
#endif

  ! Internal dependencies
  use metadata_m, only : metadata_t

  implicit none

  private
  public :: metadata_test_t

  type, extends(test_t) :: metadata_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "A metadata_t object"
  end function

  function results() result(test_results)
    type(test_description_t), allocatable :: test_descriptions(:)
    type(test_result_t), allocatable :: test_results(:)

#ifndef __GFORTRAN__
    test_descriptions = [ & 
      test_description_t( &
        string_t("component-wise construction followed by conversion to and from JSON"), &
        write_then_read_metadata) &
    ]
#else
    procedure(test_function_i), pointer :: check_write_then_read_ptr
    check_write_then_read_ptr => write_then_read_metadata

    test_descriptions = [ &
      test_description_t( &
        string_t("component-wise construction followed by conversion to and from JSON"), &
        check_write_then_read_ptr) &
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

  function write_then_read_metadata() result(test_passes)
    logical test_passes
#ifdef _CRAYFTN
    type(metadata_t) :: metadata, from_json
    metadata = metadata_t( &
      modelName = string_t("Metadata Unit Test"), &
      modelAuthor = string_t("Julienne"), &
      compilationDate = string_t("2024-06-27"), &
      activationFunction = string_t("sigmoid"), &
      usingSkipConnections = string_t("false") &
    ) &
    from_json = metadata_t(metadata%to_json())
#else
    associate(metadata => &
      metadata_t( &
        modelName = string_t("Metadata Unit Test"), &
        modelAuthor = string_t("Julienne"), &
        compilationDate = string_t("2024-06-27"), &
        activationFunction = string_t("sigmoid"), &
        usingSkipConnections = string_t("false") &
      ) &
    )
      associate(from_json => metadata_t(metadata%to_json()))
#endif
        test_passes = metadata == from_json
#ifndef _CRAYFTN
      end associate
    end associate
#endif
  end function

end module metadata_test_m
