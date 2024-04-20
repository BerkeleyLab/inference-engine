! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module network_configuration_test_m
  !! Test network_configuration_t object I/O and construction

  ! External dependencies
  use inference_engine_m, only : network_configuration_t
  use sourcery_m, only : test_t, test_result_t, test_description_t, test_description_substring, string_t, file_t
#ifdef __GFORTRAN__
  use sourcery_m, only : test_function_i
#endif

  ! Internal dependencies
  use network_configuration_m, only : network_configuration_t
  implicit none

  private
  public :: network_configuration_test_t

  type, extends(test_t) :: network_configuration_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "A network_configuration_t object"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#ifndef __GFORTRAN__
    test_descriptions = [ & 
      test_description_t( &
        string_t("component-wise construction followed by conversion to and from JSON"), &
        write_then_read_network_configuration) &
    ]
#else
    procedure(test_function_i), pointer :: check_write_then_read_ptr
    check_write_then_read_ptr => write_then_read_network_configuration

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

  function write_then_read_network_configuration() result(test_passes)
    logical test_passes

    associate(constructed_from_components=> &
      network_configuration_t(skip_connections=.false., nodes_per_layer=[2,72,2], activation_name="sigmoid"))
      associate(constructed_from_json => network_configuration_t(constructed_from_components%to_json()))
        test_passes = constructed_from_components == constructed_from_json 
      end associate
    end associate

  end function

end module network_configuration_test_m
