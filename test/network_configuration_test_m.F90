! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module network_configuration_test_m
  !! Test network_configuration_t object I/O and construction

  ! External dependencies
  use assert_m, only : assert
  use sourcery_m, only : string_t, test_t, test_result_t, file_t
  use inference_engine_m, only : network_configuration_t

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

    character(len=*), parameter :: longest_description = &
          "component-wise construction followed by conversion to and from JSON"

    associate( &
      descriptions => &
        [ character(len=len(longest_description)) :: &
          "component-wise construction followed by conversion to and from JSON" &
        ], &
      outcomes => &
        [ write_then_read_network_configuration() & 
        ] & 
    )
      call assert(size(descriptions) == size(outcomes), &
        "network_configuration_test_m(results): size(descriptions) == size(outcomes)")
      test_results = test_result_t(descriptions, outcomes)
    end associate
       
  end function

  function write_then_read_network_configuration() result(test_passes)
    logical test_passes
#ifdef _CRAYFTN
    type(network_configuration_t) :: constructed_from_components, constructed_from_json
    constructed_from_components= &
      network_configuration_t(skip_connections=.false., nodes_per_layer=[2,72,2], activation_name="sigmoid")
    constructed_from_json = network_configuration_t(constructed_from_components%to_json())
#else
    associate(constructed_from_components=> &
      network_configuration_t(skip_connections=.false., nodes_per_layer=[2,72,2], activation_name="sigmoid"))
      associate(constructed_from_json => network_configuration_t(constructed_from_components%to_json()))
#endif
        test_passes = constructed_from_components == constructed_from_json 
#ifndef _CRAYFTN
      end associate
    end associate
#endif
  end function

end module network_configuration_test_m
