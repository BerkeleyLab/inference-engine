! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module outputs_m
  use kind_parameters_m, only : rkind
  implicit none

  private
  public :: outputs_t

  type outputs_t
    private
    real(rkind), allocatable :: outputs_(:) !! network outputs
    real(rkind), allocatable :: pre_activation_in_(:,:) !! weighted & biased hidden-layer inputs for training
    real(rkind), allocatable :: pre_activation_out_(:) !! weighted & biased output for training
  contains
    procedure outputs
  end type

  interface outputs_t

     pure module function construct_from_compoents(outputs, pre_activation_in, pre_activation_out) result(new_outputs_t)
       implicit none
       real(rkind), intent(in) :: outputs(:)
       real(rkind), intent(in) :: pre_activation_in(:,:), pre_activation_out(:)
       type(outputs_t) new_outputs_t
     end function

  end interface

  interface

    pure module function outputs(self) result(output_values)
      implicit none
      class(outputs_t), intent(in) :: self
      real(rkind), allocatable :: output_values(:)
    end function

  end interface

end module outputs_m
