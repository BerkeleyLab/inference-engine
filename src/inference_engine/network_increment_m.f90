module network_increment_m
  use kind_parameters_m, only : rkind
  implicit none

  private
  public :: network_increment_t

  type network_increment_t
    private
    real(rkind), allocatable :: delta_w_in_(:,:)
    real(rkind), allocatable :: delta_w_hidden_(:,:,:)
    real(rkind), allocatable :: delta_w_out_(:,:)
    real(rkind), allocatable :: delta_b_(:,:)
    real(rkind), allocatable :: delta_b_out_(:)
  contains
    procedure, private :: add
    generic :: operator(+) => add
    procedure, private :: divide
    generic :: operator(/) => divide
  end type

  interface network_increment_t

    pure module function construct(delta_w_in, delta_w_hidden, delta_w_out, delta_b, delta_b_out) result(network_increment)
      implicit none
      real(rkind), allocatable, intent(in) :: delta_w_in(:,:)
      real(rkind), allocatable, intent(in) :: delta_w_hidden(:,:,:)
      real(rkind), allocatable, intent(in) :: delta_w_out(:,:)
      real(rkind), allocatable, intent(in) :: delta_b(:,:)
      real(rkind), allocatable, intent(in) :: delta_b_out(:)
      type(network_increment_t) network_increment
    end function

  end interface

  interface

    pure module function add(lhs, rhs) result(total)
      implicit none
      class(network_increment_t), intent(in) :: lhs
      type(network_increment_t), intent(in) :: rhs
      type(network_increment_t) total
    end function

    pure module function divide(numerator, denominator) result(ratio)
      implicit none
      class(network_increment_t), intent(in) :: numerator
      integer, intent(in) :: denominator
      type(network_increment_t) ratio
    end function

  end interface

end module network_increment_m
