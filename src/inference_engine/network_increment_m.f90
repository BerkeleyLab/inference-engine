module network_increment_m
  use kind_parameters_m, only : rkind
  implicit none

  private
  public :: network_increment_t
  public :: operator(.average.)

  type network_increment_t
    private
    real(rkind), allocatable :: delta_w_in_(:,:)
    real(rkind), allocatable :: delta_w_hidden_(:,:,:)
    real(rkind), allocatable :: delta_w_out_(:,:)
    real(rkind), allocatable :: delta_b_hidden_(:,:)
    real(rkind), allocatable :: delta_b_out_(:)
  contains
    procedure, private :: add
    generic :: operator(+) => add
    procedure, private :: divide
    generic :: operator(/) => divide
    procedure :: delta_w_in
    procedure :: delta_w_hidden
    procedure :: delta_w_out
    procedure :: delta_b_hidden
    procedure :: delta_b_out
  end type

  interface network_increment_t

    pure module function construct(delta_w_in, delta_w_hidden, delta_w_out, delta_b_hidden, delta_b_out) result(network_increment)
      implicit none
      real(rkind), intent(in) :: delta_w_in(:,:)
      real(rkind), intent(in) :: delta_w_hidden(:,:,:)
      real(rkind), intent(in) :: delta_w_out(:,:)
      real(rkind), intent(in) :: delta_b_hidden(:,:)
      real(rkind), intent(in) :: delta_b_out(:)
      type(network_increment_t) network_increment
    end function

  end interface
 
  interface operator(.average.)

     pure module function average(rhs) result(average_increment)
       implicit none
       type(network_increment_t), intent(in) :: rhs(:)
       type(network_increment_t) average_increment
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
    
    pure module function delta_w_in(self) result(my_delta_w_in)
      implicit none
      class(network_increment_t), intent(in) :: self
      real(rkind), allocatable :: my_delta_w_in(:,:)
    end function

    pure module function delta_w_hidden(self) result(my_delta_w_hidden)
      implicit none
      class(network_increment_t), intent(in) :: self
      real(rkind), allocatable :: my_delta_w_hidden(:,:,:)
    end function

    pure module function delta_w_out(self) result(my_delta_w_out)
      implicit none
      class(network_increment_t), intent(in) :: self
      real(rkind), allocatable :: my_delta_w_out(:,:)
    end function

    pure module function delta_b_hidden(self) result(my_delta_b_hidden)
      implicit none
      class(network_increment_t), intent(in) :: self
      real(rkind), allocatable :: my_delta_b_hidden(:,:)
    end function

    pure module function delta_b_out(self) result(my_delta_b_out)
      implicit none
      class(network_increment_t), intent(in) :: self
      real(rkind), allocatable :: my_delta_b_out(:)
    end function

  end interface

end module network_increment_m
