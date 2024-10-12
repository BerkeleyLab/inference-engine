module activation_m
  use iso_c_binding, only :  c_int
  use julienne_m, only : string_t
  implicit none

  private
  public :: activation_t
  public :: gelu, relu, sigmoid, step, swish

  enum, bind(C)
    enumerator :: gelu=1, relu, sigmoid, step, swish
  end enum

  character(len=*), parameter :: activation_name(*) =  [character(len("sigmoid")) :: "gelu", "relu", "sigmoid", "step", "swish"]

  type activation_t
    private
    integer(c_int) selection_ = sigmoid
  contains
    procedure, non_overridable :: function_name
    generic :: operator(==) => equals
    procedure, private      :: equals
    generic :: evaluate        => default_real_evaluate     , double_precision_evaluate
    procedure, non_overridable :: default_real_evaluate     , double_precision_evaluate
    generic :: differentiate   => default_real_differentiate, double_precision_differentiate
    procedure, non_overridable :: default_real_differentiate, double_precision_differentiate
  end type

  interface activation_t

    elemental module function construct_from_component(selection) result(activation)
      implicit none
      integer(c_int), intent(in) :: selection
      type(activation_t) activation
    end function
    
    elemental module function construct_from_name(name) result(activation)
      implicit none
      character(len=*), intent(in) :: name
      type(activation_t) activation
    end function

  end interface

  interface

    elemental module function equals(self, rhs) result(self_eq_rhs)
      implicit none
      class(activation_t), intent(in) :: self
      type(activation_t), intent(in) :: rhs
      logical self_eq_rhs
    end function

    elemental module function default_real_evaluate(self, x) result(y)
      implicit none
      class(activation_t), intent(in) :: self
      real, intent(in) :: x 
      real y 
    end function

    elemental module function double_precision_evaluate(self, x) result(y)
      implicit none
      class(activation_t), intent(in) :: self
      double precision, intent(in) :: x 
      double precision y 
    end function 

    elemental module function default_real_differentiate(self, x) result(dy_dx)
      implicit none
      class(activation_t), intent(in) :: self
      real, intent(in) :: x 
      real dy_dx 
    end function

    elemental module function double_precision_differentiate(self, x) result(dy_dx)
      implicit none
      class(activation_t), intent(in) :: self
      double precision, intent(in) :: x 
      double precision dy_dx 
    end function

    elemental module function function_name(self) result(string)
      implicit none
      class(activation_t), intent(in) :: self
      type(string_t) string
    end function

  end interface

end module activation_m
