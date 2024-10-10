submodule(activation_m) activation_s
  implicit none

  real            , parameter :: pi    = 3.141592653589793
  double precision, parameter :: pi_dp = 3.141592653589793D0

contains

    module procedure construct_from_component
      activation%selection_ = selection
    end procedure

    module procedure construct_from_name
      select case(name)
        case("gelu")
          activation%selection_ = gelu
        case("relu")
          activation%selection_ = relu
        case("sigmoid")
          activation%selection_ = sigmoid
        case("step")
          activation%selection_ = step
        case("swish")
          activation%selection_ = swish
        case default
          error stop "activation_s(construct_from_name): unknown name"
      end select
    end procedure

    module procedure default_real_evaluate
      select case(self%selection_)
        case(gelu)
          y = .5*x*(1. + erf(x/sqrt(2.)))
        case(relu) 
          y = max(0., x)
        case(sigmoid)
          y =  1./(1.+exp(-x))
        case(step)
          y = merge(1., 0., x>0.)
        case(swish)
          associate(sigmoid_activation => 1./(1.+exp(-x)))
            y = x*sigmoid_activation
          end associate
        case default
          error stop "activation_s(default_real_evaluate): unknown activation"
      end select
    end procedure

    module procedure double_precision_evaluate
      select case(self%selection_)
        case(gelu)
          y = .5D0*x*(1D0 + erf(x/sqrt(2D0)))
        case(relu) 
          y = max(0D0, x)
        case(sigmoid)
          y =  1D0/(1D0+exp(-x))
        case(step)
          y = merge(1D0, 0D0, x>0D0)
        case(swish)
          associate(sigmoid_activation => 1D0/(1D0+exp(-x)))
            y = x*sigmoid_activation
          end associate
        case default
          error stop "activation_s(double_precision_evaluate): unknown activation"
      end select
    end procedure
  
    module procedure default_real_differentiate
      select case(self%selection_)
        case(gelu)
          dy_dx = .5*(1. + erf(x/sqrt(2.))) + x*exp(-x**2/2.)/sqrt(2*pi)
        case(relu) 
          dy_dx = merge(1., 0., x>0.)
        case(sigmoid)
          dy_dx = exp(-x)/(1.+exp(-x))**2
        case(step)
          error stop "activation_s(default_real_differentiate): non-differentiable activation"
        case(swish)
          associate(sigmoid_activation => 1./(1.+exp(-x)), sigmoid_differentiate => exp(-x)/(1.+exp(-x))**2)
            dy_dx = sigmoid_activation + x * sigmoid_differentiate
          end associate
        case default
          error stop "activation_s(default_real_differentiate): unknown activation"
      end select
    end procedure

    module procedure double_precision_differentiate
      select case(self%selection_)
        case(gelu)
          dy_dx = .5D0*(1D0 + erf(x/sqrt(2D0))) + x*exp(-x**2/2D0)/sqrt(2D0*pi_dp)
        case(relu) 
          dy_dx = merge(1D0, 0D0, x>0D0)
        case(sigmoid)
          dy_dx = exp(-x)/(1D0+exp(-x))**2
        case(step)
          error stop "activation_s(double_precision_differentiate): non-differentiable activation"
        case(swish)
          associate(sigmoid_activation => 1D0/(1D0+exp(-x)), sigmoid_differentiate => exp(-x)/(1D0+exp(-x))**2)
            dy_dx = sigmoid_activation + x * sigmoid_differentiate
          end associate
        case default
          error stop "activation_s(double_precision_differentiate): unknown activation"
      end select
    end procedure

    module procedure function_name
      select case(self%selection_)
        case(gelu)
          string = "gelu"
        case(relu) 
          string = "relu"
        case(sigmoid)
          string = "sigmoid"
        case(step)
          string = "step"
        case(swish)
          string = "swish"
        case default
          error stop "activation_s(function_name): unknown activation"
      end select
    end procedure
end submodule activation_s
