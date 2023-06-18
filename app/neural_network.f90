program neural_network
  use trainable_engine_m, only : trainable_engine_t
  use string_m, only : string_t
  use sigmoid_m, only : sigmoid_t
  use kind_parameters_m, only : rkind
  use file_m, only : file_t
  use matmul_m, only : matmul_t 
  use outputs_m, only : outputs_t
  use expected_outputs_m, only : expected_outputs_t
  use inputs_m, only : inputs_t
  implicit none
  integer i,j,k,l,n,n_outer
  integer nhidden,nodes_max
  integer n_outer_iterations,n_inner_iterations
  double precision r,eta,ir,rr
  double precision cost
  integer, allocatable :: nodes(:)
  double precision, allocatable :: w(:,:,:),z(:,:),b(:,:),a(:,:),y(:),delta(:,:)
  double precision, allocatable :: dcdw(:,:,:),dcdb(:,:)
  type(sigmoid_t) sigmoid
  type(expected_outputs_t) expected_y
  real, parameter :: false = 0._rkind, true = 1._rkind
  double precision, allocatable :: harvest(:)

  open(unit=8,file='cost')
  nhidden = 2
  n_inner_iterations = 200
  n_outer_iterations = 50000
  
  allocate(nodes(0:nhidden+1))
  ! Number of nodes in each layes
  nodes(0) = 2 ! Number of nodes in the input layer
  nodes(1) = 3
  nodes(2) = 3
  nodes(3) = 1 ! Number of nodes in the output layer

  nodes_max = maxval(nodes)

  eta = 1.5d0 ! Learning parameter
  
  allocate(a(nodes_max,0:nhidden+1)) ! Activations, Layer 0: Inputs, Layer nhidden+1: Outputs
  allocate(z(nodes_max,nhidden+1)) ! z-values: Sum z_j^l = w_jk^{l} a_k^{l-1} + b_j^l
  allocate(w(nodes_max,nodes_max,nhidden+1)) ! Weights w_{jk}^l is the weight from the k'th neuron in the (l-1)'th layer to the j'th neuron in the l'th layer
  allocate(b(nodes_max,nhidden+1)) ! Bias b_j^l is the bias in j'th neuron of the l'th layer
  allocate(delta(nodes_max,nhidden+1))
  allocate(dcdw(nodes_max,nodes_max,nhidden+1)) ! Gradient of cost function with respect to weights
  allocate(dcdb(nodes_max,nhidden+1)) ! Gradient of cost function with respect with biases
  allocate(y(nodes(nhidden+1))) ! Desired output
  allocate(harvest(nodes(0)))

  w = 0.d0 ! Initialize weights
  b = 0.d0 ! Initialize biases

  call random_init(image_distinct=.true., repeatable=.true.)
  
  do n_outer = 1,n_outer_iterations

     cost = 0.d0
     dcdw = 0.d0
     dcdb = 0.d0
     
     do n = 1,n_inner_iterations

        ! Create an AND gate
        call random_number(harvest)
        a(:,0) = merge(true, false, harvest < 0.5D0)
        expected_y = and(inputs_t(a(1:nodes(0),0)))
        y = expected_y%outputs()

        ! Feedforward
        do l = 1,nhidden+1
           do j = 1,nodes(l)
              z(j,l) = 0.d0
              do k = 1,nodes(l-1)
                 z(j,l) = z(j,l) + w(j,k,l)*a(k,l-1)
              end do
              z(j,l) = z(j,l) + b(j,l)
              a(j,l) = sigmoid%activation(real(z(j,l), kind(1.)))
           end do
        end do

        do k = 1,nodes(nhidden+1)
           cost = cost + (y(k)-a(k,nhidden+1))**2
        end do
     
        do k = 1,nodes(nhidden+1)
           delta(k,nhidden+1) = (a(k,nhidden+1)-y(k))*sigmoid%activation_derivative(real(z(k,nhidden+1), kind(1.)))
        end do

        ! Backpropagate the error
        do l = nhidden,1,-1
           do j = 1,nodes(l)
              delta(j,l) = 0.d0
              do k = 1,nodes(l+1)
                 delta(j,l) = delta(j,l) + w(k,j,l+1)*delta(k,l+1)
              end do
              delta(j,l) = delta(j,l)*sigmoid%activation_derivative(real(z(j,l), kind(1.)))
           end do
        end do

        ! Sum up gradients in the inner iteration
        do l = 1,nhidden+1
            do j = 1,nodes(l)
              do k = 1,nodes(l-1)
                 dcdw(j,k,l) = dcdw(j,k,l) + a(k,l-1)*delta(j,l)
              end do
              dcdb(j,l) = dcdb(j,l) + delta(j,l)
           end do
         end do
     
     end do
  
     cost = cost/(2.d0*dble(n_inner_iterations))
     write(8,*) n_outer,log10(cost)

     do l = 1,nhidden+1
        do j = 1,nodes(l)
           do k = 1,nodes(l-1)
              dcdw(j,k,l) = dcdw(j,k,l)/dble(n_inner_iterations)
              w(j,k,l) = w(j,k,l) - eta*dcdw(j,k,l) ! Adjust weights
           end do
           dcdb(j,l) = dcdb(j,l)/dble(n_inner_iterations)
           b(j,l) = b(j,l) - eta*dcdb(j,l) ! Adjust biases
        end do
     end do

  end do

  block
    type(trainable_engine_t) trainable_engine
    type(outputs_t) outputs

    trainable_engine = trainable_engine_t(nodes, real(w, rkind), real(b, rkind), sigmoid_t(), &
      metadata = [ & 
        string_t("2-hidden-layer network"), string_t("Damian Rouson"), string_t("2023-06-18"), string_t("sigmoid"), &
        string_t("false") &
      ] &
    )
    outputs =  trainable_engine%infer([real(rkind):: 1.,1.], matmul_t())
    print *, outputs%outputs()
    outputs =  trainable_engine%infer([real(rkind):: 0.,1.], matmul_t())
    print *, outputs%outputs()
    outputs =  trainable_engine%infer([real(rkind):: 1.,0.], matmul_t())
    print *, outputs%outputs()
    outputs =  trainable_engine%infer([real(rkind):: 0.,0.], matmul_t())
    print *, outputs%outputs()
  end block

contains

  elemental function and(inputs_object) result(expected_outputs_object)
     type(inputs_t), intent(in) :: inputs_object 
     type(expected_outputs_t) expected_outputs_object 
     expected_outputs_object = expected_outputs_t([merge(false, true, sum(inputs_object%inputs())<=1.5_rkind)])
  end function

end program neural_network
