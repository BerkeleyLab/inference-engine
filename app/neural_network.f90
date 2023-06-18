module activation_m
  implicit none

contains

  double precision function sigmoid(z)
    double precision z
    sigmoid = 1.d0/(1.d0 + exp(-z))
  end function sigmoid
  
  double precision function deriv_sigmoid(z)
    double precision z
    deriv_sigmoid = exp(-z)/(1.d0 + exp(-z))**2
  end function deriv_sigmoid

end module

program neural_network
  use activation_m, only : sigmoid, deriv_sigmoid
  implicit none
  integer i,j,k,l,n,n_outer
  integer nhidden,nodes_max
  integer n_outer_iterations,n_inner_iterations
  double precision r,eta,ir,rr
  double precision cost
  integer, allocatable :: nodes(:)
  double precision, allocatable :: w(:,:,:),z(:,:),b(:,:),a(:,:),y(:),delta(:,:)
  double precision, allocatable :: dcdw(:,:,:),dcdb(:,:)

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

  w = 0.d0 ! Initialize weights
  b = 0.d0 ! Initialize biases

  
  do n_outer = 1,n_outer_iterations

     cost = 0.d0
     dcdw = 0.d0
     dcdb = 0.d0
     
     do n = 1,n_inner_iterations

        ! Create an AND gate
        do k = 1,nodes(0)
           r = rand(0)
           if (r .lt. .5d0) then
              ir = 1.d0
           else
              ir = 0.d0
           end if
           a(k,0) = ir
        end do

        if (a(1,0) + a(2,0) .le. 1.5d0) then
           y(1) = 0.d0 
        else
           y(1) = 1.d0 
        end if

        ! Feedforward
        do l = 1,nhidden+1
           do j = 1,nodes(l)
              z(j,l) = 0.d0
              do k = 1,nodes(l-1)
                 z(j,l) = z(j,l) + w(j,k,l)*a(k,l-1)
              end do
              z(j,l) = z(j,l) + b(j,l)
              a(j,l) = sigmoid(z(j,l))
           end do
        end do

        do k = 1,nodes(nhidden+1)
           cost = cost + (y(k)-a(k,nhidden+1))**2
        end do
     
        do k = 1,nodes(nhidden+1)
           delta(k,nhidden+1) = (a(k,nhidden+1)-y(k))*deriv_sigmoid(z(k,nhidden+1))
        end do

        ! Backpropagate the error
        do l = nhidden,1,-1
           do j = 1,nodes(l)
              delta(j,l) = 0.d0
              do k = 1,nodes(l+1)
                 delta(j,l) = delta(j,l) + w(k,j,l+1)*delta(k,l+1)
              end do
              delta(j,l) = delta(j,l)*deriv_sigmoid(z(j,l))
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

end program neural_network
