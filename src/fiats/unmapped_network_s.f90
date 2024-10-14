! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(neural_network_m) unmapped_network_s
  implicit none

  integer, parameter :: input_layer = 0 

contains

  module procedure double_precision_unmapped_from_json
    unmapped_network%neural_network_ = double_precision_from_json(file)
  end procedure

  module procedure default_real_infer_unmapped

    real, allocatable :: a(:,:)
    integer l

    associate(neural_network => self%neural_network_)

      call neural_network%assert_consistency()

      associate( &
        w => neural_network%weights_ &
       ,b => neural_network%biases_ &
       ,n => neural_network%nodes_ &
       ,output_layer => ubound(neural_network%nodes_,1) &
      )
        allocate(a(maxval(n), input_layer:output_layer))

        a(1:n(input_layer),input_layer) = inputs%values()

        feed_forward: &
        do l = input_layer+1, output_layer
          associate(z => matmul(w(1:n(l),1:n(l-1),l), a(1:n(l-1),l-1)) + b(1:n(l),l))
            if (l .lt. output_layer) then
               a(1:n(l),l) = neural_network%activation_%evaluate(z)
            else
               a(1:n(l),l) = z(1:n(l))
            end if
          end associate
        end do feed_forward

        outputs = tensor_t(a(1:n(output_layer), output_layer))

      end associate
    end associate

  end procedure

  module procedure double_precision_infer_unmapped

    double precision, allocatable :: a(:,:)
    integer l

    associate(neural_network => self%neural_network_)

      call neural_network%assert_consistency()

      associate( &
        w => neural_network%weights_ &
       ,b => neural_network%biases_ &
       ,n => neural_network%nodes_ &
       ,output_layer => ubound(neural_network%nodes_,1) &
      )

        allocate(a(maxval(n), input_layer:output_layer))

        a(1:n(input_layer),input_layer) = inputs%values()

        feed_forward: &
        do l = input_layer+1, output_layer
          associate(z => matmul(w(1:n(l),1:n(l-1),l), a(1:n(l-1),l-1)) + b(1:n(l),l))
            if (l .lt. output_layer) then
               a(1:n(l),l) = neural_network%activation_%evaluate(z)
            else
               a(1:n(l),l) = z(1:n(l))
            end if
          end associate
        end do feed_forward

        outputs = tensor_t(a(1:n(output_layer), output_layer))
      end associate
    end associate

  end procedure

end submodule unmapped_network_s
