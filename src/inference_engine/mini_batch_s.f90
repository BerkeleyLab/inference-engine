submodule(mini_batch_m) mini_batch_s
  use assert_m, only : assert
  implicit none

contains

    module procedure construct
      mini_batch%input_output_pairs_ = input_output_pairs
      mini_batch%network_increments_ = network_increments
    end procedure

    module procedure average_increment

      type(network_increment_t) total
      integer i

      associate(num_increments => size(self%network_increments_))

        call assert(num_increments > 0, "mini_batch_s average_increment: num_increments > 0") 

        total = self%network_increments_(1)

        do i = 2, num_increments ! In Fortran 2023, this could be a concurrent reduction
          total = total + self%network_increments_(i)
        end do

        average = total/num_increments

      end associate
    end procedure

end submodule mini_batch_s
